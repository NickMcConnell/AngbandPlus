/*
 * File: display.c
 * Purpose: Display the character on the screen or in a file
 *
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


#include "angband.h"
#include "display.h"


/*** Constants ***/


#define MAX_PANEL 12


/*** Display buffer ***/


static char display_buffer[NORMAL_HGT][NORMAL_WID + 1];


/*** Display hooks ***/


errr (*clear_hook)(void);
void (*region_erase_hook)(const region *loc);
errr (*put_ch_hook)(int x, int y, byte a, char c);
errr (*put_str_hook)(int x, int y, int n, byte a, const char *s);
bool use_bigtile_hook;


/*** Buffer access functions ***/


/*
 * Clear the buffer
 */
errr buffer_clear(void)
{
    int row, col;

    /* Clear the buffer */
    for (row = 0; row < NORMAL_HGT; row++)
    {
        for (col = 0; col < NORMAL_WID; col++)
            display_buffer[row][col] = ' ';
        display_buffer[row][NORMAL_WID] = '\0';
    }

    return 0;
}


/*
 * Add a character to the buffer
 */
errr buffer_put_ch(int x, int y, byte a, char c)
{
    display_buffer[y - 1][x] = c;

    return 0;
}


/*
 * Add a string to the buffer
 */
errr buffer_put_str(int x, int y, int n, byte a, const char *s)
{
    char *cursor = display_buffer[y - 1], *str = (char *)s;
    int col = x, size = n;

    /* Position cursor, dump the text */
    while (*str && size && (col < NORMAL_WID))
    {
        cursor[col++] = (*str);
        str++;
        if (size > 0) size--;
    }

    return 0;
}


/*
 * Return one line of the buffer
 */
char *buffer_line(int row)
{
    return display_buffer[row];
}


/*** Utility display functions ***/


/*
 * List of resistances and abilities to display
 */
static const char *player_flag_table[RES_PANELS * RES_ROWS] =
{
    "Ac :", /* OF_RES_ACID */
    "El :", /* OF_RES_ELEC */
    "Fi :", /* OF_RES_FIRE */
    "Co :", /* OF_RES_COLD */
    "Po :", /* OF_RES_POIS */
    "Lt :", /* OF_RES_LIGHT */
    "Dk :", /* OF_RES_DARK */
    "Snd:", /* OF_RES_SOUND */
    "Shr:", /* OF_RES_SHARD */
    "Nxs:", /* OF_RES_NEXUS */
    "Ntr:", /* OF_RES_NETHR */
    "Chs:", /* OF_RES_CHAOS */
    "Dsn:", /* OF_RES_DISEN */
    "FF :", /* OF_FEATHER */
    "Fe :", /* OF_RES_FEAR */
    "Bld:", /* OF_RES_BLIND */
    "Cnf:", /* OF_RES_CONFU */
    "Stn:", /* OF_RES_STUN */
    "Lit:", /* OF_LIGHT */
    "Rgn:", /* OF_REGEN */
    "ESP:", /* OF_ESP_XXX */
    "SI :", /* OF_SEE_INVIS */
    "FA :", /* OF_FREE_ACT */
    "HL :", /* OF_HOLD_LIFE */
    "Stl:", /* OF_STEALTH */
    "Src:", /* OF_SEARCH */
    "Inf:", /* OF_INFRA */
    "Tun:", /* OF_TUNNEL */
    "Spd:", /* OF_SPEED */
    "EA :", /* OF_BLOWS */
    "XS :", /* OF_SHOTS */
    "XM :", /* OF_MIGHT */
    "Dig:", /* OF_SLOW_DIGEST */
    "-HP:", /* OF_IMPAIR_HP */
    "Afr:", /* OF_AFRAID */
    "Agg:"  /* OF_AGGRAVATE */
};


static const region resist_region[] =
{
    {0 * (RES_COLS + 5), 10, RES_COLS + 4, RES_ROWS + 2},
    {1 * (RES_COLS + 5), 10, RES_COLS + 4, RES_ROWS + 2},
    {2 * (RES_COLS + 5), 10, RES_COLS + 4, RES_ROWS + 2},
    {3 * (RES_COLS + 5), 10, RES_COLS + 4, RES_ROWS + 2}
};


/*
 * Equippy chars
 */
static void display_equippy(int Ind, int row, int col)
{
    player_type *pptr = get_player(Ind);
    int i;
    byte a;
    char c;

    /* No equippy chars in distorted mode */
    if (use_bigtile_hook) return;

    /* Dump equippy chars */
    for (i = 0; i < RES_COLS - 1; i++)
    {
        /* Get attr/char for display */
        a = pptr->hist_flags[0][i].a;
        c = pptr->hist_flags[0][i].c;

        /* Dump */
        put_ch_hook(col + i, row, a, c);
    }
}


static void display_resistance_panel(int Ind, const char **resists,
    const region *bounds)
{
    player_type *pptr = get_player(Ind);
    int col = bounds->col;
    int row = bounds->row;
    size_t i, j;
    int off = 1 + A_MAX + RES_ROWS * col / (RES_COLS + 5);

    /* Header */
    put_str_hook(col, row++, -1, TERM_WHITE, "    abcdefghijklm@");

    /* Lines */
    for (i = 0; i < RES_ROWS; i++, row++)
    {
        byte name_attr = TERM_WHITE;

        /* Draw dots */
        for (j = 0; j < RES_COLS; j++)
        {
            byte attr = pptr->hist_flags[off + i][j].a;
            char sym = pptr->hist_flags[off + i][j].c;

            /* Dump proper character */
            put_ch_hook(col + 4 + j, row, attr, sym);

            /* Name color */
            if (sym == '*')
                name_attr = TERM_GREEN;
            else if ((sym != '.') && (sym != '-') && (sym != '?') &&
                (name_attr == TERM_WHITE))
                    name_attr = TERM_L_BLUE;
        }

        /* Name */
        put_str_hook(col, row, -1, name_attr, resists[i]);
    }

    /* Footer */
    put_str_hook(col, row++, -1, TERM_WHITE, "    abcdefghijklm@");

    /* Equippy */
    display_equippy(Ind, row++, col + 4);
}


static void display_player_flag_info(int Ind)
{
    int i;

    for (i = 0; i < RES_PANELS; i++)
        display_resistance_panel(Ind, player_flag_table + i * RES_ROWS, &resist_region[i]);
}


/*
 * Special display, part 2b
 */
static void display_player_stat_info(int Ind)
{
    player_type *pptr = get_player(Ind);
    int i, row, col, r_adj;
    char buf[NORMAL_WID];

    /* Row */
    row = 2;

    /* Column */
    col = 42;

    /* Print out the labels for the columns */
    put_str_hook(col + 5, row - 1, -1, TERM_WHITE, "  Self");
    put_str_hook(col + 12, row - 1, -1, TERM_WHITE, " RB");
    put_str_hook(col + 16, row - 1, -1, TERM_WHITE, " CB");
    put_str_hook(col + 20, row - 1, -1, TERM_WHITE, " EB");
    put_str_hook(col + 24, row - 1, -1, TERM_WHITE, "  Best");

    /* Display the stats */
    for (i = 0; i < A_MAX; i++)
    {
        /* Reduced */
        if (pptr->stat_cur[i] < pptr->stat_max[i])
        {
            /* Use lowercase stat name */
            put_str_hook(col, row + i, -1, TERM_WHITE, stat_names_reduced[i]);
        }

        /* Normal */
        else
        {
            /* Assume uppercase stat name */
            put_str_hook(col, row + i, -1, TERM_WHITE, stat_names[i]);
        }

        /* Indicate natural maximum */
        if (pptr->stat_max[i] == 18+100)
            put_str_hook(col + 3, row + i, -1, TERM_WHITE, "!");

        /* Internal "natural" maximum value */
        cnv_stat(pptr->stat_max[i], buf, sizeof(buf));
        put_str_hook(col + 5, row + i, -1, TERM_L_GREEN, buf);

        /* Race Bonus */
        /* Polymorphed players only get half adjustment from race */
        r_adj = pptr->race->r_adj[i];
        if (pptr->r_idx)
        {
            if (r_adj > 0) r_adj = (r_adj + 1) / 2;
            else if (r_adj < 0) r_adj = (r_adj - 1) / 2;
        }
        strnfmt(buf, sizeof(buf), "%+3d", r_adj);
        put_str_hook(col + 12, row + i, -1, TERM_L_BLUE, buf);

        /* Class Bonus */
        strnfmt(buf, sizeof(buf), "%+3d", pptr->clazz->c_adj[i]);
        put_str_hook(col + 16, row + i, -1, TERM_L_BLUE, buf);

        /* Equipment Bonus */
        strnfmt(buf, sizeof(buf), "%+3d", pptr->state.stat_add[i]);
        put_str_hook(col + 20, row + i, -1, TERM_L_BLUE, buf);

        /* Resulting "modified" maximum value */
        cnv_stat(pptr->state.stat_top[i], buf, sizeof(buf));
        put_str_hook(col + 24, row + i, -1, TERM_L_GREEN, buf);

        /* Only display stat_use if not maximal */
        if (pptr->stat_cur[i] < pptr->stat_max[i])
        {
            cnv_stat(pptr->state.stat_use[i], buf, sizeof(buf));
            put_str_hook(col + 31, row + i, -1, TERM_YELLOW, buf);
        }
    }
}


/*
 * Special display, part 2c
 *
 * How to print out the modifications and sustains.
 * Positive mods with no sustain will be light green.
 * Positive mods with a sustain will be dark green.
 * Sustains (with no modification) will be a dark green 's'.
 * Negative mods (from a curse) will be red.
 * Huge mods (>9), like from MICoMorgoth, will be a '*'
 * No mod, no sustain, will be a slate '.'
 */
static void display_player_sust_info(int Ind)
{
    player_type *pptr = get_player(Ind);
    int i, row, col, stat;
    byte a;
    char c;

    /* Row */
    row = 2;

    /* Column */
    col = 24;

    /* Header */
    put_str_hook(col, row - 1, -1, TERM_WHITE, "abcdefghijklm@");

    for (stat = 0; stat < A_MAX; ++stat)
    {
        for (i = 0; i < RES_COLS; ++i)
        {
            a = pptr->hist_flags[stat + 1][i].a;
            c = pptr->hist_flags[stat + 1][i].c;

            /* Dump proper character */
            put_ch_hook(col + i, row + stat, a, c);
        }
    }

    /* Footer */
    put_str_hook(col, row + A_MAX, -1, TERM_WHITE, "abcdefghijklm@");

    /* Equippy */
    display_equippy(Ind, row + A_MAX + 1, col);
}


static void display_panel(const data_panel *panel, int count, bool left_adj,
    const region *bounds)
{
    int i;
    char buffer[50];
    int col = bounds->col;
    int row = bounds->row;
    int w = bounds->width;
    int offset = 0;

    if (region_erase_hook) region_erase_hook(bounds);

    if (left_adj)
    {
        for (i = 0; i < count; i++)
        {
            int len = (panel[i].label? strlen(panel[i].label): 0);

            if (offset < len) offset = len;
        }
        offset += 2;
    }

    for (i = 0; i < count; i++, row++)
    {
        int len;

        if (!panel[i].label) continue;
        put_str_hook(col, row, strlen(panel[i].label), TERM_WHITE, panel[i].label);

        strnfmt(buffer, sizeof(buffer), panel[i].fmt, panel[i].value[0],
            panel[i].value[1]);

        len = strlen(buffer);
        len = ((len < w - offset)? len: (w - offset - 1));
        if (left_adj)
            put_str_hook(col + offset, row, len, panel[i].color, buffer);
        else
            put_str_hook(col + w - len, row, len, panel[i].color, buffer);
    }
}


static const region boundaries[] =
{
    /* x   y  width  rows */
    {  0,  0,     0,    0 },
    {  1,  1,    40,    8 }, /* Panel 1 */
    {  1,  9,    22,    9 }, /* Panel 2 */
    { 26,  9,    17,    9 }, /* Panel 3 */
    { 48,  9,    24,    8 }, /* Panel 4 */
    { 21,  2,    18,    5 }, /* Panel 5 */
	{  0,  0,     0,    0 },
};


static const char *show_adv_exp(int Ind)
{
    player_type *pptr = get_player(Ind);

    if (pptr->lev < PY_MAX_LEVEL)
    {
        static char buffer[30];

        strnfmt(buffer, sizeof(buffer), "%d", adv_exp(pptr->lev, pptr->expfact));
        return buffer;
    }

    return "********";
}


static const char *show_depth(int Ind)
{
    player_type *pptr = get_player(Ind);
    static char buffer[13];

    if (pptr->max_depth == 0) return "Town";

    strnfmt(buffer, sizeof(buffer), "%d' (L%d)", pptr->max_depth * 50, pptr->max_depth);
    return buffer;
}


static const char *show_armor(int Ind)
{
    player_type *pptr = get_player(Ind);
    static char buffer[12];

    strnfmt(buffer, sizeof(buffer), "[%d,%+d]", pptr->state.dis_ac,
        pptr->state.dis_to_a);
    return buffer;
}


static const char *show_fight(int Ind)
{
    player_type *pptr = get_player(Ind);
    static char buffer[12];
    int show_tofhit, show_tomhit, show_toshit;
    int show_tofdam, show_tomdam, show_tosdam;

    get_plusses(pptr, &show_tofhit, &show_tofdam, &show_tomhit, &show_tomdam,
        &show_toshit, &show_tosdam);
    strnfmt(buffer, sizeof(buffer), "(%+d,%+d)", show_tofhit, show_tofdam);
    return buffer;
}


static const char *show_melee_weapon(int Ind)
{
    player_type *pptr = get_player(Ind);
    static char buffer[12];
    int show_tofhit, show_tomhit, show_toshit;
    int show_tofdam, show_tomdam, show_tosdam;

    get_plusses(pptr, &show_tofhit, &show_tofdam, &show_tomhit, &show_tomdam,
        &show_toshit, &show_tosdam);
    strnfmt(buffer, sizeof(buffer), "(%+d,%+d)", show_tomhit, show_tomdam);
    return buffer;
}


static const char *show_missile_weapon(int Ind)
{
    player_type *pptr = get_player(Ind);
    static char buffer[12];
    int show_tofhit, show_tomhit, show_toshit;
    int show_tofdam, show_tomdam, show_tosdam;

    get_plusses(pptr, &show_tofhit, &show_tofdam, &show_tomhit, &show_tomdam,
        &show_toshit, &show_tosdam);
    strnfmt(buffer, sizeof(buffer), "(%+d,%+d)", show_toshit, show_tosdam);
    return buffer;
}


static const char *show_speed(int Ind)
{
    player_type *pptr = get_player(Ind);
    static char buffer[10];
    s16b speed = get_speed(pptr);

    if (speed == 0) return "Normal";

    strnfmt(buffer, sizeof(buffer), "%d", speed);
    return buffer;
}


static byte max_color(int val, int max)
{
    return ((val < max)? TERM_YELLOW: TERM_L_GREEN);
}


static const char *show_status(int Ind)
{
    player_type *pptr = get_player(Ind);
    int sc = pptr->sc / 10;

    switch (sc)
    {
        case 0:
        case 1: return "Pariah";
        case 2: return "Outcast";
        case 3:
        case 4: return "Unknown";
        case 5: return "Known";
        case 6:
        case 7: return "Liked";
        case 8: return "Well-liked";
        case 9:
        case 10: return "Respected";
        case 11:
        case 12: return "Role model";
        case 13: return "Feared";
        case 14:
        case 15: return "Lordly";
    }

    return format("%d", sc);
}


/* data_panel array element initializer, for ansi compliance */
#define P_I(col, lab, format, val1, val2) \
    { \
        panel[i].color = col; \
        panel[i].label = lab; \
        panel[i].fmt = format; \
        panel[i].value[0] = val1; \
        panel[i].value[1] = val2; \
        i++; \
    }


/* Colours for table items */
static const byte colour_table[] =
{
    TERM_RED, TERM_RED, TERM_RED, TERM_L_RED, TERM_ORANGE,
    TERM_YELLOW, TERM_YELLOW, TERM_GREEN, TERM_GREEN, TERM_L_GREEN,
    TERM_L_BLUE
};


#define END null2u()
static int get_panel(int Ind, int oid, data_panel *panel, size_t size)
{
    player_type *pptr = get_player(Ind);
    int ret = (int)size;

    switch (oid)
    {
        /* Panel 1 */
        case 1:
        {
            int i = 0;
            const char *player_title = get_title(pptr);

            if (pptr->ghost) player_title = "Ghost";

            my_assert(ret >= boundaries[1].page_rows);
            ret = boundaries[1].page_rows;
            P_I(TERM_L_BLUE, "Name", "%y", s2u(pptr->name), END);
            P_I(TERM_L_BLUE, "Sex", "%y", s2u(pptr->sex->title), END);
            P_I(TERM_L_BLUE, "Race", "%y", s2u(pptr->race->name), END);
            P_I(TERM_L_BLUE, "Class", "%y", s2u(pptr->clazz->name), END);
            P_I(TERM_L_BLUE, "Title", "%y", s2u(player_title), END);
            P_I(TERM_L_BLUE, "HP", "%y/%y", i2u(pptr->chp), i2u(pptr->mhp));
            P_I(TERM_L_BLUE, "SP", "%y/%y", i2u(pptr->csp), i2u(pptr->msp));
            P_I(TERM_L_BLUE, "Level", "%y", i2u(pptr->lev), END);
            my_assert(i == boundaries[1].page_rows);

            return ret;
        }

        /* Panel 2 */
        case 2:
        {
            int i = 0;
            u32b game_turn = ht_div(&pptr->game_turn, cfg_fps),
                player_turn = ht_div(&pptr->player_turn, cfg_fps),
                active_turn = ht_div(&pptr->active_turn, cfg_fps);

            my_assert(ret >= boundaries[2].page_rows);
            ret = boundaries[2].page_rows;
            P_I(max_color(pptr->lev, pptr->max_lev), "Level", "%y", i2u(pptr->lev), END);
            P_I(max_color(pptr->exp, pptr->max_exp), "Cur Exp", "%y", i2u(pptr->exp), END);
            P_I(TERM_L_GREEN, "Max Exp", "%y", i2u(pptr->max_exp), END);
            P_I(TERM_L_GREEN, "Adv Exp", "%y", s2u(show_adv_exp(Ind)), END);
            P_I(TERM_L_GREEN, "MaxDepth", "%y", s2u(show_depth(Ind)), END);
            if (!game_turn)
                P_I(TERM_SLATE, "Game Turns", "%y", s2u("N/A"), END)
            else
                P_I(TERM_L_GREEN, "Game Turns", "%y", i2u(game_turn), END)
            if (!player_turn)
                P_I(TERM_SLATE, "Player Turns", "%y", s2u("N/A"), END)
            else
                P_I(TERM_L_GREEN, "Player Turns", "%y", i2u(player_turn), END)
            if (!active_turn)
                P_I(TERM_SLATE, "Active Turns", "%y", s2u("N/A"), END)
            else
                P_I(TERM_L_GREEN, "Active Turns", "%y", i2u(active_turn), END)
            P_I(TERM_L_GREEN, "Gold", "%y", i2u(pptr->au), END);
            my_assert(i == boundaries[2].page_rows);

            return ret;
        }

        /* Panel 3 */
        case 3:
        {
            int i = 0;

            my_assert(ret >= boundaries[3].page_rows);
            ret = boundaries[3].page_rows;
            P_I(TERM_L_BLUE, "Armor", "%y", s2u(show_armor(Ind)), END);
            P_I(TERM_L_BLUE, "Fight", "%y", s2u(show_fight(Ind)), END);
            P_I(TERM_L_BLUE, "Melee", "%y", s2u(show_melee_weapon(Ind)), END);
            P_I(TERM_L_BLUE, "Shoot", "%y", s2u(show_missile_weapon(Ind)), END);
            P_I(TERM_L_BLUE, "Blows", "%y.%y/turn", i2u(pptr->state.num_blows / 100),
                i2u((pptr->state.num_blows / 10) % 10));
            P_I(TERM_L_BLUE, "Shots", "%y/turn", i2u(pptr->state.num_shots), END);
            P_I(TERM_L_BLUE, "Infra", "%y ft", i2u(pptr->state.see_infra * 10), END);
            P_I(TERM_L_BLUE, "Speed", "%y", s2u(show_speed(Ind)), END);
            P_I(TERM_L_BLUE, "Burden", "%.1y lbs", f2u(pptr->total_weight / 10.0F), END);
            my_assert(i == boundaries[3].page_rows);

            return ret;
        }

        /* Panel 4 */
        case 4:
        {
            static struct
            {
                const char *name;
                int skill;
                int div;
            } skills[] =
            {
                { "Saving Throw", SKILL_SAVE, 6 },
                { "Stealth", SKILL_STEALTH, 1 },
                { "Fighting", SKILL_TO_HIT_MELEE, 12 },
                { "Shooting", SKILL_TO_HIT_BOW, 12 },
                { "Disarming", SKILL_DISARM, 8 },
                { "Magic Device", SKILL_DEVICE, 6 },
                { "Perception", SKILL_SEARCH_FREQUENCY, 6 },
                { "Searching", SKILL_SEARCH, 6 }
            };
            int i;

            my_assert(N_ELEMENTS(skills) == boundaries[4].page_rows);
            ret = N_ELEMENTS(skills);
            if (ret > (int)size) ret = (int)size;
            for (i = 0; i < ret; i++)
            {
                s16b skill = pptr->state.skills[skills[i].skill];
                panel[i].color = TERM_L_BLUE;
                panel[i].label = skills[i].name;
                if ((skills[i].skill == SKILL_SAVE) ||
                    (skills[i].skill == SKILL_SEARCH))
                {
                    if (skill < 0) skill = 0;
                    if (skill > 100) skill = 100;
                    panel[i].fmt = "%y%%";
                    panel[i].value[0] = i2u(skill);
                    panel[i].color = colour_table[skill / 10];
                }
                else if (skills[i].skill == SKILL_DEVICE)
                {
                    if (skill > 130) skill = 130;
                    panel[i].fmt = "%y";
                    panel[i].value[0] = i2u(skill);
                    panel[i].color = colour_table[skill / 13];
                }
                else if (skills[i].skill == SKILL_SEARCH_FREQUENCY)
                {
                    if (skill <= 0) skill = 1;
                    if (skill >= 50)
                    {
                        panel[i].fmt = "1 in 1";
                        panel[i].color = colour_table[10];
                    }
                    else
                    {
                        /* Convert to % chance of searching */
                        skill = 50 - skill;
                        panel[i].fmt = "1 in %y";
                        panel[i].value[0] = i2u(skill);
                        panel[i].color = colour_table[(100 - skill * 2) / 10];
                    }
                }
                else if (skills[i].skill == SKILL_DISARM)
                {
                    /* Assume disarming a dungeon trap */
                    skill -= 5;
                    if (skill > 100) skill = 100;
                    if (skill < 2) skill = 2;
                    panel[i].fmt = "%y%%";
                    panel[i].value[0] = i2u(skill);
                    panel[i].color = colour_table[skill / 10];
                }
                else
                {
                    panel[i].fmt = "%y";
                    panel[i].value[0] = s2u(likert(skill, skills[i].div,
                        &panel[i].color));
                }
            }

            return ret;
        }

        /* Panel 5 */
        case 5:
        {
            int i = 0;

            my_assert(ret >= boundaries[5].page_rows);
            ret = boundaries[5].page_rows;
            P_I(TERM_L_BLUE, "Age", "%y", i2u(pptr->age), END);
            P_I(TERM_L_BLUE, "Height", "%y", i2u(pptr->ht), END);
            P_I(TERM_L_BLUE, "Weight", "%y", i2u(pptr->wt), END);
            P_I(TERM_L_BLUE, "Social", "%y", s2u(show_status(Ind)), END);
            P_I(TERM_L_BLUE, "Maximize", "%y", c2u('Y'), END);
            my_assert(i == boundaries[5].page_rows);

            return ret;
        }
    }

    /* Hopefully not reached */
    return 0;
}


static void display_player_xtra_info(int Ind)
{
    player_type *pptr = get_player(Ind);
    int i;
    int panels[] = { 1, 2, 3, 4, 5};
    bool left_adj[] = { 1, 0, 0, 0, 0 };
    data_panel data[MAX_PANEL];

    for (i = 0; i < (int)N_ELEMENTS(panels); i++)
    {
        int oid = panels[i];
        int rows = get_panel(Ind, oid, data, N_ELEMENTS(data));

        /* Hack: Don't show 'Level' in the first panel */
        if (oid == 1) rows -= 1;

        display_panel(data, rows, left_adj[i], &boundaries[oid]);
    }

    /* History */
    for (i = 0; i < N_HIST_LINES; i++)
        put_str_hook(1, i + 19, -1, TERM_WHITE, pptr->history[i]);
}


/*
 * Display the character on the screen or in a file (two different modes)
 *
 * The top two lines, and the bottom line (or two) are left blank.
 *
 * Mode FALSE = standard display with skills/history
 * Mode TRUE = special display with equipment flags
 */
void display_player(int Ind, bool mode)
{
    /* Clear */
    clear_hook();

    /* Stat info */
    display_player_stat_info(Ind);

    /* Special display */
    if (mode)
    {
        data_panel data[MAX_PANEL];
        int rows = get_panel(Ind, 1, data, N_ELEMENTS(data));

        display_panel(data, rows, 1, &boundaries[1]);

        /* Stat/Sustain flags */
        display_player_sust_info(Ind);

        /* Other flags */
        display_player_flag_info(Ind);
    }

    /* Standard */
    else
    {
        /* Extra info */
        display_player_xtra_info(Ind);
    }
}


/*** Status line display functions ***/


size_t display_depth(int Ind, int row, int col)
{
    player_type *pptr = get_player(Ind);
    char depths[13];
    char *text;

    if (!pptr->depth)
        my_strcpy(depths, "Town", sizeof(depths));
    else if (pptr->depth < 0)
        strnfmt(depths, sizeof(depths), "WLev %d", 0 - pptr->depth);
    else
        strnfmt(depths, sizeof(depths), "%d' (L%d)", pptr->depth * 50, pptr->depth);

    /* Display the depth */
    text = format("%-12s", depths);
    put_str_hook(col, row, -1, TERM_WHITE, text);

    return (strlen(text) + 1);
}


/*
 * Struct to describe different timed effects
 */
struct state_info
{
    int value;
    const char *str;
    size_t len;
    byte attr;
};


#define PRINT_STATE(sym, data, index, row, col) \
{ \
    size_t i; \
    \
    for (i = 0; i < N_ELEMENTS(data); i++) \
    { \
        if (index sym data[i].value) \
        { \
            if (data[i].str[0]) \
            { \
                put_str_hook(col, row, -1, data[i].attr, data[i].str); \
                return data[i].len; \
            } \
            return 0; \
        } \
    } \
}


/* Simple macro to initialise structs */
#define S(s) s, sizeof(s)


/* TMD_CUT descriptions */
static const struct state_info cut_data[] =
{
    { 1000, S("Mortal wound"), TERM_L_RED },
    {  200, S("Deep gash"),    TERM_RED },
    {  100, S("Severe cut"),   TERM_RED },
    {   50, S("Nasty cut"),    TERM_ORANGE },
    {   25, S("Bad cut"),      TERM_ORANGE },
    {   10, S("Light cut"),    TERM_YELLOW },
    {    0, S("Graze"),        TERM_YELLOW }
};


/*
 * Print cut indicator.
 */
static size_t prt_cut(int Ind, int row, int col)
{
    player_type *pptr = get_player(Ind);

    PRINT_STATE(>, cut_data, pptr->timed[TMD_CUT], row, col);
    return 0;
}


/* TMD_STUN descriptions */
static const struct state_info stun_data[] =
{
    {   100, S("Knocked out"), TERM_RED },
    {    50, S("Heavy stun"),  TERM_ORANGE },
    {     0, S("Stun"),        TERM_ORANGE }
};


/*
 * Print stun indicator.
 */
static size_t prt_stun(int Ind, int row, int col)
{
    player_type *pptr = get_player(Ind);

    PRINT_STATE(>, stun_data, pptr->timed[TMD_STUN], row, col);
    return 0;
}


/* p_ptr->food descriptions */
static const struct state_info hunger_data[] =
{
    { PY_FOOD_FAINT, S("Faint"),    TERM_RED },
    { PY_FOOD_WEAK,  S("Weak"),     TERM_ORANGE },
    { PY_FOOD_ALERT, S("Hungry"),   TERM_YELLOW },
    { PY_FOOD_FULL,  S(""),         TERM_L_GREEN },
    { PY_FOOD_MAX,   S("Full"),     TERM_L_GREEN },
    { PY_FOOD_UPPER, S("Gorged"),   TERM_GREEN }
};


/*
 * Prints status of hunger
 */
static size_t prt_hunger(int Ind, int row, int col)
{
    player_type *pptr = get_player(Ind);

    PRINT_STATE(<, hunger_data, pptr->food, row, col);
    return 0;
}


/* For the various TMD_* effects */
static const struct state_info effects[] =
{
    { TMD_BLIND,       S("Blind"),      TERM_ORANGE },
    { TMD_PARALYZED,   S("Paralyzed!"), TERM_RED },
    { TMD_CONFUSED,    S("Confused"),   TERM_ORANGE },
    { TMD_AFRAID,      S("Afraid"),     TERM_ORANGE },
    { TMD_TERROR,      S("Terror"),     TERM_RED },
    { TMD_SPRINT,      S("Sprint"),     TERM_L_GREEN },
    { TMD_POISONED,    S("Poisoned"),   TERM_ORANGE },
    { TMD_OPP_ACID,    S("RAcid"),      TERM_SLATE },
    { TMD_OPP_ELEC,    S("RElec"),      TERM_BLUE },
    { TMD_OPP_FIRE,    S("RFire"),      TERM_RED },
    { TMD_OPP_COLD,    S("RCold"),      TERM_WHITE },
    { TMD_OPP_POIS,    S("RPois"),      TERM_GREEN },
    { TMD_OPP_CONF,    S("RConf"),      TERM_VIOLET },
    { TMD_AMNESIA,     S("Amnesiac"),   TERM_ORANGE },
    { TMD_IMAGE,       S("Hallu"),      TERM_ORANGE },
    { TMD_PROTEVIL,    S("ProtEvil"),   TERM_L_GREEN },
    { TMD_INVULN,      S("Invuln"),     TERM_L_GREEN },
    { TMD_HERO,        S("Hero"),       TERM_L_GREEN },
    { TMD_SHERO,       S("Berserk"),    TERM_L_GREEN },
    { TMD_BOLD,        S("Bold"),       TERM_L_GREEN },
    { TMD_STONESKIN,   S("Stone"),      TERM_L_GREEN },
    { TMD_SHIELD,      S("Shield"),     TERM_L_GREEN },
    { TMD_BLESSED,     S("Blessed"),    TERM_L_GREEN },
    { TMD_SINVIS,      S("SInvis"),     TERM_L_GREEN },
    { TMD_SINFRA,      S("Infra"),      TERM_L_GREEN },
    { TMD_WRAITH,      S("Wraith"),     TERM_L_GREEN },
    { TMD_MEDITATE,    S("Medit"),      TERM_L_GREEN },
    { TMD_MANASHIELD,  S("MShield"),    TERM_L_GREEN },
    { TMD_INVIS,       S("Invis"),      TERM_L_GREEN },
    { TMD_MIMIC,       S("Mimic"),      TERM_L_GREEN },
    { TMD_TRAPS,       S("PTraps"),     TERM_L_GREEN },
    { TMD_BOWBRAND,    S("Brand"),      TERM_L_GREEN },
    { TMD_ESP,         S("ESP"),        TERM_L_GREEN },
    { TMD_ANCHOR,      S("Anchor"),     TERM_L_GREEN },
    { TMD_PROBTRAVEL,  S("Proba"),      TERM_L_GREEN },
    { TMD_ADRENALINE,  S("Adren"),      TERM_L_GREEN },
    { TMD_BIOFEEDBACK, S("BioFB"),      TERM_L_GREEN },
    { TMD_TOUCH,       S("Vamp"),       TERM_L_GREEN },
    { TMD_SOUL,        S("Drain"),      TERM_L_GREEN },
    { TMD_DEADLY,      S("TDeath"),     TERM_L_GREEN },
    { TMD_EPOWER,      S("EPower"),     TERM_L_GREEN },
    { TMD_ICY_AURA,    S("IcyAura"),    TERM_WHITE },
    { TMD_SGRASP,      S("Brand"),      TERM_BLUE },
    { TMD_FARSIGHT,    S("Farsight"),   TERM_L_GREEN },
    { TMD_ZFARSIGHT,   S("Farsight"),   TERM_L_GREEN },
    { TMD_REGEN,       S("Regen"),      TERM_L_GREEN },
    { TMD_HARMONY,     S("Harmony"),    TERM_L_GREEN },
    { TMD_ANTISUMMON,  S("NoSummon"),   TERM_YELLOW }
};


/*
 * Print all timed effects.
 */
static size_t prt_tmd(int Ind, int row, int col)
{
    player_type *pptr = get_player(Ind);
    size_t i, len = 0;

    for (i = 0; i < N_ELEMENTS(effects); i++)
    {
        if (pptr->timed[effects[i].value])
        {
            put_str_hook(col + len, row, -1, effects[i].attr, effects[i].str);
            len += effects[i].len;
        }
    }

    return len;
}


/*
 * Print "unignoring" status
 */
static size_t prt_unignore(int Ind, int row, int col)
{
    player_type *pptr = get_player(Ind);

    /* Unignoring */
    if (pptr->unignoring)
    {
        const char *text = "Unignoring";

        put_str_hook(col, row, -1, TERM_WHITE, text);
        return (strlen(text) + 1);
    }

    return 0;
}


/*
 * Print recall status
 */
static size_t prt_recall(int Ind, int row, int col)
{
    player_type *pptr = get_player(Ind);

    if (pptr->word_recall)
    {
        const char *text = "Recall";

        put_str_hook(col, row, -1, TERM_WHITE, text);
        return (strlen(text) + 1);
    }

    return 0;
}


/*
 * Prints Searching, Resting, or Stealth Mode status
 */
static size_t prt_state(int Ind, int row, int col)
{
    player_type *pptr = get_player(Ind);
    byte attr = TERM_WHITE;
    const char *text = "";

    /* Resting */
    if (pptr->resting)
        text = "Resting";

    /* Searching */
    else if (pptr->searching)
    {
        if (!player_has(pptr, PF_STEALTH_MODE))
            text = "Searching";
        else
        {
            attr = TERM_L_DARK;
            text = "Stealth Mode";
        }
    }

    /* Display the info (or blanks) */
    put_str_hook(col, row, -1, attr, text);

    return (text[0]? (strlen(text) + 1): 0);
}


/*
 * Prints trap detection status
 */
static size_t prt_dtrap(int Ind, int row, int col)
{
    player_type *pptr = get_player(Ind);
    byte attr = TERM_WHITE;
    const char *text = "";
    byte dtrap = get_dtrap(pptr);

    if (dtrap == 2)
    {
        attr = TERM_YELLOW;
        text = "DTrap";
    }
    if (dtrap == 1)
    {
        attr = TERM_L_GREEN;
        text = "DTrap";
    }

    /* Display the info (or blanks) */
    put_str_hook(col, row, -1, attr, text);

    return (text[0]? (strlen(text) + 1): 0);
}


/*
 * Print how many spells the player can study.
 */
static size_t prt_study(int Ind, int row, int col)
{
    player_type *pptr = get_player(Ind);
    char *text;
    int attr = TERM_WHITE;

    /* Can the player learn new spells? */
    if (pptr->new_spells)
    {
        /*
         * If the player does not carry a book with spells they can study,
         * the message is displayed in a darker colour
         */
        if (!pptr->can_study_book) attr = TERM_L_DARK;

        /* Print study message */
        text = format("Study (%d)", pptr->new_spells);
        put_str_hook(col, row, -1, attr, text);
        return (strlen(text) + 1);
    }

    return 0;
}


/* Useful typedef */
typedef size_t status_f(int Ind, int row, int col);


/*
 * Status line indicators.
 */
static status_f *status_handlers[] =
{
    prt_unignore, prt_recall, prt_state, prt_cut, prt_stun, prt_hunger, prt_study,
    prt_tmd, prt_dtrap
};


/*
 * Print the status line.
 */
void display_statusline(int Ind, int row, int col)
{
    size_t i;

    /* Display those which need redrawing */
    for (i = 0; i < N_ELEMENTS(status_handlers); i++)
        col += status_handlers[i](Ind, row, col);
}


/*
 * Print the status display subwindow
 */
void display_status_subwindow(int Ind, int row, int col)
{
    size_t i;

    for (i = 0; i < N_ELEMENTS(status_handlers); i++)
        status_handlers[i](Ind, row++, col);
}

