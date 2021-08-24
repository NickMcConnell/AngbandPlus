/*
 * File: display.c
 * Purpose: Display the character on the screen or in a file
 *
 * Copyright (c) 2019 MAngband and PWMAngband Developers
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


s16b cfg_fps = 1;


/*
 * Panel utilities
 */


/*
 * Panel line type
 */
struct panel_line
{
    byte attr;
    const char *label;
    char value[20];
};


/*
 * Panel holder type
 */
struct panel
{
    size_t len;
    size_t max;
    struct panel_line *lines;
};


/*
 * Allocate some panel lines
 */
static struct panel *panel_allocate(int n)
{
    struct panel *p = mem_zalloc(sizeof(*p));

    p->len = 0;
    p->max = n;
    p->lines = mem_zalloc(p->max * sizeof(*p->lines));

    return p;
}


/*
 * Free up panel lines
 */
static void panel_free(struct panel *p)
{
    my_assert(p);
    mem_free(p->lines);
    mem_free(p);
}


/*
 * Add a new line to the panel
 */
static void panel_line(struct panel *p, byte attr, const char *label, const char *fmt, ...)
{
    va_list vp;
    struct panel_line *pl;

    /* Get the next panel line */
    my_assert(p);
    my_assert(p->len != p->max);
    pl = &p->lines[p->len++];

    /* Set the basics */
    pl->attr = attr;
    pl->label = label;

    /* Set the value */
    va_start(vp, fmt);
    vstrnfmt(pl->value, sizeof(pl->value), fmt, vp);
    va_end(vp);
}


/*
 * Add a spacer line in a panel
 */
static void panel_space(struct panel *p)
{
    my_assert(p);
    my_assert(p->len != p->max);
    p->len++;
}


/*** Display buffer ***/


static char display_buffer[NORMAL_HGT][NORMAL_WID + 1];


/*** Display hooks ***/


errr (*clear_hook)(void);
void (*region_erase_hook)(const region *loc);
errr (*put_ch_hook)(int x, int y, u16b a, char c);
errr (*put_str_hook)(int x, int y, int n, u16b a, const char *s);
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
errr buffer_put_ch(int x, int y, u16b a, char c)
{
    display_buffer[y - 1][x] = c;

    return 0;
}


/*
 * Add a string to the buffer
 */
errr buffer_put_str(int x, int y, int n, u16b a, const char *s)
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
static const char *player_flag_table[(RES_PANELS + 1) * RES_ROWS] =
{
    "Ac :", /* ELEM_ACID */
    "El :", /* ELEM_ELEC */
    "Fi :", /* ELEM_FIRE */
    "Co :", /* ELEM_COLD */
    "Po :", /* ELEM_POIS */
    "Lt :", /* ELEM_LIGHT */
    "Dk :", /* ELEM_DARK */
    "Snd:", /* ELEM_SOUND */
    "Shr:", /* ELEM_SHARD */

    "Nxs:", /* ELEM_NEXUS */
    "Ntr:", /* ELEM_NETHER */
    "Chs:", /* ELEM_CHAOS */
    "Dsn:", /* ELEM_DISEN */
    "Lev:", /* OF_FEATHER */
    "Fe :", /* OF_PROT_FEAR */
    "Bld:", /* OF_PROT_BLIND */
    "Cnf:", /* OF_PROT_CONF */
    "Stn:", /* OF_PROT_STUN */

    "Lit:", /* OBJ_MOD_LIGHT */
    "Rgn:", /* OF_REGEN */
    "ESP:", /* OF_ESP_XXX */
    "SI :", /* OF_SEE_INVIS */
    "FA :", /* OF_FREE_ACT */
    "HL :", /* OF_HOLD_LIFE */
    "Stl:", /* OBJ_MOD_STEALTH */
    "Src:", /* OBJ_MOD_SEARCH */
    "Inf:", /* OBJ_MOD_INFRA */

    "Tun:", /* OBJ_MOD_TUNNEL */
    "Spd:", /* OBJ_MOD_SPEED */
    "EA :", /* OBJ_MOD_BLOWS */
    "XS :", /* OBJ_MOD_SHOTS */
    "XM :", /* OBJ_MOD_MIGHT */
    "Dig:", /* OF_SLOW_DIGEST */
    "-HP:", /* OF_IMPAIR_HP */
    "Afr:", /* OF_AFRAID */
    "Agg:", /* OF_AGGRAVATE */

    "Rad:", /* OF_ESP_RADIUS */
    "Evi:", /* OF_ESP_EVIL */
    "Ani:", /* OF_ESP_ANIMAL */
    "Und:", /* OF_ESP_UNDEAD */
    "Dem:", /* OF_ESP_DEMON */
    "Orc:", /* OF_ESP_ORC */
    "Tro:", /* OF_ESP_TROLL */
    "Gia:", /* OF_ESP_GIANT */
    "Dra:"  /* OF_ESP_DRAGON */
};


/*
 * Equippy chars (ASCII representation of gear in equipment slot order)
 */
static void display_equippy(struct player *p, int row, int col)
{
    int i;
    byte a;
    char c;

    /* No equippy chars in distorted mode */
    if (use_bigtile_hook) return;

    /* Dump equippy chars */
    for (i = 0; i < p->body.count; i++)
    {
        /* Get attr/char for display */
        a = p->hist_flags[0][i].a;
        c = p->hist_flags[0][i].c;

        /* Dump */
        put_ch_hook(col + i, row, a, c);
    }
}


static void display_resistance_panel(struct player *p, const char **rec, const region *bounds)
{
    size_t i;
    int j;
    int col = bounds->col;
    int row = bounds->row;
    int off = 1 + STAT_MAX + RES_ROWS * col / (p->body.count + 6);

    /* Special case: ESP flags */
    if (col == RES_PANELS * (p->body.count + 6)) col = 0;

    /* Header */
    put_str_hook(col, row++, -1, COLOUR_WHITE, "    abcdefghijklm@");

    /* Lines */
    for (i = 0; i < RES_ROWS; i++, row++)
    {
        byte name_attr = COLOUR_WHITE;

        /* Draw dots */
        for (j = 0; j <= p->body.count; j++)
        {
            byte attr = p->hist_flags[off + i][j].a;
            char sym = p->hist_flags[off + i][j].c;
            bool rune = false;

            /* Hack -- rune is known */
            if (attr >= BASIC_COLORS)
            {
                attr -= BASIC_COLORS;
                rune = true;
            }

            /* Dump proper character */
            put_ch_hook(col + 4 + j, row, attr, sym);

            /* Name color */

            /* Unknown rune */
            if (!rune) name_attr = COLOUR_SLATE;
            if (name_attr == COLOUR_SLATE) continue;

            /* Immunity */
            if (sym == '*') name_attr = COLOUR_GREEN;
            if (name_attr == COLOUR_GREEN) continue;

            /* Vulnerability */
            if (sym == '-') name_attr = COLOUR_L_RED;
            if (name_attr == COLOUR_L_RED) continue;

            /* Resistance */
            if (sym == '+') name_attr = COLOUR_L_BLUE;
            if (name_attr == COLOUR_L_BLUE) continue;

            /* Other known properties */
            if ((sym != '.') && (sym != '?') && (sym != '!'))
                name_attr = COLOUR_L_BLUE;
        }

        /* Name */
        put_str_hook(col, row, -1, name_attr, rec[i]);
    }

    /* Footer */
    put_str_hook(col, row++, -1, COLOUR_WHITE, "    abcdefghijklm@");

    /* Equippy */
    display_equippy(p, row++, col + 4);
}


static void display_player_flag_info(struct player *p)
{
    int i;
    int res_cols = p->body.count + 5;
    region resist_region[] =
    {
        {0, 10, 0, RES_ROWS + 2},
        {0, 10, 0, RES_ROWS + 2},
        {0, 10, 0, RES_ROWS + 2},
        {0, 10, 0, RES_ROWS + 2}
    };

    for (i = 0; i < N_ELEMENTS(resist_region); i++)
    {
        resist_region[i].col = i * (res_cols + 1);
        resist_region[i].width = res_cols;
    }

    for (i = 0; i < RES_PANELS; i++)
        display_resistance_panel(p, player_flag_table + i * RES_ROWS, &resist_region[i]);
}


static void display_player_esp_info(struct player *p)
{
    int res_cols = p->body.count + 5;
    region resist_region;

    resist_region.col = RES_PANELS * (res_cols + 1);
    resist_region.row = 10;
    resist_region.width = res_cols;
    resist_region.page_rows = RES_ROWS + 2;

    display_resistance_panel(p, player_flag_table + RES_PANELS * RES_ROWS, &resist_region);
}


/*
 * Special display, part 2b
 */
static void display_player_stat_info(struct player *p)
{
    int i, row, col, r_adj;
    char buf[NORMAL_WID];

    /* Row */
    row = 2;

    /* Column */
    col = 42;

    /* Print out the labels for the columns */
    put_str_hook(col + 5, row - 1, -1, COLOUR_WHITE, "  Self");
    put_str_hook(col + 12, row - 1, -1, COLOUR_WHITE, " RB");
    put_str_hook(col + 16, row - 1, -1, COLOUR_WHITE, " CB");
    put_str_hook(col + 20, row - 1, -1, COLOUR_WHITE, " EB");
    put_str_hook(col + 24, row - 1, -1, COLOUR_WHITE, "  Best");

    /* Display the stats */
    for (i = 0; i < STAT_MAX; i++)
    {
        /* Reduced or normal */
        if (p->stat_cur[i] < p->stat_max[i])
        {
            /* Use lowercase stat name */
            put_str_hook(col, row + i, -1, COLOUR_WHITE, stat_names_reduced[i]);
        }
        else
        {
            /* Assume uppercase stat name */
            put_str_hook(col, row + i, -1, COLOUR_WHITE, stat_names[i]);
        }

        /* Indicate natural maximum */
        if (p->stat_max[i] == 18+100)
            put_str_hook(col + 3, row + i, -1, COLOUR_WHITE, "!");

        /* Internal "natural" maximum value */
        cnv_stat(p->stat_max[i], buf, sizeof(buf));
        put_str_hook(col + 5, row + i, -1, COLOUR_L_GREEN, buf);

        /* Race Bonus */
        /* Polymorphed players only get half adjustment from race */
        r_adj = p->race->r_adj[i];
        if (p->poly_race)
        {
            if (r_adj > 0) r_adj = (r_adj + 1) / 2;
            else if (r_adj < 0) r_adj = (r_adj - 1) / 2;
        }
        strnfmt(buf, sizeof(buf), "%+3d", r_adj);
        put_str_hook(col + 12, row + i, -1, COLOUR_L_BLUE, buf);

        /* Class Bonus */
        strnfmt(buf, sizeof(buf), "%+3d", p->clazz->c_adj[i]);
        put_str_hook(col + 16, row + i, -1, COLOUR_L_BLUE, buf);

        /* Equipment Bonus */
        strnfmt(buf, sizeof(buf), "%+3d", p->state.stat_add[i]);
        put_str_hook(col + 20, row + i, -1, COLOUR_L_BLUE, buf);

        /* Resulting "modified" maximum value */
        cnv_stat(p->state.stat_top[i], buf, sizeof(buf));
        put_str_hook(col + 24, row + i, -1, COLOUR_L_GREEN, buf);

        /* Only display stat_use if not maximal */
        if (p->stat_cur[i] < p->stat_max[i])
        {
            cnv_stat(p->state.stat_use[i], buf, sizeof(buf));
            put_str_hook(col + 31, row + i, -1, COLOUR_YELLOW, buf);
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
static void display_player_sust_info(struct player *p)
{
    int i, row, col, stat;
    byte a;
    char c;

    /* Row */
    row = 2;

    /* Column */
    col = 24;

    /* Header */
    put_str_hook(col, row - 1, -1, COLOUR_WHITE, "abcdefghijklm@");

    for (stat = 0; stat < STAT_MAX; ++stat)
    {
        for (i = 0; i <= p->body.count; ++i)
        {
            a = p->hist_flags[stat + 1][i].a;
            c = p->hist_flags[stat + 1][i].c;

            /* Dump proper character */
            put_ch_hook(col + i, row + stat, a, c);
        }
    }

    /* Footer */
    put_str_hook(col, row + STAT_MAX, -1, COLOUR_WHITE, "abcdefghijklm@");

    /* Equippy */
    display_equippy(p, row + STAT_MAX + 1, col);
}


static void display_panel(const struct panel *p, bool left_adj, const region *bounds)
{
    size_t i;
    int col = bounds->col;
    int row = bounds->row;
    int w = bounds->width;
    int offset = 0;

    if (region_erase_hook) region_erase_hook(bounds);

    if (left_adj)
    {
        for (i = 0; i < p->len; i++)
        {
            struct panel_line *pl = &p->lines[i];
            int len = (pl->label? strlen(pl->label): 0);

            if (offset < len) offset = len;
        }
        offset += 2;
    }

    for (i = 0; i < p->len; i++, row++)
    {
        int len;
        struct panel_line *pl = &p->lines[i];

        if (!pl->label) continue;
        put_str_hook(col, row, strlen(pl->label), COLOUR_WHITE, pl->label);

        len = strlen(pl->value);
        len = ((len < w - offset)? len: (w - offset - 1));
        if (left_adj)
            put_str_hook(col + offset, row, len, pl->attr, pl->value);
        else
            put_str_hook(col + w - len, row, len, pl->attr, pl->value);
    }
}


static const char *show_adv_exp(struct player *p)
{
    if (p->lev < PY_MAX_LEVEL)
    {
        static char buffer[30];

        strnfmt(buffer, sizeof(buffer), "%d", adv_exp(p->lev, p->expfact));
        return buffer;
    }

    return "********";
}


static const char *show_depth(struct player *p)
{
    static char buffer[13];

    if (p->max_depth == 0) return "Surface";

    strnfmt(buffer, sizeof(buffer), "%d' (L%d)", p->max_depth * 50, p->max_depth);
    return buffer;
}


static const char *show_speed(struct player *p)
{
    static char buffer[10];
    s16b speed = get_speed(p);

    if (speed == 0) return "Normal";

    strnfmt(buffer, sizeof(buffer), "%d", speed);
    return buffer;
}


static byte max_color(int val, int max)
{
    return ((val < max)? COLOUR_YELLOW: COLOUR_L_GREEN);
}


/*
 * Colours for table items
 */
static const byte colour_table[] =
{
    COLOUR_RED, COLOUR_RED, COLOUR_RED, COLOUR_L_RED, COLOUR_ORANGE,
    COLOUR_YELLOW, COLOUR_YELLOW, COLOUR_GREEN, COLOUR_GREEN, COLOUR_L_GREEN,
    COLOUR_L_BLUE
};


static struct panel *get_panel_topleft(struct player *pplayer)
{
    struct panel *p = panel_allocate(7);
    const char *player_title = get_title(pplayer);

    if (pplayer->ghost) player_title = "Ghost";

    panel_line(p, COLOUR_L_BLUE, "Name", "%s", pplayer->name);
    panel_line(p, COLOUR_L_BLUE, "Sex", "%s", pplayer->sex->title);
    panel_line(p, COLOUR_L_BLUE, "Race",  "%s", pplayer->race->name);
    panel_line(p, COLOUR_L_BLUE, "Class", "%s", pplayer->clazz->name);
    panel_line(p, COLOUR_L_BLUE, "Title", "%s", player_title);
    panel_line(p, COLOUR_L_BLUE, "HP", "%d/%d", pplayer->chp, pplayer->mhp);
    panel_line(p, COLOUR_L_BLUE, "SP", "%d/%d", pplayer->csp, pplayer->msp);

    return p;
}


static struct panel *get_panel_midleft(struct player *pplayer)
{
    struct panel *p = panel_allocate(9);
    int diff = get_diff(pplayer);
    byte attr = ((diff < 0)? COLOUR_L_RED: COLOUR_L_GREEN);

    panel_line(p, max_color(pplayer->lev, pplayer->max_lev), "Level", "%d", pplayer->lev);
    panel_line(p, max_color(pplayer->exp, pplayer->max_exp), "Cur Exp", "%d", pplayer->exp);
    panel_line(p, COLOUR_L_GREEN, "Max Exp", "%d", pplayer->max_exp);
    panel_line(p, COLOUR_L_GREEN, "Adv Exp", "%s", show_adv_exp(pplayer));
    panel_space(p);
    panel_line(p, COLOUR_L_GREEN, "Gold", "%d", pplayer->au);
    panel_line(p, attr, "Burden", "%.1f lb", pplayer->upkeep->total_weight / 10.0F);
    panel_line(p, attr, "Overweight", "%d.%d lb", -diff / 10, abs(diff) % 10);
    panel_line(p, COLOUR_L_GREEN, "MaxDepth", "%s", show_depth(pplayer));

    return p;
}


static struct panel *get_panel_combat(struct player *pplayer)
{
    struct panel *p = panel_allocate(9);
    int bth, dam, hit;
    int melee_dice, melee_sides;
    int show_mhit, show_mdam;
    int show_shit, show_sdam;

    get_plusses(pplayer, &pplayer->known_state, &melee_dice, &melee_sides, &show_mhit, &show_mdam,
        &show_shit, &show_sdam);

    /* AC */
    panel_line(p, COLOUR_L_BLUE, "Armor", "[%d,%+d]", pplayer->known_state.ac,
        pplayer->known_state.to_a);

    /* Melee */
    bth = get_melee_skill(pplayer);
    dam = show_mdam;
    hit = show_mhit;

    panel_space(p);
    panel_line(p, COLOUR_L_BLUE, "Melee", "%dd%d,%+d", melee_dice, melee_sides, dam);
    panel_line(p, COLOUR_L_BLUE, "To-hit", "%d,%+d", bth / 10, hit);
    panel_line(p, COLOUR_L_BLUE, "Blows", "%d.%d/turn", pplayer->state.num_blows / 100,
        (pplayer->state.num_blows / 10 % 10));

    /* Ranged */
    bth = get_ranged_skill(pplayer);
    hit = show_shit;
    dam = show_sdam;

    panel_space(p);
    panel_line(p, COLOUR_L_BLUE, "Shoot to-dam", "%+d", dam);
    panel_line(p, COLOUR_L_BLUE, "To-hit", "%d,%+d", bth / 10, hit);
    panel_line(p, COLOUR_L_BLUE, "Shots", "%d/turn", pplayer->state.num_shots);

    return p;
}


static struct panel *get_panel_skills(struct player *pplayer)
{
    struct panel *p = panel_allocate(8);
    int skill;
    byte attr;
    const char *desc;
    int depth = pplayer->wpos.depth;

    #define BOUND(x, min, max) MIN(max, MAX(min, x))

    /* Saving throw */
    skill = BOUND(pplayer->state.skills[SKILL_SAVE], 0, 100);
    panel_line(p, colour_table[skill / 10], "Saving Throw", "%d%%", skill);

    /* Stealth */
    desc = likert(pplayer->state.skills[SKILL_STEALTH], 1, &attr);
    panel_line(p, attr, "Stealth", "%s", desc);

    /* Physical disarming: assume we're disarming a dungeon trap */
    skill = BOUND(pplayer->state.skills[SKILL_DISARM_PHYS] - depth / 5, 2, 100);
    panel_line(p, colour_table[skill / 10], "Disarm - phys.", "%d%%", skill);

    /* Magical disarming */
    skill = BOUND(pplayer->state.skills[SKILL_DISARM_MAGIC] - depth / 5, 2, 100);
    panel_line(p, colour_table[skill / 10], "Disarm - magic", "%d%%", skill);

    /* Magic devices */
    skill = pplayer->state.skills[SKILL_DEVICE];
    panel_line(p, colour_table[MIN(skill, 130) / 13], "Magic Devices", "%d", skill);

    /* Searching ability */
    skill = BOUND(pplayer->state.skills[SKILL_SEARCH], 0, 100);
    panel_line(p, colour_table[skill / 10], "Searching", "%d%%", skill);

    /* Infravision */
    panel_line(p, COLOUR_L_GREEN, "Infravision", "%d ft", pplayer->state.see_infra * 10);

    /* Speed */
    skill = get_speed(pplayer);
    attr = ((skill < 0)? COLOUR_L_RED: COLOUR_L_GREEN);
    panel_line(p, attr, "Speed", "%s", show_speed(pplayer));

    return p;
}


static struct panel *get_panel_misc(struct player *pplayer)
{
    struct panel *p = panel_allocate(7);
    u32b game_turn = ht_div(&pplayer->game_turn, cfg_fps);
    u32b player_turn = ht_div(&pplayer->player_turn, 1);
    u32b active_turn = ht_div(&pplayer->active_turn, 1);

    panel_line(p, COLOUR_L_BLUE, "Age", "%d", pplayer->age);
    panel_line(p, COLOUR_L_BLUE, "Height", "%d'%d\"", pplayer->ht / 12, pplayer->ht % 12);
    panel_line(p, COLOUR_L_BLUE, "Weight", "%dst %dlb", pplayer->wt / 14, pplayer->wt % 14);
    panel_line(p, COLOUR_L_BLUE, "Turns used:", "");
    if (!game_turn)
        panel_line(p, COLOUR_SLATE, "Game", "%s", "N/A");
    else
        panel_line(p, COLOUR_L_BLUE, "Game", "%d", game_turn);
    if (!player_turn)
        panel_line(p, COLOUR_SLATE, "Player", "%s", "N/A");
    else
        panel_line(p, COLOUR_L_BLUE, "Player", "%d", player_turn);
    if (!active_turn)
        panel_line(p, COLOUR_SLATE, "Active", "%s", "N/A");
    else
        panel_line(p, COLOUR_L_BLUE, "Active", "%d", active_turn);

    return p;
}


/*
 * Panels for main character screen
 */
static const struct
{
    region bounds;
    bool align_left;
    struct panel *(*panel)(struct player *);
} panels[] =
{
    /* x  y width rows */
    {{ 1, 1, 40, 7}, true, get_panel_topleft}, /* Name, Class, ... */
    {{22, 1, 18, 7}, false, get_panel_misc}, /* Age, ht, wt, ... */
    {{ 1, 9, 24, 9}, false, get_panel_midleft}, /* Cur Exp, Max Exp, ... */
    {{29, 9, 19, 9}, false, get_panel_combat},
    {{52, 9, 20, 7}, false, get_panel_skills}
};


static void display_player_xtra_info(struct player *pplayer)
{
    size_t i;

    for (i = 0; i < N_ELEMENTS(panels); i++)
    {
        struct panel *p = panels[i].panel(pplayer);

        display_panel(p, panels[i].align_left, &panels[i].bounds);
        panel_free(p);
    }

    /* History */
    for (i = 0; i < N_HIST_LINES; i++)
        put_str_hook(1, i + 19, -1, COLOUR_WHITE, pplayer->history[i]);
}


/*
 * Display the character on the screen or in a file (three different modes)
 *
 * The top two lines, and the bottom line (or two) are left blank.
 *
 * Mode 0 = standard display with skills/history
 * Mode 1 = special display with equipment flags
 * Mode 2 = special display with equipment flags (ESP flags)
 */
void display_player(struct player *pplayer, byte mode)
{
    /* Clear */
    clear_hook();

    /* Stat info */
    display_player_stat_info(pplayer);

    /* Special display */
    if (mode == 2)
    {
        struct panel *p = panels[0].panel(pplayer);

        display_panel(p, panels[0].align_left, &panels[0].bounds);
        panel_free(p);

        /* Stat/Sustain flags */
        display_player_sust_info(pplayer);

        /* Other flags */
        display_player_esp_info(pplayer);
    }
    else if (mode == 1)
    {
        struct panel *p = panels[0].panel(pplayer);

        display_panel(p, panels[0].align_left, &panels[0].bounds);
        panel_free(p);

        /* Stat/Sustain flags */
        display_player_sust_info(pplayer);

        /* Other flags */
        display_player_flag_info(pplayer);
    }
    else
    {
        /* Extra info */
        display_player_xtra_info(pplayer);
    }
}


/*** Status line display functions ***/


size_t display_depth(struct player *p, int row, int col)
{
    char *text;

    /* Display the depth */
    text = format("%-12s", p->depths);
    put_str_hook(col, row, -1, COLOUR_WHITE, text);

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


/*
 * Simple macro to initialize structs
 */
#define S(s) s, sizeof(s)


/*
 * TMD_CUT descriptions
 */
static const struct state_info cut_data[] =
{
    { 1000, S("Mortal wound"), COLOUR_L_RED },
    {  200, S("Deep gash"),    COLOUR_RED },
    {  100, S("Severe cut"),   COLOUR_RED },
    {   50, S("Nasty cut"),    COLOUR_ORANGE },
    {   25, S("Bad cut"),      COLOUR_ORANGE },
    {   10, S("Light cut"),    COLOUR_YELLOW },
    {    0, S("Graze"),        COLOUR_YELLOW }
};


/*
 * Print cut indicator.
 */
static size_t prt_cut(struct player *p, int row, int col)
{
    PRINT_STATE(>, cut_data, p->timed[TMD_CUT], row, col);
    return 0;
}


/*
 * TMD_STUN descriptions
 */
static const struct state_info stun_data[] =
{
    {   100, S("Knocked out"), COLOUR_RED },
    {    50, S("Heavy stun"),  COLOUR_ORANGE },
    {     0, S("Stun"),        COLOUR_ORANGE }
};


/*
 * Print stun indicator.
 */
static size_t prt_stun(struct player *p, int row, int col)
{
    PRINT_STATE(>, stun_data, p->timed[TMD_STUN], row, col);
    return 0;
}


/*
 * player->hunger descriptions
 */
static const struct state_info hunger_data[] =
{
    { PY_FOOD_FAINT,   S("Faint"),  COLOUR_RED },
    { PY_FOOD_WEAK,    S("Weak"),   COLOUR_ORANGE },
    { PY_FOOD_ALERT,   S("Hungry"), COLOUR_YELLOW },
    { PY_FOOD_FULL,    S(""),       COLOUR_L_GREEN },
    { PY_FOOD_MAX + 1, S("Full"),   COLOUR_L_GREEN }
};


/*
 * Prints status of hunger
 */
static size_t prt_hunger(struct player *p, int row, int col)
{
    PRINT_STATE(<, hunger_data, p->food, row, col);
    return 0;
}


/*
 * For the various TMD_* effects
 */
static const struct state_info effects[] =
{
    { TMD_BLIND,       S("Blind"),      COLOUR_ORANGE },
    { TMD_PARALYZED,   S("Paralyzed!"), COLOUR_RED },
    { TMD_CONFUSED,    S("Confused"),   COLOUR_ORANGE },
    { TMD_AFRAID,      S("Afraid"),     COLOUR_ORANGE },
    { TMD_TERROR,      S("Terror"),     COLOUR_RED },
    { TMD_SPRINT,      S("Sprint"),     COLOUR_L_GREEN },
    { TMD_POISONED,    S("Poisoned"),   COLOUR_ORANGE },
    { TMD_OPP_ACID,    S("RAcid"),      COLOUR_SLATE },
    { TMD_OPP_ELEC,    S("RElec"),      COLOUR_BLUE },
    { TMD_OPP_FIRE,    S("RFire"),      COLOUR_RED },
    { TMD_OPP_COLD,    S("RCold"),      COLOUR_WHITE },
    { TMD_OPP_POIS,    S("RPois"),      COLOUR_GREEN },
    { TMD_OPP_CONF,    S("RConf"),      COLOUR_VIOLET },
    { TMD_AMNESIA,     S("Amnesiac"),   COLOUR_ORANGE },
    { TMD_SCRAMBLE,    S("Scrambled"),  COLOUR_VIOLET },
    { TMD_IMAGE,       S("Hallu"),      COLOUR_ORANGE },
    { TMD_PROTEVIL,    S("ProtEvil"),   COLOUR_L_GREEN },
    { TMD_INVULN,      S("Invuln"),     COLOUR_L_GREEN },
    { TMD_HERO,        S("Hero"),       COLOUR_L_GREEN },
    { TMD_SHERO,       S("Berserk"),    COLOUR_L_GREEN },
    { TMD_BOLD,        S("Bold"),       COLOUR_L_GREEN },
    { TMD_STONESKIN,   S("Stone"),      COLOUR_L_GREEN },
    { TMD_SHIELD,      S("Shield"),     COLOUR_L_GREEN },
    { TMD_BLESSED,     S("Blessed"),    COLOUR_L_GREEN },
    { TMD_SINVIS,      S("SInvis"),     COLOUR_L_GREEN },
    { TMD_SINFRA,      S("Infra"),      COLOUR_L_GREEN },
    { TMD_WRAITHFORM,  S("Wraith"),     COLOUR_L_GREEN },
    { TMD_MEDITATE,    S("Medit"),      COLOUR_L_GREEN },
    { TMD_MANASHIELD,  S("MShield"),    COLOUR_L_GREEN },
    { TMD_INVIS,       S("Invis"),      COLOUR_L_GREEN },
    { TMD_MIMIC,       S("Mimic"),      COLOUR_L_GREEN },
    { TMD_TRAPSAFE,    S("TrapSafe"),   COLOUR_L_GREEN },
    { TMD_BOWBRAND,    S("Brand"),      COLOUR_L_GREEN },
    { TMD_ESP,         S("ESP"),        COLOUR_L_GREEN },
    { TMD_ANCHOR,      S("Anchor"),     COLOUR_L_GREEN },
    { TMD_PROBTRAVEL,  S("Proba"),      COLOUR_L_GREEN },
    { TMD_ADRENALINE,  S("Adren"),      COLOUR_L_GREEN },
    { TMD_BIOFEEDBACK, S("BioFB"),      COLOUR_L_GREEN },
    { TMD_TOUCH,       S("Vamp"),       COLOUR_L_GREEN },
    { TMD_SOUL,        S("Drain"),      COLOUR_L_GREEN },
    { TMD_DEADLY,      S("TDeath"),     COLOUR_L_GREEN },
    { TMD_EPOWER,      S("EPower"),     COLOUR_L_GREEN },
    { TMD_ICY_AURA,    S("IcyAura"),    COLOUR_WHITE },
    { TMD_SGRASP,      S("Brand"),      COLOUR_BLUE },
    { TMD_FARSIGHT,    S("Farsight"),   COLOUR_L_GREEN },
    { TMD_ZFARSIGHT,   S("Farsight"),   COLOUR_L_GREEN },
    { TMD_REGEN,       S("Regen"),      COLOUR_L_GREEN },
    { TMD_HARMONY,     S("Harmony"),    COLOUR_L_GREEN },
    { TMD_ANTISUMMON,  S("NoSummon"),   COLOUR_YELLOW }
};


/*
 * Print all timed effects.
 */
static size_t prt_tmd(struct player *p, int row, int col)
{
    size_t i, len = 0;

    for (i = 0; i < N_ELEMENTS(effects); i++)
    {
        if (p->timed[effects[i].value])
        {
            put_str_hook(col + len, row, -1, effects[i].attr, effects[i].str);
            len += effects[i].len;
        }
    }

    return len;
}


static const byte obj_feeling_color[] =
{
    /* Colors used to display each obj feeling */
    COLOUR_WHITE, /* "this looks like any other level." */
    COLOUR_L_PURPLE, /* "you sense an item of wondrous power!" */
    COLOUR_L_RED, /* "there are superb treasures here." */
    COLOUR_ORANGE, /* "there are excellent treasures here." */
    COLOUR_YELLOW, /* "there are very good treasures here." */
    COLOUR_YELLOW, /* "there are good treasures here." */
    COLOUR_L_GREEN, /* "there may be something worthwhile here." */
    COLOUR_L_GREEN, /* "there may not be much interesting here." */
    COLOUR_L_GREEN, /* "there aren't many treasures here." */
    COLOUR_L_BLUE, /* "there are only scraps of junk here." */
    COLOUR_L_BLUE /* "there are naught but cobwebs here." */
};


static const byte mon_feeling_color[] =
{
    /* Colors used to display each monster feeling */
    COLOUR_WHITE, /* "You are still uncertain about this place" */
    COLOUR_RED, /* "Omens of death haunt this place" */
    COLOUR_ORANGE, /* "This place seems murderous" */
    COLOUR_ORANGE, /* "This place seems terribly dangerous" */
    COLOUR_YELLOW, /* "You feel anxious about this place" */
    COLOUR_YELLOW, /* "You feel nervous about this place" */
    COLOUR_GREEN, /* "This place does not seem too risky" */
    COLOUR_GREEN, /* "This place seems reasonably safe" */
    COLOUR_BLUE, /* "This seems a tame, sheltered place" */
    COLOUR_BLUE /* "This seems a quiet, peaceful place" */
};


/*
 * Prints level feelings at status if they are enabled.
 */
static size_t prt_level_feeling(struct player *p, int row, int col)
{
    char obj_feeling_str[6];
    char mon_feeling_str[6];
    int new_col;
    byte obj_feeling_color_print;

    /* No feeling */
    if ((p->obj_feeling == -1) && (p->mon_feeling == -1)) return 0;

    /*
     * Convert object feeling to a symbol easier to parse for a human.
     *   0 -> '*' "this looks like any other level."
     *   1 -> '$' "you sense an item of wondrous power!" (special feeling)
     *   2 to 10 are feelings from 2 meaning superb feeling to 10 meaning naught but cobwebs
     * It is easier for the player to have poor feelings as a low number and superb feelings
     * as a higher one. So for display we reverse this numbers and substract 1. Thus (2-10)
     * becomes ('1'-'9' reversed). But before that check if the player has explored enough
     * to get a feeling. If not display as '?'.
     */
    if (p->obj_feeling == -1)
    {
        my_strcpy(obj_feeling_str, "?", sizeof(obj_feeling_str));
        obj_feeling_color_print = COLOUR_WHITE;
    }
    else
    {
        obj_feeling_color_print = obj_feeling_color[p->obj_feeling];
        if (p->obj_feeling == 0)
            my_strcpy(obj_feeling_str, "*", sizeof(obj_feeling_str));
        else if (p->obj_feeling == 1)
            my_strcpy(obj_feeling_str, "$", sizeof(obj_feeling_str));
        else
            strnfmt(obj_feeling_str, 5, "%d", (unsigned int)(11 - p->obj_feeling));
    }

    /*
     * Convert monster feeling to a symbol easier to parse for a human.
     *   0 -> '?' "this looks like any other level."
     *   1 to 9 are feelings from omens of death to quiet, peaceful
     * We also reverse this so that what we show is a danger feeling.
     */
    if (p->mon_feeling == 0)
        my_strcpy(mon_feeling_str, "?", sizeof(mon_feeling_str));
    else
        strnfmt(mon_feeling_str, 5, "%d", (unsigned int)(10 - p->mon_feeling));

    /* Display it */
    put_str_hook(col, row, -1, COLOUR_WHITE, "LF:");
    new_col = col + 3;
    put_str_hook(new_col, row, -1, mon_feeling_color[p->mon_feeling], mon_feeling_str);
    new_col += strlen(mon_feeling_str);
    put_str_hook(new_col, row, -1, COLOUR_WHITE, "-");
    new_col++;
    put_str_hook(new_col, row, -1, obj_feeling_color_print, obj_feeling_str);
    new_col += strlen(obj_feeling_str) + 1;
    return new_col - col;
}


/*
 * Print "unignoring" status
 */
static size_t prt_unignore(struct player *p, int row, int col)
{
    /* Unignoring */
    if (p->unignoring)
    {
        const char *text = "Unignoring";

        put_str_hook(col, row, -1, COLOUR_WHITE, text);
        return (strlen(text) + 1);
    }

    return 0;
}


/*
 * Print recall status
 */
static size_t prt_recall(struct player *p, int row, int col)
{
    if (p->word_recall)
    {
        const char *text = "Recall";

        put_str_hook(col, row, -1, COLOUR_WHITE, text);
        return (strlen(text) + 1);
    }

    return 0;
}


/*
 * Print deep descent status
 */
static size_t prt_descent(struct player *p, int row, int col)
{
    if (p->deep_descent)
    {
        const char *text = "Descent";

        put_str_hook(col, row, -1, COLOUR_WHITE, text);
        return (strlen(text) + 1);
    }

    return 0;
}


/*
 * Prints Resting or Stealth Mode status
 */
static size_t prt_state(struct player *p, int row, int col)
{
    byte attr = COLOUR_WHITE;
    const char *text = "";

    /* Resting */
    if (p->upkeep->resting)
        text = "Resting";

    /* Stealth mode */
    else if (p->stealthy)
    {
        attr = COLOUR_L_DARK;
        text = "Stealth Mode";
    }

    /* Display the info (or blanks) */
    put_str_hook(col, row, -1, attr, text);

    return (text[0]? (strlen(text) + 1): 0);
}


/*
 * Prints trap detection status
 */
static size_t prt_dtrap(struct player *p, int row, int col)
{
    byte attr = COLOUR_WHITE;
    const char *text = "";
    byte dtrap = get_dtrap(p);

    if (dtrap == 2)
    {
        attr = COLOUR_YELLOW;
        text = "DTrap";
    }
    if (dtrap == 1)
    {
        attr = COLOUR_L_GREEN;
        text = "DTrap";
    }

    /* Display the info (or blanks) */
    put_str_hook(col, row, -1, attr, text);

    return (text[0]? (strlen(text) + 1): 0);
}


/*
 * Print how many spells the player can study.
 */
static size_t prt_study(struct player *p, int row, int col)
{
    char *text;
    int attr = COLOUR_WHITE;

    /* Can the player learn new spells? */
    if (p->upkeep->new_spells)
    {
        /*
         * If the player does not carry a book with spells they can study,
         * the message is displayed in a darker colour
         */
        if (!p->can_study_book) attr = COLOUR_L_DARK;

        /* Print study message */
        text = format("Study (%d)", p->upkeep->new_spells);
        put_str_hook(col, row, -1, attr, text);
        return (strlen(text) + 1);
    }

    return 0;
}


/*
 * Descriptive typedef for status handlers
 */
typedef size_t status_f(struct player *p, int row, int col);


/*
 * Status line indicators.
 */
static status_f *status_handlers[] =
{
    prt_level_feeling, prt_unignore, prt_recall, prt_descent, prt_state, prt_cut, prt_stun,
    prt_hunger, prt_study, prt_tmd, prt_dtrap
};


/*
 * Print the status line.
 */
void display_statusline(struct player *p, int row, int col)
{
    size_t i;

    /* Display those which need redrawing */
    for (i = 0; i < N_ELEMENTS(status_handlers); i++)
        col += status_handlers[i](p, row, col);
}


/*
 * Print the status display subwindow
 */
void display_status_subwindow(struct player *p, int row, int col)
{
    size_t i;

    for (i = 0; i < N_ELEMENTS(status_handlers); i++)
        status_handlers[i](p, row++, col);
}

