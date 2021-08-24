

/* File: misc.c */

/*
 * Copyright (c) 1997 Ben Harrison, James E. Wilson, Robert A. Koeneke
 *
 * This software may be copied and distributed for educational, research,
 * and not for profit purposes provided that this copyright and statement
 * are included in all such copies. Other copyrights may also apply.
 */

/* Purpose: misc code */

#include "angband.h"
#include "equip.h"
#include "z-doc.h"

#include <assert.h>

/*** Screen Locations ***/

/*
 * Some screen locations for various display routines
 * Currently, row 8 and 15 are the only "blank" rows.
 * That leaves a "border" around the "stat" values.
 */

#define ROW_LEVEL               0
#define COL_LEVEL               0       /* "LEVEL xxxxxx" */

#define ROW_EXP                 1
#define COL_EXP                 0       /* "EXP xxxxxxxx" */

#define ROW_GOLD                2
#define COL_GOLD                0       /* "AU xxxxxxxxx" */

/*
#define ROW_EQUIPPY             3
#define COL_EQUIPPY             0
*/

#define ROW_STAT                4       /* Str = 5 ... Chr = 10 */
#define COL_STAT                0

#define ROW_AC                  10
#define COL_AC                  0       /* "Cur AC xxxxx" */

#define ROW_CURHP               11
#define COL_CURHP               0       /* "Cur HP xxxxx" */

#define ROW_CURSP               12
#define COL_CURSP               0       /* "Cur SP xxxxx" */

#define ROW_STATE               13
#define COL_STATE               7

#define ROW_HEALTH_BARS         14
#define COUNT_HEALTH_BARS       6       /* HP, SP, Food, Riding, Monster Track, Target */

#define ROW_EFFECTS            20
#define COUNT_EFFECTS          11       /* Could be off screen ... */

static int _npow(int x, int y)
{
    int r = 1;
    int i;
    for (i = 0; i < y; i++)
        r *= x;
    return r;
}

/* This is mostly for shops/services when the pricing grows
   too large. For example, when attempting to enchant Aglarang,
   I get the following prompt of pricing (up to the ->, of course):
                  Amt      Cost
               a) + 1     67589gp -> 67.6k
               b) + 2    139103gp ->  139k
               c) + 3    214674gp ->  215k
               d) + 4    294399gp ->  294k
               e) + 5    378409gp ->  378k
               f) + 6    466782gp ->  467k
               g) + 7    559689gp ->  560k
               h) + 8    657187gp ->  657k
               i) + 9    759447gp ->  759k
               j) +10    866527gp ->  867k
               k) +11   1176810gp -> 1.18M
*/
int big_num_round(int num, int sig_figs)
{
    assert (1 <= sig_figs);
    assert (sig_figs <= 10); /* 32-bit */

    if (num < 0)
    {
        int tmp = big_num_round(-num, sig_figs);
        return -tmp;
    }
    else
    {
        int m, q, r, t;

        /* division algorithm: n = q*m + r
           we scale m up until q has desired number of significant figures */
        t = _npow(10, sig_figs);
        for (m = 1; ; m *= 10)
        {
            q = num / m;
            if (q < t)
                break;
        }
        r = num % m;

        /* if r/m >= 1/2, then round up */
        if (2*r >= m)
            q++;

        return q*m;
    }

}

/* Convert a large number into something we humans can read.
   For example, I don't care if I have 143798124gp. I'd rather
   just know I have 144Mgp.

   123     -> 123
   1234    -> 1.23k
   12345   -> 12.3k
   123456  -> 123k
   1234567 -> 1.23M, etc */
void big_num_display(int num, char *buf)
{
    if (num < 0)
    {
        char tmp[10];
        big_num_display(-num, tmp);
        sprintf(buf, "-%s", tmp);
    }
    else if (num < 1000)
        sprintf(buf, "%d", num);
    else
    {
        const char _prefix[4] = { ' ', 'k', 'M', 'G' };
        int digits[10];
        int n = big_num_round(num, 3);
        int s = 0;
        int i, j;

        /* Get all the digits */
        for (;;)
        {
            int d = n % 10;
            assert(0 <= s && s < 10);
            digits[s] = d;
            n /= 10;
            if (n == 0)
                break;
            s++;
        }
        assert(s >= 3);

        /* Display 3 Significant Figures */
        for (i = 0, j = 0; i < 3; i++)
        {
            int p = s-i;
            int d = digits[p];

            buf[j++] = '0' + d;

            /* Check for decimal place except on last digit */
            if ((p % 3) == 0 && i < 2)
                buf[j++] = '.';
        }

        assert(1 <= s/3 && s/3 < 4);
        buf[j++] = _prefix[s/3];
        buf[j] = '\0';
    }
}

void check_mon_health_redraw(mon_ptr mon)
{
    if (!mon) return;
    if (plr->health_who == mon->id)
        plr->redraw |= PR_HEALTH_BARS;
    else if (plr->riding == mon->id)
        plr->redraw |= PR_HEALTH_BARS;
    else if (mon == who_mon(plr->target))
        plr->redraw |= PR_HEALTH_BARS;
}

/*
 * Wrap calculation of AC bonuses from Dex
 */
static int calc_adj_dex_ta(void)
{
    return ((int)(adj_dex_ta[plr->stat_ind[A_DEX]]) - 128);
}

/*
 * Converts stat num into a six-char (right justified) string
 */
void cnv_stat(int val, char *out_val)
{
    /* Above 18 */
    if (val > 18)
    {
        int bonus = (val - 18);

        if (bonus >= 220)
        {
            sprintf(out_val, "18/%3s", "***");
        }
        else if (bonus >= 100)
        {
            sprintf(out_val, "18/%03d", bonus);
        }
        else
        {
            sprintf(out_val, " 18/%02d", bonus);
        }
    }

    /* From 3 to 18 */
    else
    {
        sprintf(out_val, "    %2d", val);
    }
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
s16b modify_stat_value(int value, int amount)
{
    int    i;

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
 *  Whether daytime or not
 */
bool is_daytime(void)
{
    u32b len = TURNS_PER_TICK * TOWN_DAWN;
    if ((dun_mgr()->turn % len) < (len / 2))
        return TRUE;
    else
        return FALSE;
}

/*
 * Extract day, hour, min
 */
void extract_day_hour_min(int *day, int *hour, int *min)
{
    extract_day_hour_min_imp(dun_mgr()->turn, day, hour, min);
}
void extract_day_hour_min_imp(int turn, int *day, int *hour, int *min)
{
    const s32b A_DAY = TURNS_PER_TICK * TOWN_DAWN;
    s32b turn_in_today = (turn + A_DAY / 4) % A_DAY;

    switch (plr->start_race)
    {
    case RACE_VAMPIRE:
    case RACE_MON_VAMPIRE:
    case RACE_SKELETON:
    case RACE_ZOMBIE:
    case RACE_SPECTRE:
        *day = (turn - A_DAY * 3 / 4) / A_DAY + 1;
        break;
    default:
        *day = (turn + A_DAY / 4) / A_DAY + 1;
        break;
    }
    *hour = (24 * turn_in_today / A_DAY) % 24;
    *min = (1440 * turn_in_today / A_DAY) % 60;
}

/*
 * Print time
 */
void prt_time(void)
{
    /* TODO: I'm removing this from top screen ... Cleanup callers
    int day, hour, min;
    c_put_str(TERM_WHITE, "             ", ROW_DAY, COL_DAY);
    extract_day_hour_min(&day, &hour, &min);
    if (day < 1000) c_put_str(TERM_WHITE, format("Day%3d", day), ROW_DAY, COL_DAY);
    else c_put_str(TERM_WHITE, "Day***", ROW_DAY, COL_DAY);
    c_put_str(TERM_WHITE, format("%2d:%02d", hour, min), ROW_DAY, COL_DAY+7);
    */
}



#define EQUIPPY_MAIN 1

/*
 * Equippy chars
 */
static void display_player_equippy(int y, int x, u16b mode)
{
    int i;
    term_char_t tc;

    object_type *o_ptr;

    Term_erase(x, y, 12);

    /* Dump equippy chars */
    for (i = 1; i <= equip_max(); i++)
    {
        o_ptr = equip_obj(i);

        if (mode == EQUIPPY_MAIN && i > 12) break; /* Hack: This will overwrite the map display otherwise ... */

        if (o_ptr && equippy_chars)
        {
            tc = obj_display_char(o_ptr);
        }
        else
        {
            tc.c = ' ';
            tc.a = TERM_WHITE;
        }
        if (term_char_is_ascii(tc))
            Term_queue_term_char(point_create(x+i-1, y), tc);
        else
        {
            map_char_t mc = {0};
            map_char_push(&mc, visual_get("CHASM", 0));
            map_char_push(&mc, tc);
            Term_queue_map_char(point_create(x+i-1, y), &mc);
        }
    }
}

#define ROW_EQUIPPY             3
#define COL_EQUIPPY             0       /* equippy chars */

static void print_equippy(void)
{
    rect_t r = ui_char_info_rect();
    display_player_equippy(r.y + ROW_EQUIPPY, r.x + COL_EQUIPPY, EQUIPPY_MAIN);
}

/*
 * Print character stat in given row, column
 */
static void prt_stat(int stat)
{
    char tmp[32];
    rect_t r = ui_char_info_rect();


    /* Display "injured" stat */
    if (plr->stat_cur[stat] < plr->stat_max[stat])
    {
        put_str(stat_names_reduced[stat], r.y + ROW_STAT + stat, r.x + COL_STAT);
        cnv_stat(plr->stat_use[stat], tmp);
        c_put_str(TERM_YELLOW, tmp, r.y + ROW_STAT + stat, r.x + COL_STAT + 6);
    }

    /* Display "healthy" stat */
    else
    {
        put_str(stat_names[stat], r.y + ROW_STAT + stat, r.x + COL_STAT);
        cnv_stat(plr->stat_use[stat], tmp);
        c_put_str(TERM_L_GREEN, tmp, r.y + ROW_STAT + stat, r.x + COL_STAT + 6);
    }

    /* Indicate natural maximum */
    if (plr->stat_max[stat] == plr->stat_max_max[stat])
    {
        put_str("!", r.y + ROW_STAT + stat, r.x + 3);

    }
}


#if 0
    {TERM_VIOLET, "Kw", "Kawarimi"},
    {TERM_L_DARK, "Mr", "Mirr"},
    {TERM_RED, "TCf", "TchCnf"},
    {TERM_UMBER, "Sl", "Stealth"},
    {TERM_L_DARK, "Hd", "Hiding"},
    {TERM_L_BLUE, "Rc", "Recall"},
    {TERM_WHITE, "Al", "Alter"},
    {TERM_WHITE, "SCo", "SCold"},
    {TERM_BLUE, "SEl", "SElec"},
    {TERM_L_DARK, "SSh", "SShadow"},
    {TERM_YELLOW, "EMi", "ExMight"},
    {TERM_RED, "Gi", "Giant"},
    {TERM_L_DARK, "AMl", "AntiMulti"},
    {TERM_ORANGE, "AT", "AntiTele"},
    {TERM_RED, "AM", "AntiMagic"},
    {TERM_SLATE, "Pa", "Patience"},
    {TERM_SLATE, "Rv", "Revenge"},
    {TERM_L_DARK, "Rs", "RuneSword"},
    {TERM_RED, "Vm", "Vampiric"},
    {TERM_WHITE, "Cu", "Cure"},
    {TERM_L_DARK, "ET", "EvilTele"},
    {TERM_RED, "At", "Attacks"},
    {TERM_ORANGE, "Sh", "Shield"},
    {TERM_YELLOW, "Sk", "Seek"},
    {TERM_RED, "Rv", "Revenge"},
    {TERM_L_BLUE, "Si", "Sight"},
    {TERM_WHITE, "Fs", "Feast"},
    {TERM_YELLOW, "Q", "Quick"},
    {TERM_L_BLUE, "Sp", "Special"},
    {TERM_YELLOW, "", "Duelist Target Goes Here!"},
    {TERM_L_RED, "Fl", "Flurry"},
    {TERM_RED, "Fl", "FLURRY"},
    {TERM_RED, "Rt", "Rite"},
    {TERM_L_DARK, "Dis", "Disenchant"},
    {TERM_L_BLUE, "ST", "StopTime"},
    {TERM_L_DARK, "DS", "Stealth"},
    {TERM_L_BLUE, "ND", "Dodge"},
    {TERM_VIOLET, "KS", "Spree"},
    {TERM_YELLOW, "QW", "Quickwalk"},
    {TERM_L_BLUE, "IP", "InvenProt"},
    {TERM_YELLOW, "Sk", "Shrike"},
    {TERM_L_BLUE, "ST", "STime"},
    {TERM_YELLOW, "2", "DblMove"},
    {TERM_L_BLUE, "Sl", "Stealth"},
    {TERM_L_BLUE, "Fs", "Fast"},
    {TERM_L_BLUE, "Rl", "Retaliate"},
    {TERM_L_BLUE, "Of", "Offense"},
    {TERM_L_BLUE, "Df", "Defense"},
    {TERM_L_BLUE, "Bl", "Blink"},
    {TERM_L_GREEN, "DT", "DTrap"},
    {TERM_YELLOW, "DT", "DTrap"},
    {TERM_YELLOW, "Lt", "Light"},
    {TERM_L_DARK, "Dk", "Dark"},
    {TERM_YELLOW, "$$", "Hoarding"},
    {0, NULL, NULL}
};
#endif
/*
 *  Show status bar
 */
static void prt_status(void)
{
    plr_tim_status_bar();
    #if 0
    if (!view_unsafe_grids && in_bounds(plr->pos.y, plr->pos.x))
    {
        if (cave_at(plr->pos)->info & CAVE_IN_DETECT)
            ADD_FLG(BAR_DTRAP);
    }

    if (prace_is_(RACE_MON_VAMPIRE))
    {
        if ((cave_at(plr->pos)->info & (CAVE_GLOW | CAVE_MNDK)) == CAVE_GLOW)
            ADD_FLG(BAR_VAMPIRE_LIGHT);
        else
            ADD_FLG(BAR_VAMPIRE_DARK);
    }


    if (plr->special_defense & NINJA_KAWARIMI)
    {
        ADD_FLG(BAR_KAWARIMI);
        if (prace_is_(RACE_MON_SPIDER))
            bar[BAR_KAWARIMI].lstr = "PhaseShield";
    }

    /* Confusing Hands */
    if (plr->special_attack & ATTACK_CONFUSE) ADD_FLG(BAR_ATTKCONF);


    /* Mahouken */

    if (plr->tim_sh_domination) ADD_FLG(BAR_SH_DOMINATION);

    if (plr->pclass == CLASS_MYSTIC)
    {
        switch(mystic_get_toggle())
        {
        case MYSTIC_TOGGLE_STEALTH:
            ADD_FLG(BAR_MYSTIC_STEALTH);
            break;
        case MYSTIC_TOGGLE_FAST:
            ADD_FLG(BAR_MYSTIC_FAST);
            break;
        case MYSTIC_TOGGLE_RETALIATE:
            ADD_FLG(BAR_MYSTIC_RETALIATE);
            break;
        case MYSTIC_TOGGLE_OFFENSE:
            ADD_FLG(BAR_MYSTIC_OFFENSE);
            break;
        case MYSTIC_TOGGLE_DEFENSE:
            ADD_FLG(BAR_MYSTIC_DEFENSE);
            break;
        }
    }

    if (plr->prace == RACE_MON_LEPRECHAUN)
    {
        switch(leprechaun_get_toggle())
        {
        case LEPRECHAUN_TOGGLE_BLINK:
            ADD_FLG(BAR_BLINK);
            break;
        case LEPRECHAUN_TOGGLE_HOARDING:
            ADD_FLG(BAR_HOARDING);
            break;
        }
    }
    if (plr->prace == RACE_MON_POSSESSOR || plr->prace == RACE_MON_MIMIC)
    {
        switch(possessor_get_toggle())
        {
        case LEPRECHAUN_TOGGLE_BLINK:
            ADD_FLG(BAR_BLINK);
            break;
        }
    }


    if (plr->sense_artifact) ADD_FLG(BAR_SPECIAL);

    if (world_player) ADD_FLG(BAR_THE_WORLD);

    #endif
}

/*
 * Prints level
 */
static void prt_level(void)
{
    char tmp[32];
    rect_t r = ui_char_info_rect();

    sprintf(tmp, "%6d", plr->lev);

    if (plr->lev >= plr->max_plv)
    {
        put_str("LEVEL ", r.y + ROW_LEVEL, r.x + COL_LEVEL);
        c_put_str(TERM_L_GREEN, tmp, r.y + ROW_LEVEL, r.x + COL_LEVEL + 6);

    }
    else
    {
        put_str("Level ", r.y + ROW_LEVEL, r.x + COL_LEVEL);
        c_put_str(TERM_YELLOW, tmp, r.y + ROW_LEVEL, r.x + COL_LEVEL + 6);

    }
}

/*
 * Display the experience
 */
static void prt_exp(void)
{
    char out_val[32];
    rect_t r = ui_char_info_rect();

    if (!exp_need || plr->prace == RACE_ANDROID)
    {
        char tmp[10];
        big_num_display(plr->exp, tmp);
        sprintf(out_val, "%8.8s", tmp);
    }
    else
    {
        if (plr->lev >= PY_MAX_LEVEL)
            (void)sprintf(out_val, "********");
        else
        {
            char tmp[10];
            int  diff = exp_requirement(plr->lev) - plr->exp;
            big_num_display(diff, tmp);
            sprintf(out_val, "%8.8s", tmp);
        }
    }

    if (plr->exp >= plr->max_exp)
    {
        if (plr->prace == RACE_ANDROID) put_str("Cst ", r.y + ROW_EXP, r.x + COL_EXP);
        else put_str("EXP ", r.y + ROW_EXP, r.x + COL_EXP);
        c_put_str(TERM_L_GREEN, out_val, r.y + ROW_EXP, r.x + COL_EXP + 4);
    }
    else
    {
        put_str("Exp ", r.y + ROW_EXP, r.x + COL_EXP);
        c_put_str(TERM_YELLOW, out_val, r.y + ROW_EXP, r.x + COL_EXP + 4);
    }
}

/*
 * Prints current gold
 */
static void prt_gold(void)
{
    char tmp[10];
    char out_val[32];
    rect_t r = ui_char_info_rect();

    big_num_display(plr->au, tmp);
    sprintf(out_val, "%8.8s", tmp);


    put_str("AU ", r.y + ROW_GOLD, r.x + COL_GOLD);
    c_put_str(TERM_L_GREEN, out_val, r.y + ROW_GOLD, r.x + COL_GOLD + 4);
}



/*
 * Prints current AC
 */
static void prt_ac(void)
{
    char tmp[32];
    rect_t r = ui_char_info_rect();

    put_str("AC ", r.y + ROW_AC, r.x + COL_AC);
    sprintf(tmp, "%5d", plr->dis_ac + plr->dis_to_a);
    c_put_str(TERM_L_GREEN, tmp, r.y + ROW_AC, r.x + COL_AC + 7);

}

/*
 * Prints Cur/Max hit points
 */
static void prt_hp(void)
{
    char tmp[32];
    rect_t r = ui_char_info_rect();

    byte color;

    put_str("HP", r.y + ROW_CURHP, r.x + COL_CURHP);

    sprintf(tmp, "%4d", plr->chp);

    if (plr->chp >= plr->mhp)
        color = TERM_L_GREEN;
    else if (plr->chp > (plr->mmhp * hitpoint_warn) / 10)
        color = TERM_YELLOW;
    else
        color = TERM_RED;

    c_put_str(color, tmp, r.y + ROW_CURHP, r.x + COL_CURHP + 3);

    put_str( "/", r.y + ROW_CURHP, r.x + COL_CURHP + 7 );

    sprintf(tmp, "%4d", plr->mmhp);
    color = TERM_L_GREEN;

    c_put_str(color, tmp, r.y + ROW_CURHP, r.x + COL_CURHP + 8);
}

/*
 * Prints players max/cur spell points
 */
static void prt_sp(void)
{
    char tmp[32];
    byte color;
    rect_t r = ui_char_info_rect();

    if (plr->msp == 0)
    {
        Term_erase(r.x + COL_CURSP, r.y + ROW_CURSP, r.cx);
        return;
    }

    put_str("SP", r.y + ROW_CURSP, r.x + COL_CURSP);
    sprintf(tmp, "%4d", plr->csp);
    if (plr->csp >= plr->msp)
        color = TERM_L_GREEN;
    else if (plr->csp > (plr->msp * mana_warn) / 10)
        color = TERM_YELLOW;
    else
        color = TERM_RED;

    c_put_str(color, tmp, r.y + ROW_CURSP, r.x + COL_CURSP + 3);

    put_str( "/", r.y + ROW_CURSP, r.x + COL_CURSP + 7 );

    sprintf(tmp, "%4d", plr->msp);
    color = TERM_L_GREEN;

    c_put_str(color, tmp, r.y + ROW_CURSP, r.x + COL_CURSP + 8);
}

static int _depth_width = 0;
rect_t ui_status_bar_rect(void)
{
    return rect_create(
        _depth_width,
        Term->hgt - 1,
        Term->wid - _depth_width,
        1
    );
}

static void prt_depth(void)
{
    char buf[100];
    byte attr = TERM_WHITE;
    rect_t r;

    /* Hack: We share space with the statusbar, and we
       are always drawn first. ui_status_bar_rect() will
       adjust the result depending on the value of _depth_width.*/
    _depth_width = 0;
    r = ui_status_bar_rect();

    if (cave->type->id == D_SURFACE)
    {
        if (dun_world_town_id())
            sprintf(buf, "%s", town_name(dun_world_town_id()));
        else
            sprintf(buf, "Wilderness: L%d", cave->difficulty);
    }
    else if (cave->type->id == D_QUEST)
    {
        sprintf(buf, "Quest: L%d", cave->dun_lvl);
        /* Level is "special" until completed */
        if (quests_get_current()->status < QS_COMPLETED)
            attr = TERM_L_BLUE;
    }
    else
    {
        if (depth_in_feet)
            sprintf(buf, "%s: %d ft", cave->type->name, cave->dun_lvl * 50);
        else
            sprintf(buf, "%s: L%d", cave->type->name, cave->dun_lvl);

        /* Get color of level based on feeling  -JSV- */
        switch (cave->feeling)
        {
        case  0: attr = TERM_SLATE;   break; /* Unknown */
        case  1: attr = TERM_L_BLUE;  break; /* Special */
        case  2: attr = TERM_VIOLET;  break; /* Horrible visions */
        case  3: attr = TERM_RED;     break; /* Very dangerous */
        case  4: attr = TERM_L_RED;   break; /* Very bad feeling */
        case  5: attr = TERM_ORANGE;  break; /* Bad feeling */
        case  6: attr = TERM_YELLOW;  break; /* Nervous */
        case  7: attr = TERM_L_UMBER; break; /* Luck is turning */
        case  8: attr = TERM_L_WHITE; break; /* Don't like */
        case  9: attr = TERM_WHITE;   break; /* Reasonably safe */
        case 10: attr = TERM_WHITE;   break; /* Boring place */
        }
        /* Level is "special" until completed */
        if (quests_get_current() && quests_get_current()->status < QS_COMPLETED)
            attr = TERM_L_BLUE;
    }

    Term_erase(r.x, r.y, r.cx);
    c_put_str(attr, buf, r.y, r.x);
    _depth_width = strlen(buf) + 1;
}

/*
 * Prints Searching, Resting, Paralysis, or 'count' status
 * Display is always exactly 10 characters wide (see below)
 *
 * This function was a major bottleneck when resting, so a lot of
 * the text formatting code was optimized in place below.
 */
static void prt_state(void)
{
    byte attr = TERM_WHITE;
    rect_t r = ui_char_info_rect();

    char text[20];

    /* Repeating */
    if (command_rep)
    {
        if (command_rep > 999)
        {
            (void)sprintf(text, "%2d00", command_rep / 100);

        }
        else
        {
            (void)sprintf(text, "  %2d", command_rep);

        }
    }

    /* Action */
    else
    {
        switch(plr->action)
        {
            case ACTION_GLITTER:
            {
                strcpy(text, "Lure");
                break;
            }
            case ACTION_SEARCH:
            {
                strcpy(text, "Sear");
                break;
            }
            case ACTION_REST:
            {
                int i;

                /* Start with "Rest" */
                strcpy(text, "    ");


                /* Extensive (timed) rest */
                if (resting >= 1000)
                {
                    i = resting / 100;
                    text[3] = '0';
                    text[2] = '0';
                    text[1] = '0' + (i % 10);
                    text[0] = '0' + (i / 10);
                }

                /* Long (timed) rest */
                else if (resting >= 100)
                {
                    i = resting;
                    text[3] = '0' + (i % 10);
                    i = i / 10;
                    text[2] = '0' + (i % 10);
                    text[1] = '0' + (i / 10);
                }

                /* Medium (timed) rest */
                else if (resting >= 10)
                {
                    i = resting;
                    text[3] = '0' + (i % 10);
                    text[2] = '0' + (i / 10);
                }

                /* Short (timed) rest */
                else if (resting > 0)
                {
                    i = resting;
                    text[3] = '0' + (i);
                }

                /* Rest until healed */
                else if (resting == -1)
                {
                    text[0] = text[1] = text[2] = text[3] = '*';
                }

                /* Rest until done */
                else if (resting == -2)
                {
                    text[0] = text[1] = text[2] = text[3] = '&';
                }
                break;
            }
            case ACTION_LEARN:
            {
                strcpy(text, "Lear");
                if (new_mane) attr = TERM_L_RED;
                break;
            }
            case ACTION_KAMAE:
            {
                int i;
                for (i = 0; i < MAX_KAMAE; i++)
                    if (plr->special_defense & (KAMAE_GENBU << i)) break;
                switch (i)
                {
                    case 0: attr = TERM_GREEN;break;
                    case 1: attr = TERM_WHITE;break;
                    case 2: attr = TERM_L_BLUE;break;
                    case 3: attr = TERM_L_RED;break;
                }
                strcpy(text, kamae_shurui[i].desc);
                break;
            }
            case ACTION_KATA:
            {
                int i;
                for (i = 0; i < MAX_KATA; i++)
                    if (plr->special_defense & (KATA_IAI << i)) break;
                strcpy(text, kata_shurui[i].desc);
                break;
            }
            case ACTION_SING:
            {
                strcpy(text, "Sing");
                break;
            }
            case ACTION_QUICK_WALK:
            {
                strcpy(text, "Fast");
                break;
            }
            case ACTION_SPELL:
            {
                strcpy(text, "Spel");
                break;
            }
            case ACTION_STALK:
            {
                strcpy(text, "Stlk");
                break;
            }
            default:
            {
                strcpy(text, "    ");
                break;
            }
        }
    }

    /* Display the info (or blanks) */
    c_put_str(attr, format("%5.5s",text), r.y + ROW_STATE, r.x + COL_STATE);
}


static void prt_speed(doc_ptr doc)
{
    int speed = plr->pspeed;
    bool is_fast = plr_tim_find(T_FAST);
    bool is_slow = plr_tim_find(T_SLOW);
    byte attr = TERM_WHITE;
    char buf[32] = "";

    /* Hack -- Visually "undo" the Search Mode Slowdown */
    if (plr->action == ACTION_SEARCH && !plr_tim_find(T_LIGHT_SPEED)) speed += 10;

    /* Fast */
    if (speed > 0)
    {
        if (plr->riding)
        {
            monster_type *m_ptr = dun_mon(cave, plr->riding);
            if (mon_tim_find(m_ptr, T_FAST) && !mon_tim_find(m_ptr, T_SLOW)) attr = TERM_L_BLUE;
            else if (mon_tim_find(m_ptr, T_SLOW) && !mon_tim_find(m_ptr, T_FAST)) attr = TERM_VIOLET;
            else attr = TERM_GREEN;
        }
        else if ((is_fast && !is_slow) || plr_tim_find(T_LIGHT_SPEED)) attr = TERM_YELLOW;
        else if (is_slow && !is_fast) attr = TERM_VIOLET;
        else attr = TERM_L_GREEN;
        sprintf(buf, "Fast (+%d)", speed);
    }

    /* Slow */
    else if (speed < 0)
    {
        if (plr->riding)
        {
            monster_type *m_ptr = dun_mon(cave, plr->riding);
            if (mon_tim_find(m_ptr, T_FAST) && !mon_tim_find(m_ptr, T_SLOW)) attr = TERM_L_BLUE;
            else if (mon_tim_find(m_ptr, T_SLOW) && !mon_tim_find(m_ptr, T_FAST)) attr = TERM_VIOLET;
            else attr = TERM_RED;
        }
        else if (is_fast && !is_slow) attr = TERM_YELLOW;
        else if (is_slow && !is_fast) attr = TERM_VIOLET;
        else attr = TERM_L_UMBER;
        sprintf(buf, "Slow (%d)", speed);
    }
    else if (plr->riding)
    {
        attr = TERM_GREEN;
        strcpy(buf, "Riding");
    }
    else return;

    doc_printf(doc, "<color:%c>%s</color>\n", attr_to_attr_char(attr), buf);
}

/*****************************************************************************
 Effects (Cut, Stun, Fear, etc)
 *****************************************************************************/
static void prt_fear(doc_ptr doc)
{
    int lvl = fear_level_p();

    switch (lvl)
    {
    case FEAR_UNEASY:
        doc_insert(doc, "<color:U>Uneasy</color>\n");
        break;
    case FEAR_NERVOUS:
        doc_insert(doc, "<color:y>Nervous</color>\n");
        break;
    case FEAR_SCARED:
        doc_insert(doc, "<color:o>Scared</color>\n");
        break;
    case FEAR_TERRIFIED:
        doc_insert(doc, "<color:r>Terrified</color>\n");
        break;
    case FEAR_PETRIFIED:
        doc_insert(doc, "<color:v>Petrified</color>\n");
        break;
    }
}

static void prt_food(doc_ptr doc)
{
    if (plr->food < PY_FOOD_FAINT)
        doc_insert(doc, "<color:r>Faint</color>\n");
    else if (plr->food < PY_FOOD_WEAK)
        doc_insert(doc, "<color:o>Weak</color>\n");
    else if (plr->food < PY_FOOD_ALERT)
        doc_insert(doc, "<color:y>Hungry</color>\n");
    else if (plr->food < PY_FOOD_MAX)
        doc_insert(doc, "<color:G>Full</color>\n");
    else
        doc_insert(doc, "<color:g>Gorged</color>\n");
}

static void prt_effects(void)
{
    int i;
    rect_t r = ui_char_info_rect();
    doc_ptr doc = doc_alloc(r.cx);

    doc_insert(doc, "<style:table>");

    r.y += ROW_EFFECTS;
    r.cy -= ROW_EFFECTS;
    Term_clear_rect(r);

    prt_speed(doc);
    if (plr->cursed & 0x0000000F)
    {
        if (plr->cursed & OFC_PERMA_CURSE)
            doc_insert(doc, "<color:D>*CURSED*</color>\n");
        else if (plr->cursed & OFC_HEAVY_CURSE)
            doc_insert(doc, "<color:D>CURSED</color>\n");
        else
            doc_insert(doc, "<color:D>Cursed</color>\n");
    }

    if (plr_tim_find(T_SUPERSTEALTH) || plr->pclass == CLASS_NINJA)
    {
        if (plr->special_defense & NINJA_S_STEALTH)
            doc_insert(doc, "<color:D>Hiding</color>\n");
        else
            doc_insert(doc, "<color:y>Exposed</color>\n");
    }
    if (plr_tim_find(T_CLOAK_INVIS))
    {
        if (plr->special_defense & DEFENSE_INVISIBLE)
            doc_insert(doc, "<color:D>Invisible</color>\n");
        else
            doc_insert(doc, "<color:y>Exposed</color>\n");
    }
    if (plr->mimic_form != MIMIC_NONE)
        doc_printf(doc, "<color:r>[%.10s]</color>\n", plr_race()->name);
    if (monk_armour_aux)
        doc_insert(doc, "<color:r>Heavy Armor</color>\n");
    if (plr->cumber_glove)
        doc_insert(doc, "<color:r>Encumbrance</color>\n");
    for (i = 0; i < MAX_HANDS; i++)
    {
        if (have_flag(plr->attack_info[i].paf_flags, PAF_HEAVY))
        {
            doc_insert(doc, "<color:r>Heavy Wield</color>\n");
            break;
        }
    }
    for (i = 0; i < MAX_HANDS; i++)
    {
        if (have_flag(plr->attack_info[i].paf_flags, PAF_ICKY))
        {
            doc_insert(doc, "<color:g>Icky Wield</color>\n");
            break;
        }
    }
    if (plr->shooter_info.heavy_shoot)
        doc_insert(doc, "<color:r>Heavy Shoot</color>\n");
    plr_tim_prt_effects(doc);
    if (plr->afraid) prt_fear(doc);
    if (plr->clp != 1000)
    {
        byte color;
        int  pct = plr->clp/10;
        if (pct < 25) color = TERM_VIOLET;
        else if (pct < 50) color = TERM_RED;
        else if (pct < 70) color = TERM_L_RED;
        else if (pct < 90) color = TERM_YELLOW;
        else if (pct <= 100) color = TERM_L_UMBER;
        else if (pct < 110) color = TERM_L_UMBER;
        else if (pct < 120) color = TERM_YELLOW;
        else if (pct < 130) color = TERM_L_RED;
        else if (pct < 150) color = TERM_RED;
        else color = TERM_VIOLET;
        doc_printf(doc, "Life:<color:%c>%d%%</color>\n", attr_to_attr_char(color), pct);
    }
    if (plr->food >= PY_FOOD_FULL || plr->food < PY_FOOD_ALERT)
        prt_food(doc);
    if (plr->wizard)
        doc_insert(doc, "<color:B>Wizard</color>\n");
    /* XXX These could be hooked, but so many classes need to "Study" */
    if (plr->pclass == CLASS_SKILLMASTER)
    {
        int amt = skillmaster_new_skills();
        if (amt > 0)
            doc_printf(doc, "<color:B>Study (%d)</color>\n", amt);
    }
    else if (plr->new_spells && plr->pclass != CLASS_RAGE_MAGE && plr->pclass != CLASS_SAMURAI)
        doc_printf(doc, "<color:B>Study (%d)</color>\n", plr->new_spells);
    if (plr->fasting)
        doc_insert(doc, "<color:g>Fasting</color>\n");

    plr_hook_prt_effects(doc);

    doc_sync_term(doc, doc_range_top_lines(doc, r.cy), doc_pos_create(r.x, r.y)); 
    doc_free(doc);
}

/*****************************************************************************
 Health/Status Bars
 *****************************************************************************/
static void prt_hp_bar(doc_ptr doc)
{
    byte a;
    int  pct;
    
    doc_insert_term_char(doc, plr_get_display_char_attr());

    pct = 100 * plr->chp / plr->mhp;

    if (pct >= 100) a = TERM_L_GREEN;
    else if (pct > hitpoint_warn*10) a = TERM_YELLOW;
    else a = TERM_RED;

    doc_printf(doc, " <color:%c>%3d%%</color>", attr_to_attr_char(a), pct);
    if (plr_tim_find(T_STUN))
        doc_printf(doc, " <color:B>%3d%%</color>", plr_tim_amount(T_STUN));
    doc_newline(doc);
}

static void prt_sp_bar(doc_ptr doc)
{
    byte a;
    int  pct;
    int  k_idx;
    int  tval = TV_LIFE_BOOK;

    if (plr->realm1)
        tval = TV_LIFE_BOOK + plr->realm1 - 1;

    k_idx = lookup_kind(tval, 0);
    doc_insert_char(doc, k_info[k_idx].x_attr, k_info[k_idx].x_char);

    pct = 100 * plr->csp / plr->msp;

    if (pct >= 100) a = TERM_L_GREEN;
    else if (pct > mana_warn*10) a = TERM_YELLOW;
    else a = TERM_RED;

    doc_printf(doc, " <color:%c>%3d%%</color>\n", attr_to_attr_char(a), pct);
}

static obj_ptr _lightsource(void)
{
    slot_t slot = equip_find_obj(TV_LIGHT, SV_ANY);
    if (slot)
        return equip_obj(slot);
    return NULL;
}
static void prt_light_bar(doc_ptr doc)
{
    int     k_idx;
    int     lite = plr_light(plr->pos);
    obj_ptr obj = _lightsource();

    if (obj) k_idx = obj->k_idx;
    else k_idx = lookup_kind(TV_LIGHT, SV_LIGHT_TORCH);
    
    doc_insert_char(doc, k_info[k_idx].x_attr, k_info[k_idx].x_char);
    doc_printf(doc, " %3d'", 10*plr->cur_light);
    if (lite <= 0)
        doc_printf(doc, " <color:D>%d</color>", -lite);
    else
        doc_printf(doc, " <color:y>%d</color>", lite);
    doc_newline(doc);
}

static void prt_food_bar(doc_ptr doc)
{
    byte attr;
    int  pct;
    int  k_idx;

    k_idx = lookup_kind(TV_FOOD, SV_FOOD_RATION);
    doc_insert_char(doc, k_info[k_idx].x_attr, k_info[k_idx].x_char);

    pct = 100 * plr->food / PY_FOOD_FULL;

    if (pct >= 100) attr = TERM_L_GREEN;
    else if (pct >= 50) attr = TERM_WHITE;
    else if (pct >= 30) attr = TERM_YELLOW;
    else if (pct >= 20) attr = TERM_ORANGE;
    else if (pct >= 10) attr = TERM_L_RED;
    else attr = TERM_VIOLET;

    doc_printf(doc, " <color:%c>%3d%%</color>\n", attr_to_attr_char(attr), pct);
}

static void prt_mon_health_bar(u32b m_idx, doc_ptr doc)
{
    monster_type *m_ptr;
    byte base_attr = TERM_WHITE;

    m_ptr = dun_mon(cave, m_idx);
    if (!m_ptr) return;

    if (m_ptr == who_mon(plr->target))
        base_attr = TERM_L_RED;

    else if (m_idx == plr->riding)
        base_attr = TERM_L_BLUE;

    /* Tracking an unseen monster */
    if (!m_ptr->ml)
    {
        
        doc_insert_term_char(doc, mon_race_visual_fuzzy(m_ptr->apparent_race));
        if (who_is_mon_id(plr->duelist_target, m_idx))
            doc_insert_char(doc, TERM_VIOLET, '*');
        else if (who_is_mon_id(plr->target, m_idx))
            doc_insert_char(doc, TERM_L_RED, '*');
        else
            doc_insert_char(doc, TERM_WHITE, ' ');
        doc_printf(doc, "  <color:%c>?%%</color>\n", attr_to_attr_char(base_attr));
    }

    /* Tracking a hallucinatory monster */
    else if (plr_tim_find(T_HALLUCINATE))
    {
        doc_insert_char(doc, base_attr, '?');
        if (who_is_mon_id(plr->duelist_target, m_idx))
            doc_insert_char(doc, TERM_VIOLET, '*');
        else if (who_is_mon_id(plr->target, m_idx))
            doc_insert_char(doc, TERM_L_RED, '*');
        else
            doc_insert_char(doc, TERM_WHITE, ' ');
        doc_printf(doc, "  <color:%c>?%%</color>\n", attr_to_attr_char(base_attr));
    }

    /* Tracking a dead monster (???) */
    else if (m_ptr->hp < 0)
    {
    }

    /* Tracking a visible monster */
    else if (m_ptr->maxhp > 0 && m_ptr->max_maxhp > 0)
    {
        int pct = 100 * m_ptr->hp / m_ptr->max_maxhp;
        byte attr = TERM_RED;/* Default to almost dead */

        doc_insert_term_char(doc, mon_visual(m_ptr));

        if (pct >= 100 || m_ptr->hp == m_ptr->maxhp) attr = TERM_L_GREEN;
        else if (pct >= 60) attr = TERM_YELLOW;
        else if (pct >= 25) attr = TERM_ORANGE;
        else if (pct >= 10) attr = TERM_L_RED;

        /* J* 100% ... */
        if (who_is_mon_id(plr->duelist_target, m_idx))
            doc_insert_char(doc, TERM_VIOLET, '*');
        else if (who_is_mon_id(plr->target, m_idx))
            doc_insert_char(doc, TERM_L_RED, '*');
        else if (m_idx == plr->riding)
            doc_insert_char(doc, TERM_L_BLUE, '@');
        else
            doc_insert_char(doc, TERM_WHITE, ' ');

        doc_printf(doc, "<color:%c>%3d%%</color> ", attr_to_attr_char(attr), pct); 
        mon_tim_display(m_ptr, doc);
        doc_newline(doc);
    }
}

static void prt_health_bars(void)
{
    int i;
    rect_t r = ui_char_info_rect();
    doc_ptr doc = doc_alloc(r.cx + 1);

    doc_insert(doc, "<style:table>");

    r.y = r.y + ROW_HEALTH_BARS;
    r.cy -= ROW_HEALTH_BARS;

    for (i = 0; i < COUNT_HEALTH_BARS; i++)
        Term_erase(r.x, r.y + i, r.cx);

    if (display_hp_bar)
        prt_hp_bar(doc);
    if (display_sp_bar && plr->msp)
        prt_sp_bar(doc);
    if (display_food_bar)
        prt_food_bar(doc);
    if (display_light_bar)
        prt_light_bar(doc);
    if (who_is_mon(plr->duelist_target))
        prt_mon_health_bar(who_mon(plr->duelist_target)->id, doc);
    if (plr->riding)
        prt_mon_health_bar(plr->riding, doc);
    if ( plr->health_who
      && plr->health_who != plr->riding
      && !who_is_mon_id(plr->duelist_target, plr->health_who) )
    {
        prt_mon_health_bar(plr->health_who, doc);
    }
    if ( who_is_mon(plr->target)
      && !who_is_mon_id(plr->target, plr->riding)
      && !who_is_mon_id(plr->target, plr->health_who)
      && !who_equals(plr->target, plr->duelist_target) )
    {
        prt_mon_health_bar(who_mon(plr->target)->id, doc);
    }
    if (who_is_pos(plr->target))
    {
        point_t d = point_subtract(who_pos(plr->target), plr->pos);
        doc_printf(doc, "<color:r>T:</color> %c%2d %c%2d\n", 
                (d.y > 0) ? 'S' : 'N', abs(d.y),
                (d.x > 0) ? 'E' : 'W', abs(d.x));
    }

    doc_sync_term(doc, doc_range_all(doc), doc_pos_create(r.x, r.y)); 
    doc_free(doc);
}

/*
 * Display basic info (mostly left of map)
 */
static void prt_frame_basic(void)
{
    int i;

    /* Level/Experience */
    prt_level();
    prt_exp();

    /* All Stats */
    for (i = 0; i < 6; i++) prt_stat(i);

    /* Armor */
    prt_ac();

    /* Hitpoints */
    prt_hp();

    /* Spellpoints */
    prt_sp();

    /* Gold */
    prt_gold();

    /* Current depth */
    prt_depth();
}


/*
 * Display extra info (mostly below map)
 */
static void prt_frame_extra(void)
{
    /* State */
    prt_state();

    prt_health_bars();
    prt_effects();

    prt_status();
}


/*
 * Hack -- display inventory in sub-windows
 */
static void _fix_inven_aux(void)
{
    doc_ptr doc;
    int     w, h;
    int     options = INV_IGNORE_INSCRIPTIONS;

    if (!use_pack_slots)
        options |= INV_NO_LABELS;

    Term_clear();
    Term_get_size(&w, &h);

    doc = doc_alloc(w);

    doc_insert(doc, "<style:table>");
    pack_display(doc, obj_exists, options);
    doc_insert(doc, "</style>");
    doc_sync_term(
        doc,
        doc_range_top_lines(doc, h),
        doc_pos_create(0, 0)
    );
    doc_free(doc);
}
static void fix_inven(void)
{
    int j;

    for (j = 0; j < 8; j++)
    {
        term *old = Term;
        if (!angband_term[j]) continue;
        if (!(window_flag[j] & PW_INVEN)) continue;
        Term_activate(angband_term[j]);
        _fix_inven_aux();
        Term_fresh();
        Term_activate(old);
    }
}

/*
 * Hack -- display equipment in sub-windows
 */
static void _fix_equip_aux(void)
{
    doc_ptr doc;
    int     w, h;

    Term_clear();
    Term_get_size(&w, &h);

    doc = doc_alloc(w);

    doc_insert(doc, "<style:table>");
    equip_display(doc, NULL, INV_IGNORE_INSCRIPTIONS);
    doc_insert(doc, "</style>");
    doc_sync_term(
        doc,
        doc_range_top_lines(doc, h),
        doc_pos_create(0, 0)
    );
    doc_free(doc);
}
static void fix_equip(void)
{
    int j;

    for (j = 0; j < 8; j++)
    {
        term *old = Term;
        if (!angband_term[j]) continue;
        if (!(window_flag[j] & (PW_EQUIP))) continue;
        Term_activate(angband_term[j]);
        _fix_equip_aux();
        Term_fresh();
        Term_activate(old);
    }
}


/*
 * Hack -- display equipment in sub-windows
 */
static void fix_spell(void)
{
    int j;

    /* Scan windows */
    for (j = 0; j < 8; j++)
    {
        term *old = Term;

        /* No window */
        if (!angband_term[j]) continue;

        /* No relevant flags */
        if (!(window_flag[j] & (PW_SPELL))) continue;

        /* Activate */
        Term_activate(angband_term[j]);

        /* Display spell list */
        /* display_spell_list();*/

        /* Fresh */
        Term_fresh();

        /* Restore */
        Term_activate(old);
    }
}


static void _fix_message_aux(void)
{
    int     i;
    doc_ptr doc;
    u32b    current_turn = 0;
    int     current_row = 0;
    int     w, h;

    Term_get_size(&w, &h);
    doc = doc_alloc(w);

    for (i = MIN(h, msg_count() - 1); i >= 0; i--)
    {
        msg_ptr m = msg_get(i);

        if (m->turn != current_turn)
        {
            if (doc_cursor(doc).y > current_row + 1)
                doc_newline(doc);
            current_turn = m->turn;
            current_row = doc_cursor(doc).y;
        }
        doc_insert_text(doc, m->color, str_buffer(m->msg));
        if (m->count > 1)
        {
            char buf[10];
            sprintf(buf, " (x%d)", m->count);
            doc_insert_text(doc, m->color, buf);
        }
        doc_newline(doc);
    }
    doc_sync_term(
        doc,
        doc_range_bottom_lines(doc, h),
        doc_pos_create(0, 0)
    );
    doc_free(doc);
}
/*
 * Display recent messages in sub-windows
 */
static void fix_message(void)
{
    int j;

    /* Scan windows */
    for (j = 0; j < 8; j++)
    {
        term *old = Term;

        /* No window */
        if (!angband_term[j]) continue;

        /* No relevant flags */
        if (!(window_flag[j] & (PW_MESSAGE))) continue;

        /* Activate */
        Term_activate(angband_term[j]);
        _fix_message_aux();
        Term_fresh();
        Term_activate(old);
    }
}


/*
 * Hack -- display overhead view in sub-windows
 *
 * Note that the "player" symbol does NOT appear on the map.
 */
static void fix_overhead(void)
{
    int j;

    int cy, cx;

    /* Scan windows */
    for (j = 0; j < 8; j++)
    {
        term *old = Term;
        int wid, hgt;

        /* No window */
        if (!angband_term[j]) continue;

        /* No relevant flags */
        if (!(window_flag[j] & (PW_OVERHEAD))) continue;

        /* Activate */
        Term_activate(angband_term[j]);

        /* Full map in too small window is useless  */
        Term_get_size(&wid, &hgt);
        /*??if (wid > COL_MAP + 2 && hgt > ROW_MAP + 2)*/
        {
            /* Redraw map */
            display_map(&cy, &cx);

            /* Fresh */
            Term_fresh();
        }

        /* Restore */
        Term_activate(old);
    }
}


/*
 * Hack -- display dungeon view in sub-windows
 */
static void fix_dungeon(void)
{
    int j;

    /* Scan windows */
    for (j = 0; j < 8; j++)
    {
        term *old = Term;

        /* No window */
        if (!angband_term[j]) continue;

        /* No relevant flags */
        if (!(window_flag[j] & (PW_DUNGEON))) continue;

        /* Activate */
        Term_activate(angband_term[j]);

        /* Redraw dungeon view */
        display_dungeon();

        /* Fresh */
        Term_fresh();

        /* Restore */
        Term_activate(old);
    }
}


/*
 * Hack -- display monster recall in sub-windows
 */
static void fix_monster(void)
{
    int j;

    /* Scan windows */
    for (j = 0; j < 8; j++)
    {
        term *old = Term;

        /* No window */
        if (!angband_term[j]) continue;

        /* No relevant flags */
        if (!(window_flag[j] & (PW_MONSTER))) continue;

        /* Activate */
        Term_activate(angband_term[j]);

        /* Display monster race info */
        if (plr->monster_race_idx)
        {
            mon_race_ptr race = mon_race_lookup(plr->monster_race_idx);
            int y;
            doc_ptr doc = doc_alloc(MIN(72, Term->wid));
            mon_display_doc(race, doc);

            for (y = 0; y < Term->hgt; y++)
                Term_erase(0, y, 255);

            doc_sync_term(doc, doc_range_all(doc), doc_pos_create(0, 0));
            doc_free(doc);
        }
        /* Fresh */
        Term_fresh();

        /* Restore */
        Term_activate(old);
    }
}


/*
 * Hack -- display object recall in sub-windows
 */
static void fix_object(void)
{
    int j;

    /* Scan windows */
    for (j = 0; j < 8; j++)
    {
        term *old = Term;

        /* No window */
        if (!angband_term[j]) continue;

        /* No relevant flags */
        if (!(window_flag[j] & (PW_OBJECT))) continue;

        /* Activate */
        Term_activate(angband_term[j]);

        /* Display monster race info */
        if (plr->object_kind_idx) display_koff(plr->object_kind_idx);

        /* Fresh */
        Term_fresh();

        /* Restore */
        Term_activate(old);
    }
}


/*
 * Calculate number of spells player should have, and forget,
 * or remember, spells until that number is properly reflected.
 *
 * Note that this function induces various "status" messages,
 * which must be bypasses until the character is created.
 */
static void calc_spells(void)
{
    int            i, j, k, levels;
    int            num_allowed;
    int                     num_boukyaku = 0;

    magic_type        *s_ptr;
    int which;
    int bonus = 0;


    cptr p;

    /* Hack -- must be literate */
    if (!mp_ptr->spell_book) return;

    /* Hack -- wait for creation */
    if (!character_generated) return;

    /* Hack -- handle "xtra" mode */
    if (character_xtra) return;

    if ( plr->pclass == CLASS_SORCERER
      || plr->pclass == CLASS_RED_MAGE
      || plr->pclass == CLASS_SKILLMASTER )
    {
        plr->new_spells = 0;
        return;
    }

    p = spell_category_name(mp_ptr->spell_book);

    /* Determine the number of spells allowed */
    levels = plr->lev - mp_ptr->spell_first + 1;

    /* Hack -- no negative spells */
    if (levels < 0) levels = 0;

    /* Extract total allowed spells */
    num_allowed = (adj_mag_study[plr->stat_ind[get_spell_stat()]] * levels / 2);

    if ((plr->pclass != CLASS_SAMURAI) && (mp_ptr->spell_book != TV_LIFE_BOOK))
    {
        bonus = 4;
    }
    if (plr->pclass == CLASS_SAMURAI || plr->pclass == CLASS_RAGE_MAGE)
    {
        num_allowed = 32;
    }
    else if ( plr->pclass == CLASS_MAGE
           || plr->pclass == CLASS_PRIEST
           || plr->pclass == CLASS_YELLOW_MAGE
           || plr->pclass == CLASS_GRAY_MAGE )
    {
        if (num_allowed>(96+bonus)) num_allowed = 96+bonus;
    }
    else if (plr->realm2 == REALM_NONE)
    {
        num_allowed = (num_allowed+1)/2;
        if (num_allowed>(32+bonus)) num_allowed = 32+bonus;
    }
    else
    {
        if (num_allowed>(80+bonus)) num_allowed = 80+bonus;
    }

    /* Count the number of spells we know */
    for (j = 0; j < 64; j++)
    {
        /* Count known spells */
        if ((j < 32) ?
            (plr->spell_forgotten1 & (1L << j)) :
            (plr->spell_forgotten2 & (1L << (j - 32))))
        {
            num_boukyaku++;
        }
    }

    /* See how many spells we must forget or may learn */
    plr->new_spells = num_allowed + plr->add_spells + num_boukyaku - plr->learned_spells;

    /* Forget spells which are too hard */
    for (i = 63; i >= 0; i--)
    {
        /* Efficiency -- all done */
        if (!plr->spell_learned1 && !plr->spell_learned2) break;
        if (plr->pclass == CLASS_RAGE_MAGE) break;
        if (plr->pclass == CLASS_SAMURAI) break;
        if (plr->pclass == CLASS_GRAY_MAGE) break;

        /* Access the spell */
        j = plr->spell_order[i];

        /* Skip non-spells */
        if (j >= 99) continue;


        /* Get the spell */
        if (!is_magic((j < 32) ? plr->realm1 : plr->realm2))
        {
            if (j < 32)
                s_ptr = &technic_info[plr->realm1 - MIN_TECHNIC][j];
            else
                s_ptr = &technic_info[plr->realm2 - MIN_TECHNIC][j%32];
        }
        else if (j < 32)
            s_ptr = &mp_ptr->info[plr->realm1-1][j];
        else
            s_ptr = &mp_ptr->info[plr->realm2-1][j%32];

        /* Skip spells we are allowed to know */
        if (s_ptr->slevel <= plr->lev) continue;

        /* Is it known? */
        if ((j < 32) ?
            (plr->spell_learned1 & (1L << j)) :
            (plr->spell_learned2 & (1L << (j - 32))))
        {
            /* Mark as forgotten */
            if (j < 32)
            {
                plr->spell_forgotten1 |= (1L << j);
                which = plr->realm1;
            }
            else
            {
                plr->spell_forgotten2 |= (1L << (j - 32));
                which = plr->realm2;
            }

            /* No longer known */
            if (j < 32)
            {
                plr->spell_learned1 &= ~(1L << j);
                which = plr->realm1;
            }
            else
            {
                plr->spell_learned2 &= ~(1L << (j - 32));
                which = plr->realm2;
            }

            /* Message */
            msg_format("You have forgotten the %s of %s.", p,
            do_spell(which, j%32, SPELL_NAME));


            /* One more can be learned */
            plr->new_spells++;
        }
    }


    /* Forget spells if we know too many spells */
    for (i = 63; i >= 0; i--)
    {
        /* Stop when possible */
        if (plr->new_spells >= 0) break;

        if (plr->pclass == CLASS_RAGE_MAGE) break;
        if (plr->pclass == CLASS_SAMURAI) break;
        if (plr->pclass == CLASS_GRAY_MAGE) break;

        /* Efficiency -- all done */
        if (!plr->spell_learned1 && !plr->spell_learned2) break;

        /* Get the (i+1)th spell learned */
        j = plr->spell_order[i];

        /* Skip unknown spells */
        if (j >= 99) continue;

        /* Forget it (if learned) */
        if ((j < 32) ?
            (plr->spell_learned1 & (1L << j)) :
            (plr->spell_learned2 & (1L << (j - 32))))
        {
            /* Mark as forgotten */
            if (j < 32)
            {
                plr->spell_forgotten1 |= (1L << j);
                which = plr->realm1;
            }
            else
            {
                plr->spell_forgotten2 |= (1L << (j - 32));
                which = plr->realm2;
            }

            /* No longer known */
            if (j < 32)
            {
                plr->spell_learned1 &= ~(1L << j);
                which = plr->realm1;
            }
            else
            {
                plr->spell_learned2 &= ~(1L << (j - 32));
                which = plr->realm2;
            }

            /* Message */
            msg_format("You have forgotten the %s of %s.", p,
                   do_spell(which, j%32, SPELL_NAME));


            /* One more can be learned */
            plr->new_spells++;
        }
    }


    /* Check for spells to remember */
    for (i = 0; i < 64; i++)
    {
        /* None left to remember */
        if (plr->new_spells <= 0) break;
        if (plr->pclass == CLASS_RAGE_MAGE) break;
        if (plr->pclass == CLASS_SAMURAI) break;
        if (plr->pclass == CLASS_GRAY_MAGE) break;

        /* Efficiency -- all done */
        if (!plr->spell_forgotten1 && !plr->spell_forgotten2) break;

        /* Get the next spell we learned */
        j = plr->spell_order[i];

        /* Skip unknown spells */
        if (j >= 99) break;

        /* Access the spell */
        if (!is_magic((j < 32) ? plr->realm1 : plr->realm2))
        {
            if (j < 32)
                s_ptr = &technic_info[plr->realm1 - MIN_TECHNIC][j];
            else
                s_ptr = &technic_info[plr->realm2 - MIN_TECHNIC][j%32];
        }
        else if (j<32)
            s_ptr = &mp_ptr->info[plr->realm1-1][j];
        else
            s_ptr = &mp_ptr->info[plr->realm2-1][j%32];

        /* Skip spells we cannot remember */
        if (s_ptr->slevel > plr->lev) continue;

        /* First set of spells */
        if ((j < 32) ?
            (plr->spell_forgotten1 & (1L << j)) :
            (plr->spell_forgotten2 & (1L << (j - 32))))
        {
            /* No longer forgotten */
            if (j < 32)
            {
                plr->spell_forgotten1 &= ~(1L << j);
                which = plr->realm1;
            }
            else
            {
                plr->spell_forgotten2 &= ~(1L << (j - 32));
                which = plr->realm2;
            }

            /* Known once more */
            if (j < 32)
            {
                plr->spell_learned1 |= (1L << j);
                which = plr->realm1;
            }
            else
            {
                plr->spell_learned2 |= (1L << (j - 32));
                which = plr->realm2;
            }

            /* Message */
            msg_format("You have remembered the %s of %s.",
                   p, do_spell(which, j%32, SPELL_NAME));


            /* One less can be learned */
            plr->new_spells--;
        }
    }

    k = 0;

    if (plr->realm2 == REALM_NONE && plr->realm1)
    {
        /* Count spells that can be learned */
        for (j = 0; j < 32; j++)
        {
            if (!is_magic(plr->realm1)) s_ptr = &technic_info[plr->realm1-MIN_TECHNIC][j];
            else s_ptr = &mp_ptr->info[plr->realm1-1][j];

            /* Skip spells we cannot remember */
            if (s_ptr->slevel > plr->lev) continue;

            /* Skip spells we already know */
            if (plr->spell_learned1 & (1L << j))
            {
                continue;
            }

            /* Count it */
            k++;
        }
        if (k>32) k = 32;
        if ( plr->new_spells > k
          && (mp_ptr->spell_book == TV_LIFE_BOOK
           || mp_ptr->spell_book == TV_HISSATSU_BOOK
           || mp_ptr->spell_book == TV_RAGE_BOOK))
        {
            plr->new_spells = k;
        }
    }

    if (plr->new_spells < 0) plr->new_spells = 0;

    /* Spell count changed */
    if (plr->old_spells != plr->new_spells)
    {
        /* Message if needed */
        if (plr->new_spells && plr->pclass != CLASS_RAGE_MAGE)
        {
            /* Message
            msg_format("You can learn %d more %s%s.",
                   plr->new_spells, p,
                   (plr->new_spells != 1) ? "s" : "");*/

        }

        /* Save the new_spells value */
        plr->old_spells = plr->new_spells;

        /* Redraw Study Status */
        plr->redraw |= PR_EFFECTS;

        /* Redraw object recall */
        plr->window |= (PW_OBJECT);
    }
}


/*
 * Calculate maximum mana. You do not need to know any spells.
 * Note that mana is lowered by heavy (or inappropriate) armor.
 *
 * This function induces status messages.
 */
static int _racial_mana_adjust(int i)
{
    int     result = 0;
    race_t *race_ptr;

    /* XXX Dragon's get special treatment. For example, an Attack dragon might
     * have +8 Str in race->stats, which gives *way* too much mana for this build,
     * even if it is capped to +5. In this case, we want restricted mana to limit
     * breathing (vs just cranking up spell costs), and we'll just ignore stats
     * for the adjustment. cf _realms in race_dragon.c */
    if (plr->prace == RACE_MON_DRAGON)
        return dragon_get_realm(plr->dragon_realm)->mana_adj;

    /* When doppelgangers mimic, their assumed form affects their mana */
    if (plr->prace == RACE_DOPPELGANGER)
        race_ptr = get_race();
    /* but when anybody else mimics, we continue to use their true original race */
    else
        race_ptr = get_true_race();

    /* psion's best racial modifier wins */
    if (plr->pclass == CLASS_PSION)
    {
        result = MAX(race_ptr->stats[A_INT],
                    MAX(race_ptr->stats[A_WIS], race_ptr->stats[A_CHR]));
    }
    else
        result = race_ptr->stats[i];

    /* Some possessor forms have too much mana ... */
    if ( (plr->prace == RACE_MON_POSSESSOR || plr->prace == RACE_MON_MIMIC)
      && plr->current_r_idx )
    {
        mon_race_ptr race = plr_mon_race();
        switch (race->body.class_id)
        {
        /* biff these classes */
        case CLASS_MAULER:
        case CLASS_BLOOD_KNIGHT:
            result = -5;
            break;
        case CLASS_ROGUE:
            result = -1;
            break;
        /* full bonus */
        case CLASS_MAGE:
        case CLASS_HIGH_MAGE:
        case CLASS_HIGH_PRIEST:
        case CLASS_SORCERER:
            break;
        /* restricted bonus */
        case CLASS_PRIEST:
        case CLASS_MINDCRAFTER:
            result = MIN(3, result);
            break;
        default:
            result = MIN(2, result);
        }
    }

    if (result > 5) result = 5;
    if (result < -5) result = -5;

    return result;
}

static void _calc_encumbrance(void)
{
    caster_info *caster_ptr = get_caster_info();
    int          weight = 0;

    /* Gloves */
    if (caster_ptr->options & CASTER_GLOVE_ENCUMBRANCE)
    {
        int slot;
        for (slot = equip_find_first(obj_is_gloves);
                slot;
                slot = equip_find_next(obj_is_gloves, slot))
        {
            u32b         flgs[OF_ARRAY_SIZE];
            object_type *o_ptr = equip_obj(slot);

            obj_flags(o_ptr, flgs);

            if (!(have_flag(flgs, OF_FREE_ACT)) &&
                !(have_flag(flgs, OF_MAGIC_MASTERY)) &&
                !((have_flag(flgs, OF_DEX)) && (o_ptr->pval > 0)))
            {
                plr->cumber_glove = TRUE;
                break;
            }
        }
    }

    /* Armor/Weapon Weight */
    weight = equip_weight(obj_is_armor);
    if (caster_ptr->encumbrance.weapon_pct)
    {
        int wgt = equip_weight(obj_is_weapon);
        int pct = caster_ptr->encumbrance.weapon_pct;
        weight += wgt * pct / 100;
    }
    if (weight > caster_ptr->encumbrance.max_wgt)
    {
        plr->cumber_armor = TRUE;
        plr->cumber_armor_amt = weight - caster_ptr->encumbrance.max_wgt;
    }
}

static void _report_encumbrance(void)
{
    if (character_xtra) return;

    if (plr->old_cumber_glove != plr->cumber_glove)
    {
        if (plr->cumber_glove)
            msg_print("Your covered hands feel unsuitable for spellcasting.");
        else
            msg_print("Your hands feel more suitable for spellcasting.");
        plr->old_cumber_glove = plr->cumber_glove;
        plr->redraw |= PR_EFFECTS;
    }

    if (plr->old_cumber_armor != plr->cumber_armor)
    {
        if (plr->cumber_armor)
            msg_print("The weight of your equipment encumbers your movement.");
        else
            msg_print("You feel able to move more freely.");
        plr->old_cumber_armor = plr->cumber_armor;
    }
}

static void calc_mana(void)
{
    int             msp, lvl;
    caster_info    *caster_ptr = get_caster_info();

    plr->cumber_glove = FALSE;
    plr->cumber_armor = FALSE;
    plr->cumber_armor_amt = 0;

    if (!caster_ptr)
    {
        if (plr->msp) /* Possessor shifted from Caster body to non-caster body ... */
        {
            plr->msp = 0;
            plr->csp = 0;
            plr->redraw |= (PR_MANA);
        }
        return;
    }

    if ( (caster_ptr->options & (CASTER_USE_HP | CASTER_USE_AU | CASTER_USE_CONCENTRATION))
      || plr->lev < caster_ptr->min_level)
    {
        plr->msp = 0;
        plr->csp = 0;
        plr->redraw |= (PR_MANA);
        return;
    }

    if (caster_ptr->min_level) /* Hack: 0 => 1 */
        lvl = plr->lev - caster_ptr->min_level + 1;
    else
        lvl = plr->lev;

    if (caster_ptr->options & CASTER_SUPERCHARGE_MANA)
    {
        msp = (adj_mag_mana[plr->stat_ind[caster_ptr->which_stat]] + 10) * 2;
        if (msp) msp += (msp * _racial_mana_adjust(caster_ptr->which_stat) / 20);
    }
    else if (plr->pclass == CLASS_WILD_TALENT)
    {
        /* Wild-Talents don't really have a spell stat */
        msp = 5 + plr_prorata_level_aux(400, 2, 1, 1);
    }
    else
    {
        int idx = plr->stat_ind[caster_ptr->which_stat];

        msp = adj_mag_mana[idx] * (lvl + 3)/4;
        if (msp) msp++;
        if (msp)
        {
            int adj = _racial_mana_adjust(caster_ptr->which_stat);
            msp += (msp * adj / 20);
        }

        if (msp && (plr_pseudo_class_id() == CLASS_SORCERER)) msp += msp*(25+plr->lev)/100;
    }

    _calc_encumbrance();
    if (plr->cumber_glove)
        msp = 3 * msp / 4;

    if (plr->cumber_armor)
    {
        int div = caster_ptr->encumbrance.enc_wgt;
        if (!div) div = 800;
        if (caster_ptr->options & CASTER_SUPERCHARGE_MANA)
        {
            plr->cumber_armor = FALSE;
            plr->cumber_armor_amt = 0;
        }
        else
            msp -= msp * plr->cumber_armor_amt / div; 
    }

    if (msp < 0) msp = 0;
    msp = spell_cap(msp);

    if (plr->msp != msp)
    {
        int delta = plr->msp - plr->csp;
        int csp = msp - delta;

        plr->msp = msp;

        /* Preserve the amount of used up mana whenever the total changes */
        if (csp < 0)
            plr->csp = 0;
        else if (plr->csp > 0 && csp >= 0)
            plr->csp = csp;

        if (plr->csp >= msp && !(caster_ptr->options & CASTER_SUPERCHARGE_MANA))
        {
            plr->csp = msp;
            plr->csp_frac = 0;
        }

        plr->redraw |= (PR_MANA);
        plr->window |= (PW_SPELL);
    }

    _report_encumbrance();
}

/*
 * Utility: Often, we need to calculate an amount based on player level.
 * This might be for spell damage, or perhaps to calculate player AC bonuses or hitpoints.
 * At any rate, it is useful from a design standpoint to make this calculation non-linear.
 *
 * Note: If you want a linear proration, call plr_prorata_level_aux(amt, 1, 0, 0);
 * Note: See also design/plr_prorata_level.ods
 */
int plr_prorata_level(int amt)
{
    return plr_prorata_level_aux(amt, 1, 1, 1);
}

int plr_prorata_level_aux(int amt, int w1, int w2, int w3)
{
    int result = 0;
    int wt = w1 + w2 + w3;
    int l = plr->lev;

    assert(wt > 0);
    if (l == 50)
        return amt; /* make sure the player gets the entire amt */

    result += amt * l * w1 / (50*wt);
    result += amt * l * l * w2 / (50*50*wt);
    result += (amt * l * l / 50) * l * w3 / (50*50*wt); /* 2^31/50^3 is about 17000 */

    return result;
}

/* Experimental: Adjust the non-linearity of extra hp distribution based on class.
   It's probably best to have all this in one place. See also the hp.ods design doc. */
static int _calc_xtra_hp(int amt)
{
    int w1 = 1, w2 = 1, w3 = 1;
    int class_id = plr_pseudo_class_id();

    if (plr->pclass == CLASS_SKILLMASTER)
        return skillmaster_calc_xtra_hp(amt);

    switch (class_id)
    {
    case CLASS_WARRIOR:
    case CLASS_CAVALRY:
    case CLASS_BLOOD_KNIGHT:
    case CLASS_MAULER:
        w1 = 1; w2 = 0; w3 = 0;
        break;

    case CLASS_PALADIN:
    case CLASS_CHAOS_WARRIOR:
    case CLASS_SAMURAI:
    case CLASS_ARCHER:
    case CLASS_WEAPONSMITH:
    case CLASS_WEAPONMASTER:
    case CLASS_RAGE_MAGE:
        w1 = 2; w2 = 1; w3 = 0;
        break;

    case CLASS_ROGUE:
    case CLASS_MONK:
    case CLASS_NINJA:
    case CLASS_RUNE_KNIGHT:
        w1 = 1; w2 = 1; w3 = 0;
        break;

    case CLASS_RED_MAGE:
    case CLASS_BLUE_MAGE:
    case CLASS_MIRROR_MASTER:
    case CLASS_TIME_LORD:
    case CLASS_NECROMANCER:
        w1 = 0; w2 = 1; w3 = 1;
        break;

    case CLASS_MAGE:
    case CLASS_HIGH_MAGE:
    case CLASS_SORCERER:
    case CLASS_YELLOW_MAGE:
    case CLASS_GRAY_MAGE:
        w1 = 0; w2 = 0; w3 = 1;
        break;

    case CLASS_WARLOCK:
        switch (plr->psubclass)
        {
        case WARLOCK_GIANTS:
            w1 = 1; w2 = 0; w3 = 0;
            break;
        case WARLOCK_DEMONS:
        case WARLOCK_UNDEAD:
            w1 = 2; w2 = 1; w3 = 0;
            break;
        case WARLOCK_DRAGONS:
        case WARLOCK_HOUNDS:
            w1 = 1; w2 = 1; w3 = 0;
            break;
        case WARLOCK_ANGELS:
            w1 = 1; w2 = 1; w3 = 1;
            break;
        case WARLOCK_SPIDERS:
            w1 = 0; w2 = 1; w3 = 1;
            break;
        }
        break;
    }

    return plr_prorata_level_aux(amt, w1, w2, w3);
}

/*
 * Calculate the players (maximal) hit points
 * Adjust current hitpoints if necessary
 */
static void calc_hitpoints(void)
{
    int      mmhp, mhp;
    race_t  *race_ptr = get_race();
    class_t *class_ptr = get_class();
    personality_ptr pers_ptr = get_personality();

    mmhp = plr->player_hp[plr->lev - 1] * 10 / 100; /* 255 hp total */
    mmhp += _calc_xtra_hp(300);

    mmhp = mmhp * race_ptr->life / 100;
    mmhp = mmhp * class_ptr->life / 100;
    mmhp = mmhp * pers_ptr->life / 100;
    mmhp = mmhp * plr->life / 100;

    if (plr->prace == RACE_MON_DRAGON)
    {
        dragon_realm_ptr realm = dragon_get_realm(plr->dragon_realm);
        mmhp = mmhp * realm->life / 100;
    }

    mmhp += class_ptr->base_hp;
    mmhp += race_ptr->base_hp;

    if (mmhp < 1)
        mmhp = 1;

    /* apply temp hp bonuses from timers et. al. (Heroism, Berserk, ...) */
    mmhp += plr->xtra_hp;

    mhp = mmhp;
    mhp -= mhp*(1000 - plr->clp)/2000;
    if (plr->mmhp != mmhp)
    {
        plr->mmhp = mmhp;
        plr->redraw |= PR_HP;
    }
    if (plr->mhp != mhp)
    {
        if (plr->mhp)
            plr->chp = mhp * plr->chp / plr->mhp;
        else /* birthing */
            plr->chp = mhp;

        plr->chp_frac = 0;
        plr->mhp = mhp;
        plr->redraw |= PR_HP;
    }
}



/*
 * Extract and set the current "lite radius"
 *
 * SWD: Experimental modification: multiple light sources have additive effect.
 *
 */
static void _calc_torch_imp(object_type *o_ptr)
{
    u32b flgs[OF_ARRAY_SIZE];
    obj_flags(o_ptr, flgs);
    if (o_ptr->tval == TV_LIGHT)
    {
        if (have_flag(flgs, OF_DARKNESS))
        {
            if (o_ptr->sval == SV_LIGHT_TORCH)
                plr->cur_light -= 1;
            else if (o_ptr->sval == SV_LIGHT_LANTERN)
                plr->cur_light -= 2;
            else if (o_ptr->art_id || o_ptr->art_name || o_ptr->replacement_art_id)
                plr->cur_light -= 3;
            else /* dark stone */
                plr->cur_light -= 2;
        }
        else if (o_ptr->sval == SV_LIGHT_TORCH && o_ptr->xtra4 > 0)
            plr->cur_light += 1;
        else if (o_ptr->sval == SV_LIGHT_LANTERN && o_ptr->xtra4 > 0)
            plr->cur_light += 2;
        else if (o_ptr->art_id || o_ptr->art_name || o_ptr->replacement_art_id)
        {
            plr->cur_light += 3;
        }
        else if (o_ptr->sval == SV_LIGHT_FEANOR)
            plr->cur_light += 2;
        if (have_flag(flgs, OF_LIGHT)) plr->cur_light++;
    }
    else
    {
        if (have_flag(flgs, OF_DARKNESS))
            plr->cur_light--;
        else if (have_flag(flgs, OF_LIGHT))
            plr->cur_light++;
    }
}
static void calc_torch(void)
{
    plr->cur_light = 0;

    if (demigod_is_(DEMIGOD_APOLLO))
        plr->cur_light++;
    if (prace_is_(RACE_MON_ANGEL))
        plr->cur_light++;
    if (prace_is_(RACE_MON_SWORD))
        plr->cur_light += sword_calc_torch();
    if (prace_is_(RACE_MON_RING))
        plr->cur_light += ring_calc_torch();
    if (elemental_is_(ELEMENTAL_FIRE))
        plr->cur_light++;

    equip_for_each(_calc_torch_imp);

    /*
     * check if the player doesn't have light radius,
     * but does weakly glow as an intrinsic.
     */
    if (plr->cur_light <= 0 && plr->weak_lite) plr->cur_light++;
    plr->cur_light += plr->self_lite;

    /* sanity ... */
    if (plr->cur_light > DUN_BLAST_MAX) plr->cur_light = DUN_BLAST_MAX;

    if (plr_tim_find(T_SUPERSTEALTH) && plr->pclass != CLASS_NECROMANCER)
        plr->cur_light -= 3;

    /* Notice changes in the "lite radius" */
    if (plr->old_lite != plr->cur_light)
    {
        plr->update |= (PU_LIGHT | PU_MON_LIGHT | PU_MONSTERS);
        plr->old_lite = plr->cur_light;
    }
}


int plr_total_weight(void)
{
    int weight = 0;

    weight += equip_weight(NULL);
    weight += pack_weight(NULL);
    weight += quiver_weight(NULL);

    return weight;
}

/*
 * Computes current weight limit.
 */
int weight_limit(void)
{
    int i;

    /* Weight limit based only on strength */
    i = (int)adj_str_wgt[plr->stat_ind[A_STR]] * 50; /* Constant was 100 */

    /* Return the result */
    return i;
}

static bool _is_martial_arts(void)
{
    int i;
    for (i = 0; i < MAX_HANDS; i++)
    {
        if (plr->attack_info[i].type == PAT_MONK)
            return TRUE;
    }
    return FALSE;
}

/* PU_INNATE is now separate from PU_BONUS to prevent memory corruption */
void calc_innate_attacks(void)
{
    vec_clear(plr->innate_blows);
    plr_hook_calc_innate_attacks();
    mut_calc_innate_attacks();
}

/*
 * Calculate the players current "state", taking into account
 * not only race/class intrinsics, but also objects being worn
 * and temporary spell effects.
 *
 * See also calc_mana() and calc_hitpoints().
 *
 * Take note of the new "speed code", in particular, a very strong
 * player will start slowing down as soon as he reaches 150 pounds,
 * but not until he reaches 450 pounds will he be half as fast as
 * a normal kobold. This both hurts and helps the player, hurts
 * because in the old days a player could just avoid 300 pounds,
 * and helps because now carrying 300 pounds is not very painful.
 *
 * The "weapon" and "bow" do *not* add to the bonuses to hit or to
 * damage, since that would affect non-combat things. These values
 * are actually added in later, at the appropriate place.
 *
 * This function induces various "status" messages.
 */
void calc_bonuses(void)
{
    int             i, j, hold, neutral[2], arm, slot;
    s16b            old_speed = plr->pspeed;
    s16b            old_life = plr->life;
    object_type     *o_ptr;
    u32b flgs[OF_ARRAY_SIZE];
    bool            riding_levitation = FALSE;

    class_t *class_ptr = get_class();
    race_t *race_ptr = get_race();
    personality_ptr pers_ptr = get_personality();

    /* Save the old vision stuff */
    bool old_telepathy = plr->telepathy;
    bool old_esp_animal = plr->esp_animal;
    bool old_esp_undead = plr->esp_undead;
    bool old_esp_demon = plr->esp_demon;
    bool old_esp_orc = plr->esp_orc;
    bool old_esp_troll = plr->esp_troll;
    bool old_esp_giant = plr->esp_giant;
    bool old_esp_dragon = plr->esp_dragon;
    bool old_esp_human = plr->esp_human;
    bool old_esp_evil = plr->esp_evil;
    bool old_esp_good = plr->esp_good;
    bool old_esp_nonliving = plr->esp_nonliving;
    bool old_esp_unique = plr->esp_unique;
    bool old_esp_magical = plr->esp_magical;
    bool old_esp_living = plr->esp_living;
    s16b old_see_inv = plr->see_inv;

    /* Save the old armor class */
    s16b old_dis_ac = plr->dis_ac;
    s16b old_dis_to_a = plr->dis_to_a;

    s16b stats[MAX_STATS] = {0};

    /* Clear the stat modifiers */
    for (i = 0; i < 6; i++) plr->stat_add[i] = 0;


    /* Clear the Displayed/Real armor class */
    plr->dis_ac = plr->ac = 0;

    /* Clear the Displayed/Real Bonuses */
    plr_shoot_info_wipe(&plr->shooter_info);

    plr->dis_to_a = plr->to_a = 0;
    plr->ac_adj = 0;
    plr->bonus_to_a = 0;
    plr->bonus_speed = 0;
    plr->to_h_m = 0;
    plr->to_d_m = 0;
    plr->easy_2weapon = FALSE;
    plr->speciality_equip = FALSE;

    plr->to_d_spell = 0;

    plr->to_m_chance = 0;

    plr->weapon_ct = 0;
    for (i = 0; i < MAX_HANDS; i++)
    {
        plr_attack_info_ptr info = &plr->attack_info[i];
        plr_attack_info_wipe(info);
        info->which = i;
    }
    plr_attack_info_wipe(&plr->innate_attack_info);
    plr->innate_attack_info.type = PAT_INNATE;

    plr->spells_per_round = 100;

    /* Start with "normal" speed */
    plr->pspeed = 0;

    /* Clear all the flags */
    plr->cursed = 0L;
    plr->pass_wall = FALSE;
    plr->kill_wall = FALSE;
    plr->pass_web = FALSE;
    plr->clear_web = FALSE;
    plr->pass_tree = FALSE;
    plr->centaur_armor = FALSE;
    plr->dec_mana = 0;
    plr->spell_power = 0;
    plr->device_power = 0;
    plr->spell_cap = 0;
    plr->easy_spell = 0;
    plr->heavy_spell = FALSE;
    plr->see_inv = 0;
    plr->free_act = 0;
    plr->slow_digest = FALSE;
    plr->regen = 100;
    plr->can_swim = FALSE;
    plr->levitation = FALSE;
    plr->hold_life = 0;
    plr->auto_id = FALSE;
    plr->auto_pseudo_id = FALSE;
    plr->auto_id_sp = 0;
    plr->cult_of_personality = FALSE;
    plr->telepathy = FALSE;
    plr->esp_animal = FALSE;
    plr->esp_undead = FALSE;
    plr->esp_demon = FALSE;
    plr->esp_orc = FALSE;
    plr->esp_troll = FALSE;
    plr->esp_giant = FALSE;
    plr->esp_dragon = FALSE;
    plr->esp_human = FALSE;
    plr->esp_evil = FALSE;
    plr->esp_good = FALSE;
    plr->esp_nonliving = FALSE;
    plr->esp_unique = FALSE;
    plr->esp_magical = FALSE;
    plr->esp_living = FALSE;
    plr->wizard_sight = FALSE;
    plr->weak_lite = FALSE;
    plr->self_lite = 0;
    plr->sustain_str = FALSE;
    plr->sustain_int = FALSE;
    plr->sustain_wis = FALSE;
    plr->sustain_con = FALSE;
    plr->sustain_dex = FALSE;
    plr->sustain_chr = FALSE;

    res_clear();

    plr->life = 0;
    plr->xtra_hp = 0;
    plr->reflect = FALSE;

    plr->sh_fire = FALSE;
    plr->sh_elec = FALSE;
    plr->sh_cold = FALSE;
    plr->sh_shards = FALSE;
    plr->sh_retaliation = FALSE;
    plr->sh_fear = FALSE;
    plr->sh_holy = FALSE;
    plr->revenge = FALSE;
    plr->innocence = FALSE;

    plr->repel_evil = FALSE;
    plr->repel_good = FALSE;
    plr->repel_monsters = FALSE;

    plr->anti_magic = FALSE;
    plr->res_magic = FALSE;
    plr->vuln_magic = FALSE;
    plr->anti_tele = FALSE;
    plr->stealthy_snipe = FALSE;
    plr->nimble_dodge = FALSE;
    plr->warning = FALSE;
    plr->mighty_throw = FALSE;
    plr->see_nocto = 0;
    plr->easy_capture = FALSE;
    plr->easy_realm1 = REALM_NONE;

    plr->block_summon = FALSE;
    plr->block_multiply = FALSE;
    plr->block_teleport = FALSE;
    plr->block_magic = FALSE;
    plr->block_steal = FALSE;

    plr->move_random = FALSE;

    plr->magic_resistance = 0;
    plr->good_luck = FALSE;
    plr->rune_elem_prot = FALSE;
    plr->no_eldritch = FALSE;
    plr->no_charge_drain = FALSE;
    plr->no_cut = BOOL(get_race()->flags & RACE_IS_NONLIVING); /* XXX */
    plr->no_slow = FALSE;
    plr->no_passwall_dam = FALSE;
    plr->melt_armor = FALSE;

    plr->ryoute = FALSE;

    plr->return_ammo = FALSE;
    plr->painted_target = FALSE;
    plr->cleave = FALSE;
    plr->vorpal = FALSE;
    plr->whirlwind = FALSE;
    plr->entrenched = FALSE;
    plr->lightning_reflexes = FALSE;
    plr->clear_mind = FALSE;
    plr->inven_prot = FALSE;
    plr->ambush = 0;
    plr->backstab = 0;
    plr->shoot_sleeping = 0;
    plr->shoot_fleeing = 0;
    plr->peerless_stealth = FALSE;
    plr->fairy_stealth = FALSE;
    plr->open_terrain_ct = 0;

    plr->quick_walk = FALSE;
    plr->monk_lvl = 0;
    plr->monk_tbl = NULL;

    plr->align = mon_pack_align(plr_pack());

    if (plr->lev >= 35)
        plr->auto_pseudo_id = TRUE;

    if (mut_present(MUT_FLEET_OF_FOOT)) plr->quick_walk = TRUE;
    if (mut_present(MUT_DEMONIC_GRASP)) plr->no_charge_drain = TRUE;

    plr->see_infra = race_ptr->infra;

    /* calc_skills() */
    {
        skills_t c_extra = class_ptr->extra_skills;
        skills_t r_extra = race_ptr->extra_skills;
        skills_t a_extra = pers_ptr->skills;

        skills_scale(&c_extra, plr->lev, 50);
        skills_scale(&r_extra, plr->lev, 50);

        a_extra.stl = 0; /* Hengband never gave extra personality stealth with level ... */
        skills_scale(&a_extra, plr->lev, 50);

        skills_wipe(&plr->skills);
        skills_add(&plr->skills, &class_ptr->skills);
        skills_add(&plr->skills, &c_extra);
        skills_add(&plr->skills, &race_ptr->skills);
        skills_add(&plr->skills, &r_extra);
        skills_add(&plr->skills, &pers_ptr->skills);
        skills_add(&plr->skills, &a_extra);

        if (plr->prace == RACE_MON_DRAGON)
        {
            dragon_realm_ptr realm = dragon_get_realm(plr->dragon_realm);
            skills_t         extra = realm->skills;

            extra.stl = 0;
            skills_scale(&extra, plr->lev, 50);

            skills_add(&plr->skills, &realm->skills);
            skills_add(&plr->skills, &extra);
        }
    }

    plr->skill_tht = plr->skills.thb;
    plr->skill_dig = 0;

    slot = equip_find_obj(TV_LIGHT, SV_ANY);
    if (slot)
    {
        o_ptr = equip_obj(slot);
        if (obj_is_specified_art(o_ptr, "~.Vecna"))
            plr->see_nocto = DUN_VIEW_MAX;
        else if (obj_is_specified_art(o_ptr, "~.Nature"))
            plr->easy_realm1 = REALM_NATURE;
        else if (obj_is_specified_art(o_ptr, "~.Life"))
            plr->easy_realm1 = REALM_LIFE;
        else if (obj_is_specified_art(o_ptr, "~.Sorcery"))
            plr->easy_realm1 = REALM_SORCERY;
        else if (obj_is_specified_art(o_ptr, "~.Chaos"))
            plr->easy_realm1 = REALM_CHAOS;
        else if (obj_is_specified_art(o_ptr, "~.Death"))
            plr->easy_realm1 = REALM_DEATH;
        else if (obj_is_specified_art(o_ptr, "~.Trump"))
            plr->easy_realm1 = REALM_TRUMP;
        else if (obj_is_specified_art(o_ptr, "~.Daemon"))
            plr->easy_realm1 = REALM_DAEMON;
        else if (obj_is_specified_art(o_ptr, "~.Crusade"))
            plr->easy_realm1 = REALM_CRUSADE;
        else if (obj_is_specified_art(o_ptr, "~.Craft"))
            plr->easy_realm1 = REALM_CRAFT;
        else if (obj_is_specified_art(o_ptr, "~.Armageddon"))
            plr->easy_realm1 = REALM_ARMAGEDDON;
    }

    if (mut_present(MUT_VULN_ELEM))
    {
        res_add_vuln(GF_ACID);
        res_add_vuln(GF_ELEC);
        res_add_vuln(GF_FIRE);
        res_add_vuln(GF_COLD);
    }

    #if 0
    if (plr->tim_sh_elements)
    {
    }

    #endif

    /* Personalities */
    if (pers_ptr->calc_bonuses)
        pers_ptr->calc_bonuses();

    if (mut_present(MUT_GOOD_LUCK))
        plr->good_luck = TRUE;

    /* Hack -- apply racial/class stat maxes */
    /* Apply the racial modifiers */
    for (i = 0; i < MAX_STATS; i++)
    {
        plr->stat_add[i] += (race_ptr->stats[i] + class_ptr->stats[i] + pers_ptr->stats[i]);
    }

    mut_calc_bonuses();  /* Process before equip for MUT_FLESH_ROT */
    equip_calc_bonuses();
    pack_calc_bonuses();

    /* lose monk posture if not performing martial arts */
    if (plr->special_defense & KAMAE_MASK)
    {
        if ( !mut_present(MUT_DRACONIAN_METAMORPHOSIS)
          && plr->attack_info[0].type != PAT_MONK
          && plr->attack_info[1].type != PAT_MONK )
        {
            set_action(ACTION_NONE);
        }
    }
    mut_calc_stats(stats); /* mut goes first for MUT_ILL_NORM, which masks charisma mods of other mutations */
    plr_tim_stats(stats);
    plr_hook_calc_stats(stats);

    for (i = 0; i < MAX_STATS; i++)
        plr->stat_add[i] += stats[i];

    if (plr->cursed & OFC_TELEPORT) plr->cursed &= ~(OFC_TELEPORT_SELF);

    /* Hack -- aura of fire also provides light */
    if (plr->sh_fire) plr->weak_lite = TRUE;

    if (plr->telepathy != old_telepathy)
        plr->update |= PU_MONSTERS;

    if ((plr->esp_animal != old_esp_animal) ||
        (plr->esp_undead != old_esp_undead) ||
        (plr->esp_demon != old_esp_demon) ||
        (plr->esp_orc != old_esp_orc) ||
        (plr->esp_troll != old_esp_troll) ||
        (plr->esp_giant != old_esp_giant) ||
        (plr->esp_dragon != old_esp_dragon) ||
        (plr->esp_human != old_esp_human) ||
        (plr->esp_evil != old_esp_evil) ||
        (plr->esp_good != old_esp_good) ||
        (plr->esp_nonliving != old_esp_nonliving) ||
        (plr->esp_unique != old_esp_unique) ||
        plr->esp_magical != old_esp_magical ||
        plr->esp_living != old_esp_living )
    {
        plr->update |= PU_MONSTERS;
    }

    if (plr->see_inv != old_see_inv)
        plr->update |= PU_MONSTERS;

    /* Call the class hook after scanning equipment but before calculating encumbrance, et. al.*/
    plr_hook_calc_bonuses();
    plr_tim_calc_bonuses();
    /* XXX after race/class->calc_bonuses in case someone locks a new timer. */

    /* Calculate stats after calling class hook */
    for (i = 0; i < MAX_STATS; i++)
    {
        int top, use, ind;

        top = modify_stat_value(plr->stat_max[i], plr->stat_add[i]);
        if (plr->stat_top[i] != top)
        {
            plr->stat_top[i] = top;
            plr->redraw |= (PR_STATS);
        }

        use = modify_stat_value(plr->stat_cur[i], plr->stat_add[i]);
        if (i == A_CHR && mut_present(MUT_ILL_NORM))
        {
            /* 10 to 18/90 charisma, guaranteed, based on level */
            if (use < 8 + 2 * plr->lev)
                use = 8 + 2 * plr->lev;
        }
        if (plr->stat_use[i] != use)
        {
            plr->stat_use[i] = use;
            plr->redraw |= (PR_STATS);
        }

        /* Values: 3, 4, ..., 17 */
        if (use <= 18) ind = (use - 3);

        /* Ranges: 18/00-18/09, ..., 18/210-18/219 */
        else if (use <= 18+219) ind = (15 + (use - 18) / 10);

        /* Range: 18/220+ */
        else ind = (37);

        if (plr->stat_ind[i] != ind)
        {
            plr->stat_ind[i] = ind;
            if (i == A_CON)
                plr->update |= (PU_HP);
            else if (get_spell_stat() == i)
                plr->update |= (PU_MANA | PU_SPELLS);
        }
    }

    plr->life += adj_con_mhp[plr->stat_ind[A_CON]];
    if (plr->life != old_life)
        plr->update |= PU_HP;

    /* Bloating slows the player down (a little) */
    if (plr->food >= PY_FOOD_MAX) plr->pspeed -= 10;

    /* Barehanded Combat */
    for (i = 0; i < MAX_HANDS; i++)
    {
        if (plr->attack_info[i].type == PAT_MONK)
        {
            int bonus = skills_martial_arts_calc_bonus();
            plr->attack_info[i].to_h += bonus;
            plr->attack_info[i].dis_to_h += bonus;
        }
    }

    /* Dual Wielding */
    for (arm = 0; arm < MAX_ARMS; arm++)
    {
        int rhand = 2*arm;
        int lhand = 2*arm+1;
        object_type *robj = NULL, *lobj = NULL;

        if (plr->attack_info[rhand].type == PAT_WEAPON)
            robj = equip_obj(plr->attack_info[rhand].slot);

        if (plr->attack_info[lhand].type == PAT_WEAPON)
            lobj = equip_obj(plr->attack_info[lhand].slot);

        if (robj && lobj)
        {
            int pct, to_d, w1, w2;
            int w_div = 8;
            int skill = skills_dual_wielding_current();
            int class_id = plr_pseudo_class_id();

            /* Some classes (e.g. Berserkers) don't mind dual wielding with heavy weapons */
            switch (class_id)
            {
            case CLASS_WARRIOR:
            case CLASS_BLOOD_KNIGHT:
                w_div = 18;
                break;
            case CLASS_SAMURAI:
            case CLASS_PALADIN:
                w_div = 12;
                break;
            }

            if ( obj_is_specified_art(robj, "|.Quickthorn")
              && obj_is_specified_art(lobj, "|.Littlethorn") )
            {
                add_flag(plr->attack_info[rhand].paf_flags, PAF_GENJI);
                add_flag(plr->attack_info[lhand].paf_flags, PAF_GENJI);
                plr->pspeed += 7;
                plr->to_a += 10;
                plr->dis_to_a += 10;
            }

            w1 = robj->weight;
            w2 = lobj->weight;

            /* cf design/combat.ods ... This is very generous! */
            pct = 650 * skill/WEAPON_EXP_MASTER;

            if (have_flag(plr->attack_info[rhand].paf_flags, PAF_GENJI))
            {
                pct += 150;
                if (w1 >= 130)
                    w1 -= (w1 - 130) / 2;

                if (w2 >= 130)
                    w2 -= (w2 - 130) / 2;
            }
            else if (mut_present(MUT_AMBIDEXTROUS))
                pct += 100;

            if (plr->easy_2weapon)
                pct += 100;

            if ( lobj->tval == TV_SWORD
              && (lobj->sval == SV_MAIN_GAUCHE || lobj->sval == SV_WAKIZASHI) )
            {
                pct += 50;
            }

            plr->attack_info[rhand].skill_mul = pct + 10 * (130 - w1) / w_div;
            plr->attack_info[lhand].skill_mul = pct + 10 * (130 - w2) / w_div;

            if (robj->tval == TV_POLEARM && robj->weight > 100)
                plr->attack_info[rhand].skill_mul -= 50;

            if (lobj->tval == TV_POLEARM && lobj->weight > 100)
                plr->attack_info[lhand].skill_mul -= 50;

            if ( obj_is_specified_art(robj, "|.Musashi1")
              && obj_is_specified_art(lobj, "|.Musashi2") )
            {
                plr->attack_info[rhand].skill_mul = 1000;
                plr->attack_info[lhand].skill_mul = 1000;
            }
            else
            {
                if (obj_is_specified_art(robj, "|.Musashi1"))
                    plr->attack_info[rhand].skill_mul = MAX(plr->attack_info[rhand].skill_mul, 750);
                if (obj_is_specified_art(lobj, "|.Musashi2"))
                    plr->attack_info[lhand].skill_mul = MAX(plr->attack_info[lhand].skill_mul, 750);
            }

            plr->attack_info[rhand].skill_mul = MIN(MAX(100, plr->attack_info[rhand].skill_mul), 1000);
            plr->attack_info[lhand].skill_mul = MIN(MAX(100, plr->attack_info[lhand].skill_mul), 1000);

            /* Cap Str bonus unless Genji */
            to_d = ((int)(adj_str_td[plr->stat_ind[A_STR]]) - 128);
            if (to_d > 0 && !have_flag(plr->attack_info[rhand].paf_flags, PAF_GENJI))
            {
                int new_to_d = to_d * skill/WEAPON_EXP_MASTER;

                plr->attack_info[rhand].to_d -= (to_d - new_to_d);
                plr->attack_info[rhand].dis_to_d -= (to_d - new_to_d);
                plr->attack_info[lhand].to_d -= (to_d - new_to_d);
                plr->attack_info[lhand].dis_to_d -= (to_d - new_to_d);
            }
        }
    }

    /* Having more than one set of arms for combat is very powerful ... */
    for (i = 2; i < MAX_HANDS; i++)
    {
        if (plr->attack_info[i].type != PAT_NONE)
        {
            plr->attack_info[i].skill_mul = plr->attack_info[i].skill_mul * 90 / 100;
            if (plr->attack_info[i].skill_mul < 100)
                plr->attack_info[i].skill_mul = 100;
        }
    }


    /* Extract the current weight (in tenth pounds) */
    j = plr_total_weight();

    if (!plr->riding)
    {
        /* Extract the "weight limit" (in tenth pounds) */
        i = weight_limit();
    }
    else
    {
        monster_type *riding_m_ptr = dun_mon(cave, plr->riding);
        monster_race *riding_r_ptr = riding_m_ptr->race;
        int speed = riding_m_ptr->mspeed;
        int old_pspeed = plr->pspeed;

        /* XXX get base monster speed (cf _fast_on and _slow_on in mon_tim.c) */
        if (mon_tim_find(riding_m_ptr, T_FAST)) speed -= 10;
        if (mon_tim_find(riding_m_ptr, T_SLOW)) speed += 10;

        if (speed > 0)
        {
            int skill = skills_riding_current();
            plr->pspeed = speed * (skill*3 + plr->lev*160 - 10000)/22000;
            if (plr->pspeed < 0) plr->pspeed = 0;
        }
        else
        {
            plr->pspeed = speed;
        }

        if (plr->prace == RACE_MON_RING)
        {
            /* Hey, the player *is* a Ring of Speed, right? */
            if (old_pspeed > 0)
                plr->pspeed += (old_pspeed + 2) / 3;
        }
        else
        {
            plr->pspeed += (skills_riding_current() + plr->lev *160)/3200;
        }

        /* XXX mon T_FAST and T_SLOW should give full effect regardless of plr skill and level */
        if (mon_tim_find(riding_m_ptr, T_FAST)) plr->pspeed += 10;
        if (mon_tim_find(riding_m_ptr, T_SLOW)) plr->pspeed -= 10;

        if (warlock_is_(WARLOCK_DRAGONS))
        {
            switch (warlock_get_toggle())
            {
            case WARLOCK_DRAGON_TOGGLE_CANTER:
                plr->pspeed += 3;
                break;
            case WARLOCK_DRAGON_TOGGLE_GALLOP:
            case WARLOCK_DRAGON_TOGGLE_HEROIC_CHARGE:
                plr->pspeed += 10;
                break;
            }
        }

        riding_levitation = mon_can_fly(riding_m_ptr);
        if (mon_is_aquatic(riding_m_ptr) || mon_can_swim(riding_m_ptr))
            plr->can_swim = TRUE;

        if (!mon_can_passwall(riding_m_ptr)) plr->pass_wall = FALSE;
        if (mon_can_tunnel(riding_m_ptr)) plr->kill_wall = TRUE;

        if (skills_riding_current() < RIDING_EXP_SKILLED) j += (150 * 3 * (RIDING_EXP_SKILLED - skills_riding_current())) / RIDING_EXP_SKILLED;

        /* Extract the "weight limit" */
        i = 1500 + riding_r_ptr->alloc.lvl * 25;
    }

    /* XXX XXX XXX Apply "encumbrance" from weight */
    if (j > i) plr->pspeed -= ((j - i) / (i / 5));

    /* Searching slows the player down
       TODO: This is dumb. Increase the energy used on movement instead! Also,
       only search as the player moves. */
    if (plr->action == ACTION_SEARCH) plr->pspeed -= 10;


    /* Actual Modifier Bonuses (Un-inflate stat bonuses) */
    plr->to_a += calc_adj_dex_ta();
    plr->dis_to_a += calc_adj_dex_ta();

    if (plr->prace != RACE_MON_BEHOLDER)
    {
        int to_d = ((int)(adj_str_td[plr->stat_ind[A_STR]]) - 128);
        int to_h = ((int)(adj_str_th[plr->stat_ind[A_STR]]) - 128) +
                   ((int)(adj_dex_th[plr->stat_ind[A_DEX]]) - 128);
        plr->to_d_m += to_d;
        plr->innate_attack_info.to_d += to_d;
        plr->innate_attack_info.dis_to_d += to_d;
        plr->to_h_m += to_h;
        plr->innate_attack_info.to_h += to_h;
        plr->innate_attack_info.dis_to_h += to_h;
    }

    plr->shooter_info.to_h  += ((int)(adj_dex_th[plr->stat_ind[A_DEX]]) - 128);
    plr->shooter_info.dis_to_h  += ((int)(adj_dex_th[plr->stat_ind[A_DEX]]) - 128);
    plr->shooter_info.to_h  += ((int)(adj_str_th[plr->stat_ind[A_STR]]) - 128);
    plr->shooter_info.dis_to_h  += ((int)(adj_str_th[plr->stat_ind[A_STR]]) - 128);

    for (i = 0; i < MAX_HANDS; i++)
    {
        int to_d = ((int)(adj_str_td[plr->stat_ind[A_STR]]) - 128);
        int to_h = ((int)(adj_str_th[plr->stat_ind[A_STR]]) - 128) +
                   ((int)(adj_dex_th[plr->stat_ind[A_DEX]]) - 128);
        plr->attack_info[i].to_d += to_d;
        plr->attack_info[i].to_h += to_h;
        plr->attack_info[i].dis_to_d += to_d;
        plr->attack_info[i].dis_to_h += to_h;
    }

    /* Obtain the "hold" value */
    hold = adj_str_hold[plr->stat_ind[A_STR]];

    /* Examine the "current bow" */
    plr->shooter_info.slot = equip_find_obj(TV_BOW, SV_ANY);
    if (plr->shooter_info.slot)
    {
        o_ptr = equip_obj(plr->shooter_info.slot);

        if (weaponmaster_is_(WEAPONMASTER_CROSSBOWS))
        {
            int idx = plr->stat_ind[A_STR] + 4;
            if (idx > 40-3)
                idx = 40-3;
            hold = adj_str_hold[idx];
        }

        /* It is hard to carry a heavy bow */
        if (hold < o_ptr->weight / 10)
        {
            plr->shooter_info.to_h  += 2 * (hold - o_ptr->weight / 10);
            plr->shooter_info.dis_to_h  += 2 * (hold - o_ptr->weight / 10);
            plr->shooter_info.heavy_shoot = TRUE;
            plr->shooter_info.base_shot = 100;
            plr->shooter_info.xtra_shot = 0;
        }

        /* Hack: hold is also used for inventory weapons and other stuff, so undo
           the Crossbowmaster adjustment */
        if (weaponmaster_is_(WEAPONMASTER_CROSSBOWS))
        {
            hold = adj_str_hold[plr->stat_ind[A_STR]];
        }

        switch (o_ptr->sval)
        {
            case SV_SLING:
            {
                plr->shooter_info.tval_ammo = TV_SHOT;
                break;
            }

            case SV_SHORT_BOW:
            case SV_LONG_BOW:
            case SV_GREAT_BOW:
            {
                plr->shooter_info.tval_ammo = TV_ARROW;
                break;
            }

            case SV_LIGHT_XBOW:
            case SV_HEAVY_XBOW:
            {
                plr->shooter_info.tval_ammo = TV_BOLT;
                break;
            }
            case SV_CRIMSON:
            case SV_RAILGUN:
            case SV_HARP:
            {
                plr->shooter_info.tval_ammo = TV_NONE;
                break;
            }
        }

        /* Experimental: All classes can reduce ammo breakage based on
         * Archery Skill. calc_shooter_bonus might decrement this amount
         * further. */
        if (plr->skills.thb > 80)
            plr->shooter_info.breakage = 90 - (plr->skills.thb - 80)/2;

        /* Experimental: Everbody gets extra shots based on bow skill. This
         * makes Race and Personality choices relevant. */
        if (plr->shooter_info.base_shot < plr->skills.thb && !plr->shooter_info.heavy_shoot && !heavy_armor())
            plr->shooter_info.base_shot += (plr->skills.thb - plr->shooter_info.base_shot)/2;

        plr_tim_calc_shooter_bonuses(o_ptr, &plr->shooter_info);
        if (race_ptr->hooks.calc_shooter_bonuses)
            race_ptr->hooks.calc_shooter_bonuses(o_ptr, &plr->shooter_info);

        if (class_ptr->hooks.calc_shooter_bonuses)
            class_ptr->hooks.calc_shooter_bonuses(o_ptr, &plr->shooter_info);

        if (plr->shooter_info.breakage < 0) plr->shooter_info.breakage = 0;
        if (plr->shooter_info.base_shot < 0) plr->shooter_info.base_shot = 0;
    }

    /* Blows Calculation */
    for (i = 0; i < MAX_HANDS; i++)
    {
        plr_attack_info_ptr info = &plr->attack_info[i];
        int             tmp_hold = hold;
        int             arm = i/2;

        if (info->type == PAT_NONE) continue;

        o_ptr = equip_obj(info->slot);
        if (!o_ptr) continue;

        obj_flags(o_ptr, flgs);

        if (plr_attack_info_two_hand(info))
            tmp_hold *= 2;

        if (tmp_hold < o_ptr->weight / 10)
        {
            info->to_h += 2 * (tmp_hold - o_ptr->weight / 10);
            info->dis_to_h += 2 * (tmp_hold - o_ptr->weight / 10);
            add_flag(info->paf_flags, PAF_HEAVY);
        }
        else if (have_flag(info->paf_flags, PAF_TWO_HANDS) && tmp_hold < o_ptr->weight/5)
        {
            if (plr->pclass != CLASS_MAULER)
                add_flag(info->paf_flags, PAF_NEEDS_TWO_HANDS);
        }

        if ( i % 2 == 1
          && plr->attack_info[i-1].type != PAT_NONE
          && o_ptr->tval == TV_SWORD
          && (o_ptr->sval == SV_MAIN_GAUCHE || o_ptr->sval == SV_WAKIZASHI) )
        {
            plr->to_a += 5;
            plr->dis_to_a += 5;
        }

        /* calc_weapon_bonuses
         * This should also init the blows_calc in preparation for calc_base_blows,
         * but this is not finished just yet. In the meantime init_blows_calc in
         * combat.c is required. */
        init_blows_calc(o_ptr, info);

        plr_tim_calc_weapon_bonuses(o_ptr, info);
        if (class_ptr->hooks.calc_weapon_bonuses)
            class_ptr->hooks.calc_weapon_bonuses(o_ptr, info);
        if (race_ptr->hooks.calc_weapon_bonuses)
            race_ptr->hooks.calc_weapon_bonuses(o_ptr, info);

        /* Hacks */
        if (o_ptr->tval == TV_SWORD && o_ptr->sval == SV_POISON_NEEDLE)
            info->blows_calc.max = 100;
        if (arm > 0)
            info->blows_calc.max = MAX(100, info->blows_calc.max - 100);

        /* Calculate Blows */
        if (!have_flag(info->paf_flags, PAF_HEAVY))
        {
            plr_calc_blows_hand(i);
            if (plr->special_defense & KATA_FUUJIN) info->xtra_blow -= 100;

            if (o_ptr->tval == TV_SWORD && o_ptr->sval == SV_POISON_NEEDLE)
            {
                info->base_blow = 100;
                info->xtra_blow = 0;
            }

            if (NUM_BLOWS(i) < 0)
            {
                info->base_blow = 0;
                info->xtra_blow = 0;
            }

            plr->skill_dig += o_ptr->weight / 10;
        }

        /* Two Handed wielding bonus */
        if (plr_attack_info_two_hand_bonus(info))
        {
            int bonus_to_h=0, bonus_to_d=0;
            bonus_to_d = ((int)(adj_str_td[plr->stat_ind[A_STR]]) - 128) * 3/4;
            bonus_to_h = ((int)(adj_str_th[plr->stat_ind[A_STR]]) - 128) + ((int)(adj_dex_th[plr->stat_ind[A_DEX]]) - 128);

            info->to_h += MAX(bonus_to_h,1);
            info->dis_to_h += MAX(bonus_to_h,1);
            info->to_d += MAX(bonus_to_d,1);
            info->dis_to_d += MAX(bonus_to_d,1);
        }

        if (plr->riding)
        {
            if ((o_ptr->tval == TV_POLEARM) && ((o_ptr->sval == SV_LANCE) || (o_ptr->sval == SV_HEAVY_LANCE)))
            {
                info->to_h +=15;
                info->dis_to_h +=15;
                info->to_dd += 2;
            }
            else if (!(have_flag(flgs, OF_RIDING)))
            {
                int penalty;
                if ( plr->pclass == CLASS_BEASTMASTER
                  || plr->pclass == CLASS_CAVALRY
                  || skillmaster_riding_prof() >= RIDING_EXP_EXPERT )
                {
                    penalty = 5;
                }
                else
                {
                    penalty = plr_riding_lvl() - skills_riding_current() / 80;
                    penalty += 30;
                    if (penalty < 30) penalty = 30;
                }
                info->to_h -= penalty;
                info->dis_to_h -= penalty;
                add_flag(info->paf_flags, PAF_ICKY_RIDING);
            }
        }
    }

    for (i = 0; i < vec_length(plr->innate_blows); i++)
    {
        mon_blow_ptr blow = vec_get(plr->innate_blows, i);
        if (race_ptr->hooks.calc_innate_bonuses)
            race_ptr->hooks.calc_innate_bonuses(blow);
    }

    if (plr->riding)
    {
        int penalty = 0;
        if ( plr->pclass == CLASS_BEASTMASTER
          || plr->pclass == CLASS_CAVALRY
          || skillmaster_riding_prof() >= RIDING_EXP_EXPERT )
        {
            if (plr->shooter_info.tval_ammo != TV_ARROW) penalty = 5;
        }
        else
        {
            penalty = plr_riding_lvl() - skills_riding_current() / 80;
            penalty += 30;
            if (penalty < 30) penalty = 30;
        }
        if (plr->shooter_info.tval_ammo == TV_BOLT) penalty *= 2;
        plr->shooter_info.to_h -= penalty;
        plr->shooter_info.dis_to_h -= penalty;
    }

    /* Different calculation for monks with empty hands */
    for (i = 0; i < MAX_HANDS; i++)
    {
        plr_attack_info_ptr info = &plr->attack_info[i];
        int arm = i / 2;
        if (info->type == PAT_MONK)
        {
            /* calculate blows */
            plr_calc_blows_hand(i);
            info->base_blow -= arm*100;
            if (info->base_blow < 100)
                info->base_blow = 100;

            plr_tim_calc_weapon_bonuses(NULL, info);
            /* XXX move calcs below to monk classes? 
            if (class_ptr->calc_weapon_bonuses)
                class_ptr->calc_weapon_bonuses(NULL, info);
            if (race_ptr->calc_weapon_bonuses)
                race_ptr->calc_weapon_bonuses(NULL, info); */
            /* calculate (+h,+d) bonuses */
            if (!heavy_armor())
            {
                info->to_h += (plr->monk_lvl / 3);
                info->dis_to_h += (plr->monk_lvl / 3);

                info->to_d += (plr->monk_lvl / 6);
                info->dis_to_d += (plr->monk_lvl / 6);
            }
            if (plr->pclass == CLASS_FORCETRAINER && plr->magic_num1[0])
            {
                info->to_d += (plr->magic_num1[0]/5);
                info->dis_to_d += (plr->magic_num1[0]/5);
            }
            if (plr_attack_info_two_hand(info))
            {
                int bonus_to_h=0, bonus_to_d=0;
                bonus_to_d = ((int)(adj_str_td[plr->stat_ind[A_STR]]) - 128) * 3/4;
                bonus_to_h = ((int)(adj_str_th[plr->stat_ind[A_STR]]) - 128) + ((int)(adj_dex_th[plr->stat_ind[A_DEX]]) - 128);

                info->to_h += MAX(bonus_to_h,1);
                info->dis_to_h += MAX(bonus_to_h,1);
                info->to_d += MAX(bonus_to_d,1);
                info->dis_to_d += MAX(bonus_to_d,1);
            }
        }
    }

    if (plr->riding && plr->prace != RACE_MON_RING)
        plr->levitation = riding_levitation;

    monk_armour_aux = FALSE;

    if (heavy_armor())
    {
        monk_armour_aux = TRUE;
    }

    /* Weapon Skills */
    for (i = 0; i < MAX_HANDS; i++)
    {
        plr_attack_info_ptr info = &plr->attack_info[i];
        obj_ptr obj = NULL;

        if (info->type != PAT_WEAPON) continue;
        obj = equip_obj(info->slot);

        if (obj->tval == TV_SHIELD)
        {
            int bonus = skills_shield_calc_bonus(obj->sval);

            assert(weaponmaster_get_toggle() == TOGGLE_SHIELD_BASH);
            info->to_h += bonus;
            info->dis_to_h += bonus;
        }
        else
        {
            int bonus = skills_weapon_calc_bonus(obj->tval, obj->sval);
            info->to_h += bonus;
            info->dis_to_h += bonus;
            if (plr_allow_martial_arts())
            {
                if (skills_weapon_is_icky(obj->tval, obj->sval))
                {
                    info->to_h -= 40;
                    info->dis_to_h -= 40;
                    add_flag(info->paf_flags, PAF_ICKY);
                }
            }
        }
        if (obj_is_specified_art(obj, "\\.Iron Ball"))
            plr->align -= 1000;
    }

    /* limit speed ... this is purely cosmetic (cf speed_to_energy) */
    if (plr->pspeed > 90)
        plr->pspeed = 90;

    if (plr->pspeed < -50)
        plr->pspeed = -50;

    /* Display the speed (if needed) */
    if (plr->pspeed != old_speed)
        plr->redraw |= PR_EFFECTS;

    /* Regeneration */
    if (plr->special_defense & (KAMAE_MASK | KATA_MASK))
        plr->regen /= 2;

    if (plr->cursed & OFC_SLOW_REGEN)
        plr->regen /= 5;

    if (plr->regen < 0)
        plr->regen = 0;

    /* Robe of the Twilight forces AC to 0 */
    if (equip_find_ego(EGO_ROBE_TWILIGHT))
    {
        if (plr->to_a > (0 - plr->ac))
            plr->to_a = 0 - plr->ac;
        if (plr->dis_to_a > (0 - plr->dis_ac))
            plr->dis_to_a = 0 - plr->dis_ac;
    }

    /* Redraw armor (if needed) */
    if (plr->dis_ac != old_dis_ac || plr->dis_to_a != old_dis_to_a)
    {
        plr->redraw |= PR_ARMOR;
    }

    /* Affect Skill -- stealth (bonus one) */
    plr->skills.stl += 1;

    if (plr->action == ACTION_STALK) plr->skills.stl += (plr->lev+2)/3;

    /* Affect Skill -- disarming (DEX and INT) */
    plr->skills.dis += adj_dex_dis[plr->stat_ind[A_DEX]];
    plr->skills.dis += adj_int_dis[plr->stat_ind[A_INT]];

    /* Affect Skill -- magic devices (INT) */
    plr->skills.dev += adj_int_dev[plr->stat_ind[A_INT]];

    /* Affect Skill -- saving throw (WIS) */
    plr->skills.sav += adj_wis_sav[plr->stat_ind[A_WIS]];

    /* Affect Skill -- digging (STR) */
    plr->skill_dig += adj_str_dig[plr->stat_ind[A_STR]];

    if ( plr->fairy_stealth
      && plr->personality != PERS_SEXY
      && (plr->cursed & OFC_AGGRAVATE) )
    {
        plr->cursed &= ~(OFC_AGGRAVATE);
        plr->skills.stl = MIN(plr->skills.stl - 3, (plr->skills.stl + 2) / 2);
    }

    /* Peerless Stealth is just like the Shadow Fairy, but can even negate the
       aggravation of Sexy characters! */
    if (plr->peerless_stealth && plr->cursed & OFC_AGGRAVATE)
    {
        plr->cursed &= ~(OFC_AGGRAVATE);
        plr->skills.stl = MIN(plr->skills.stl - 3, (plr->skills.stl + 2) / 2);
    }

    /* Limit Skill -- stealth from 0 to 30 */
    if (plr->skills.stl > 30) plr->skills.stl = 30;
    if (plr->skills.stl < 0) plr->skills.stl = 0;

    /* Limit Skill -- digging from 1 up */
    if (plr->skill_dig < 1) plr->skill_dig = 1;

    /* Adjust saving throw ... order matters! */
    if (plr->anti_magic)
        plr->skills.sav = MAX(plr->skills.sav, 90 + plr->lev);

    if (plr->vuln_magic)
        plr->skills.sav = 10;

    if (plr->res_magic)
        plr->skills.sav = MAX(plr->skills.sav, 95 + plr->lev);

    for (i = 0, j = 0; i < 8; i++)
    {
        switch (plr->vir_types[i])
        {
        case VIRTUE_JUSTICE:
            plr->align += plr->virtues[i] * 2;
            break;
        case VIRTUE_CHANCE:
            /* Do nothing */
            break;
        case VIRTUE_NATURE:
        case VIRTUE_HARMONY:
            neutral[j++] = i;
            break;
        case VIRTUE_UNLIFE:
            plr->align -= plr->virtues[i];
            break;
        default:
            plr->align += plr->virtues[i];
            break;
        }
    }

    for (i = 0; i < j; i++)
    {
        if (plr->align > 0)
        {
            plr->align -= plr->virtues[neutral[i]] / 2;
            if (plr->align < 0) plr->align = 0;
        }
        else if (plr->align < 0)
        {
            plr->align += plr->virtues[neutral[i]] / 2;
            if (plr->align > 0) plr->align = 0;
        }
    }

    /* Hack -- handle "xtra" mode */
    if (character_xtra) return;

    /* Take note when "heavy bow" changes */
    if (plr->old_heavy_shoot != plr->shooter_info.heavy_shoot)
    {
        if (plr->shooter_info.heavy_shoot)
            msg_print("You have trouble wielding such a heavy bow.");
        else if (equip_find_obj(TV_BOW, SV_ANY))
            msg_print("You have no trouble wielding your bow.");
        else
            msg_print("You feel relieved to put down your heavy bow.");

        plr->old_heavy_shoot = plr->shooter_info.heavy_shoot;
        plr->redraw |= PR_EFFECTS;
    }

    for (i = 0 ; i < MAX_HANDS ; i++)
    {
        bool heavy = have_flag(plr->attack_info[i].paf_flags, PAF_HEAVY);
        bool icky_riding = have_flag(plr->attack_info[i].paf_flags, PAF_ICKY_RIDING);
        bool icky = have_flag(plr->attack_info[i].paf_flags, PAF_ICKY);

        if (plr->old_heavy_wield[i] != heavy)
        {
            if (heavy)
                msg_print("You have trouble wielding such a heavy weapon.");
            else if (plr->attack_info[i].type != PAT_NONE)
                msg_print("You have no trouble wielding your weapon.");
            else
                msg_print("You feel relieved to put down your heavy weapon.");

            plr->old_heavy_wield[i] = heavy;
            plr->redraw |= PR_EFFECTS;
        }

        if (plr->old_riding_wield[i] != icky_riding)
        {
            if (icky_riding)
                msg_print("This weapon is not suitable for use while riding.");
            else if (!plr->riding)
                msg_print("This weapon was not suitable for use while riding.");
            else if (plr->attack_info[i].type != PAT_NONE)
                msg_print("This weapon is suitable for use while riding.");

            plr->old_riding_wield[i] = icky_riding;
        }

        if (plr->old_icky_wield[i] != icky)
        {
            if (icky)
            {
                msg_print("You do not feel comfortable with your weapon.");
                if (hack_mind)
                    virtue_add(VIRTUE_FAITH, -1);
            }
            else if (plr->attack_info[i].type != PAT_NONE)
                msg_print("You feel comfortable with your weapon.");
            else
                msg_print("You feel more comfortable after removing your weapon.");

            plr->old_icky_wield[i] = icky;
            plr->redraw |= PR_EFFECTS;
        }
    }

    if (plr->riding && (plr->old_riding_ryoute != plr->riding_ryoute))
    {
        if (plr->riding_ryoute)
            msg_print("You are using all hands for fighting and you can't control your mount.");
        else
            msg_print("You begin to control your mount with one hand.");

        plr->old_riding_ryoute = plr->riding_ryoute;
    }

    if ( (plr_allow_martial_arts() || plr->pclass == CLASS_NINJA || plr->pclass == CLASS_SCOUT)
      && (monk_armour_aux != monk_notify_aux) )
    {
        if (heavy_armor())
        {
            msg_print("The weight of your armor disrupts your balance.");
            if (hack_mind)
                virtue_add(VIRTUE_HARMONY, -1);
        }
        else
            msg_print("You regain your balance.");

        monk_notify_aux = monk_armour_aux;
        plr->redraw |= PR_EFFECTS;
    }

    /* Apply some maximums ... Note: Rune-Knights must limit to just 15%
     * Otherwise, they could use double or even triple {absorption}! */
    if ( plr->magic_resistance > 15
      && !prace_is_(RACE_MON_GOLEM)
      && !prace_is_(MIMIC_MIST)
      && !prace_is_(RACE_MON_POSSESSOR)
      && !prace_is_(RACE_MON_MIMIC) )
    {
        plr->magic_resistance = 15;
    }

    /* XXX AC can never go negative */
    /* XXX remember adjustment for the duelist */
    if (plr->ac + plr->to_a < 0)
    {
        plr->ac_adj = -(plr->ac + plr->to_a);
        plr->to_a += plr->ac_adj;
        plr->dis_to_a += plr->ac_adj;
    }
}



/*
 * Handle "plr->notice"
 */
void notice_stuff(void)
{
    if (!plr->notice) return;

    /* no need to notice while melee is in progress ...
     * if auras destroy items, of if the player levels up
     * during the middle of battle, wait until the attacks
     * are finished before processing */
    if (plr_attack_current()) return;

    if (plr->notice & PN_EXP)
    {
        plr->notice &= ~PN_EXP;
        check_experience();
    }

    if (plr->notice & PN_OPTIMIZE_PACK)
    {
        /* Clear the bit first ... */
        plr->notice &= ~PN_OPTIMIZE_PACK;
        /* ... as the pack will refuse the optimize if locked,
         * adding back PN_OPTIMIZE_PACK */
        pack_optimize();
    }

    if (plr->notice & PN_OPTIMIZE_QUIVER)
    {
        plr->notice &= ~PN_OPTIMIZE_QUIVER;
        quiver_optimize();
    }
    if ((plr->notice & PN_CARRY) && !travel.run)
    {
        plr->notice &= ~PN_CARRY;
        pack_delayed_describe();
        if (!(plr->notice & PN_CARRY))
            quiver_delayed_describe();
    }
}


/*
 * Handle "plr->update"
 */
void update_stuff(void)
{
    /* Update stuff */
    if (!plr->update) return;
    if (plr->dun_id != cave->id) return;

    /* never calc_bonuses during melee! (cf mauler.c) */
    if (plr->update & (PU_BONUS) && !plr_attack_current())
    {
        plr->update &= ~(PU_BONUS);
        calc_bonuses();
    }

    if (plr->update & (PU_TORCH))
    {
        plr->update &= ~(PU_TORCH);
        calc_torch();
    }

    if (plr->update & (PU_HP))
    {
        plr->update &= ~(PU_HP);
        calc_hitpoints();
    }

    if (plr->update & (PU_MANA))
    {
        plr->update &= ~(PU_MANA);
        calc_mana();
    }

    if (plr->update & (PU_SPELLS))
    {
        plr->update &= ~(PU_SPELLS);
        calc_spells();
    }

    /* Never rebuild innate attacks during player melee. Note
     * that we often calc_bonuses during melee and this could
     * lead to memory errors if we delete the current mon_blow_ptr.
     * In general, innate attacks need to update as the player
     * gains levels, or as the player gains/loses mutations.
     * Rules like no gaze attacks if blind are handled in plr_attack.c */
    if ((plr->update & PU_INNATE) && !plr_attack_current())
    {
        plr->update &= ~PU_INNATE;
        calc_innate_attacks();
    }

    /* Character is not ready yet, no screen updates */
    if (!character_generated) return;


    /* Character is in "icky" mode, no screen updates */
    if (character_icky) return;


    if (plr->update & (PU_UN_LIGHT))
    {
        plr->update &= ~(PU_UN_LIGHT);
        dun_forget_light(cave);
    }

    if (plr->update & (PU_UN_VIEW))
    {
        plr->update &= ~(PU_UN_VIEW);
        dun_forget_view(cave);
    }

    if (plr->update & (PU_VIEW))
    {
        plr->update &= ~(PU_VIEW);
        dun_update_view(cave);
    }

    if (plr->update & PU_LIGHT)
    {
        plr->update &= ~(PU_LIGHT | PU_MON_LIGHT);
        dun_update_light(cave);
    }
    else if (plr->update & PU_MON_LIGHT)
    {
        plr->update &= ~PU_MON_LIGHT;
        dun_update_mon_light(cave);
    }

    if (plr->update & (PU_FLOW))
    {
        plr->update &= ~(PU_FLOW);
        dun_update_flow(cave);
    }
    if (plr->update & (PU_MON_FLOW))
    {
        plr->update &= ~(PU_MON_FLOW);
        dun_update_mon_flow(cave);
    }

    /*
     * Mega-Hack -- Delayed visual update
     * Only used if update_view(), update_lite() or update_mon_lite() was called
     */
    if (plr->update & (PU_DELAY_VIS))
    {
        plr->update &= ~(PU_DELAY_VIS);
        delayed_visual_update(cave);
    }

    if (plr->update & (PU_MONSTERS))
    {
        plr->update &= ~(PU_MONSTERS);
        update_monsters(FALSE);
    }
}

/*
 * Handle "plr->redraw"
 */
void redraw_stuff(void)
{
    dun_mgr_ptr dm = dun_mgr();

    /* Redraw stuff */
    if (!plr->redraw) return;
    if (plr->dun_id != cave->id) return;

    /* Character is not ready yet, no screen updates */
    if (!character_generated) return;

    /* Character is in "icky" mode, no screen updates */
    if (character_icky) return;

    if (dm->prof)
        z_timer_resume(&dm->prof->redraw_timer);

    /* Laziness ... */
    if ((plr->redraw & PR_HP) && display_hp_bar)
        plr->redraw |= PR_HEALTH_BARS;
    if ((plr->redraw & PR_MANA) && display_sp_bar)
        plr->redraw |= PR_HEALTH_BARS;
    if (plr->redraw & (PR_DEPTH|PR_BASIC))
        plr->redraw |= PR_STATUS;

    /* Hack -- clear the screen *FIRST* */
    if (plr->redraw & PR_WIPE)
    {
        plr->redraw &= ~PR_WIPE;
        if (!(plr->redraw & PR_MSG_LINE))
            msg_line_clear();
        Term_clear();
    }

    if (plr->redraw & PR_MAP)
    {
        plr->redraw &= ~(PR_MAP | PR_MSG_LINE_MAP);
        prt_map();
    }
    else if (plr->redraw & PR_MSG_LINE_MAP)
    {
        plr->redraw &= ~PR_MSG_LINE_MAP;
        msg_line_delayed_clear();
    }

    if (plr->redraw & (PR_BASIC))
    {
        plr->redraw &= ~(PR_BASIC);
        plr->redraw &= ~(PR_STATS);
        plr->redraw &= ~(PR_LEV | PR_EXP | PR_GOLD);
        plr->redraw &= ~(PR_ARMOR | PR_HP | PR_MANA);
        plr->redraw &= ~(PR_DEPTH);
        prt_frame_basic();
        prt_time();
    }

    if (plr->redraw & (PR_EQUIPPY))
    {
        plr->redraw &= ~(PR_EQUIPPY);
        print_equippy(); /* To draw / delete equippy chars */
    }

    if (plr->redraw & (PR_LEV))
    {
        plr->redraw &= ~(PR_LEV);
        prt_level();
    }

    if (plr->redraw & (PR_EXP))
    {
        plr->redraw &= ~(PR_EXP);
        prt_exp();
    }

    if (plr->redraw & (PR_STATS))
    {
        plr->redraw &= ~(PR_STATS);
        prt_stat(A_STR);
        prt_stat(A_INT);
        prt_stat(A_WIS);
        prt_stat(A_DEX);
        prt_stat(A_CON);
        prt_stat(A_CHR);
    }

    if (plr->redraw & (PR_ARMOR))
    {
        plr->redraw &= ~(PR_ARMOR);
        prt_ac();
    }

    if (plr->redraw & (PR_HP))
    {
        plr->redraw &= ~(PR_HP);
        prt_hp();
    }

    if (plr->redraw & (PR_MANA))
    {
        plr->redraw &= ~(PR_MANA);
        prt_sp();
    }

    if (plr->redraw & (PR_GOLD))
    {
        plr->redraw &= ~(PR_GOLD);
        prt_gold();
    }

    if (plr->redraw & (PR_DEPTH))
    {
        plr->redraw &= ~(PR_DEPTH);
        prt_depth();
    }

    if (plr->redraw & (PR_EXTRA))
    {
        plr->redraw &= ~(PR_EXTRA);
        plr->redraw &= ~(PR_STATE | PR_STATUS | PR_HEALTH_BARS | PR_EFFECTS);
        prt_frame_extra();
    }

    if (plr->redraw & (PR_HEALTH_BARS))
    {
        plr->redraw &= ~PR_HEALTH_BARS;
        prt_health_bars();
    }

    if (plr->redraw & (PR_EFFECTS))
    {
        plr->redraw &= ~PR_EFFECTS;
        prt_effects();
    }

    if (plr->redraw & (PR_STATUS))
    {
        plr->redraw &= ~(PR_STATUS);
        prt_status();
    }

    if (plr->redraw & (PR_STATE))
    {
        plr->redraw &= ~(PR_STATE);
        prt_state();
    }

    /* *LAST* */
    if (plr->redraw & PR_MSG_LINE)
    {
        plr->redraw &= ~PR_MSG_LINE;
        msg_line_redraw();
    }
    if (dm->prof)
        z_timer_pause(&dm->prof->redraw_timer);
}


/*
 * Handle "plr->window"
 */
void window_stuff(void)
{
    int j;

    u32b mask = 0L;


    /* Nothing to do */
    if (!plr->window) return;
    if (plr->dun_id != cave->id) return;

    /* Scan windows */
    for (j = 0; j < 8; j++)
    {
        /* Save usable flags */
        if (angband_term[j]) mask |= window_flag[j];
    }

    /* Apply usable flags */
    plr->window &= mask;

    /* Nothing to do */
    if (!plr->window) return;


    /* Display inventory */
    if (plr->window & (PW_INVEN))
    {
        plr->window &= ~(PW_INVEN);
        fix_inven();
    }

    /* Display equipment */
    if (plr->window & (PW_EQUIP))
    {
        plr->window &= ~(PW_EQUIP);
        fix_equip();
    }

    /* Display spell list */
    if (plr->window & (PW_SPELL))
    {
        plr->window &= ~(PW_SPELL);
        fix_spell();
    }

    /* Display overhead view */
    if (plr->window & (PW_MESSAGE))
    {
        plr->window &= ~(PW_MESSAGE);
        fix_message();
    }

    /* Display overhead view */
    if (plr->window & (PW_OVERHEAD))
    {
        plr->window &= ~(PW_OVERHEAD);
        fix_overhead();
    }

    /* Display overhead view */
    if (plr->window & (PW_DUNGEON))
    {
        plr->window &= ~(PW_DUNGEON);
        fix_dungeon();
    }

    /* Display monster recall */
    if (plr->window & (PW_MONSTER))
    {
        plr->window &= ~(PW_MONSTER);
        fix_monster();
    }

    if (plr->window & PW_WORLD_MAP)
    {
        plr->window &= ~PW_WORLD_MAP;
        fix_world_map();
    }

    if (plr->window & PW_OBJECT_LIST)
    {
        plr->window &= ~(PW_OBJECT_LIST);
        fix_object_list();
    }

    if (plr->window & PW_MONSTER_LIST)
    {
        plr->window &= ~(PW_MONSTER_LIST);
        fix_monster_list();
    }

    /* Display object recall */
    if (plr->window & (PW_OBJECT))
    {
        plr->window &= ~(PW_OBJECT);
        fix_object();
    }
}


/*
 * Handle "plr->update" and "plr->redraw" and "plr->window"
 */
void handle_stuff(void)
{
    if (plr->dun_id != cave->id) return;
    if (plr->update) update_stuff();
    if (plr->redraw) redraw_stuff();
    if (plr->window) window_stuff();
}

int heavy_armor_limit(void)
{
    return 100 + plr->lev*4;
}

bool heavy_armor(void)
{
    u16b monk_arm_wgt = 0;
    bool check = FALSE;

    /* XXX Conside adding CLASS_HEAVY_ARMOR to class_t.flags.
     * XXX Skillmasters should only be restricted when not using a weapon */
    if (plr->pclass == CLASS_SKILLMASTER && !_is_martial_arts())
        check = FALSE;
    else if (plr_allow_martial_arts())
        check = TRUE;
    else if (plr->pclass == CLASS_NINJA || plr->pclass == CLASS_SCOUT)
        check = TRUE;

    if (!check) return FALSE;

    monk_arm_wgt = equip_weight(obj_is_armor);
    return monk_arm_wgt > heavy_armor_limit();
}

