/*
 * File: util.c
 * Purpose: Utility functions
 *
 * Copyright (c) 1997 Ben Harrison, James E. Wilson, Robert A. Koeneke
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
#include "tvalsval.h"


/* List of { tval, name } pairs. */
static const grouper tval_names[] =
{
    {TV_SKELETON, "skeleton"},
    {TV_BOTTLE, "bottle"},
    {TV_STONE, "stone"},
    {TV_CORPSE, "corpse"},
    {TV_SPIKE, "spike"},
    {TV_CHEST, "chest"},
    {TV_HORN, "horn"},
    {TV_ROCK, "rock"},
    {TV_SHOT, "shot"},
    {TV_ARROW, "arrow"},
    {TV_BOLT, "bolt"},
    {TV_BOW, "bow"},
    {TV_DIGGING, "digger"},
    {TV_HAFTED, "hafted"},
    {TV_POLEARM, "polearm"},
    {TV_SWORD, "sword"},
    {TV_MSTAFF, "mage staff"},
    {TV_BOOTS, "boots"},
    {TV_GLOVES, "gloves"},
    {TV_HELM, "helm"},
    {TV_CROWN, "crown"},
    {TV_SHIELD, "shield"},
    {TV_CLOAK, "cloak"},
    {TV_SOFT_ARMOR, "soft armour"},
    {TV_HARD_ARMOR, "hard armour"},
    {TV_DRAG_ARMOR, "dragon armour"},
    {TV_LIGHT, "light"},
    {TV_AMULET, "amulet"},
    {TV_RING, "ring"},
    {TV_STAFF, "staff"},
    {TV_WAND, "wand"},
    {TV_ROD, "rod"},
    {TV_SCROLL, "scroll"},
    {TV_POTION, "potion"},
    {TV_FLASK, "flask"},
    {TV_FOOD, "food"},
    {TV_CROP, "crop"},
    {TV_MAGIC_BOOK, "magic book"},
    {TV_PRAYER_BOOK, "prayer book"},
    {TV_SORCERY_BOOK, "sorcery book"},
    {TV_SHADOW_BOOK, "shadow book"},
    {TV_HUNT_BOOK, "hunt book"},
    {TV_PSI_BOOK, "psi book"},
    {TV_DEATH_BOOK, "death book"},
    {TV_ELEM_BOOK, "elemental book"},
    {TV_SUMMON_BOOK, "summoning book"},
    {TV_GOLD, "gold"}
};


/*
 * Returns the numeric equivalent tval of the textual tval `name`.
 */
int tval_find_idx(const char *name)
{
    size_t i = 0;
    unsigned int r;

    if (sscanf(name, "%u", &r) == 1) return r;

    for (i = 0; i < N_ELEMENTS(tval_names); i++)
    {
        if (!my_stricmp(name, tval_names[i].name))
            return tval_names[i].tval;
    }

    return -1;
}


/*
 * Returns the textual equivalent tval of the numeric tval `name`.
 */
const char *tval_find_name(int tval)
{
    size_t i = 0;

    for (i = 0; i < N_ELEMENTS(tval_names); i++)
    {
        if (tval == tval_names[i].tval)
            return tval_names[i].name;
    }

    return "unknown";
}


/*
 * Formats 'fmt' into 'buf', with the following formatting characters:
 *
 * '~' at the end of a word (e.g. "fridge~") will pluralise
 *
 * '|x|y|' will be output as 'x' if singular or 'y' if plural (e.g. "kni|fe|ves|")
 *
 * '#' will be replaced with 'modstr' (which may contain the pluralising
 * formats given above).
 */
size_t obj_desc_name_format(char *buf, size_t max, size_t end, const char *fmt, const char *modstr,
    bool pluralise)
{
    /* Copy the string */
    while (*fmt)
    {
        if (*fmt == '&')
        {
            while (*fmt == ' ' || *fmt == '&')
                fmt++;
            continue;
        }

        /* Pluralizer (regular English plurals) */
        else if (*fmt == '~')
        {
            char prev = *(fmt - 1);

            if (!pluralise)
            {
                fmt++;
                continue;
            }

            /* e.g. cutlass-e-s, torch-e-s, box-e-s */
            if (prev == 's' || prev == 'h' || prev == 'x')
                strnfcat(buf, max, &end, "es");
            else
                strnfcat(buf, max, &end, "s");
        }

        /* Special plurals */
        else if (*fmt == '|')
        {
            /* e.g. kni|fe|ves| */
            const char *singular = fmt + 1;
            const char *plural   = strchr(singular, '|');
            const char *endmark  = NULL;

            if (plural)
            {
                plural++;
                endmark = strchr(plural, '|');
            }

            if (!singular || !plural || !endmark) return end;

            if (!pluralise)
                strnfcat(buf, max, &end, "%.*s", plural - singular - 1, singular);
            else
                strnfcat(buf, max, &end, "%.*s", endmark - plural, plural);

            fmt = endmark;
        }

        /* Add modstr, with pluralisation if relevant */
        else if (*fmt == '#')
            end = obj_desc_name_format(buf, max, end, modstr, NULL, pluralise);

        else
            buf[end++] = *fmt;

        fmt++;
    }

    buf[end] = 0;

    return end;
}


/*
 * Return the numeric sval of the object kind with the given `tval` and name `name`.
 */
int lookup_sval(int tval, const char *name)
{
    int k;
    unsigned int r;

    if (sscanf(name, "%u", &r) == 1) return r;

    /* Look for it */
    for (k = 1; k < z_info->k_max; k++)
    {
        object_kind *k_ptr = &k_info[k];
        char cmp_name[MSG_LEN];

        if (!k_ptr || !k_ptr->name) continue;

        obj_desc_name_format(cmp_name, sizeof(cmp_name), NULL, k_ptr->name, 0, FALSE);

        /* Found a match */
        if ((k_ptr->tval == tval) && !my_stricmp(cmp_name, name)) return k_ptr->sval;
    }

    plog_fmt("No object (\"%s\",\"%s\")", tval_find_name(tval), name);
    return -1;
}


/*
 * Return the object kind with the given `tval` and `sval`, or NULL.
 */
object_kind *lookup_kind(int tval, int sval)
{
    int k;

    /* Look for it */
    for (k = 0; k < z_info->k_max; k++)
    {
        object_kind *kind = &k_info[k];

        if ((kind->tval == tval) && (kind->sval == sval)) return kind;
    }

    /* Failure */
    plog_fmt("No object: %d:%d (%s)", tval, sval, tval_find_name(tval));
    return NULL;
}


/*
 * Converts stat num into a six-char (right justified) string
 */
void cnv_stat(int val, char *out_val, size_t out_len)
{
    /* Above 18 */
    if (val > 18)
    {
        int bonus = (val - 18);

        if (bonus >= 220)
            strnfmt(out_val, out_len, "18/***");
        else if (bonus >= 100)
            strnfmt(out_val, out_len, "18/%03d", bonus);
        else
            strnfmt(out_val, out_len, " 18/%02d", bonus);
    }

    /* From 3 to 18 */
    else
        strnfmt(out_val, out_len, "    %2d", val);
}


/*
 * Accept a color index character; if legal, return the color.
 *
 * Unlike Sangband, we don't translate these colours here.
 */
int color_char_to_attr(char c)
{
    int a;

    /* Is negative -- Spit it right back out */
    if (c < 0) return (c);

    /* Is a space or '\0' -- Return black */
    if ((c == '\0') || (c == ' ')) return (TERM_DARK);

    /* Shimmering objects */
    if (c == 'x') return (TERM_MULTI);

    /* Special coloring */
    if (c == 'X') return (TERM_SPECIAL);
    if (c == 'S') return (TERM_SYMBOL);

    /* Search the color table */
    for (a = 0; a < BASIC_COLORS; a++)
    {
        /* Look for the index */
        if (color_table[a].index_char == c) break;
    }

    /* If we don't find the color, we assume white */
    if (a == BASIC_COLORS) return (TERM_WHITE);

    /* Return the color */
    return (a);
}


/*
 * Converts a string to a terminal color byte.
 */
int color_text_to_attr(const char *name)
{
    int a;

    for (a = 0; a < MAX_COLORS; a++)
    {
        if (my_stricmp(name, color_table[a].name) == 0) return (a);
    }

    /* Default to white */
    return (TERM_WHITE);
}


/*
 * Find the start of a possible Roman numerals suffix by going back from the
 * end of the string to a space, then checking that all the remaining chars
 * are valid Roman numerals.
 */
static char *find_roman_suffix_start(const char *buf)
{
    char *start = strrchr(buf, ' ');
    char *p;

    if (start)
    {
        start++;
        p = start;
        while (*p)
        {
            if ((*p != 'I') && (*p != 'V') && (*p != 'X') && (*p != 'L') &&
                (*p != 'C') && (*p != 'D') && (*p != 'M'))
            {
                start = NULL;
                break;
            }
            ++p;
        }
    }
    return start;
}


/*
 * Converts an arabic numeral (int) to a roman numeral (char *).
 *
 * An arabic numeral is accepted in parameter `n`, and the corresponding
 * upper-case roman numeral is placed in the parameter `roman`.  The
 * length of the buffer must be passed in the `bufsize` parameter.  When
 * there is insufficient room in the buffer, or a roman numeral does not
 * exist (e.g. non-positive integers) a value of FALSE is returned and the
 * `roman` buffer will be the empty string.  On success, a value of TRUE is
 * returned and the zero-terminated roman numeral is placed in the
 * parameter `roman`.
 *
 */
static bool int_to_roman(int n, char *roman, size_t bufsize)
{
    /* Roman symbols */
    char roman_symbol_labels[13][3] =
        {"M", "CM", "D", "CD", "C", "XC", "L", "XL", "X", "IX", "V", "IV", "I"};
    int roman_symbol_values[13] =
        {1000, 900, 500, 400, 100, 90, 50, 40, 10, 9, 5, 4, 1};

    /* Clear the roman numeral buffer */
    roman[0] = '\0';

    /* Roman numerals have no zero or negative numbers */
    if (n < 1) return FALSE;

    /* Build the roman numeral in the buffer */
    while (n > 0)
    {
        int i = 0;

        /* Find the largest possible roman symbol */
        while (n < roman_symbol_values[i]) i++;

        /* No room in buffer, so abort */
        if (strlen(roman) + strlen(roman_symbol_labels[i]) + 1 > bufsize) break;

        /* Add the roman symbol to the buffer */
        my_strcat(roman, roman_symbol_labels[i], bufsize);

        /* Decrease the value of the arabic numeral */
        n -= roman_symbol_values[i];
    }

    /* Ran out of space and aborted */
    if (n > 0)
    {
        /* Clean up and return */
        roman[0] = '\0';
        return FALSE;
    }

    return TRUE;
}


/*
 * Converts a roman numeral (char *) to an arabic numeral (int).
 *
 * The null-terminated roman numeral is accepted in the `roman`
 * parameter and the corresponding integer arabic numeral is returned.
 * Only upper-case values are considered. When the `roman` parameter
 * is empty or does not resemble a roman numeral, a value of -1 is
 * returned.
 */
static int roman_to_int(const char *roman)
{
    size_t i;
    int n = 0;
    char *p;
    char roman_token_chr1[] = "MDCLXVI";
    const char *roman_token_chr2[] = {0, 0, "DM", 0, "LC", 0, "VX"};
    int roman_token_vals[7][3] =
        {{1000}, {500}, {100, 400, 900}, {50}, {10, 40, 90}, {5}, {1, 4, 9}};

    if (strlen(roman) == 0) return -1;

    /*
     * Check each character for a roman token, and look ahead to the
     * character after this one to check for subtraction
     */
    for (i = 0; i < strlen(roman); i++)
    {
        char c1, c2;
        int c1i, c2i;

        /* Get the first and second chars of the next roman token */
        c1 = roman[i];
        c2 = roman[i + 1];

        /* Find the index for the first character */
        p = strchr(roman_token_chr1, c1);
        if (p) c1i = p - roman_token_chr1;
        else return -1;

        /* Find the index for the second character */
        c2i = 0;
        if (roman_token_chr2[c1i] && c2)
        {
            p = strchr(roman_token_chr2[c1i], c2);
            if (p)
            {
                c2i = (p - roman_token_chr2[c1i]) + 1;

                /* Two-digit token, so skip a char on the next pass */
                i++;
            }
        }

        /* Increase the arabic numeral */
        n += roman_token_vals[c1i][c2i];
    }

    return n;
}


/*
 * Converts a string to a terminal color byte.
 */
bool get_incarnation(int n, char *name, size_t len)
{
    char *buf;

    /* Handle incrementing name suffix */
    buf = find_roman_suffix_start(name);
    if (!buf) return FALSE;

    /* Try to increment the roman suffix */
    return int_to_roman((roman_to_int(buf) + n), buf, (len - (buf - (char *)&name)));
}


/*
 * Returns a "rating" of x depending on y, and sets "attr" to the
 * corresponding "attribute".
 */
const char *likert(int x, int y, byte *attr)
{
    /* Paranoia */
    if (y <= 0) y = 1;

    /* Negative values */
    if (x < 0)
    {
        *attr = TERM_RED;
        return ("Very Bad");
    }

    /* Analyze the value */
    switch ((x / y))
    {
        case 0:
        case 1:
        {
            *attr = TERM_RED;
            return ("Bad");
        }
        case 2:
        {
            *attr = TERM_RED;
            return ("Poor");
        }
        case 3:
        case 4:
        {
            *attr = TERM_YELLOW;
            return ("Fair");
        }
        case 5:
        {
            *attr = TERM_YELLOW;
            return ("Good");
        }
        case 6:
        {
            *attr = TERM_YELLOW;
            return ("Very Good");
        }
        case 7:
        case 8:
        {
            *attr = TERM_L_GREEN;
            return ("Excellent");
        }
        case 9:
        case 10:
        case 11:
        case 12:
        case 13:
        {
            *attr = TERM_L_GREEN;
            return ("Superb");
        }
        case 14:
        case 15:
        case 16:
        case 17:
        {
            *attr = TERM_L_GREEN;
            return ("Heroic");
        }
        default:
        {
            *attr = TERM_L_GREEN;
            return ("Legendary");
        }
    }
}


/*
 * Base experience levels, may be adjusted up for race and/or class
 */
static s32b player_exp[PY_MAX_LEVEL] =
{
    10,
    25,
    45,
    70,
    100,
    140,
    200,
    280,
    380,
    500,
    650,
    850,
    1100,
    1400,
    1800,
    2300,
    2900,
    3600,
    4400,
    5400,
    6800,
    8400,
    10200,
    12500,
    17500,
    25000,
    35000L,
    50000L,
    75000L,
    100000L,
    150000L,
    200000L,
    275000L,
    350000L,
    450000L,
    550000L,
    700000L,
    850000L,
    1000000L,
    1250000L,
    1500000L,
    1800000L,
    2100000L,
    2400000L,
    2700000L,
    3000000L,
    3500000L,
    4000000L,
    4500000L,
    5000000L
};


s32b adv_exp(s16b lev, s16b expfact)
{
    /* Max reached */
    if (lev >= PY_MAX_LEVEL) return 0L;

    /* High exp: first divide by 100 to avoid overflow */
    if (lev >= 20) return (s32b)((player_exp[lev - 1] / 100L) * expfact);

    /* Low exp */
    return (s32b)(player_exp[lev - 1] * expfact / 100L);
}


/*
 * Check an item against the item tester info
 */
bool item_tester_okay(struct player *p, const object_type *o_ptr)
{
    /* Hack -- allow listing empty slots */
    if (item_tester_full) return (TRUE);

    /* Require an item */
    if (!o_ptr->kind) return (FALSE);

    /* Hack -- ignore "gold" */
    if (o_ptr->tval == TV_GOLD) return (FALSE);

    /* Check the tval */
    if (item_tester_tval)
    {
        if (item_tester_tval != o_ptr->tval) return (FALSE);
    }

    /* Check the hook */
    if (item_tester_hook)
    {
        if (!(*item_tester_hook)(p, o_ptr)) return (FALSE);
    }

    /* Assume okay */
    return (TRUE);
}


/*
 * Determine whether an object is ammo
 *
 * o_ptr is the object to check
 */
bool obj_is_ammo(struct player *p, const object_type *o_ptr)
{
    return ((o_ptr->tval == TV_SHOT) || (o_ptr->tval == TV_ARROW) ||
        (o_ptr->tval == TV_BOLT) || (o_ptr->tval == TV_ROCK));
}


struct player_class *player_id2class(guid id)
{
    struct player_class *c;

    for (c = classes; c; c = c->next)
    {
        if (guid_eq(c->cidx, id)) break;
    }

    return c;
}


int player_cmax(void)
{
    int n = 0;
    struct player_class *c;

    for (c = classes; c; c = c->next) n++;

    return n;
}


struct player_race *player_id2race(guid id)
{
    struct player_race *r;

    for (r = races; r; r = r->next)
    {
        if (guid_eq(r->ridx, id)) break;
    }

    return r;
}


int player_rmax(void)
{
    int n = 0;
    struct player_race *r;

    for (r = races; r; r = r->next) n++;

    return n;
}
