/*
 * File: z-util.c
 * Purpose: Low-level string handling and other utilities.
 *
 * Copyright (c) 1997-2005 Ben Harrison, Robert Ruehlmann.
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


/*
 * Global variables for temporary use
 */
char char_tmp = 0;
byte byte_tmp = 0;
sint sint_tmp = 0;
uint uint_tmp = 0;
long long_tmp = 0;
errr errr_tmp = 0;


/*
 * Global pointers for temporary use
 */
const char *cptr_tmp = NULL;
void *vptr_tmp = NULL;


/*
 * Constant bool meaning true
 */
bool bool_true = 1;


/*
 * Constant bool meaning false
 */
bool bool_false = 0;


/*
 * Global NULL const char *
 */
const char *cptr_null = NULL;


/*
 * Global NULL void *
 */
void *vptr_null = NULL;


/*
 * Global SELF void *
 */
void *vptr_self = (void *)(&vptr_self);


/*
 * Convenient storage of the program name
 */
char *argv0 = NULL;


/*
 * A routine that does nothing
 */
void func_nothing(void)
{
    /* Do nothing */
}


/*
 * A routine that always returns "success"
 */
errr func_success(void)
{
    return (0);
}


/*
 * A routine that always returns a simple "problem code"
 */
errr func_problem(void)
{
    return (1);
}


/*
 * A routine that always returns a simple "failure code"
 */
errr func_failure(void)
{
    return (-1);
}


/*
 * A routine that always returns "true"
 */
bool func_true(void)
{
    return (1);
}


/*
 * A routine that always returns "false"
 */
bool func_false(void)
{
    return (0);
}


/*
 * Count the number of characters in a UTF-8 encoded string
 */
size_t utf8_strlen(char *s)
{
    size_t i = 0, j = 0;

    while (s[i])
    {
        if ((s[i] & 0xc0) != 0x80) j++;
        i++;
    }

    return j;
}


/*
 * Clip a null-terminated UTF-8 string 's' to 'n' unicode characters.
 * e.g. utf8_clipto("example", 4) will clip after 'm', resulting in 'exam'.
 */
void utf8_clipto(char *s, size_t n)
{
    size_t i = 0, j = 0;
    bool terminate_next = false;

    if (n == 0)
    {
        s[i] = 0;
        return;
    }

    while (s[i])
    {
        if ((s[i] & 0xc0) != 0x80)
        {
            j++;
            if (terminate_next) break;
            if (j == n) terminate_next = true;
        }
        i++;
    }
    s[i] = 0;
}


/*
 * Case insensitive comparison between two strings
 */
int my_stricmp(const char *s1, const char *s2)
{
    char ch1 = 0;
    char ch2 = 0;

    /* Just loop */
    while (true)
    {
        /* We've reached the end of both strings simultaneously */
        if ((*s1 == 0) && (*s2 == 0))
        {
            /* We're still here, so s1 and s2 are equal */
            return (0);
        }

        ch1 = toupper((unsigned char)*s1);
        ch2 = toupper((unsigned char)*s2);

        /* If the characters don't match */
        if (ch1 != ch2)
        {
            /* return the difference between them */
            return ((int)(ch1 - ch2));
        }

        /* Step on through both strings */
        s1++;
        s2++;
    }
}


/*
 * Case insensitive comparison between the first n characters of two strings
 */
int my_strnicmp(const char *a, const char *b, int n)
{
    const char *s1, *s2;
    char z1, z2;

    /* Scan the strings */
    for (s1 = a, s2 = b; n > 0; s1++, s2++, n--)
    {
        z1 = toupper((unsigned char)*s1);
        z2 = toupper((unsigned char)*s2);
        if (z1 < z2) return (-1);
        if (z1 > z2) return (1);
        if (!z1) return (0);
    }

    return 0;
}


/*
 * The my_strcpy() function copies up to 'bufsize'-1 characters from 'src'
 * to 'buf' and NULL-terminates the result.  The 'buf' and 'src' strings may
 * not overlap.
 *
 * my_strcpy() returns strlen(src).  This makes checking for truncation
 * easy.  Example: if (my_strcpy(buf, src, sizeof(buf)) >= sizeof(buf)) ...;
 *
 * This function should be equivalent to the strlcpy() function in BSD.
 */
size_t my_strcpy(char *buf, const char *src, size_t bufsize)
{
    size_t len = strlen(src);
    size_t ret = len;

    /* Paranoia */
    if (bufsize == 0) return ret;

    /* Truncate */
    if (len >= bufsize) len = bufsize - 1;

    /* Copy the string and terminate it */
    memcpy(buf, src, len);
    buf[len] = '\0';

    /* Return strlen(src) */
    return ret;
}


/*
 * The my_strcat() tries to append a string to an existing NULL-terminated string.
 * It never writes more characters into the buffer than indicated by 'bufsize' and
 * NULL-terminates the buffer.  The 'buf' and 'src' strings may not overlap.
 *
 * my_strcat() returns strlen(buf) + strlen(src).  This makes checking for
 * truncation easy.  Example:
 * if (my_strcat(buf, src, sizeof(buf)) >= sizeof(buf)) ...;
 *
 * This function should be equivalent to the strlcat() function in BSD.
 */
size_t my_strcat(char *buf, const char *src, size_t bufsize)
{
    size_t dlen = strlen(buf);

    /* Is there room left in the buffer? */
    if (dlen < bufsize - 1)
    {
        /* Append as much as possible  */
        return (dlen + my_strcpy(buf + dlen, src, bufsize - dlen));
    }
    else
    {
        /* Return without appending */
        return (dlen + strlen(src));
    }
}


/*
 * Capitalise the first letter of string 'buf'.
 */
void my_strcap(char *buf)
{
    if (buf && buf[0])
        buf[0] = toupper((unsigned char)buf[0]);
}


/*
 * Determine if string "a" is equal to string "b"
 */
bool streq(const char *a, const char *b)
{
    return (!strcmp(a, b));
}


/*
 * Determine if string "t" is a suffix of string "s"
 */
bool suffix(const char *s, const char *t)
{
    size_t tlen = strlen(t);
    size_t slen = strlen(s);

    /* Check for incompatible lengths */
    if (tlen > slen) return false;

    /* Compare "t" to the end of "s" */
    return (!strcmp(s + slen - tlen, t));
}


/*
 * Determine if string "t" is a prefix of string "s"
 */
bool prefix(const char *s, const char *t)
{
    /* Scan "t" */
    while (*t)
    {
        /* Compare content and length */
        if (*t++ != *s++) return false;
    }

    /* Matched, we have a prefix */
    return true;
}


/*
 * Redefinable "plog" action
 */
void (*plog_aux)(const char *) = NULL;


/*
 * Print (or log) a "warning" message (ala "perror()")
 * Note the use of the (optional) "plog_aux" hook.
 */
void plog(const char *str)
{
    /* Use the "alternative" function if possible */
    if (plog_aux) (*plog_aux)(str);

    /* Just do a labeled fprintf to stderr */
    else fprintf(stderr, "%s: %s\n", (argv0? argv0: "???"), str);
}


/*
 * Redefinable "quit" action
 */
void (*quit_aux)(const char *) = NULL;


/*
 * Exit (ala "exit()").  If 'str' is NULL, do "exit(0)".
 * If 'str' begins with "+" or "-", do "exit(atoi(str))".
 * Otherwise, plog() 'str' and exit with an error code of -1.
 * But always use 'quit_aux', if set, before anything else.
 */
void quit(const char *str)
{
    char buf[MSG_LEN];

    /* Save exit string */
    if (str) my_strcpy(buf, str, sizeof(buf));

    /* Attempt to use the aux function */
    /* This was passing buf, which is a bad idea if quit() is called with NULL */
    if (quit_aux) (*quit_aux)(str);

    /* Success */
    if (!str) exit(0);

    /* Extract a "special error code" */
    if ((buf[0] == '-') || (buf[0] == '+')) exit(atoi(buf));

    /* Send the string to plog() */
    plog(buf);

    /* Failure */
    exit(-1);
}


/*
 * Redefinable "assert" action
 */
void (*assert_aux)(void) = NULL;


/*
 * Check a char for "vowel-hood"
 */
bool is_a_vowel(int ch)
{
    switch (tolower((unsigned char)ch))
    {
        case 'a':
        case 'e':
        case 'i':
        case 'o':
        case 'u':
        return true;
    }

    return false;
}


/* hturn manipulations */


u32b ht_diff(hturn *ht_ptr1, hturn *ht_ptr2)
{
    u32b delta_era = ht_ptr1->era - ht_ptr2->era;

    if ((delta_era > 1) || ((delta_era == 1) && (ht_ptr1->turn >= ht_ptr2->turn)))
        return HTURN_ERA_FLIP;

    if (delta_era == 0) return ht_ptr1->turn - ht_ptr2->turn;

    return HTURN_ERA_FLIP + ht_ptr1->turn - ht_ptr2->turn;
}


char *ht_show(hturn *ht_ptr)
{
    static char buf[MSG_LEN];

    if (!ht_ptr->era)
        strnfmt(buf, sizeof(buf), "%lu", ht_ptr->turn);
    else
        strnfmt(buf, sizeof(buf), "%lu%06lu", ht_ptr->era, ht_ptr->turn);

    return &buf[0];
}


void ht_copy(hturn *ht_ptr1, hturn *ht_ptr2)
{
    ht_ptr1->era = ht_ptr2->era;
    ht_ptr1->turn = ht_ptr2->turn;
}


void ht_add(hturn *ht_ptr, u32b value)
{
    u32b new_turn = value + ht_ptr->turn;

    while (new_turn >= HTURN_ERA_FLIP)
    {
        ht_ptr->era++;
        new_turn -= HTURN_ERA_FLIP;
    }

    ht_ptr->turn = new_turn;
}


void ht_reset(hturn *ht_ptr)
{
    ht_ptr->era = 0;
    ht_ptr->turn = 0;
}


bool ht_zero(hturn *ht_ptr)
{
    return ((ht_ptr->turn == 0) && (ht_ptr->era == 0));
}


int ht_cmp(hturn *ht_ptr1, hturn *ht_ptr2)
{
    if (ht_ptr1->era > ht_ptr2->era) return 1;

    if (ht_ptr1->era < ht_ptr2->era) return -1;

    if (ht_ptr1->turn > ht_ptr2->turn) return 1;

    if (ht_ptr1->turn < ht_ptr2->turn) return -1;

    return 0;
}


u32b ht_div(hturn *ht_ptr, s16b value)
{
    if (ht_ptr->era > HTURN_ERA_MAX_DIV) return 0;

    return (ht_ptr->era * HTURN_ERA_FLIP + ht_ptr->turn) / value;
}


/*
 * Case insensitive strstr by Dave Sinkula
 */
char *my_stristr(const char *haystack, const char *needle)
{
    if (!*needle) return (char*)haystack;

    for ( ; *haystack; ++haystack)
    {
        if (toupper((unsigned char)*haystack) == toupper((unsigned char)*needle))
        {
            const char *h, *n;

            /* Matched starting char -- loop through remaining chars. */
            for (h = haystack, n = needle; *h && *n; ++h, ++n)
            {
                if (toupper((unsigned char)*h) != toupper((unsigned char)*n)) break;
            }

            /* Matched all of 'needle' to null termination */
            /* Return the start of the match */
            if (!*n) return (char*)haystack;
        }
    }

    return NULL;
}


/*
 * Arithmetic mean of the first 'size' entries of the array 'nums'
 */
int mean(int *nums, int size)
{
    int i, total = 0;

    for (i = 0; i < size; i++) total += nums[i];

    return total / size;
}


/*
 * Variance of the first 'size' entries of the array 'nums'
 */
int variance(int *nums, int size)
{
    int i, avg, total = 0;

    avg = mean(nums, size);

    for (i = 0; i < size; i++)
    {
        int delta = nums[i] - avg;

        total += delta * delta;
    }

    return total / size;
}


void sort(void *base, size_t nmemb, size_t smemb, int (*comp)(const void *, const void *))
{
    qsort(base, nmemb, smemb, comp);
}


/*
 * Rewrite string s in-place "skipping" every occurrence of character c except
 * those preceded by character e
 */
void strskip(char *s, const char c, const char e)
{
    char *in = s;
    char *out = s;
    bool escapeseen = false;

    while (*in)
    {
        if ((*in != c) && ((*in != e) || escapeseen))
        {
            /* Not escaping anything */
            if (escapeseen)
            {
                *out = e;
                out++;
            }
            *out = *in;
            out++;
            escapeseen = false;
        }

        /* Maybe escaping something */
        else if (*in == e)
            escapeseen = true;

        /* Add the escaped character */
        else if (escapeseen)
        {
            *out = *in;
            out++;
            escapeseen = false;
        }
        in++;
    }
    *out = 0;
}


/*
 * Rewrite string s in-place removing escape character c.
 * Note that pairs of c will leave one instance of c in out.
 */
void strescape(char *s, const char c)
{
    char *in = s;
    char *out = s;
    bool escapenext = false;

    while (*in)
    {
        if ((*in != c) || escapenext)
        {
            *out = *in;
            out++;
            escapenext = false;
        }
        else if (*in == c)
            escapenext = true;
        in++;
    }
    *out = 0;
}


/*
 * Returns true if string only contains spaces
 */
bool contains_only_spaces(const char *s)
{
    char spaces[] = " \t";

    while (*s)
    {
        if (strchr(spaces, *s) != NULL) return false;
        s++;
    }
    return true;
}


u32b djb2_hash(const char *str)
{
    u32b hash = 5381;
    int c = *str;

    while (c)
    {
        hash = ((hash << 5) + hash) + c; /* hash * 33 + c */
        c = *++str;
    }

    return hash;
}
