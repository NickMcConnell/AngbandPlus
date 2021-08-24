/*
 * File: option.c
 * Purpose: Options table and definitions.
 *
 * Copyright (c) 1997 Ben Harrison
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
 * Option screen interface
 */
int option_page[OP_MAX][OPT_PAGE_PER];


static struct option_entry
{
    const char *name;
    const char *description;
    int type;
    bool normal;
    bool server;
} options[] =
{
    #define OP(a, b, c, d, e) {#a, b, OP_##c, d, e},
    #include "list-options.h"
    #undef OP
    {"max", "", OP_MAX, false, false}
};


/*
 * Given an option index, return its name
 */
const char *option_name(int opt)
{
    if (opt >= OPT_MAX) return NULL;
    return options[opt].name;
}


/*
 * Given an option index, return its description
 */
const char *option_desc(int opt)
{
    if (opt >= OPT_MAX) return NULL;
    return options[opt].description;
}


/*
 * Determine the type of option (score, birth etc)
 */
int option_type(int opt)
{
    if (opt >= OPT_MAX) return 0;
    return options[opt].type;
}


/* false if the option is off by default, true otherwise */
bool option_normal(int opt)
{
    if (opt >= OPT_MAX) return false;
    return options[opt].normal;
}


/* false if the option is irrelevant on the server, true otherwise */
bool option_server(int opt)
{
    if (opt >= OPT_MAX) return false;
    return options[opt].server;
}


void option_init(void)
{
    int opt, page;

    /* Allocate options to pages */
    for (page = 0; page < OP_MAX; page++)
    {
        int page_opts = 0;

        for (opt = 0; opt < OPT_MAX; opt++)
        {
            if ((options[opt].type == page) && (page_opts < OPT_PAGE_PER))
                option_page[page][page_opts++] = opt;
        }
        while (page_opts < OPT_PAGE_PER)
            option_page[page][page_opts++] = OPT_none;
    }
}
