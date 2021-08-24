/*
 * File: option.h
 * Purpose: Options table and definitions.
 */

#ifndef INCLUDED_OPTIONS_H
#define INCLUDED_OPTIONS_H

/*
 * Option types
 */
enum
{
    OP_INTERFACE = 0,
    OP_MANGBAND,
    OP_BIRTH,

    OP_MAX
};

/*
 * Information for "do_cmd_options()".
 */
#define OPT_PAGE_PER    16

/*
 * Option indexes
 */
enum
{
    #define OP(a, b, c, d, e) OPT_##a,
    #include "list-options.h"
    #undef OP
    OPT_MAX
};

#define OPT(opt_name) Client_setup.options[OPT_##opt_name]
#define OPT_P(P, opt_name) (P)->other.opt[OPT_##opt_name]

/*
 * The option data structures
 */
extern int option_page[OP_MAX][OPT_PAGE_PER];

/*
 * Functions
 */
extern const char *option_name(int opt);
extern const char *option_desc(int opt);
extern int option_type(int opt);
extern bool option_normal(int opt);
extern bool option_server(int opt);
extern void option_init(void);

#endif /* INCLUDED_OPTIONS_H */
