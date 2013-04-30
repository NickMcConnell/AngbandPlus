/*
 * File: options.h
 * Purpose: Options table and definitions.
 */

#ifndef INCLUDED_OPTIONS_H
#define INCLUDED_OPTIONS_H

/*** Functions ***/

/* Given an option index, return its name */
extern const char *option_name(int opt);

/* Given an option index, return its description */
extern const char *option_desc(int opt);

/* Set an an option, return TRUE if successful */
extern bool option_set(bool *opts, const char *opt, bool on);

/* Reset options to defaults */
extern void option_set_defaults(bool *opts);

/*** Option display definitions ***/

/*
 * Information for "do_cmd_options()".
 */
#define OPT_PAGE_MAX    4
#define OPT_PAGE_PER    15

#define OPT_PAGE_BIRTH  3

/* The option data structures */
extern const int option_page[OPT_PAGE_MAX][OPT_PAGE_PER];

#endif /* INCLUDED_OPTIONS_H */
