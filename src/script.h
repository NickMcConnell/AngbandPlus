/* File: script.h */

#define INCLUDED_SCRIPT_H

#include "angband.h"


/*
 * Initalize the scripting support
 */
extern errr script_init(void);

/*
 * Free the resources for the scripting support
 */
extern errr script_free(void);

/* The call_lua function, used to call lua code! */
extern bool call_lua(cptr hook, cptr fmt, cptr ret, ...);

/*
 * Display the script debug menu
 */
extern void do_cmd_script(void);

/*
 * Execute a string of scripting code
 */
extern bool script_do_string(cptr script);

/*
 * Execute a file with scripting code
 */
extern bool script_do_file(cptr filename);