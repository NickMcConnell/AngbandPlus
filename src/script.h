/* File: script.h */

#ifndef INCLUDED_SCRIPT_H
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

/*
 * Execute one of the scripts attached to an object
 */
extern void apply_object_trigger(int trigger_id, object_type *o_ptr, cptr format, ...);

/*
 * Callback for using an object
 */
extern bool use_object(object_type *o_ptr, bool *ident, int aim);

/*
 * Execute a script attached to a field
 */
extern bool apply_field_trigger(cptr script, field_type *f_ptr, cptr format, va_list vp);
extern void const_field_trigger(cptr script, const field_type *f_ptr, cptr format, va_list vp);

/*
 * Debug lua stack depth
 */
extern void debug_lua_stack(void);

#endif /* INCLUDED_SCRIPT_H */

