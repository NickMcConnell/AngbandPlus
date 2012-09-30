/* File: script.c */

#include "angband.h"

#include "script.h"


#include "lua/lua.h"
#include "lua/lualib.h"
#include "lua/lauxlib.h"
#include "lua/tolua.h"
#include "lua/luadebug.h"


/*
 * Lua state
 */
static lua_State* L = NULL;


static int xxx_msgf(lua_State *L)
{
	cptr text = lua_tostring(L, 1);
	if (text) msgf(text);
	lua_pop(L, 1);

	return 0;
}


static int xxx_msg_flush(lua_State *L)
{
	/* Hack - ignore parameter */
	(void) L;
	
	message_flush();

	return 0;
}


static int xxx_build_script_path(lua_State *L)
{
	char buf[1024];
	cptr filename;

	if (!tolua_istype(L, 1, LUA_TSTRING,0))
		tolua_error(L, "#vinvalid type in variable assignment.");

	filename = tolua_getstring(L, 1, 0);

	path_make(buf, ANGBAND_DIR_SCRIPT, filename);

	tolua_pushstring(L, buf);

	return 1;
}


static int xxx_get_rumor(lua_State *L)
{
	errr err;
	char buf[1024];

	switch (randint1(20))
	{
		case 1:
			err = get_rnd_line("chainswd.txt", 0, buf);
			break;
		case 2:
			err = get_rnd_line("error.txt", 0, buf);
			break;
		case 3:
		case 4:
		case 5:
			err = get_rnd_line("death.txt", 0, buf);
			break;
		default:
			err = get_rnd_line("rumors.txt", 0, buf);
			break;
	}

	/* An error occured */
	if (err) strcpy(buf, "Some rumors are wrong.");

	/* Push the rumor */
	tolua_pushstring(L, buf);

	/* One result */
	return (1);
}

static int xxx_get_rnd_line(lua_State *L)
{
	cptr filename;
	char buf[1024];
	int err;

	filename = lua_tostring(L, 1);

	/* Pop off the filename */
	lua_pop(L, 1);

	err = get_rnd_line(filename, 0, buf);

	/* Check for error */
	if (err)
		return 0;

	tolua_pushstring(L, buf);
	return (1);
}

static int xxx_get_aim_dir(lua_State *L)
{
	int dir;
	bool success;

	success = get_aim_dir(&dir);
	tolua_pushbool(L, success);
	lua_pushnumber(L, dir);

	return 2;
}


static int xxx_fire_beam(lua_State *L)
{
	int typ, dir, dam;
	bool result;

	typ = (int)luaL_check_number(L, 1);
	dir = (int)luaL_check_number(L, 2);
	dam = (int)luaL_check_number(L, 3);
	result = fire_beam(typ, dir, dam);
	tolua_pushbool(L, result);

	return 1;
}


#ifdef RISCOS
extern char *riscosify_name(const char *);

static int xxx_dofile(lua_State *L)
{			
	cptr filename = lua_tostring(L, 1);
	
	/* Pop off the filename */
	lua_pop(L, 1);

	if (filename)
	{
	 	return lua_dofile(L, riscosify_name(filename));
	}

	return 0;
}
#endif /* RISCOS */

static int xxx_building_options(lua_State *L)
{
	int i;
	
	/* number of arguments */
	int n = lua_gettop(L);
	
	cptr *strings;
		
	/* Make array of strings */
	C_MAKE(strings, n, cptr);

	for (i = 1; i <= n; i++)
	{
		if (!tolua_istype(L,i,LUA_TSTRING,0))
		{
			tolua_error(L, "#vinvalid type in lua_building_options().");
		}
		
		strings[i - 1] =  lua_tostring(L, i);
	}
	
	/* Print them out */
	print_building_options(strings, n);

	/* Free saved pointers */
	FREE((vptr) strings);

	return 0;
}


static const struct luaL_reg anglib[] =
{
	{"msg_print", xxx_msgf},
	{"msg_flush", xxx_msg_flush},
	{"get_aim_dir", xxx_get_aim_dir},
	{"fire_beam", xxx_fire_beam},
	{"build_script_path", xxx_build_script_path},
	{"get_rumor", xxx_get_rumor},
	{"get_rnd_line", xxx_get_rnd_line},
	{"building_options", xxx_building_options},
#ifdef RISCOS
	{"dofile", xxx_dofile},
#endif /* RISCOS */
};


#define MULTIADIC(name, op) \
	static int name(lua_State* L) \
	{ \
		int i, n = lua_gettop(L);\
		\
		int result = luaL_check_int(L, 1); \
		\
		for (i = 2; i <= n; i++)\
		{	\
			result = result op luaL_check_int(L, i); \
		}	\
		lua_pushnumber(L, result); \
		return 1; \
	}

#define DYADIC(name, op) \
    static int name(lua_State* L) \
	{ \
        lua_pushnumber(L, luaL_check_int(L, 1) op luaL_check_int(L, 2)); \
		return 1; \
    }

#define MONADIC(name, op) \
    static int name(lua_State* L) \
	{ \
        lua_pushnumber(L, op luaL_check_int(L, 1)); \
		return 1; \
    }


DYADIC(intMod,      % )
MULTIADIC(intAnd,      & )
MULTIADIC(intOr,       | )
DYADIC(intXor,      ^ )
DYADIC(intShiftl,   <<)
DYADIC(intShiftr,   >>)
MONADIC(intBitNot,  ~ )


static int math_min(lua_State *L)
{
	int i;

	/* number of arguments */
	int n = lua_gettop(L);

	long dmin = luaL_check_number(L, 1);

	for (i = 2; i <= n; i++)
	{
		long d = luaL_check_number(L, i);
		if (d < dmin) dmin = d;
	}

	lua_pushnumber(L, dmin);

	return 1;
}


static int math_max(lua_State *L)
{
	int i;

	/* number of arguments */
	int n = lua_gettop(L);
	
	long dmax = luaL_check_number(L, 1);

	for (i = 2; i <= n; i++)
	{
		long d = luaL_check_number(L, i);
		if (d > dmax) dmax = d;
	}

	lua_pushnumber(L, dmax);

	return 1;
}

static const struct luaL_reg intMathLib[] =
{
    {"mod",    intMod   },
    {"bAnd",   intAnd   },
    {"bOr",    intOr    },
    {"bXor",   intXor   },
    {"bNot",   intBitNot},
    {"shiftl", intShiftl},
    {"shiftr", intShiftr},
	{"min",    math_min },
	{"max",    math_max },
};

/*
 * Execute a piece of lua code, passing global variables and returning values.
 *
 * The "format" string should consist of zero or more format codes for
 * values to pass to the script, optionally followed by a ':' and zero or more
 * format codes for values to pass and return. Format codes are:
 *
 * i - an integer
 * b - a boolean
 * s - a string [cptr]
 * p - a pointer to a user-defined type
 *
 * For 'i', 'b', and 's', the vararg list should include a cptr giving the name
 * of the variable to pass it in, followed by a value of the correct type (if
 * before the ':') or a pointer to a variable of the correct type (if after).
 * The macros LUA_VAR(x) and LUA_RETURN(x) pass a variable in the appropriate
 * form for before or after the ':' respectively.
 *
 * For 'p', it should include a cptr with the variable name, a cptr with the
 * name of the type, and the pointer itself, in order.
 *
 * If the script returns values, they are returned in the return variables,
 * in the order they are declared in the format. If fewer values are returned
 * than there are returned variables, the remaining variables recieve the
 * values they have at the end of the lua code - generally the same as was
 * passed in, unless the lua code changed it. Any excess return values are
 * ignored.
 *
 * The value returned for string arguments is always made with string_make() 
 * and should be freed with string_free() when no longer needed. 
 */
static bool call_lua_hook(cptr script, cptr format, va_list vp)
{
	int i, status;
	cptr vars[20];
	void *out[20];
	int first_return = 0;

	int oldtop = lua_gettop(L);

	bool success;

	for (i = 0; format[i] && i < 20; i++)
	{
		cptr type, var;
		
		switch (format[i])
		{
		case 'i':
			/* Get the next argument */
			var = va_arg(vp, cptr);
			vars[i] = var;
	
			lua_pushnumber(L, va_arg(vp, int));
			lua_setglobal(L, var);
			break;

		case 'b':
			/* Get the next argument */
			var = va_arg(vp, cptr);
			vars[i] = var;
	
			tolua_pushbool(L, va_arg(vp, int));
			lua_setglobal(L, var);
			break;

		case 's':
			/* Get the next argument */
			var = va_arg(vp, cptr);
			vars[i] = var;
	
			tolua_pushstring(L, va_arg(vp, char *));
			lua_setglobal(L, var);
			break;

		case 'p':
			/* Get the next argument */
			var = va_arg(vp, cptr);
			vars[i] = var;

			/* Get the type */
			type = va_arg(vp, cptr);

			tolua_pushusertype(L, va_arg(vp, void *), tolua_tag(L, type));
			lua_setglobal(L, var);
			break;
			
		case ':':
			/* Start of return arguments */
			break;
		}

		if (format[i] == ':')
		{
			i++;
			break;
		}
	}

	/* Save the first return argument (or the '\0' if none) */
	first_return = i;

	for (i = first_return; format[i] && i < 20; i++)
	{
		cptr var;
		int *ival;
		bool *bval;
		cptr *sval;
		
		switch (format[i])
		{
		case 'i':
			/* Get the next argument */
			var = va_arg(vp, cptr);
			vars[i] = var;

			ival = va_arg(vp, int *);
			out[i] = (void *)ival;
	
			lua_pushnumber(L, *ival);
			lua_setglobal(L, var);
			break;

		case 'b':
			/* Get the next argument */
			var = va_arg(vp, cptr);
			vars[i] = var;

			bval = va_arg(vp, bool *);
			out[i] = (void *)bval;
	
			tolua_pushbool(L, *bval);
			lua_setglobal(L, var);
			break;

		case 's':
			/* Get the next argument */
			var = va_arg(vp, cptr);
			vars[i] = var;

			sval = va_arg(vp, cptr *);
			out[i] = (void *)sval;
	
			lua_pushstring(L, *sval);
			lua_setglobal(L, var);
			break;

		default:
			vars[i] = NULL;
			out[i] = NULL;
			break;
		}
	}
			
	status = lua_dostring(L, script);

	if (status == 0)
	{
		int *ival;
		bool *bval;
		cptr *sval;

		int n = 1;
		int top = lua_gettop(L);
		
		for (i = first_return; format[i] && i < 20; i++)
		{
			int where;
			
			if (top < oldtop + n)
			{
				lua_getglobal(L, vars[i]);
				where = -1;
			}
			else
				where = oldtop + n;
			
			switch (format[i])
			{
			case 'i':
				ival = (int *)out[i];
				*ival = tolua_getnumber(L, where, 0);
				n++;
				break;

			case 'b':
				bval = (bool *)out[i];
				*bval = tolua_getbool(L, where, FALSE);
				n++;
				break;

			case 's':
				sval = (cptr *)out[i];
				*sval = string_make(tolua_getstring(L, where, ""));
				n++;
				break;
			}
		}

		lua_settop(L, oldtop);

		/* Worked */
		success = TRUE;
	}
	else
	{
		/* We failed */
		success = FALSE;
	}
	
	/* Clear variables */
	for (i = 0; format[i] && i < 20; i++)
	{
		switch (format[i])
		{
		case 'i':
		case 'b':
		case 's':
		case 'p':
			lua_pushnil(L);
			lua_setglobal(L, vars[i]);
			break;
		}
	}
	
	/* Done */
	return (success);
}

/*
 * Apply an object trigger, a small lua script which can be attached to an
 * object type or a specific item (usually an ego-item or artifact).
 *
 * Currently defined triggers, and their normal arguments, include:
 *
 * TRIGGER_USE - for activating a wearable item or using any other item.
 * Wearable items neither take nor return values. Other items may or may not
 * have a 'dir' value, depending on type, and may return 'result' and 'ident' 
 * which indicate if the action used a charge and if it should identify the
 * object, respectively.
 *
 * TRIGGER_MAKE - called once near the end of object generation. Takes one
 * argument, 'lev', which is the level the object is being generated at for
 * non-artifacts and the level of the artifact for artifacts.
 *
 * TRIGGER_BONUS - called on worn items during calc_bonuses(). No arguments.
 *
 * TRIGGER_SMASH - called for potions when they break.
 *
 * TRIGGER_DESC - called to get an activation/use description for an item.
 * Returns a string describing the activation or use.
 */
void apply_object_trigger(int trigger_id, object_type *o_ptr, cptr format, ...)
{
	va_list vp;
	
	object_kind *k_ptr = &k_info[o_ptr->k_idx];

	cptr script = NULL;
	
	void *q_ptr = NULL;
	
	bool success;
	
	if (o_ptr->trigger[trigger_id])
		script = quark_str(o_ptr->trigger[trigger_id]);
	else if (k_ptr->trigger[trigger_id])
		script = k_text + k_ptr->trigger[trigger_id];
	else
		return;
	
	/* Save parameter so recursion works. */
	lua_getglobal (L, "object");
	if (tolua_istype(L, -1, tolua_tag(L, "object_type"), 0))
	{
		q_ptr = tolua_getuserdata(L, -1, NULL);
	}
	lua_pop(L,1);

	/* Set parameters (really global) */
	tolua_pushusertype(L, (void*)o_ptr, tolua_tag(L, "object_type"));
	lua_setglobal(L, "object");
	
	/* Begin the Varargs Stuff */
	va_start(vp, format);
	
	success = call_lua_hook(script, format, vp);

	/* End the Varargs Stuff */
	va_end(vp);
	
	/* Restore global so recursion works*/
	tolua_pushusertype(L, q_ptr, tolua_tag(L,"object_type"));
	lua_setglobal(L, "object");
	
	/* Paranoia */
	if (!success)
	{
		msgf("Script for object: %v failed.", OBJECT_STORE_FMT(o_ptr, FALSE, 3));
	}
}

/*
 * Callback for using an object
 *
 * If the object is a scroll of identify this function may have a side effect.
 * The side effect is that the o_ptr no longer points at the scroll of identity
 * because of the sorting done by the identify_spell.
 * This side effect is countered in do_cmd_read_scroll_aux, the only place where
 * it matters.
 */
bool use_object(object_type *o_ptr, bool *id_return, int dir)
{
	bool result = TRUE, ident = *id_return;

	apply_object_trigger(TRIGGER_USE, o_ptr, "i:bb",
			LUA_VAR(dir), LUA_RETURN(result), LUA_RETURN(ident));

	*id_return = ident;
	return result;
}

static bool field_delete = FALSE;

/*
 * Delete current field when finish processing.
 *
 * This is a horrible name...
 */
void deleteme(void)
{
	field_delete = TRUE;
}

/*
 * Apply an field trigger, a small lua script which does
 * what the old field action functions did.
 */
bool apply_field_trigger(cptr script, field_type *f_ptr, cptr format, va_list vp)
{
	field_thaum *t_ptr = &t_info[f_ptr->t_idx];

	void *q_ptr = NULL;
	
	bool success;
	bool delete_save, delete;
		
	/* Save parameter so recursion works. */
	lua_getglobal (L, "field");
	if (tolua_istype(L, -1, tolua_tag(L, "field_type"), 0))
	{
		q_ptr = tolua_getuserdata(L, -1, NULL);
	}
	lua_pop(L,1);
	delete_save = field_delete;
	
	/* Default to no deletion */
	field_delete = FALSE;

	/* Set parameters (really global) */
	tolua_pushusertype(L, (void*)f_ptr, tolua_tag(L, "field_type"));
	lua_setglobal(L, "field");
	
	success = call_lua_hook(script, format, vp);
	
	/* Restore global so recursion works*/
	tolua_pushusertype(L, q_ptr, tolua_tag(L,"field_type"));
	lua_setglobal(L, "field");
	delete = field_delete;
	field_delete = delete_save;
	
	/* Paranoia */
	if (!success)
	{
		msgf("Script for field: %s failed.", t_ptr->name);
	}
	
	/* Does the field want to be deleted? */
	return (delete);
}


/*
 * Apply an field trigger, a small lua script which does
 * what the old field action functions did.
 *
 * This version doesn't modify the field, but uses a copy instead.
 * This allows const versions of field hooks.
 *
 * The field cannot be deleted.
 */
void const_field_trigger(cptr script, const field_type *f_ptr, cptr format, va_list vp)
{
	/* Structure copy to get local working version */
	field_type temp_field = *f_ptr;
	
	(void) apply_field_trigger(script, &temp_field, format, vp);
}

static void line_hook(lua_State *L, lua_Debug *ar)
{
	int j;

	/* Scan windows */
	for (j = 0; j < ANGBAND_TERM_MAX; j++)
	{
		term *old = Term;

		/* No window */
		if (!angband_term[j]) continue;

		/* No relevant flags */
		if (window_flag[j] & PW_SCRIPT_SOURCE)
		{
			/* Activate */
			Term_activate(angband_term[j]);

			lua_getstack(L, 0, ar);
			lua_getinfo(L, "S", ar);
			show_file(ar->source + 1, ar->short_src, ar->currentline - 1, 1);

			/* Fresh */
			Term_fresh();

			/* Restore */
			Term_activate(old);
		}
		else if (window_flag[j] & PW_SCRIPT_VARS)
		{
			char buf[1024];

			/* Activate */
			Term_activate(angband_term[j]);

			path_make(buf, ANGBAND_DIR_SCRIPT, "trace.lua");

			/* Execute the file */
			script_do_file(buf);

			/* Fresh */
			Term_fresh();

			/* Restore */
			Term_activate(old);
		}
	}
}


static void script_trace_start(void)
{
	if (!L) return;

	lua_setlinehook(L, line_hook);
}


static void script_trace_stop(void)
{
	if (!L) return;

	lua_setlinehook(L, NULL);
}


void do_cmd_script(void)
{
	int ch;
	char tmp[80];


	/* Save screen */
	screen_save();

	/* Clear screen */
	Term_clear();

	/* Ask for a choice */
	prtf(0, 2, "Debug scripts");

	/* Give some choices */
	prtf(5, 4, "(1) Execute a script file");
	prtf(5, 5, "(2) Execute a script command");
	prtf(5, 6, "(3) Start tracing scripts");
	prtf(5, 7, "(4) Stop tracing scripts");
	prtf(5, 8, "(5) Re-initialize scripts");

	/* Prompt */
	prtf(0, 15, "Command: ");

	/* Prompt */
	ch = inkey();

	/* Load screen */
	screen_load();

	switch (ch)
	{
		case '1':
		{
			char buf[1024];

			/* Prompt */
			prtf(0, 0, "Lua script: ");

			/* Default filename */
			sprintf(tmp, "test.lua");

			/* Ask for a file */
			if (!askfor_aux(tmp, 80)) break;

			/* Clear the prompt */
			clear_msg();

			path_make(buf, ANGBAND_DIR_SCRIPT, tmp);

			/* Execute the file */
			script_do_file(buf);

			break;
		}
		case '2':
		{
			/* Prompt */
			prtf(0, 0, "Lua command: ");

			/* Empty default */
			tmp[0] = 0;

			/* Ask for a command */
			if (!askfor_aux(tmp, 80)) break;

			/* Clear the prompt */
			clear_msg();

			/* Execute the command */
			script_do_string(tmp);

			break;
		}
		case '3':
		{
			script_trace_start();

			break;
		}
		case '4':
		{
			script_trace_stop();

			break;
		}
		case '5':
		{
			char buf[1024];

			/* Initialization code */
			path_make(buf, ANGBAND_DIR_SCRIPT, "init.lua");
			script_do_file(buf);

			break;
		}
	}
}


extern int tolua_player_open(lua_State* tolua_S);
extern void tolua_player_close(lua_State* tolua_S);
extern int tolua_object_open(lua_State* tolua_S);
extern void tolua_object_close(lua_State* tolua_S);
extern int tolua_monst_open(lua_State* tolua_S);
extern void tolua_monst_close(lua_State* tolua_S);
extern int tolua_random_open(lua_State* tolua_S);
extern void tolua_random_close(lua_State* tolua_S);
extern int tolua_ui_open(lua_State* tolua_S);
extern void tolua_ui_close(lua_State* tolua_S);
extern int tolua_misc_open(lua_State* tolua_S);
extern void tolua_misc_close(lua_State* tolua_S);
extern int tolua_spell_open(lua_State* tolua_S);
extern void tolua_spell_close(lua_State* tolua_S);
extern int tolua_field_open(lua_State* tolua_S);
extern void tolua_field_close(lua_State* tolua_S);


/*
 * Initialize scripting support
 */
errr script_init(void)
{
	char buf[1024];

	/* Start the interpreter with default stack size */
	L = lua_open(0);

	/* Register the Lua base libraries */
	lua_baselibopen(L);
	lua_strlibopen(L);
	lua_dblibopen(L);

	/* Register library with binary functions */
	luaL_openl(L, intMathLib);

	/* Register the Angband base library */
	luaL_openl(L, anglib);

	/* Register various Angband libraries */
	tolua_player_open(L);
	tolua_object_open(L);
	tolua_monst_open(L);
	tolua_random_open(L);
	tolua_ui_open(L);
	tolua_misc_open(L);
	tolua_spell_open(L);
	tolua_field_open(L);

	/* Initialization code */
	path_make(buf, ANGBAND_DIR_SCRIPT, "init.lua");
	script_do_file(buf);

	return 0;
}


errr script_free(void)
{
	if (L)
	{
		lua_close(L);
		return 0;
	}
	else
	{
		/* Error */
		return -1;
	}
}


bool script_do_string(cptr script)
{
	if (!L) return FALSE;

	if (!lua_dostring(L, script)) return TRUE;

	return FALSE;
}


bool script_do_file(cptr filename)
{
	if (!L) return FALSE;
#ifdef RISCOS
	{
		char *realname = riscosify_name(filename);
		if (!lua_dofile(L, realname)) return TRUE;
	}
#else /* RISCOS */
	if (!lua_dofile(L, filename)) return TRUE;
#endif /* RISCOS */

	return FALSE;
}

/*
 * Does the player have a certain resistance?
 */
bool player_res(u32b flag)
{
	return ((p_ptr->flags[1] & flag) ? TRUE : FALSE);
}


/*
 * Debug lua stack overflow
 */
#include "lua/lstate.h"
void debug_lua_stack(void)
{
	/* Need interpreter */
	if (!L) return;
	
	msgf("Current stack depth: %d", L->stack_last - L->top);
}
