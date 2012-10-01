/* File: script.c */

#include "angband.h"


/*
 * Lua state
 */
static lua_State* L = NULL;


static int xxx_build_script_path(lua_State *L)
{
	char buf[1024];
	cptr filename;

	filename = luaL_checkstring(L, 1);

	path_build(buf, sizeof(buf), ANGBAND_DIR_SCRIPT, filename);

	tolua_pushstring(L, buf);

	return 1;
}

static const struct luaL_reg anglib[] =
{
	{"build_script_path", xxx_build_script_path},
	{NULL, NULL},
};


#define luaL_check_bit(L, n)  ((long)luaL_checknumber(L, n))
#define luaL_check_ubit(L, n) ((unsigned long)luaL_check_bit(L, n))

static int int_not(lua_State* L)
{
	lua_pushnumber(L, ~luaL_check_bit(L, 1));
	return 1;
}

static int int_mod(lua_State* L)
{
	lua_pushnumber(L, luaL_check_bit(L, 1) % luaL_check_bit(L, 2));
	return 1;
}

static int int_or(lua_State *L)
{
	int n = lua_gettop(L), i;
	long w = luaL_check_bit(L, 1);
	for (i = 2; i <= n; i++)
		w |= luaL_check_bit(L, i);
	lua_pushnumber(L, w);
	return 1;
}

static int int_xor(lua_State *L)
{
	int n = lua_gettop(L), i;
	long w = luaL_check_bit(L, 1);
	for (i = 2; i <= n; i++)
		w ^= luaL_check_bit(L, i);
	lua_pushnumber(L, w);
	return 1;
}

static int int_and(lua_State *L)
{
	int n = lua_gettop(L), i;
	long w = luaL_check_bit(L, 1);
	for (i = 2; i <= n; i++)
		w &= luaL_check_bit(L, i);
	lua_pushnumber(L, w);
	return 1;
}

static int int_lshift(lua_State* L)
{
	lua_pushnumber(L, luaL_check_bit(L, 1) << luaL_check_ubit(L, 2));
	return 1;
}

static int int_rshift(lua_State* L)
{
	lua_pushnumber(L, luaL_check_ubit(L, 1) >> luaL_check_ubit(L, 2));
	return 1;
}

static int int_arshift(lua_State* L)
{
	lua_pushnumber(L, luaL_check_bit(L, 1) >> luaL_check_ubit(L, 2));
	return 1;
}

static const struct luaL_reg bitlib[] =
{
	{"bNot",    int_not},
	{"iMod",    int_mod},  /* "mod" already in Lua math library */
	{"bAnd",    int_and},
	{"bOr",     int_or},
	{"bXor",    int_xor},
	{"lshift",  int_lshift},
	{"rshift",  int_rshift},
	{"arshift", int_arshift},
	{NULL,      NULL}
};


/*
 * Call a Lua event handler
 *
 * Calls a Lua event handler with the name 'hook',
 * with arguments defined by the format string 'fmt',
 * and return values as defined by the 'ret' format string.
 * The next arguments to call_lua() are the arguments to
 * be passed to the Lua event handler, followed by pointers
 * to the expected return values.
 *
 * ToDo: explain the format strings ...
 *
 * The return value indicates if the hook was called
 * successfully (TRUE) or if there was an error (FALSE).
 *
 * Note that string and object_type* return values have to be
 * copied to a save place before the next call to Lua since
 * they will be lost when garbage collection takes place.
 * So store the values with string_make() or object_copy() into
 * a save location if you need them afterwards.
 */
bool call_lua(cptr hook, cptr fmt, cptr ret, ...)
{
	va_list ap;
	int i = 0;
	int num_args = 0;
	int num_res;

	va_start(ap, ret);

	/* Select the global event handler */
	lua_getglobal(L, "notify_event_hook");

	/* Push the event name */
	lua_pushstring(L, hook);

	/* Push and count the arguments */
	while (fmt[i])
	{
		switch (fmt[i++])
		{
			case 'd':
				tolua_pushnumber(L, va_arg(ap, int));
				num_args++;
				break;
			case 'l':
				tolua_pushnumber(L, va_arg(ap, long));
				num_args++;
				break;
			case 'b':
				tolua_pushboolean(L, va_arg(ap, int));
				num_args++;
				break;
			case 's':
				tolua_pushstring(L, va_arg(ap, cptr));
				num_args++;
				break;
			case 'O':
				tolua_pushusertype(L, va_arg(ap, object_type*), "object_type");
				num_args++;
				break;
			case 'M':
				tolua_pushusertype(L, va_arg(ap, monster_type*), "monster_type");
				num_args++;
				break;
			case '(':
			case ')':
			case ',':
				break;
		}
	}

	/* HACK - Count the return values */
	num_res = strlen(ret);

	lua_pushliteral(L, "_TRACEBACK");
	lua_rawget(L, LUA_GLOBALSINDEX);  /* get traceback function */
	lua_insert(L, 1);  /* put it under chunk and args */

	/* Call the function */
	if (lua_pcall(L, num_args + 1, num_res, 1))
	{
		/* An error occured */
		va_end(ap);
		return FALSE;
	}

	/* Get the return values */
	for (i = 0; i < num_res; i++)
	{
		switch (ret[i])
		{
			case 'd':
			{
				int *tmp = va_arg(ap, int*);

				if (lua_isnumber(L, -num_res + i))
					*tmp = tolua_tonumber(L, -num_res + i, 0);
				break;
			}
			case 'l':
			{
				long *tmp = va_arg(ap, long*);

				if (lua_isnumber(L, -num_res + i))
					*tmp = tolua_tonumber(L, -num_res + i, 0);
				break;
			}
			case 'b':
			{
				bool *tmp = va_arg(ap, bool*);
				*tmp = tolua_toboolean(L, -num_res + i, FALSE);
				break;
			}
			case 's':
			{
				cptr *tmp = va_arg(ap, cptr*);
				if (lua_isstring(L, -num_res + i))
					*tmp = tolua_tostring(L, -num_res + i, "");
				break;
			}
			case 'O':
			{
				tolua_Error tolua_err;
				object_type **tmp = va_arg(ap, object_type**);
				if (tolua_isusertype(L, -num_res + i, "object_type", 0, &tolua_err))
					*tmp = tolua_touserdata(L, -num_res + i, NULL);
				break;
			}
			case 'M':
			{
				tolua_Error tolua_err;
				object_type **tmp = va_arg(ap, monster_type**);
				if (tolua_isusertype(L, -num_res + i, "monster_type", 0, &tolua_err))
					*tmp = tolua_touserdata(L, -num_res + i, NULL);
				break;
			}
		}
	}

	/* Remove the results and the error handler */
	lua_pop(L, num_res + 1);

	va_end(ap);

	/* Success */
	return TRUE;
}


/*
 * Callback for using an object
 */
/*bool use_object(object_type *o_ptr, bool *ident)
{
	bool used_up = FALSE;

	if (!call_lua("use_object", "(O)", "bb",
	               o_ptr,
	               ident, &used_up))
	{
		*ident = FALSE;
		used_up = FALSE;
	}

	return (used_up);
}*/


/*void player_turn_hook(void)
{
	call_lua("player_turn", "", "");
}*/


/*static void line_hook(lua_State *L, lua_Debug *ar)
{
	int j;

	for (j = 0; j < 8; j++)
	{
		term *old = Term;

		if (!angband_term[j]) continue;

		if (op_ptr->window_flag[j] & PW_SCRIPT_SOURCE)
		{
			Term_activate(angband_term[j]);

			lua_getstack(L, 0, ar);
			lua_getinfo(L, "S", ar);
			show_file(ar->source + 1, ar->short_src, ar->currentline - 1, 1);

			Term_fresh();

			Term_activate(old);
		}
		else if (op_ptr->window_flag[j] & PW_SCRIPT_VARS)
		{
			char buf[1024];

			Term_activate(angband_term[j]);

			path_build(buf, sizeof(buf), ANGBAND_DIR_SCRIPT, "trace.lua");

			script_do_file(buf);

			Term_fresh();

			Term_activate(old);
		}
	}
}*/


/*static void script_trace_start(void)
{
	if (!L) return;

	lua_sethook(L, line_hook, LUA_MASKLINE, 0);
}


static void script_trace_stop(void)
{
	if (!L) return;

	lua_sethook(L, line_hook, 0, 0);
}*/


void do_cmd_script(void)
{
	int ch;
	char tmp[80];


	/* Save screen */
	screen_save();

	/* Clear screen */
	Term_clear();

	/* Ask for a choice */
	prt("Debug scripts", 2, 0);

	/* Give some choices */
	prt("(1) Execute a script file", 4, 5);
	prt("(2) Execute a script command", 5, 5);
	/*prt("(3) Start tracing scripts", 6, 5);
	prt("(4) Stop tracing scripts", 7, 5);*/
	prt("(5) Re-initialize scripts", 8, 5);

	/* Prompt */
	prt("Command: ", 15, 0);

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
			prt("Lua script: ", 0, 0);

			/* Default filename */
			sprintf(tmp, "test.lua");

			/* Ask for a file */
			if (!askfor_aux(tmp, sizeof(tmp))) break;

			/* Clear the prompt */
			prt("", 0, 0);

			path_build(buf, sizeof(buf), ANGBAND_DIR_SCRIPT, tmp);

			/* Execute the file */
			script_do_file(buf);

			break;
		}
		case '2':
		{
			/* Prompt */
			prt("Lua command: ", 0, 0);

			/* Empty default */
			strcpy(tmp, "");

			/* Ask for a command */
			if (!askfor_aux(tmp, sizeof(tmp))) break;

			/* Clear the prompt */
			prt("", 0, 0);

			/* Execute the command */
			script_do_string(tmp);

			break;
		}
		/*case '3':
		{
			script_trace_start();

			break;
		}
		case '4':
		{
			script_trace_stop();

			break;
		}*/
		case '5':
		{
			char buf[1024];

			/* Initialization code */
			path_build(buf, sizeof(buf), ANGBAND_DIR_SCRIPT, "init.lua");
			script_do_file(buf);

			break;
		}
	}
}

extern int tolua_init_open(lua_State* tolua_S);
extern void tolua_init_close(lua_State* tolua_S);
extern int tolua_types_open(lua_State* tolua_S);
extern void tolua_types_close(lua_State* tolua_S);


/*
 * Initialize scripting support
 */
errr script_init(void)
{
	char buf[1024];

	/* Start the interpreter with default stack size */
	L = lua_open();

	/* Register the Lua base libraries */
	luaopen_base(L);
	luaopen_string(L);
	luaopen_debug(L);
	luaopen_table(L);

	/* Register library with binary functions */
	luaL_openlib(L, "bitlib", bitlib, 0);

	/* Register the Angband base library */
	luaL_openlib(L, "angband", anglib, 0);

	/* Register various Angband libraries */
	tolua_init_open(L);
	tolua_types_open(L);

	/* Initialization code */
	path_build(buf, sizeof(buf), ANGBAND_DIR_SCRIPT, "init.lua");
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

cave_type *lua_cave(int y, int x)
{
	return (&(cave[y][x]));
}

monster_type *lua_monster(int m_idx)
{
	return (&m_list[m_idx]);
}

monster_race *lua_r_info(int r_idx)
{
	return (&(r_info[r_idx]));
}

object_type *lua_inven(int slot)
{
	return (&(inventory[slot]));
}

object_kind *lua_kind(object_type *o_ptr)
{
	return (&(k_info[o_ptr->k_idx]));
}

object_type *lua_object(int oidx)
{
	return (&(o_list[oidx]));
}

object_kind *lua_kind_index(int k_idx)
{
	return (&(k_info[k_idx]));
}

dungeon_info_type *lua_dungeon(int which)
{
	return (&(d_info[which]));
}

bool get_monster_flag1(int r_idx, u32b flag)
{
	monster_race *r_ptr = &r_info[r_idx];
	if (r_ptr->flags1 & (flag)) return (TRUE);
	return (FALSE);
}

bool get_monster_flag2(int r_idx, u32b flag)
{
	monster_race *r_ptr = &r_info[r_idx];
	if (r_ptr->flags2 & (flag)) return (TRUE);
	return (FALSE);
}

bool get_monster_flag3(int r_idx, u32b flag)
{
	monster_race *r_ptr = &r_info[r_idx];
	if (r_ptr->flags3 & (flag)) return (TRUE);
	return (FALSE);
}

bool get_monster_flag4(int r_idx, u32b flag)
{
	monster_race *r_ptr = &r_info[r_idx];
	if (r_ptr->flags4 & (flag)) return (TRUE);
	return (FALSE);
}

bool get_monster_flag5(int r_idx, u32b flag)
{
	monster_race *r_ptr = &r_info[r_idx];
	if (r_ptr->flags5 & (flag)) return (TRUE);
	return (FALSE);
}

bool get_monster_flag6(int r_idx, u32b flag)
{
	monster_race *r_ptr = &r_info[r_idx];
	if (r_ptr->flags6 & (flag)) return (TRUE);
	return (FALSE);
}

bool get_monster_flag7(int r_idx, u32b flag)
{
	monster_race *r_ptr = &r_info[r_idx];
	if (r_ptr->flags7 & (flag)) return (TRUE);
	return (FALSE);
}

bool get_monster_flag8(int r_idx, u32b flag)
{
	monster_race *r_ptr = &r_info[r_idx];
	if (r_ptr->flags8 & (flag)) return (TRUE);
	return (FALSE);
}

bool get_monster_flag9(int r_idx, u32b flag)
{
	monster_race *r_ptr = &r_info[r_idx];
	if (r_ptr->flags9 & (flag)) return (TRUE);
	return (FALSE);
}

bool get_object_flag1(object_type *o_ptr, u32b flag)
{
	u32b f1, f2, f3, f4;
	object_flags(o_ptr, &f1, &f2, &f3, &f4);
	if (f1 & (flag)) return (TRUE);
	return (FALSE);
}

bool get_object_flag2(object_type *o_ptr, u32b flag)
{
	u32b f1, f2, f3, f4;
	object_flags(o_ptr, &f1, &f2, &f3, &f4);
	if (f2 & (flag)) return (TRUE);
	return (FALSE);
}

bool get_object_flag3(object_type *o_ptr, u32b flag)
{
	u32b f1, f2, f3, f4;
	object_flags(o_ptr, &f1, &f2, &f3, &f4);
	if (f3 & (flag)) return (TRUE);
	return (FALSE);
}

bool get_object_flag4(object_type *o_ptr, u32b flag)
{
	u32b f1, f2, f3, f4;
	object_flags(o_ptr, &f1, &f2, &f3, &f4);
	if (f4 & (flag)) return (TRUE);
	return (FALSE);
}

bool get_monster_ability(monster_type *m_ptr, u32b flag)
{
	if (m_ptr->abilities & (flag)) return (TRUE);
	return (FALSE);
}

char *get_monster_desc(monster_type *m_ptr, int mode)
{
	cptr            res;
	monster_race    *r_ptr = &r_info[m_ptr->r_idx];
	cptr            name = (r_name + r_ptr->name);
	char            silly_name[80];
	bool            seen, pron;
	char		*desc;

	/* Are we hallucinating? (Idea from Nethack...) */
	if (p_ptr->image)
	{
		if (randint(2) == 1)
		{
			monster_race *hallu_race;

			do
			{
				hallu_race = &r_info[randint(max_r_idx-2)];
			}
			while (hallu_race->flags1 & RF1_UNIQUE);

			strcpy(silly_name, (r_name + hallu_race->name));
		}
		else
		{
			get_rnd_line("silly.txt", silly_name);
		}

		/* Better not strcpy it, or we could corrupt r_info... */
		name = silly_name;
	}

	/* Can we "see" it (exists + forced, or visible + not unforced) */
	seen = (m_ptr && ((mode & 0x80) || (!(mode & 0x40) && m_ptr->ml)));

	/* Sexed Pronouns (seen and allowed, or unseen and allowed) */
	pron = (m_ptr && ((seen && (mode & 0x20)) || (!seen && (mode & 0x10))));

	/* First, try using pronouns, or describing hidden monsters */
	if (!seen || pron)
	{
		/* an encoding of the monster "sex" */
		int kind = 0x00;

		/* Extract the gender (if applicable) */
		if (r_ptr->flags1 & (RF1_FEMALE)) kind = 0x20;
		else if (r_ptr->flags1 & (RF1_MALE)) kind = 0x10;

		/* Ignore the gender (if desired) */
		if (!m_ptr || !pron) kind = 0x00;


		/* Assume simple result */
		res = "it";

		/* Brute force: split on the possibilities */
		switch (kind + (mode & 0x07))
		{
			/* Neuter, or unknown */
			case 0x00: res = "it"; break;
			case 0x01: res = "it"; break;
			case 0x02: res = "its"; break;
			case 0x03: res = "itself"; break;
			case 0x04: res = "something"; break;
			case 0x05: res = "something"; break;
			case 0x06: res = "something's"; break;
			case 0x07: res = "itself"; break;

			/* Male (assume human if vague) */
			case 0x10: res = "he"; break;
			case 0x11: res = "him"; break;
			case 0x12: res = "his"; break;
			case 0x13: res = "himself"; break;
			case 0x14: res = "someone"; break;
			case 0x15: res = "someone"; break;
			case 0x16: res = "someone's"; break;
			case 0x17: res = "himself"; break;

			/* Female (assume human if vague) */
			case 0x20: res = "she"; break;
			case 0x21: res = "her"; break;
			case 0x22: res = "her"; break;
			case 0x23: res = "herself"; break;
			case 0x24: res = "someone"; break;
			case 0x25: res = "someone"; break;
			case 0x26: res = "someone's"; break;
			case 0x27: res = "herself"; break;
		}

		/* Copy the result */
		(void)strcpy(desc, res);
	}

	/* Handle visible monsters, "reflexive" request */
	else if ((mode & 0x02) && (mode & 0x01))
	{
		/* The monster is visible, so use its gender */
		if (r_ptr->flags1 & (RF1_FEMALE)) strcpy(desc, "herself");
		else if (r_ptr->flags1 & (RF1_MALE)) strcpy(desc, "himself");
		else strcpy(desc, "itself");
	}


	/* Handle all other visible monster requests */
	else
	{
		/* It could be a Unique */
		if ((r_ptr->flags1 & (RF1_UNIQUE)) && !(p_ptr->image))
		{
			/* Start with the name (thus nominative and objective) */
			(void)strcpy(desc, name);
		}

		/* It could be an indefinite monster */
		else if (mode & 0x08)
		{
			/* XXX Check plurality for "some" */

			/* Indefinite monsters need an indefinite article */
			(void)strcpy(desc, is_a_vowel(name[0]) ? "an " : "a ");
			(void)strcat(desc, name);
		}

		/* It could be a normal, definite, monster */
		else
		{
			/* Definite monsters need a definite article */
			if (is_pet(m_ptr))
                                (void)strcpy(desc, "Your ");
			else
				(void)strcpy(desc, "the ");

			(void)strcat(desc, name);
		}

		/* Handle the Possessive as a special afterthought */
		if (mode & 0x02)
		{
			/* XXX Check for trailing "s" */

			/* Simply append "apostrophe" and "s" */
			(void)strcat(desc, "'s");
		}
	}

	return (desc);
}

void lua_update_monsters()
{
	p_ptr->update |= (PU_MONSTERS);
}

void lua_update_stuff()
{
        p_ptr->update |= (PU_BONUS);
        p_ptr->update |= (PU_TORCH);
        p_ptr->update |= (PU_HP);
        p_ptr->update |= (PU_MANA);
        p_ptr->update |= (PU_SPELLS);
        p_ptr->update |= (PU_VIEW);
        p_ptr->update |= (PU_LITE);
        p_ptr->update |= (PU_FLOW);
        p_ptr->update |= (PU_BODY);
        p_ptr->redraw |= (PR_MANA);
        p_ptr->redraw |= (PR_HP);
        p_ptr->redraw |= (PR_GOLD);
        p_ptr->redraw |= (PR_STATS);
        p_ptr->redraw |= (PR_BASIC);
        p_ptr->redraw |= (PR_EXTRA);
	p_ptr->redraw |= (PR_SPEED);
	p_ptr->redraw |= (PR_ARMOR);
	p_ptr->window |= (PW_PLAYER);
}

/* Direction functions for lua. */
int lua_get_aim_dir()
{
	int dir = 0;
	get_aim_dir(&dir);
	return (dir);
}

int lua_get_rep_dir()
{
	int dir = 0;
	get_rep_dir(&dir);
	return (dir);
}

/* Other functions */
bool lua_cave_empty_bold(int y, int x)
{
	if (cave_empty_bold(y, x)) return (TRUE);
	return (FALSE);
}

bool get_cave_info_flag(int y, int x, u32b flag)
{
	if (cave[y][x].info & (flag)) return (TRUE);
	return (FALSE);
}

bool lua_tgt_pt()
{
	int x, y;
	if (!tgt_pt(&x, &y)) return (FALSE);
	
	global_y = y;
	global_x = x;
	return (TRUE);
}

bool lua_in_bounds(int y, int x)
{
	if (in_bounds(y, x)) return (TRUE);
	return (FALSE);
}

bool lua_in_bounds2(int y, int x)
{
	if (in_bounds2(y, x)) return (TRUE);
	return (FALSE);
}

bool lua_player_has_los_bold(int y, int x)
{
	if (lua_player_has_los_bold(y, x)) return (TRUE);
	return (FALSE);
}

void lua_project(int who, int rad, int y, int x, s32b dam, int typ, int mode)
{
	int flg;
	if (mode == 1) flg = (PROJECT_STOP | PROJECT_KILL);
	else if (mode == 2) flg = (PROJECT_JUMP | PROJECT_GRID | PROJECT_KILL);
	project(who, rad, y, x, dam, typ, flg);
}

void memorize_race_flag1(int r_idx, u32b flag)
{
	monster_race *r_ptr = &r_info[r_idx];
	r_ptr->r_flags1 |= flag;
}


void memorize_race_flag2(int r_idx, u32b flag)
{
	monster_race *r_ptr = &r_info[r_idx];
	r_ptr->r_flags2 |= flag;
}


void memorize_race_flag3(int r_idx, u32b flag)
{
	monster_race *r_ptr = &r_info[r_idx];
	r_ptr->r_flags3 |= flag;
}

void memorize_race_flag4(int r_idx, u32b flag)
{
	monster_race *r_ptr = &r_info[r_idx];
	r_ptr->r_flags4 |= flag;
}

void memorize_race_flag5(int r_idx, u32b flag)
{
	monster_race *r_ptr = &r_info[r_idx];
	r_ptr->r_flags5 |= flag;
}

void memorize_race_flag6(int r_idx, u32b flag)
{
	monster_race *r_ptr = &r_info[r_idx];
	r_ptr->r_flags6 |= flag;
}

void memorize_race_flag7(int r_idx, u32b flag)
{
	monster_race *r_ptr = &r_info[r_idx];
	r_ptr->r_flags7 |= flag;
}

void memorize_race_flag8(int r_idx, u32b flag)
{
	monster_race *r_ptr = &r_info[r_idx];
	r_ptr->r_flags8 |= flag;
}

void memorize_race_flag9(int r_idx, u32b flag)
{
	monster_race *r_ptr = &r_info[r_idx];
	r_ptr->r_flags9 |= flag;
}

void give_monster_ability(monster_type *m_ptr, u32b flag)
{
	m_ptr->abilities |= flag;
}

void remove_monster_ability(monster_type *m_ptr, u32b flag)
{
	m_ptr->abilities &= ~(flag);
}

void give_object_flag1(object_type *o_ptr, u32b flag)
{
	o_ptr->art_flags1 |= flag;
}

void give_object_flag2(object_type *o_ptr, u32b flag)
{
	o_ptr->art_flags2 |= flag;
}

void give_object_flag3(object_type *o_ptr, u32b flag)
{
	o_ptr->art_flags3 |= flag;
}

void give_object_flag4(object_type *o_ptr, u32b flag)
{
	o_ptr->art_flags4 |= flag;
}

void remove_object_flag1(object_type *o_ptr, u32b flag)
{
	o_ptr->art_flags1 &= flag;
}

void remove_object_flag2(object_type *o_ptr, u32b flag)
{
	o_ptr->art_flags2 &= flag;
}

void remove_object_flag3(object_type *o_ptr, u32b flag)
{
	o_ptr->art_flags3 &= flag;
}

void remove_object_flag4(object_type *o_ptr, u32b flag)
{
	o_ptr->art_flags4 &= flag;
}

bool lua_mod(int moddedint, int modint)
{
	if ((moddedint % modint) == 0) return (TRUE);

	return (FALSE);
}

bool get_player_monster_ability(u32b flag)
{
	if (p_ptr->boss_abilities & (flag)) return (TRUE);
	return (FALSE);
}

void give_monster_race_flag1(int r_idx, u32b flag)
{
	monster_race *r_ptr = &r_info[r_idx];

	r_ptr->flags1 |= flag;
}

void give_monster_race_flag2(int r_idx, u32b flag)
{
	monster_race *r_ptr = &r_info[r_idx];

	r_ptr->flags2 |= flag;
}

void give_monster_race_flag3(int r_idx, u32b flag)
{
	monster_race *r_ptr = &r_info[r_idx];

	r_ptr->flags3 |= flag;
}

void give_monster_race_flag4(int r_idx, u32b flag)
{
	monster_race *r_ptr = &r_info[r_idx];

	r_ptr->flags4 |= flag;
}

void give_monster_race_flag5(int r_idx, u32b flag)
{
	monster_race *r_ptr = &r_info[r_idx];

	r_ptr->flags5 |= flag;
}

void give_monster_race_flag6(int r_idx, u32b flag)
{
	monster_race *r_ptr = &r_info[r_idx];

	r_ptr->flags6 |= flag;
}

void give_monster_race_flag7(int r_idx, u32b flag)
{
	monster_race *r_ptr = &r_info[r_idx];

	r_ptr->flags7 |= flag;
}

void give_monster_race_flag8(int r_idx, u32b flag)
{
	monster_race *r_ptr = &r_info[r_idx];

	r_ptr->flags8 |= flag;
}

void give_monster_race_flag9(int r_idx, u32b flag)
{
	monster_race *r_ptr = &r_info[r_idx];

	r_ptr->flags9 |= flag;
}

void remove_monster_race_flag1(int r_idx, u32b flag)
{
	monster_race *r_ptr = &r_info[r_idx];

	r_ptr->flags1 &= flag;
}

void remove_monster_race_flag2(int r_idx, u32b flag)
{
	monster_race *r_ptr = &r_info[r_idx];

	r_ptr->flags2 &= flag;
}

void remove_monster_race_flag3(int r_idx, u32b flag)
{
	monster_race *r_ptr = &r_info[r_idx];

	r_ptr->flags3 &= flag;
}

void remove_monster_race_flag4(int r_idx, u32b flag)
{
	monster_race *r_ptr = &r_info[r_idx];

	r_ptr->flags4 &= flag;
}

void remove_monster_race_flag5(int r_idx, u32b flag)
{
	monster_race *r_ptr = &r_info[r_idx];

	r_ptr->flags5 &= flag;
}

void remove_monster_race_flag6(int r_idx, u32b flag)
{
	monster_race *r_ptr = &r_info[r_idx];

	r_ptr->flags6 &= flag;
}

void remove_monster_race_flag7(int r_idx, u32b flag)
{
	monster_race *r_ptr = &r_info[r_idx];

	r_ptr->flags7 &= flag;
}

void remove_monster_race_flag8(int r_idx, u32b flag)
{
	monster_race *r_ptr = &r_info[r_idx];

	r_ptr->flags8 &= flag;
}

void remove_monster_race_flag9(int r_idx, u32b flag)
{
	monster_race *r_ptr = &r_info[r_idx];

	r_ptr->flags9 &= flag;
}

void give_dungeon_flag1(int dinfo, u32b flag)
{
	dungeon_info_type *d_ptr;
	d_ptr = &d_info[dinfo];

	d_ptr->flags1 |= flag;
}

void remove_dungeon_flag1(int dinfo, u32b flag)
{
	dungeon_info_type *d_ptr;
	d_ptr = &d_info[dinfo];

	d_ptr->flags1 &= flag;
}

void lua_cave_mark(int y, int x, u32b flag)
{
	cave_type *c_ptr = &cave[y][x];

	c_ptr->info |= flag;
}

void lua_get_string(int len)
{
	char *luastring;

	sprintf(tmpluastring, "");

	if (!(askfor_aux(tmpluastring, len))) sprintf(tmpluastring, "");
}

bool get_feat_flag1(int feat, u32b flag)
{
	if (f_info[feat].flags1 & flag) return (TRUE);
	return (FALSE);
}

void give_object_ident(object_type *o_ptr, u32b flag)
{
	o_ptr->ident |= flag;
}

void lua_create_object_inven(int tval, int sval, int number)
{
	object_type	forge;
	object_type	*q_ptr;

	/* Get local object */
        q_ptr = &forge;

        object_prep(q_ptr, lookup_kind(tval, sval));
        q_ptr->number = number;
        object_aware(q_ptr);
        object_known(q_ptr);
        (void)inven_carry(q_ptr, FALSE);
}

char *lua_get_object_desc(int slot)
{
	char desc[80];
	object_type *o_ptr = &inventory[slot];
	object_desc(desc, o_ptr, TRUE, 0);

	return(desc);
}

/* The above function doesn't work, and I don't want to spend hours on this. */
void item_has_been_disabled_message(int slot)
{
	char desc[80];
	object_type *o_ptr = &inventory[slot];
	object_desc(desc, o_ptr, TRUE, 0);

	msg_format("%s has been disabled!", desc);
}

void lua_revive_in_town()
{
	int x, y;
	for (x = 0; x < wild_max_x; x++)
	{
		for (y = 0; y < wild_max_y; y++)
		{
			if (wild[x][y].town == p_ptr->town_num)
			{
				p_ptr->wild_x = x;
				p_ptr->wild_y = y;
			}
		}
	}
}

/* Check if a monster is alive or not. */
bool is_alive(monster_type *m_ptr)
{
	bool res = FALSE;
	if (m_ptr) res = TRUE;

	return (res);
}

/* Use a scripted attack, spell, etc... */
void lua_monster_script(int m_idx, cptr mscript)
{
	call_lua(mscript, "(d)", "", m_idx);
}
