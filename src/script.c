/* File: script.c */

/* Purpose: scripting in lua */

/*
 * Copyright (c) 2001 Dark God
 *
 * This software may be copied and distributed for educational, research, and
 * not for profit purposes provided that this copyright and statement are
 * included in all such copies.
 */

#ifdef USE_LUA

#include "angband.h"

#include "lua.h"
#include "lualib.h"
#include "lauxlib.h"
#include "tolua.h"

int  tolua_monster_open (lua_State *L);
int  tolua_player_open (lua_State *L);
int  tolua_player_c_open (lua_State *L);
int  tolua_util_open (lua_State *L);
int  tolua_z_pack_open (lua_State *L);
int  tolua_object_open (lua_State *L);
int  tolua_spells_open (lua_State *L);
int  tolua_quest_open (lua_State *L);

/*
 * Lua state
 */
lua_State* L = NULL;

/* T.o.M.E. Lua error message handler */
static int pern_errormessage(lua_State *L)
{
        char buf[200];
        cptr str = luaL_check_string(L, 1);
        int i = 0, j = 0;

        while (str[i])
        {
                if (str[i] != '\n')
                {
                        buf[j++] = str[i];
                }
                else
                {
                        buf[j] = '\0';
                        cmsg_format(TERM_VIOLET, "LUA: %s", buf);
                        j = 0;
                }
                i++;
        }
        buf[j] = '\0';
        cmsg_format(TERM_VIOLET, "LUA: %s", buf);
        return (0);
}

static struct luaL_reg pern_iolib[] =
{
        {"_ALERT", pern_errormessage},
};

#define DYADIC(name, op) \
    s32b name(s32b a, s32b b) { \
		return (a op b); \
    }

#define MONADIC(name, op) \
    s32b name(s32b b) { \
		return (op b); \
    }


DYADIC(intMod,      % )
DYADIC(intAnd,      & )
DYADIC(intOr,       | )
DYADIC(intXor,      ^ )
DYADIC(intShiftl,   <<)
DYADIC(intShiftr,   >>)
MONADIC(intBitNot,  ~ )


/* Initialize lua scripting */
void init_lua()
{
	/* Start the interpreter with default stack size */
	L = lua_open(0);

	/* Register the Lua base libraries */
	lua_baselibopen(L);
	lua_strlibopen(L);
	lua_iolibopen(L);
	lua_dblibopen(L);

	/* Register T.o.M.E. lua debug library */
	luaL_openl(L, pern_iolib);

        /* Register the T.o.M.E. main APIs */
        tolua_player_open(L);
        tolua_util_open(L);
        tolua_z_pack_open(L);
        tolua_object_open(L);
        tolua_monster_open(L);
        tolua_spells_open(L);
        tolua_quest_open(L);

        /* Load the first lua file */
        pern_dofile("init.lua");
}

bool pern_dofile(char *file)
{
	char buf[1024];
        int oldtop = lua_gettop(L);

	/* Build the filename */
        path_build(buf, 1024, ANGBAND_DIR_SCPT, file);

        lua_dofile(L, buf);
        lua_settop(L, oldtop);

        return (FALSE);
}

bool exec_lua(char *file)
{
        int oldtop = lua_gettop(L);
        lua_dostring(L, file);
        lua_settop(L, oldtop);
        return (FALSE);
}

#endif
