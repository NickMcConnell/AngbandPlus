/* File: lua.c */

/*
 * 
 * This file defines bindings for the lua extension language.
 *
 * Note that ALL public interface between Angband and Lua is done
 * through this file. 
 *
 * First written for Angband 2.1 aka "Combustible".
 */

#include "angband.h"

#include "lua/include/lua.h"
#include "lua/include/lualib.h"


/*
 * Does nothing for now.
 */

static lua_State* __lua = NULL;


/*
 * Register the Angband->Lua public interface.
 */

static void lua_anglibopen(lua_State* lua) {
  /* We do nothing for now. */
}


errr init_lua(void) {

  /* 
   * Start the interpreter. 
   * The argument is the stack size, 0 means default.
   */
  __lua = lua_open(0);

  /* Register the Lua base libraries. */

  lua_baselibopen(__lua);
  lua_strlibopen(__lua);
  lua_dblibopen(__lua);

  lua_anglibopen(__lua);

  return 0;
}
