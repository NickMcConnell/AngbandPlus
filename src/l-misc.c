/*
** Lua binding: misc
** Generated automatically by tolua 4.0a - angband.
*/

#include "lua/tolua.h"

/* Exported function */
int  tolua_misc_open (lua_State* tolua_S);
void tolua_misc_close (lua_State* tolua_S);

#include "angband.h"

/* function to register type */
static void toluaI_reg_types (lua_State* tolua_S)
{
(void) tolua_S;	/* Hack - prevent compiler warnings */
}

/* Open function */
int tolua_misc_open (lua_State* tolua_S)
{
 tolua_open(tolua_S);
 toluaI_reg_types(tolua_S);
 TOLUA_DEF(TRUE);
 TOLUA_DEF(FALSE);
 return 1;
}
/* Close function */
void tolua_misc_close (lua_State* tolua_S)
{
 TOLUA_UNDEF(TRUE);
 TOLUA_UNDEF(FALSE);
}
