/*
** Lua binding: random
** Generated automatically by tolua 4.0a - angband.
*/

#include "lua/tolua.h"

/* Exported function */
int  tolua_random_open (lua_State* tolua_S);
void tolua_random_close (lua_State* tolua_S);

#include "angband.h"

/* function to register type */
static void toluaI_reg_types (lua_State* tolua_S)
{
(void) tolua_S;	/* Hack - prevent compiler warnings */
}

/* error messages */
#define TOLUA_ERR_SELF tolua_error(tolua_S,"invalid 'self'")
#define TOLUA_ERR_ASSIGN tolua_error(tolua_S,"#vinvalid type in variable assignment.")

/* get function: Rand_quick */
static int toluaI_get_random_Rand_quick(lua_State* tolua_S)
{
  tolua_pushbool(tolua_S,(int)Rand_quick);
 return 1;
}

/* set function: Rand_quick */
static int toluaI_set_random_Rand_quick(lua_State* tolua_S)
{
  if (!tolua_istype(tolua_S,1,tolua_tag(tolua_S,"bool"),0))
   TOLUA_ERR_ASSIGN;
  Rand_quick = ((bool)  tolua_getbool(tolua_S,1,0));
 return 0;
}

/* function: randint0 */
static int toluaI_random_randint000(lua_State* tolua_S)
{
 if (
     !tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
     !tolua_isnoobj(tolua_S,2)
 )
 {
  tolua_error(tolua_S,"#ferror in function 'randint0'.");
  return 0;
 }
 else
 {
  u32b m = ((u32b)  tolua_getnumber(tolua_S,1,0));
  {
   s32b toluaI_ret = (s32b)  randint0(m);
   tolua_pushnumber(tolua_S,(long)toluaI_ret);
  }
 }
 return 1;
}

/* function: randint1 */
static int toluaI_random_randint100(lua_State* tolua_S)
{
 if (
     !tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
     !tolua_isnoobj(tolua_S,2)
 )
 {
  tolua_error(tolua_S,"#ferror in function 'randint1'.");
  return 0;
 }
 else
 {
  u32b m = ((u32b)  tolua_getnumber(tolua_S,1,0));
  {
   s32b toluaI_ret = (s32b)  randint1(m);
   tolua_pushnumber(tolua_S,(long)toluaI_ret);
  }
 }
 return 1;
}

/* function: rand_range */
static int toluaI_random_rand_range00(lua_State* tolua_S)
{
 if (
     !tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
     !tolua_istype(tolua_S,2,LUA_TNUMBER,0) ||
     !tolua_isnoobj(tolua_S,3)
 )
 {
  tolua_error(tolua_S,"#ferror in function 'rand_range'.");
  return 0;
 }
 else
 {
  u32b A = ((u32b)  tolua_getnumber(tolua_S,1,0));
  u32b B = ((u32b)  tolua_getnumber(tolua_S,2,0));
  {
   s32b toluaI_ret = (s32b)  rand_range(A,B);
   tolua_pushnumber(tolua_S,(long)toluaI_ret);
  }
 }
 return 1;
}

/* function: rand_spread */
static int toluaI_random_rand_spread00(lua_State* tolua_S)
{
 if (
     !tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
     !tolua_istype(tolua_S,2,LUA_TNUMBER,0) ||
     !tolua_isnoobj(tolua_S,3)
 )
 {
  tolua_error(tolua_S,"#ferror in function 'rand_spread'.");
  return 0;
 }
 else
 {
  u32b A = ((u32b)  tolua_getnumber(tolua_S,1,0));
  u32b D = ((u32b)  tolua_getnumber(tolua_S,2,0));
  {
   s32b toluaI_ret = (s32b)  rand_spread(A,D);
   tolua_pushnumber(tolua_S,(long)toluaI_ret);
  }
 }
 return 1;
}

/* function: damroll */
static int toluaI_random_damroll00(lua_State* tolua_S)
{
 if (
     !tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
     !tolua_istype(tolua_S,2,LUA_TNUMBER,0) ||
     !tolua_isnoobj(tolua_S,3)
 )
 {
  tolua_error(tolua_S,"#ferror in function 'damroll'.");
  return 0;
 }
 else
 {
  uint num = ((uint)  tolua_getnumber(tolua_S,1,0));
  uint sides = ((uint)  tolua_getnumber(tolua_S,2,0));
  {
   uint toluaI_ret = (uint)  damroll(num,sides);
   tolua_pushnumber(tolua_S,(long)toluaI_ret);
  }
 }
 return 1;
}

/* function: maxroll */
static int toluaI_random_maxroll00(lua_State* tolua_S)
{
 if (
     !tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
     !tolua_istype(tolua_S,2,LUA_TNUMBER,0) ||
     !tolua_isnoobj(tolua_S,3)
 )
 {
  tolua_error(tolua_S,"#ferror in function 'maxroll'.");
  return 0;
 }
 else
 {
  uint num = ((uint)  tolua_getnumber(tolua_S,1,0));
  uint sides = ((uint)  tolua_getnumber(tolua_S,2,0));
  {
   uint toluaI_ret = (uint)  maxroll(num,sides);
   tolua_pushnumber(tolua_S,(long)toluaI_ret);
  }
 }
 return 1;
}

/* function: Rand_state_init */
static int toluaI_random_Rand_state_init00(lua_State* tolua_S)
{
 if (
     !tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
     !tolua_isnoobj(tolua_S,2)
 )
 {
  tolua_error(tolua_S,"#ferror in function 'Rand_state_init'.");
  return 0;
 }
 else
 {
  u32b seed = ((u32b)  tolua_getnumber(tolua_S,1,0));
  {
   Rand_state_init(seed);
  }
 }
 return 0;
}

/* function: Rand_div */
static int toluaI_random_Rand_div00(lua_State* tolua_S)
{
 if (
     !tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
     !tolua_isnoobj(tolua_S,2)
 )
 {
  tolua_error(tolua_S,"#ferror in function 'Rand_div'.");
  return 0;
 }
 else
 {
  u32b m = ((u32b)  tolua_getnumber(tolua_S,1,0));
  {
   u32b toluaI_ret = (u32b)  Rand_div(m);
   tolua_pushnumber(tolua_S,(long)toluaI_ret);
  }
 }
 return 1;
}

/* function: Rand_normal */
static int toluaI_random_Rand_normal00(lua_State* tolua_S)
{
 if (
     !tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
     !tolua_istype(tolua_S,2,LUA_TNUMBER,0) ||
     !tolua_isnoobj(tolua_S,3)
 )
 {
  tolua_error(tolua_S,"#ferror in function 'Rand_normal'.");
  return 0;
 }
 else
 {
  int mean = ((int)  tolua_getnumber(tolua_S,1,0));
  int stand = ((int)  tolua_getnumber(tolua_S,2,0));
  {
   s16b toluaI_ret = (s16b)  Rand_normal(mean,stand);
   tolua_pushnumber(tolua_S,(long)toluaI_ret);
  }
 }
 return 1;
}

/* function: Rand_simple */
static int toluaI_random_Rand_simple00(lua_State* tolua_S)
{
 if (
     !tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
     !tolua_isnoobj(tolua_S,2)
 )
 {
  tolua_error(tolua_S,"#ferror in function 'Rand_simple'.");
  return 0;
 }
 else
 {
  u32b m = ((u32b)  tolua_getnumber(tolua_S,1,0));
  {
   u32b toluaI_ret = (u32b)  Rand_simple(m);
   tolua_pushnumber(tolua_S,(long)toluaI_ret);
  }
 }
 return 1;
}

/* function: one_in_ */
static int toluaI_random_one_in_00(lua_State* tolua_S)
{
 if (
     !tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
     !tolua_isnoobj(tolua_S,2)
 )
 {
  tolua_error(tolua_S,"#ferror in function 'one_in_'.");
  return 0;
 }
 else
 {
  u32b m = ((u32b)  tolua_getnumber(tolua_S,1,0));
  {
   u32b toluaI_ret = (u32b)  one_in_(m);
   tolua_pushnumber(tolua_S,(long)toluaI_ret);
  }
 }
 return 1;
}

/* function: saving_throw */
static int toluaI_random_saving_throw00(lua_State* tolua_S)
{
 if (
     !tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
     !tolua_isnoobj(tolua_S,2)
 )
 {
  tolua_error(tolua_S,"#ferror in function 'saving_throw'.");
  return 0;
 }
 else
 {
  s32b m = ((s32b)  tolua_getnumber(tolua_S,1,0));
  {
   bool toluaI_ret = (bool)  saving_throw(m);
   tolua_pushbool(tolua_S,(int)toluaI_ret);
  }
 }
 return 1;
}

/* Open function */
int tolua_random_open (lua_State* tolua_S)
{
 tolua_open(tolua_S);
 toluaI_reg_types(tolua_S);
 tolua_globalvar(tolua_S,"Rand_quick",toluaI_get_random_Rand_quick,toluaI_set_random_Rand_quick);
 tolua_function(tolua_S,NULL,"randint0",toluaI_random_randint000);
 tolua_function(tolua_S,NULL,"randint1",toluaI_random_randint100);
 tolua_function(tolua_S,NULL,"rand_range",toluaI_random_rand_range00);
 tolua_function(tolua_S,NULL,"rand_spread",toluaI_random_rand_spread00);
 tolua_function(tolua_S,NULL,"damroll",toluaI_random_damroll00);
 tolua_function(tolua_S,NULL,"maxroll",toluaI_random_maxroll00);
 tolua_function(tolua_S,NULL,"Rand_state_init",toluaI_random_Rand_state_init00);
 tolua_function(tolua_S,NULL,"Rand_div",toluaI_random_Rand_div00);
 tolua_function(tolua_S,NULL,"Rand_normal",toluaI_random_Rand_normal00);
 tolua_function(tolua_S,NULL,"Rand_simple",toluaI_random_Rand_simple00);
 tolua_function(tolua_S,NULL,"one_in_",toluaI_random_one_in_00);
 tolua_function(tolua_S,NULL,"saving_throw",toluaI_random_saving_throw00);
 return 1;
}
/* Close function */
void tolua_random_close (lua_State* tolua_S)
{
 lua_getglobals(tolua_S);
 lua_pushstring(tolua_S,"Rand_quick"); lua_pushnil(tolua_S); lua_rawset(tolua_S,-3);
 lua_pop(tolua_S,1);
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"randint0");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"randint1");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"rand_range");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"rand_spread");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"damroll");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"maxroll");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"Rand_state_init");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"Rand_div");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"Rand_normal");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"Rand_simple");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"one_in_");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"saving_throw");
}
