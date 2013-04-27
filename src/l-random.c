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

/* function: randint0 */
static int toluaI_random_randint000(lua_State* tolua_S)
{
 if (!tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
     !tolua_isnoobj(tolua_S,2))
 {
  TOLUA_ERR_FN(randint0);
 } else {
  u32b m = ((u32b)  tolua_getnumber(tolua_S,1,0));
  s32b toluaI_ret = (s32b)  randint0(m);
  tolua_pushnumber(tolua_S,(long)toluaI_ret);
 }
 return 1;
}

/* function: randint1 */
static int toluaI_random_randint100(lua_State* tolua_S)
{
 if (!tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
     !tolua_isnoobj(tolua_S,2))
 {
  TOLUA_ERR_FN(randint1);
 } else {
  u32b m = ((u32b)  tolua_getnumber(tolua_S,1,0));
  s32b toluaI_ret = (s32b)  randint1(m);
  tolua_pushnumber(tolua_S,(long)toluaI_ret);
 }
 return 1;
}

/* function: rand_range */
static int toluaI_random_rand_range00(lua_State* tolua_S)
{
 if (!tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
     !tolua_istype(tolua_S,2,LUA_TNUMBER,0) ||
     !tolua_isnoobj(tolua_S,3))
 {
  TOLUA_ERR_FN(rand_range);
 } else {
  u32b A = ((u32b)  tolua_getnumber(tolua_S,1,0));
  u32b B = ((u32b)  tolua_getnumber(tolua_S,2,0));
  s32b toluaI_ret = (s32b)  rand_range(A,B);
  tolua_pushnumber(tolua_S,(long)toluaI_ret);
 }
 return 1;
}

/* function: rand_spread */
static int toluaI_random_rand_spread00(lua_State* tolua_S)
{
 if (!tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
     !tolua_istype(tolua_S,2,LUA_TNUMBER,0) ||
     !tolua_isnoobj(tolua_S,3))
 {
  TOLUA_ERR_FN(rand_spread);
 } else {
  u32b A = ((u32b)  tolua_getnumber(tolua_S,1,0));
  u32b D = ((u32b)  tolua_getnumber(tolua_S,2,0));
  s32b toluaI_ret = (s32b)  rand_spread(A,D);
  tolua_pushnumber(tolua_S,(long)toluaI_ret);
 }
 return 1;
}

/* function: damroll */
static int toluaI_random_damroll00(lua_State* tolua_S)
{
 if (!tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
     !tolua_istype(tolua_S,2,LUA_TNUMBER,0) ||
     !tolua_isnoobj(tolua_S,3))
 {
  TOLUA_ERR_FN(damroll);
 } else {
  uint num = ((uint)  tolua_getnumber(tolua_S,1,0));
  uint sides = ((uint)  tolua_getnumber(tolua_S,2,0));
  uint toluaI_ret = (uint)  damroll(num,sides);
  tolua_pushnumber(tolua_S,(long)toluaI_ret);
 }
 return 1;
}

/* function: one_in_ */
static int toluaI_random_one_in_00(lua_State* tolua_S)
{
 if (!tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
     !tolua_isnoobj(tolua_S,2))
 {
  TOLUA_ERR_FN(one_in_);
 } else {
  u32b m = ((u32b)  tolua_getnumber(tolua_S,1,0));
  u32b toluaI_ret = (u32b)  one_in_(m);
  tolua_pushnumber(tolua_S,(long)toluaI_ret);
 }
 return 1;
}

/* function: saving_throw */
static int toluaI_random_saving_throw00(lua_State* tolua_S)
{
 if (!tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
     !tolua_isnoobj(tolua_S,2))
 {
  TOLUA_ERR_FN(saving_throw);
 } else {
  s32b m = ((s32b)  tolua_getnumber(tolua_S,1,0));
  bool toluaI_ret = (bool)  saving_throw(m);
  tolua_pushbool(tolua_S,(int)toluaI_ret);
 }
 return 1;
}

/* function: m_bonus */
static int toluaI_random_m_bonus00(lua_State* tolua_S)
{
 if (!tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
     !tolua_istype(tolua_S,2,LUA_TNUMBER,0) ||
     !tolua_isnoobj(tolua_S,3))
 {
  TOLUA_ERR_FN(m_bonus);
 } else {
  int max = ((int)  tolua_getnumber(tolua_S,1,0));
  int level = ((int)  tolua_getnumber(tolua_S,2,0));
  s16b toluaI_ret = (s16b)  m_bonus(max,level);
  tolua_pushnumber(tolua_S,(long)toluaI_ret);
 }
 return 1;
}

/* Open function */
int tolua_random_open (lua_State* tolua_S)
{
 tolua_open(tolua_S);
 toluaI_reg_types(tolua_S);
 TOLUA_FUN(randint0,toluaI_random_randint000);
 TOLUA_FUN(randint1,toluaI_random_randint100);
 TOLUA_FUN(rand_range,toluaI_random_rand_range00);
 TOLUA_FUN(rand_spread,toluaI_random_rand_spread00);
 TOLUA_FUN(damroll,toluaI_random_damroll00);
 TOLUA_FUN(one_in_,toluaI_random_one_in_00);
 TOLUA_FUN(saving_throw,toluaI_random_saving_throw00);
 TOLUA_FUN(m_bonus,toluaI_random_m_bonus00);
 return 1;
}
/* Close function */
void tolua_random_close (lua_State* tolua_S)
{
 TOLUA_UNDEF(randint0);
 TOLUA_UNDEF(randint1);
 TOLUA_UNDEF(rand_range);
 TOLUA_UNDEF(rand_spread);
 TOLUA_UNDEF(damroll);
 TOLUA_UNDEF(one_in_);
 TOLUA_UNDEF(saving_throw);
 TOLUA_UNDEF(m_bonus);
}
