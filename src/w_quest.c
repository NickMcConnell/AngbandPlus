/*
** Lua binding: quest
** Generated automatically by tolua 4.0a - angband on Mon Jul 14 20:44:03 2003.
*/

#include "lua/tolua.h"

/* Exported function */
int tolua_quest_open (lua_State* tolua_S);
void tolua_quest_close (lua_State* tolua_S);

#include "angband.h"
static quest_type *lua_get_quest(int q_idx){return &quest[q_idx];}

/* function to register type */
static void toluaI_reg_types (lua_State* tolua_S)
{
 tolua_usertype(tolua_S,"quest_type");
}

/* error messages */
#define TOLUA_ERR_SELF tolua_error(tolua_S,"invalid 'self'")
#define TOLUA_ERR_ASSIGN tolua_error(tolua_S,"#vinvalid type in variable assignment.")

/* get function: silent of class  quest_type */
static int toluaI_get_quest_quest_type_silent(lua_State* tolua_S)
{
  quest_type* self = (quest_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->silent);
 return 1;
}

/* set function: silent of class  quest_type */
static int toluaI_set_quest_quest_type_silent(lua_State* tolua_S)
{
  quest_type* self = (quest_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->silent = ((bool)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: dynamic_desc of class  quest_type */
static int toluaI_get_quest_quest_type_dynamic_desc(lua_State* tolua_S)
{
  quest_type* self = (quest_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->dynamic_desc);
 return 1;
}

/* set function: dynamic_desc of class  quest_type */
static int toluaI_set_quest_quest_type_dynamic_desc(lua_State* tolua_S)
{
  quest_type* self = (quest_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->dynamic_desc = ((bool)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: status of class  quest_type */
static int toluaI_get_quest_quest_type_status(lua_State* tolua_S)
{
  quest_type* self = (quest_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->status);
 return 1;
}

/* set function: status of class  quest_type */
static int toluaI_set_quest_quest_type_status(lua_State* tolua_S)
{
  quest_type* self = (quest_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->status = ((s16b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: level of class  quest_type */
static int toluaI_get_quest_quest_type_level(lua_State* tolua_S)
{
  quest_type* self = (quest_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->level);
 return 1;
}

/* set function: level of class  quest_type */
static int toluaI_set_quest_quest_type_level(lua_State* tolua_S)
{
  quest_type* self = (quest_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->level = ((s16b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: type of class  quest_type */
static int toluaI_get_quest_quest_type_type(lua_State* tolua_S)
{
  quest_type* self = (quest_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->type);
 return 1;
}

/* set function: type of class  quest_type */
static int toluaI_set_quest_quest_type_type(lua_State* tolua_S)
{
  quest_type* self = (quest_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->type = ((byte)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: max_q_idx */
static int toluaI_get_quest_max_q_idx(lua_State* tolua_S)
{
 tolua_pushnumber(tolua_S,(long)max_q_idx);
 return 1;
}

/* set function: max_q_idx */
static int toluaI_set_quest_max_q_idx(lua_State* tolua_S)
{
 if (!tolua_istype(tolua_S,1,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  max_q_idx = ((s16b)  tolua_getnumber(tolua_S,1,0));
 return 0;
}

/* get function: quest */
static int toluaI_get_quest_quest_aux(lua_State* tolua_S)
{
 int toluaI_index;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 tolua_error(tolua_S,"invalid type in array indexing.");
 toluaI_index = (int)tolua_getnumber(tolua_S,2,0)-1;
 if (toluaI_index<0 || toluaI_index>=max_q_idx)
 tolua_error(tolua_S,"array indexing out of range.");
 tolua_pushusertype(tolua_S,(void*)&quest[toluaI_index],tolua_tag(tolua_S,"quest_type"));
 return 1;
}

/* set function: quest */
static int toluaI_set_quest_quest_aux(lua_State* tolua_S)
{
 int toluaI_index;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 tolua_error(tolua_S,"invalid type in array indexing.");
 toluaI_index = (int)tolua_getnumber(tolua_S,2,0)-1;
 if (toluaI_index<0 || toluaI_index>=max_q_idx)
 tolua_error(tolua_S,"array indexing out of range.");
  quest[toluaI_index] = *((quest_type*)  tolua_getusertype(tolua_S,3,0));
 return 0;
}

/* function: lua_get_quest */
static int toluaI_quest_quest00(lua_State* tolua_S)
{
 if (
 !tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
 !tolua_isnoobj(tolua_S,2)
 )
 goto tolua_lerror;
 else
 {
  int q_idx = ((int)  tolua_getnumber(tolua_S,1,0));
 {
  quest_type* toluaI_ret = (quest_type*)  lua_get_quest(q_idx);
 tolua_pushusertype(tolua_S,(void*)toluaI_ret,tolua_tag(tolua_S,"quest_type"));
 }
 }
 return 1;
tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'quest'.");
 return 0;
}

/* function: add_new_quest */
static int toluaI_quest_new_quest00(lua_State* tolua_S)
{
 if (
 !tolua_istype(tolua_S,1,LUA_TSTRING,0) ||
 !tolua_isnoobj(tolua_S,2)
 )
 goto tolua_lerror;
 else
 {
  char* name = ((char*)  tolua_getstring(tolua_S,1,0));
 {
  s16b toluaI_ret = (s16b)  add_new_quest(name);
 tolua_pushnumber(tolua_S,(long)toluaI_ret);
 }
 }
 return 1;
tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'new_quest'.");
 return 0;
}

/* function: desc_quest */
static int toluaI_quest_quest_desc00(lua_State* tolua_S)
{
 if (
 !tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
 !tolua_istype(tolua_S,2,LUA_TNUMBER,0) ||
 !tolua_istype(tolua_S,3,LUA_TSTRING,0) ||
 !tolua_isnoobj(tolua_S,4)
 )
 goto tolua_lerror;
 else
 {
  int q_idx = ((int)  tolua_getnumber(tolua_S,1,0));
  int d = ((int)  tolua_getnumber(tolua_S,2,0));
  char* desc = ((char*)  tolua_getstring(tolua_S,3,0));
 {
  desc_quest(q_idx,d,desc);
 }
 }
 return 0;
tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'quest_desc'.");
 return 0;
}

/* function: lua_get_new_bounty_monster */
static int toluaI_quest_get_new_bounty_monster00(lua_State* tolua_S)
{
 if (
 !tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
 !tolua_isnoobj(tolua_S,2)
 )
 goto tolua_lerror;
 else
 {
  int lev = ((int)  tolua_getnumber(tolua_S,1,0));
 {
  int toluaI_ret = (int)  lua_get_new_bounty_monster(lev);
 tolua_pushnumber(tolua_S,(long)toluaI_ret);
 }
 }
 return 1;
tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'get_new_bounty_monster'.");
 return 0;
}

/* Open function */
int tolua_quest_open (lua_State* tolua_S)
{
 tolua_open(tolua_S);
 toluaI_reg_types(tolua_S);
 tolua_constant(tolua_S,NULL,"QUEST_STATUS_IGNORED",QUEST_STATUS_IGNORED);
 tolua_constant(tolua_S,NULL,"QUEST_STATUS_UNTAKEN",QUEST_STATUS_UNTAKEN);
 tolua_constant(tolua_S,NULL,"QUEST_STATUS_TAKEN",QUEST_STATUS_TAKEN);
 tolua_constant(tolua_S,NULL,"QUEST_STATUS_COMPLETED",QUEST_STATUS_COMPLETED);
 tolua_constant(tolua_S,NULL,"QUEST_STATUS_REWARDED",QUEST_STATUS_REWARDED);
 tolua_constant(tolua_S,NULL,"QUEST_STATUS_FAILED",QUEST_STATUS_FAILED);
 tolua_constant(tolua_S,NULL,"QUEST_STATUS_FINISHED",QUEST_STATUS_FINISHED);
 tolua_constant(tolua_S,NULL,"QUEST_STATUS_FAILED_DONE",QUEST_STATUS_FAILED_DONE);
 tolua_cclass(tolua_S,"quest_type","");
 tolua_tablevar(tolua_S,"quest_type","silent",toluaI_get_quest_quest_type_silent,toluaI_set_quest_quest_type_silent);
 tolua_tablevar(tolua_S,"quest_type","dynamic_desc",toluaI_get_quest_quest_type_dynamic_desc,toluaI_set_quest_quest_type_dynamic_desc);
 tolua_tablevar(tolua_S,"quest_type","status",toluaI_get_quest_quest_type_status,toluaI_set_quest_quest_type_status);
 tolua_tablevar(tolua_S,"quest_type","level",toluaI_get_quest_quest_type_level,toluaI_set_quest_quest_type_level);
 tolua_tablevar(tolua_S,"quest_type","type",toluaI_get_quest_quest_type_type,toluaI_set_quest_quest_type_type);
 tolua_globalvar(tolua_S,"max_q_idx",toluaI_get_quest_max_q_idx,toluaI_set_quest_max_q_idx);
 tolua_globalarray(tolua_S,"quest_aux",toluaI_get_quest_quest_aux,toluaI_set_quest_quest_aux);
 tolua_function(tolua_S,NULL,"quest",toluaI_quest_quest00);
 tolua_function(tolua_S,NULL,"new_quest",toluaI_quest_new_quest00);
 tolua_function(tolua_S,NULL,"quest_desc",toluaI_quest_quest_desc00);
 tolua_function(tolua_S,NULL,"get_new_bounty_monster",toluaI_quest_get_new_bounty_monster00);
 return 1;
}
/* Close function */
void tolua_quest_close (lua_State* tolua_S)
{
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"QUEST_STATUS_IGNORED");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"QUEST_STATUS_UNTAKEN");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"QUEST_STATUS_TAKEN");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"QUEST_STATUS_COMPLETED");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"QUEST_STATUS_REWARDED");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"QUEST_STATUS_FAILED");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"QUEST_STATUS_FINISHED");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"QUEST_STATUS_FAILED_DONE");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"quest_type");
 lua_getglobals(tolua_S);
 lua_pushstring(tolua_S,"max_q_idx"); lua_pushnil(tolua_S); lua_rawset(tolua_S,-3);
 lua_pop(tolua_S,1);
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"quest_aux");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"quest");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"new_quest");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"quest_desc");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"get_new_bounty_monster");
}
