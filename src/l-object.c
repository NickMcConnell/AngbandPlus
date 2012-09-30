/*
** Lua binding: object
** Generated automatically by tolua 4.0a - angband.
*/

#include "lua/tolua.h"

/* Exported function */
int  tolua_object_open (lua_State* tolua_S);
void tolua_object_close (lua_State* tolua_S);

#include "angband.h"
#include "script.h"

/* function to register type */
static void toluaI_reg_types (lua_State* tolua_S)
{
(void) tolua_S;	/* Hack - prevent compiler warnings */
 tolua_usertype(tolua_S,"obj_theme");
 tolua_usertype(tolua_S,"object_type");
 tolua_usertype(tolua_S,"object_kind");
}

/* error messages */
#define TOLUA_ERR_SELF tolua_error(tolua_S,"invalid 'self'")
#define TOLUA_ERR_ASSIGN tolua_error(tolua_S,"#vinvalid type in variable assignment.")

/* get function: name of class  object_kind */
static int toluaI_get_object_object_kind_name(lua_State* tolua_S)
{
  object_kind* self = (object_kind*)  tolua_getusertype(tolua_S,1,0);
  if (!self) TOLUA_ERR_SELF;
  tolua_pushnumber(tolua_S,(long)self->name);
 return 1;
}

/* set function: name of class  object_kind */
static int toluaI_set_object_object_kind_name(lua_State* tolua_S)
{
  object_kind* self = (object_kind*)  tolua_getusertype(tolua_S,1,0);
  if (!self) TOLUA_ERR_SELF;
  if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
   TOLUA_ERR_ASSIGN;
  self->name = ((u32b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: text of class  object_kind */
static int toluaI_get_object_object_kind_text(lua_State* tolua_S)
{
  object_kind* self = (object_kind*)  tolua_getusertype(tolua_S,1,0);
  if (!self) TOLUA_ERR_SELF;
  tolua_pushnumber(tolua_S,(long)self->text);
 return 1;
}

/* set function: text of class  object_kind */
static int toluaI_set_object_object_kind_text(lua_State* tolua_S)
{
  object_kind* self = (object_kind*)  tolua_getusertype(tolua_S,1,0);
  if (!self) TOLUA_ERR_SELF;
  if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
   TOLUA_ERR_ASSIGN;
  self->text = ((u32b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: tval of class  object_kind */
static int toluaI_get_object_object_kind_tval(lua_State* tolua_S)
{
  object_kind* self = (object_kind*)  tolua_getusertype(tolua_S,1,0);
  if (!self) TOLUA_ERR_SELF;
  tolua_pushnumber(tolua_S,(long)self->tval);
 return 1;
}

/* set function: tval of class  object_kind */
static int toluaI_set_object_object_kind_tval(lua_State* tolua_S)
{
  object_kind* self = (object_kind*)  tolua_getusertype(tolua_S,1,0);
  if (!self) TOLUA_ERR_SELF;
  if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
   TOLUA_ERR_ASSIGN;
  self->tval = ((byte)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: sval of class  object_kind */
static int toluaI_get_object_object_kind_sval(lua_State* tolua_S)
{
  object_kind* self = (object_kind*)  tolua_getusertype(tolua_S,1,0);
  if (!self) TOLUA_ERR_SELF;
  tolua_pushnumber(tolua_S,(long)self->sval);
 return 1;
}

/* set function: sval of class  object_kind */
static int toluaI_set_object_object_kind_sval(lua_State* tolua_S)
{
  object_kind* self = (object_kind*)  tolua_getusertype(tolua_S,1,0);
  if (!self) TOLUA_ERR_SELF;
  if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
   TOLUA_ERR_ASSIGN;
  self->sval = ((byte)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: pval of class  object_kind */
static int toluaI_get_object_object_kind_pval(lua_State* tolua_S)
{
  object_kind* self = (object_kind*)  tolua_getusertype(tolua_S,1,0);
  if (!self) TOLUA_ERR_SELF;
  tolua_pushnumber(tolua_S,(long)self->pval);
 return 1;
}

/* set function: pval of class  object_kind */
static int toluaI_set_object_object_kind_pval(lua_State* tolua_S)
{
  object_kind* self = (object_kind*)  tolua_getusertype(tolua_S,1,0);
  if (!self) TOLUA_ERR_SELF;
  if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
   TOLUA_ERR_ASSIGN;
  self->pval = ((s16b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: to_h of class  object_kind */
static int toluaI_get_object_object_kind_to_h(lua_State* tolua_S)
{
  object_kind* self = (object_kind*)  tolua_getusertype(tolua_S,1,0);
  if (!self) TOLUA_ERR_SELF;
  tolua_pushnumber(tolua_S,(long)self->to_h);
 return 1;
}

/* set function: to_h of class  object_kind */
static int toluaI_set_object_object_kind_to_h(lua_State* tolua_S)
{
  object_kind* self = (object_kind*)  tolua_getusertype(tolua_S,1,0);
  if (!self) TOLUA_ERR_SELF;
  if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
   TOLUA_ERR_ASSIGN;
  self->to_h = ((s16b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: to_d of class  object_kind */
static int toluaI_get_object_object_kind_to_d(lua_State* tolua_S)
{
  object_kind* self = (object_kind*)  tolua_getusertype(tolua_S,1,0);
  if (!self) TOLUA_ERR_SELF;
  tolua_pushnumber(tolua_S,(long)self->to_d);
 return 1;
}

/* set function: to_d of class  object_kind */
static int toluaI_set_object_object_kind_to_d(lua_State* tolua_S)
{
  object_kind* self = (object_kind*)  tolua_getusertype(tolua_S,1,0);
  if (!self) TOLUA_ERR_SELF;
  if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
   TOLUA_ERR_ASSIGN;
  self->to_d = ((s16b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: to_a of class  object_kind */
static int toluaI_get_object_object_kind_to_a(lua_State* tolua_S)
{
  object_kind* self = (object_kind*)  tolua_getusertype(tolua_S,1,0);
  if (!self) TOLUA_ERR_SELF;
  tolua_pushnumber(tolua_S,(long)self->to_a);
 return 1;
}

/* set function: to_a of class  object_kind */
static int toluaI_set_object_object_kind_to_a(lua_State* tolua_S)
{
  object_kind* self = (object_kind*)  tolua_getusertype(tolua_S,1,0);
  if (!self) TOLUA_ERR_SELF;
  if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
   TOLUA_ERR_ASSIGN;
  self->to_a = ((s16b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: ac of class  object_kind */
static int toluaI_get_object_object_kind_ac(lua_State* tolua_S)
{
  object_kind* self = (object_kind*)  tolua_getusertype(tolua_S,1,0);
  if (!self) TOLUA_ERR_SELF;
  tolua_pushnumber(tolua_S,(long)self->ac);
 return 1;
}

/* set function: ac of class  object_kind */
static int toluaI_set_object_object_kind_ac(lua_State* tolua_S)
{
  object_kind* self = (object_kind*)  tolua_getusertype(tolua_S,1,0);
  if (!self) TOLUA_ERR_SELF;
  if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
   TOLUA_ERR_ASSIGN;
  self->ac = ((s16b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: dd of class  object_kind */
static int toluaI_get_object_object_kind_dd(lua_State* tolua_S)
{
  object_kind* self = (object_kind*)  tolua_getusertype(tolua_S,1,0);
  if (!self) TOLUA_ERR_SELF;
  tolua_pushnumber(tolua_S,(long)self->dd);
 return 1;
}

/* set function: dd of class  object_kind */
static int toluaI_set_object_object_kind_dd(lua_State* tolua_S)
{
  object_kind* self = (object_kind*)  tolua_getusertype(tolua_S,1,0);
  if (!self) TOLUA_ERR_SELF;
  if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
   TOLUA_ERR_ASSIGN;
  self->dd = ((byte)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: ds of class  object_kind */
static int toluaI_get_object_object_kind_ds(lua_State* tolua_S)
{
  object_kind* self = (object_kind*)  tolua_getusertype(tolua_S,1,0);
  if (!self) TOLUA_ERR_SELF;
  tolua_pushnumber(tolua_S,(long)self->ds);
 return 1;
}

/* set function: ds of class  object_kind */
static int toluaI_set_object_object_kind_ds(lua_State* tolua_S)
{
  object_kind* self = (object_kind*)  tolua_getusertype(tolua_S,1,0);
  if (!self) TOLUA_ERR_SELF;
  if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
   TOLUA_ERR_ASSIGN;
  self->ds = ((byte)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: weight of class  object_kind */
static int toluaI_get_object_object_kind_weight(lua_State* tolua_S)
{
  object_kind* self = (object_kind*)  tolua_getusertype(tolua_S,1,0);
  if (!self) TOLUA_ERR_SELF;
  tolua_pushnumber(tolua_S,(long)self->weight);
 return 1;
}

/* set function: weight of class  object_kind */
static int toluaI_set_object_object_kind_weight(lua_State* tolua_S)
{
  object_kind* self = (object_kind*)  tolua_getusertype(tolua_S,1,0);
  if (!self) TOLUA_ERR_SELF;
  if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
   TOLUA_ERR_ASSIGN;
  self->weight = ((s16b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: cost of class  object_kind */
static int toluaI_get_object_object_kind_cost(lua_State* tolua_S)
{
  object_kind* self = (object_kind*)  tolua_getusertype(tolua_S,1,0);
  if (!self) TOLUA_ERR_SELF;
  tolua_pushnumber(tolua_S,(long)self->cost);
 return 1;
}

/* set function: cost of class  object_kind */
static int toluaI_set_object_object_kind_cost(lua_State* tolua_S)
{
  object_kind* self = (object_kind*)  tolua_getusertype(tolua_S,1,0);
  if (!self) TOLUA_ERR_SELF;
  if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
   TOLUA_ERR_ASSIGN;
  self->cost = ((s32b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: flags of class  object_kind */
static int toluaI_get_object_object_kind_flags(lua_State* tolua_S)
{
 int toluaI_index;
  object_kind* self;
 lua_pushstring(tolua_S,".self");
 lua_rawget(tolua_S,1);
 self = (object_kind*)  lua_touserdata(tolua_S,-1);
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
  tolua_error(tolua_S,"invalid type in array indexing.");
 toluaI_index = (int)tolua_getnumber(tolua_S,2,0);
 if (toluaI_index<0 || toluaI_index>=4)
  tolua_error(tolua_S,"array object_kind: flags indexing out of range.");
 tolua_pushnumber(tolua_S,(long)self->flags[toluaI_index]);
 return 1;
}

/* set function: flags of class  object_kind */
static int toluaI_set_object_object_kind_flags(lua_State* tolua_S)
{
 int toluaI_index;
  object_kind* self;
 lua_pushstring(tolua_S,".self");
 lua_rawget(tolua_S,1);
 self = (object_kind*)  lua_touserdata(tolua_S,-1);
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
  tolua_error(tolua_S,"invalid type in array indexing.");
 toluaI_index = (int)tolua_getnumber(tolua_S,2,0);
 if (toluaI_index<0 || toluaI_index>=4)
  tolua_error(tolua_S,"array object_kind: flags indexing out of range.");
  self->flags[toluaI_index] = ((u32b)  tolua_getnumber(tolua_S,3,0));
 return 0;
}

/* get function: locale of class  object_kind */
static int toluaI_get_object_object_kind_locale(lua_State* tolua_S)
{
 int toluaI_index;
  object_kind* self;
 lua_pushstring(tolua_S,".self");
 lua_rawget(tolua_S,1);
 self = (object_kind*)  lua_touserdata(tolua_S,-1);
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
  tolua_error(tolua_S,"invalid type in array indexing.");
 toluaI_index = (int)tolua_getnumber(tolua_S,2,0);
 if (toluaI_index<0 || toluaI_index>=4)
  tolua_error(tolua_S,"array object_kind: locale indexing out of range.");
 tolua_pushnumber(tolua_S,(long)self->locale[toluaI_index]);
 return 1;
}

/* set function: locale of class  object_kind */
static int toluaI_set_object_object_kind_locale(lua_State* tolua_S)
{
 int toluaI_index;
  object_kind* self;
 lua_pushstring(tolua_S,".self");
 lua_rawget(tolua_S,1);
 self = (object_kind*)  lua_touserdata(tolua_S,-1);
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
  tolua_error(tolua_S,"invalid type in array indexing.");
 toluaI_index = (int)tolua_getnumber(tolua_S,2,0);
 if (toluaI_index<0 || toluaI_index>=4)
  tolua_error(tolua_S,"array object_kind: locale indexing out of range.");
  self->locale[toluaI_index] = ((byte)  tolua_getnumber(tolua_S,3,0));
 return 0;
}

/* get function: chance of class  object_kind */
static int toluaI_get_object_object_kind_chance(lua_State* tolua_S)
{
 int toluaI_index;
  object_kind* self;
 lua_pushstring(tolua_S,".self");
 lua_rawget(tolua_S,1);
 self = (object_kind*)  lua_touserdata(tolua_S,-1);
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
  tolua_error(tolua_S,"invalid type in array indexing.");
 toluaI_index = (int)tolua_getnumber(tolua_S,2,0);
 if (toluaI_index<0 || toluaI_index>=4)
  tolua_error(tolua_S,"array object_kind: chance indexing out of range.");
 tolua_pushnumber(tolua_S,(long)self->chance[toluaI_index]);
 return 1;
}

/* set function: chance of class  object_kind */
static int toluaI_set_object_object_kind_chance(lua_State* tolua_S)
{
 int toluaI_index;
  object_kind* self;
 lua_pushstring(tolua_S,".self");
 lua_rawget(tolua_S,1);
 self = (object_kind*)  lua_touserdata(tolua_S,-1);
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
  tolua_error(tolua_S,"invalid type in array indexing.");
 toluaI_index = (int)tolua_getnumber(tolua_S,2,0);
 if (toluaI_index<0 || toluaI_index>=4)
  tolua_error(tolua_S,"array object_kind: chance indexing out of range.");
  self->chance[toluaI_index] = ((byte)  tolua_getnumber(tolua_S,3,0));
 return 0;
}

/* get function: level of class  object_kind */
static int toluaI_get_object_object_kind_level(lua_State* tolua_S)
{
  object_kind* self = (object_kind*)  tolua_getusertype(tolua_S,1,0);
  if (!self) TOLUA_ERR_SELF;
  tolua_pushnumber(tolua_S,(long)self->level);
 return 1;
}

/* set function: level of class  object_kind */
static int toluaI_set_object_object_kind_level(lua_State* tolua_S)
{
  object_kind* self = (object_kind*)  tolua_getusertype(tolua_S,1,0);
  if (!self) TOLUA_ERR_SELF;
  if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
   TOLUA_ERR_ASSIGN;
  self->level = ((byte)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: extra of class  object_kind */
static int toluaI_get_object_object_kind_extra(lua_State* tolua_S)
{
  object_kind* self = (object_kind*)  tolua_getusertype(tolua_S,1,0);
  if (!self) TOLUA_ERR_SELF;
  tolua_pushnumber(tolua_S,(long)self->extra);
 return 1;
}

/* set function: extra of class  object_kind */
static int toluaI_set_object_object_kind_extra(lua_State* tolua_S)
{
  object_kind* self = (object_kind*)  tolua_getusertype(tolua_S,1,0);
  if (!self) TOLUA_ERR_SELF;
  if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
   TOLUA_ERR_ASSIGN;
  self->extra = ((byte)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: d_attr of class  object_kind */
static int toluaI_get_object_object_kind_d_attr(lua_State* tolua_S)
{
  object_kind* self = (object_kind*)  tolua_getusertype(tolua_S,1,0);
  if (!self) TOLUA_ERR_SELF;
  tolua_pushnumber(tolua_S,(long)self->d_attr);
 return 1;
}

/* set function: d_attr of class  object_kind */
static int toluaI_set_object_object_kind_d_attr(lua_State* tolua_S)
{
  object_kind* self = (object_kind*)  tolua_getusertype(tolua_S,1,0);
  if (!self) TOLUA_ERR_SELF;
  if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
   TOLUA_ERR_ASSIGN;
  self->d_attr = ((byte)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: d_char of class  object_kind */
static int toluaI_get_object_object_kind_d_char(lua_State* tolua_S)
{
  object_kind* self = (object_kind*)  tolua_getusertype(tolua_S,1,0);
  if (!self) TOLUA_ERR_SELF;
  tolua_pushnumber(tolua_S,(long)self->d_char);
 return 1;
}

/* set function: d_char of class  object_kind */
static int toluaI_set_object_object_kind_d_char(lua_State* tolua_S)
{
  object_kind* self = (object_kind*)  tolua_getusertype(tolua_S,1,0);
  if (!self) TOLUA_ERR_SELF;
  if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
   TOLUA_ERR_ASSIGN;
  self->d_char = ((char)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: x_attr of class  object_kind */
static int toluaI_get_object_object_kind_x_attr(lua_State* tolua_S)
{
  object_kind* self = (object_kind*)  tolua_getusertype(tolua_S,1,0);
  if (!self) TOLUA_ERR_SELF;
  tolua_pushnumber(tolua_S,(long)self->x_attr);
 return 1;
}

/* set function: x_attr of class  object_kind */
static int toluaI_set_object_object_kind_x_attr(lua_State* tolua_S)
{
  object_kind* self = (object_kind*)  tolua_getusertype(tolua_S,1,0);
  if (!self) TOLUA_ERR_SELF;
  if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
   TOLUA_ERR_ASSIGN;
  self->x_attr = ((byte)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: x_char of class  object_kind */
static int toluaI_get_object_object_kind_x_char(lua_State* tolua_S)
{
  object_kind* self = (object_kind*)  tolua_getusertype(tolua_S,1,0);
  if (!self) TOLUA_ERR_SELF;
  tolua_pushnumber(tolua_S,(long)self->x_char);
 return 1;
}

/* set function: x_char of class  object_kind */
static int toluaI_set_object_object_kind_x_char(lua_State* tolua_S)
{
  object_kind* self = (object_kind*)  tolua_getusertype(tolua_S,1,0);
  if (!self) TOLUA_ERR_SELF;
  if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
   TOLUA_ERR_ASSIGN;
  self->x_char = ((char)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: flavor of class  object_kind */
static int toluaI_get_object_object_kind_flavor(lua_State* tolua_S)
{
  object_kind* self = (object_kind*)  tolua_getusertype(tolua_S,1,0);
  if (!self) TOLUA_ERR_SELF;
  tolua_pushnumber(tolua_S,(long)self->flavor);
 return 1;
}

/* set function: flavor of class  object_kind */
static int toluaI_set_object_object_kind_flavor(lua_State* tolua_S)
{
  object_kind* self = (object_kind*)  tolua_getusertype(tolua_S,1,0);
  if (!self) TOLUA_ERR_SELF;
  if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
   TOLUA_ERR_ASSIGN;
  self->flavor = ((byte)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: easy_know of class  object_kind */
static int toluaI_get_object_object_kind_easy_know(lua_State* tolua_S)
{
  object_kind* self = (object_kind*)  tolua_getusertype(tolua_S,1,0);
  if (!self) TOLUA_ERR_SELF;
  tolua_pushbool(tolua_S,(int)self->easy_know);
 return 1;
}

/* set function: easy_know of class  object_kind */
static int toluaI_set_object_object_kind_easy_know(lua_State* tolua_S)
{
  object_kind* self = (object_kind*)  tolua_getusertype(tolua_S,1,0);
  if (!self) TOLUA_ERR_SELF;
  if (!tolua_istype(tolua_S,2,tolua_tag(tolua_S,"bool"),0))
   TOLUA_ERR_ASSIGN;
  self->easy_know = ((bool)  tolua_getbool(tolua_S,2,0));
 return 0;
}

/* get function: aware of class  object_kind */
static int toluaI_get_object_object_kind_aware(lua_State* tolua_S)
{
  object_kind* self = (object_kind*)  tolua_getusertype(tolua_S,1,0);
  if (!self) TOLUA_ERR_SELF;
  tolua_pushbool(tolua_S,(int)self->aware);
 return 1;
}

/* set function: aware of class  object_kind */
static int toluaI_set_object_object_kind_aware(lua_State* tolua_S)
{
  object_kind* self = (object_kind*)  tolua_getusertype(tolua_S,1,0);
  if (!self) TOLUA_ERR_SELF;
  if (!tolua_istype(tolua_S,2,tolua_tag(tolua_S,"bool"),0))
   TOLUA_ERR_ASSIGN;
  self->aware = ((bool)  tolua_getbool(tolua_S,2,0));
 return 0;
}

/* get function: tried of class  object_kind */
static int toluaI_get_object_object_kind_tried(lua_State* tolua_S)
{
  object_kind* self = (object_kind*)  tolua_getusertype(tolua_S,1,0);
  if (!self) TOLUA_ERR_SELF;
  tolua_pushbool(tolua_S,(int)self->tried);
 return 1;
}

/* set function: tried of class  object_kind */
static int toluaI_set_object_object_kind_tried(lua_State* tolua_S)
{
  object_kind* self = (object_kind*)  tolua_getusertype(tolua_S,1,0);
  if (!self) TOLUA_ERR_SELF;
  if (!tolua_istype(tolua_S,2,tolua_tag(tolua_S,"bool"),0))
   TOLUA_ERR_ASSIGN;
  self->tried = ((bool)  tolua_getbool(tolua_S,2,0));
 return 0;
}

/* get function: treasure of class  obj_theme */
static int toluaI_get_object_obj_theme_treasure(lua_State* tolua_S)
{
  obj_theme* self = (obj_theme*)  tolua_getusertype(tolua_S,1,0);
  if (!self) TOLUA_ERR_SELF;
  tolua_pushnumber(tolua_S,(long)self->treasure);
 return 1;
}

/* set function: treasure of class  obj_theme */
static int toluaI_set_object_obj_theme_treasure(lua_State* tolua_S)
{
  obj_theme* self = (obj_theme*)  tolua_getusertype(tolua_S,1,0);
  if (!self) TOLUA_ERR_SELF;
  if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
   TOLUA_ERR_ASSIGN;
  self->treasure = ((byte)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: combat of class  obj_theme */
static int toluaI_get_object_obj_theme_combat(lua_State* tolua_S)
{
  obj_theme* self = (obj_theme*)  tolua_getusertype(tolua_S,1,0);
  if (!self) TOLUA_ERR_SELF;
  tolua_pushnumber(tolua_S,(long)self->combat);
 return 1;
}

/* set function: combat of class  obj_theme */
static int toluaI_set_object_obj_theme_combat(lua_State* tolua_S)
{
  obj_theme* self = (obj_theme*)  tolua_getusertype(tolua_S,1,0);
  if (!self) TOLUA_ERR_SELF;
  if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
   TOLUA_ERR_ASSIGN;
  self->combat = ((byte)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: magic of class  obj_theme */
static int toluaI_get_object_obj_theme_magic(lua_State* tolua_S)
{
  obj_theme* self = (obj_theme*)  tolua_getusertype(tolua_S,1,0);
  if (!self) TOLUA_ERR_SELF;
  tolua_pushnumber(tolua_S,(long)self->magic);
 return 1;
}

/* set function: magic of class  obj_theme */
static int toluaI_set_object_obj_theme_magic(lua_State* tolua_S)
{
  obj_theme* self = (obj_theme*)  tolua_getusertype(tolua_S,1,0);
  if (!self) TOLUA_ERR_SELF;
  if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
   TOLUA_ERR_ASSIGN;
  self->magic = ((byte)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: tools of class  obj_theme */
static int toluaI_get_object_obj_theme_tools(lua_State* tolua_S)
{
  obj_theme* self = (obj_theme*)  tolua_getusertype(tolua_S,1,0);
  if (!self) TOLUA_ERR_SELF;
  tolua_pushnumber(tolua_S,(long)self->tools);
 return 1;
}

/* set function: tools of class  obj_theme */
static int toluaI_set_object_obj_theme_tools(lua_State* tolua_S)
{
  obj_theme* self = (obj_theme*)  tolua_getusertype(tolua_S,1,0);
  if (!self) TOLUA_ERR_SELF;
  if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
   TOLUA_ERR_ASSIGN;
  self->tools = ((byte)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: k_idx of class  object_type */
static int toluaI_get_object_object_type_k_idx(lua_State* tolua_S)
{
  object_type* self = (object_type*)  tolua_getusertype(tolua_S,1,0);
  if (!self) TOLUA_ERR_SELF;
  tolua_pushnumber(tolua_S,(long)self->k_idx);
 return 1;
}

/* set function: k_idx of class  object_type */
static int toluaI_set_object_object_type_k_idx(lua_State* tolua_S)
{
  object_type* self = (object_type*)  tolua_getusertype(tolua_S,1,0);
  if (!self) TOLUA_ERR_SELF;
  if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
   TOLUA_ERR_ASSIGN;
  self->k_idx = ((s16b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: iy of class  object_type */
static int toluaI_get_object_object_type_iy(lua_State* tolua_S)
{
  object_type* self = (object_type*)  tolua_getusertype(tolua_S,1,0);
  if (!self) TOLUA_ERR_SELF;
  tolua_pushnumber(tolua_S,(long)self->iy);
 return 1;
}

/* set function: iy of class  object_type */
static int toluaI_set_object_object_type_iy(lua_State* tolua_S)
{
  object_type* self = (object_type*)  tolua_getusertype(tolua_S,1,0);
  if (!self) TOLUA_ERR_SELF;
  if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
   TOLUA_ERR_ASSIGN;
  self->iy = ((s16b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: ix of class  object_type */
static int toluaI_get_object_object_type_ix(lua_State* tolua_S)
{
  object_type* self = (object_type*)  tolua_getusertype(tolua_S,1,0);
  if (!self) TOLUA_ERR_SELF;
  tolua_pushnumber(tolua_S,(long)self->ix);
 return 1;
}

/* set function: ix of class  object_type */
static int toluaI_set_object_object_type_ix(lua_State* tolua_S)
{
  object_type* self = (object_type*)  tolua_getusertype(tolua_S,1,0);
  if (!self) TOLUA_ERR_SELF;
  if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
   TOLUA_ERR_ASSIGN;
  self->ix = ((s16b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: tval of class  object_type */
static int toluaI_get_object_object_type_tval(lua_State* tolua_S)
{
  object_type* self = (object_type*)  tolua_getusertype(tolua_S,1,0);
  if (!self) TOLUA_ERR_SELF;
  tolua_pushnumber(tolua_S,(long)self->tval);
 return 1;
}

/* set function: tval of class  object_type */
static int toluaI_set_object_object_type_tval(lua_State* tolua_S)
{
  object_type* self = (object_type*)  tolua_getusertype(tolua_S,1,0);
  if (!self) TOLUA_ERR_SELF;
  if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
   TOLUA_ERR_ASSIGN;
  self->tval = ((byte)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: sval of class  object_type */
static int toluaI_get_object_object_type_sval(lua_State* tolua_S)
{
  object_type* self = (object_type*)  tolua_getusertype(tolua_S,1,0);
  if (!self) TOLUA_ERR_SELF;
  tolua_pushnumber(tolua_S,(long)self->sval);
 return 1;
}

/* set function: sval of class  object_type */
static int toluaI_set_object_object_type_sval(lua_State* tolua_S)
{
  object_type* self = (object_type*)  tolua_getusertype(tolua_S,1,0);
  if (!self) TOLUA_ERR_SELF;
  if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
   TOLUA_ERR_ASSIGN;
  self->sval = ((byte)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: pval of class  object_type */
static int toluaI_get_object_object_type_pval(lua_State* tolua_S)
{
  object_type* self = (object_type*)  tolua_getusertype(tolua_S,1,0);
  if (!self) TOLUA_ERR_SELF;
  tolua_pushnumber(tolua_S,(long)self->pval);
 return 1;
}

/* set function: pval of class  object_type */
static int toluaI_set_object_object_type_pval(lua_State* tolua_S)
{
  object_type* self = (object_type*)  tolua_getusertype(tolua_S,1,0);
  if (!self) TOLUA_ERR_SELF;
  if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
   TOLUA_ERR_ASSIGN;
  self->pval = ((s16b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: discount of class  object_type */
static int toluaI_get_object_object_type_discount(lua_State* tolua_S)
{
  object_type* self = (object_type*)  tolua_getusertype(tolua_S,1,0);
  if (!self) TOLUA_ERR_SELF;
  tolua_pushnumber(tolua_S,(long)self->discount);
 return 1;
}

/* set function: discount of class  object_type */
static int toluaI_set_object_object_type_discount(lua_State* tolua_S)
{
  object_type* self = (object_type*)  tolua_getusertype(tolua_S,1,0);
  if (!self) TOLUA_ERR_SELF;
  if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
   TOLUA_ERR_ASSIGN;
  self->discount = ((byte)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: number of class  object_type */
static int toluaI_get_object_object_type_number(lua_State* tolua_S)
{
  object_type* self = (object_type*)  tolua_getusertype(tolua_S,1,0);
  if (!self) TOLUA_ERR_SELF;
  tolua_pushnumber(tolua_S,(long)self->number);
 return 1;
}

/* set function: number of class  object_type */
static int toluaI_set_object_object_type_number(lua_State* tolua_S)
{
  object_type* self = (object_type*)  tolua_getusertype(tolua_S,1,0);
  if (!self) TOLUA_ERR_SELF;
  if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
   TOLUA_ERR_ASSIGN;
  self->number = ((byte)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: weight of class  object_type */
static int toluaI_get_object_object_type_weight(lua_State* tolua_S)
{
  object_type* self = (object_type*)  tolua_getusertype(tolua_S,1,0);
  if (!self) TOLUA_ERR_SELF;
  tolua_pushnumber(tolua_S,(long)self->weight);
 return 1;
}

/* set function: weight of class  object_type */
static int toluaI_set_object_object_type_weight(lua_State* tolua_S)
{
  object_type* self = (object_type*)  tolua_getusertype(tolua_S,1,0);
  if (!self) TOLUA_ERR_SELF;
  if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
   TOLUA_ERR_ASSIGN;
  self->weight = ((s16b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: to_h of class  object_type */
static int toluaI_get_object_object_type_to_h(lua_State* tolua_S)
{
  object_type* self = (object_type*)  tolua_getusertype(tolua_S,1,0);
  if (!self) TOLUA_ERR_SELF;
  tolua_pushnumber(tolua_S,(long)self->to_h);
 return 1;
}

/* set function: to_h of class  object_type */
static int toluaI_set_object_object_type_to_h(lua_State* tolua_S)
{
  object_type* self = (object_type*)  tolua_getusertype(tolua_S,1,0);
  if (!self) TOLUA_ERR_SELF;
  if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
   TOLUA_ERR_ASSIGN;
  self->to_h = ((s16b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: to_d of class  object_type */
static int toluaI_get_object_object_type_to_d(lua_State* tolua_S)
{
  object_type* self = (object_type*)  tolua_getusertype(tolua_S,1,0);
  if (!self) TOLUA_ERR_SELF;
  tolua_pushnumber(tolua_S,(long)self->to_d);
 return 1;
}

/* set function: to_d of class  object_type */
static int toluaI_set_object_object_type_to_d(lua_State* tolua_S)
{
  object_type* self = (object_type*)  tolua_getusertype(tolua_S,1,0);
  if (!self) TOLUA_ERR_SELF;
  if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
   TOLUA_ERR_ASSIGN;
  self->to_d = ((s16b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: to_a of class  object_type */
static int toluaI_get_object_object_type_to_a(lua_State* tolua_S)
{
  object_type* self = (object_type*)  tolua_getusertype(tolua_S,1,0);
  if (!self) TOLUA_ERR_SELF;
  tolua_pushnumber(tolua_S,(long)self->to_a);
 return 1;
}

/* set function: to_a of class  object_type */
static int toluaI_set_object_object_type_to_a(lua_State* tolua_S)
{
  object_type* self = (object_type*)  tolua_getusertype(tolua_S,1,0);
  if (!self) TOLUA_ERR_SELF;
  if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
   TOLUA_ERR_ASSIGN;
  self->to_a = ((s16b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: ac of class  object_type */
static int toluaI_get_object_object_type_ac(lua_State* tolua_S)
{
  object_type* self = (object_type*)  tolua_getusertype(tolua_S,1,0);
  if (!self) TOLUA_ERR_SELF;
  tolua_pushnumber(tolua_S,(long)self->ac);
 return 1;
}

/* set function: ac of class  object_type */
static int toluaI_set_object_object_type_ac(lua_State* tolua_S)
{
  object_type* self = (object_type*)  tolua_getusertype(tolua_S,1,0);
  if (!self) TOLUA_ERR_SELF;
  if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
   TOLUA_ERR_ASSIGN;
  self->ac = ((s16b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: timeout of class  object_type */
static int toluaI_get_object_object_type_timeout(lua_State* tolua_S)
{
  object_type* self = (object_type*)  tolua_getusertype(tolua_S,1,0);
  if (!self) TOLUA_ERR_SELF;
  tolua_pushnumber(tolua_S,(long)self->timeout);
 return 1;
}

/* set function: timeout of class  object_type */
static int toluaI_set_object_object_type_timeout(lua_State* tolua_S)
{
  object_type* self = (object_type*)  tolua_getusertype(tolua_S,1,0);
  if (!self) TOLUA_ERR_SELF;
  if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
   TOLUA_ERR_ASSIGN;
  self->timeout = ((s16b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: dd of class  object_type */
static int toluaI_get_object_object_type_dd(lua_State* tolua_S)
{
  object_type* self = (object_type*)  tolua_getusertype(tolua_S,1,0);
  if (!self) TOLUA_ERR_SELF;
  tolua_pushnumber(tolua_S,(long)self->dd);
 return 1;
}

/* set function: dd of class  object_type */
static int toluaI_set_object_object_type_dd(lua_State* tolua_S)
{
  object_type* self = (object_type*)  tolua_getusertype(tolua_S,1,0);
  if (!self) TOLUA_ERR_SELF;
  if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
   TOLUA_ERR_ASSIGN;
  self->dd = ((byte)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: ds of class  object_type */
static int toluaI_get_object_object_type_ds(lua_State* tolua_S)
{
  object_type* self = (object_type*)  tolua_getusertype(tolua_S,1,0);
  if (!self) TOLUA_ERR_SELF;
  tolua_pushnumber(tolua_S,(long)self->ds);
 return 1;
}

/* set function: ds of class  object_type */
static int toluaI_set_object_object_type_ds(lua_State* tolua_S)
{
  object_type* self = (object_type*)  tolua_getusertype(tolua_S,1,0);
  if (!self) TOLUA_ERR_SELF;
  if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
   TOLUA_ERR_ASSIGN;
  self->ds = ((byte)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: inscription of class  object_type */
static int toluaI_get_object_object_type_inscription(lua_State* tolua_S)
{
  object_type* self = (object_type*)  tolua_getusertype(tolua_S,1,0);
  if (!self) TOLUA_ERR_SELF;
  tolua_pushnumber(tolua_S,(long)self->inscription);
 return 1;
}

/* set function: inscription of class  object_type */
static int toluaI_set_object_object_type_inscription(lua_State* tolua_S)
{
  object_type* self = (object_type*)  tolua_getusertype(tolua_S,1,0);
  if (!self) TOLUA_ERR_SELF;
  if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
   TOLUA_ERR_ASSIGN;
  self->inscription = ((u16b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: xtra_name of class  object_type */
static int toluaI_get_object_object_type_xtra_name(lua_State* tolua_S)
{
  object_type* self = (object_type*)  tolua_getusertype(tolua_S,1,0);
  if (!self) TOLUA_ERR_SELF;
  tolua_pushnumber(tolua_S,(long)self->xtra_name);
 return 1;
}

/* set function: xtra_name of class  object_type */
static int toluaI_set_object_object_type_xtra_name(lua_State* tolua_S)
{
  object_type* self = (object_type*)  tolua_getusertype(tolua_S,1,0);
  if (!self) TOLUA_ERR_SELF;
  if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
   TOLUA_ERR_ASSIGN;
  self->xtra_name = ((u16b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: flags of class  object_type */
static int toluaI_get_object_object_type_flags(lua_State* tolua_S)
{
 int toluaI_index;
  object_type* self;
 lua_pushstring(tolua_S,".self");
 lua_rawget(tolua_S,1);
 self = (object_type*)  lua_touserdata(tolua_S,-1);
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
  tolua_error(tolua_S,"invalid type in array indexing.");
 toluaI_index = (int)tolua_getnumber(tolua_S,2,0);
 if (toluaI_index<0 || toluaI_index>=4)
  tolua_error(tolua_S,"array object_type: flags indexing out of range.");
 tolua_pushnumber(tolua_S,(long)self->flags[toluaI_index]);
 return 1;
}

/* set function: flags of class  object_type */
static int toluaI_set_object_object_type_flags(lua_State* tolua_S)
{
 int toluaI_index;
  object_type* self;
 lua_pushstring(tolua_S,".self");
 lua_rawget(tolua_S,1);
 self = (object_type*)  lua_touserdata(tolua_S,-1);
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
  tolua_error(tolua_S,"invalid type in array indexing.");
 toluaI_index = (int)tolua_getnumber(tolua_S,2,0);
 if (toluaI_index<0 || toluaI_index>=4)
  tolua_error(tolua_S,"array object_type: flags indexing out of range.");
  self->flags[toluaI_index] = ((u32b)  tolua_getnumber(tolua_S,3,0));
 return 0;
}

/* get function: kn_flags of class  object_type */
static int toluaI_get_object_object_type_kn_flags(lua_State* tolua_S)
{
 int toluaI_index;
  object_type* self;
 lua_pushstring(tolua_S,".self");
 lua_rawget(tolua_S,1);
 self = (object_type*)  lua_touserdata(tolua_S,-1);
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
  tolua_error(tolua_S,"invalid type in array indexing.");
 toluaI_index = (int)tolua_getnumber(tolua_S,2,0);
 if (toluaI_index<0 || toluaI_index>=4)
  tolua_error(tolua_S,"array object_type: kn_flags indexing out of range.");
 tolua_pushnumber(tolua_S,(long)self->kn_flags[toluaI_index]);
 return 1;
}

/* set function: kn_flags of class  object_type */
static int toluaI_set_object_object_type_kn_flags(lua_State* tolua_S)
{
 int toluaI_index;
  object_type* self;
 lua_pushstring(tolua_S,".self");
 lua_rawget(tolua_S,1);
 self = (object_type*)  lua_touserdata(tolua_S,-1);
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
  tolua_error(tolua_S,"invalid type in array indexing.");
 toluaI_index = (int)tolua_getnumber(tolua_S,2,0);
 if (toluaI_index<0 || toluaI_index>=4)
  tolua_error(tolua_S,"array object_type: kn_flags indexing out of range.");
  self->kn_flags[toluaI_index] = ((u32b)  tolua_getnumber(tolua_S,3,0));
 return 0;
}

/* get function: next_o_idx of class  object_type */
static int toluaI_get_object_object_type_next_o_idx(lua_State* tolua_S)
{
  object_type* self = (object_type*)  tolua_getusertype(tolua_S,1,0);
  if (!self) TOLUA_ERR_SELF;
  tolua_pushnumber(tolua_S,(long)self->next_o_idx);
 return 1;
}

/* set function: next_o_idx of class  object_type */
static int toluaI_set_object_object_type_next_o_idx(lua_State* tolua_S)
{
  object_type* self = (object_type*)  tolua_getusertype(tolua_S,1,0);
  if (!self) TOLUA_ERR_SELF;
  if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
   TOLUA_ERR_ASSIGN;
  self->next_o_idx = ((s16b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: cost of class  object_type */
static int toluaI_get_object_object_type_cost(lua_State* tolua_S)
{
  object_type* self = (object_type*)  tolua_getusertype(tolua_S,1,0);
  if (!self) TOLUA_ERR_SELF;
  tolua_pushnumber(tolua_S,(long)self->cost);
 return 1;
}

/* set function: cost of class  object_type */
static int toluaI_set_object_object_type_cost(lua_State* tolua_S)
{
  object_type* self = (object_type*)  tolua_getusertype(tolua_S,1,0);
  if (!self) TOLUA_ERR_SELF;
  if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
   TOLUA_ERR_ASSIGN;
  self->cost = ((s32b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: feeling of class  object_type */
static int toluaI_get_object_object_type_feeling(lua_State* tolua_S)
{
  object_type* self = (object_type*)  tolua_getusertype(tolua_S,1,0);
  if (!self) TOLUA_ERR_SELF;
  tolua_pushnumber(tolua_S,(long)self->feeling);
 return 1;
}

/* set function: feeling of class  object_type */
static int toluaI_set_object_object_type_feeling(lua_State* tolua_S)
{
  object_type* self = (object_type*)  tolua_getusertype(tolua_S,1,0);
  if (!self) TOLUA_ERR_SELF;
  if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
   TOLUA_ERR_ASSIGN;
  self->feeling = ((byte)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: a_idx of class  object_type */
static int toluaI_get_object_object_type_a_idx(lua_State* tolua_S)
{
  object_type* self = (object_type*)  tolua_getusertype(tolua_S,1,0);
  if (!self) TOLUA_ERR_SELF;
  tolua_pushnumber(tolua_S,(long)self->a_idx);
 return 1;
}

/* set function: a_idx of class  object_type */
static int toluaI_set_object_object_type_a_idx(lua_State* tolua_S)
{
  object_type* self = (object_type*)  tolua_getusertype(tolua_S,1,0);
  if (!self) TOLUA_ERR_SELF;
  if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
   TOLUA_ERR_ASSIGN;
  self->a_idx = ((byte)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: info of class  object_type */
static int toluaI_get_object_object_type_info(lua_State* tolua_S)
{
  object_type* self = (object_type*)  tolua_getusertype(tolua_S,1,0);
  if (!self) TOLUA_ERR_SELF;
  tolua_pushnumber(tolua_S,(long)self->info);
 return 1;
}

/* set function: info of class  object_type */
static int toluaI_set_object_object_type_info(lua_State* tolua_S)
{
  object_type* self = (object_type*)  tolua_getusertype(tolua_S,1,0);
  if (!self) TOLUA_ERR_SELF;
  if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
   TOLUA_ERR_ASSIGN;
  self->info = ((byte)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: allocated of class  object_type */
static int toluaI_get_object_object_type_allocated(lua_State* tolua_S)
{
  object_type* self = (object_type*)  tolua_getusertype(tolua_S,1,0);
  if (!self) TOLUA_ERR_SELF;
  tolua_pushbool(tolua_S,(int)self->allocated);
 return 1;
}

/* set function: allocated of class  object_type */
static int toluaI_set_object_object_type_allocated(lua_State* tolua_S)
{
  object_type* self = (object_type*)  tolua_getusertype(tolua_S,1,0);
  if (!self) TOLUA_ERR_SELF;
  if (!tolua_istype(tolua_S,2,tolua_tag(tolua_S,"bool"),0))
   TOLUA_ERR_ASSIGN;
  self->allocated = ((bool)  tolua_getbool(tolua_S,2,0));
 return 0;
}

/* get function: o_max */
static int toluaI_get_object_o_max(lua_State* tolua_S)
{
  tolua_pushnumber(tolua_S,(long)o_max);
 return 1;
}

/* set function: o_max */
static int toluaI_set_object_o_max(lua_State* tolua_S)
{
  if (!tolua_istype(tolua_S,1,LUA_TNUMBER,0))
   TOLUA_ERR_ASSIGN;
  o_max = ((s16b)  tolua_getnumber(tolua_S,1,0));
 return 0;
}

/* get function: o_cnt */
static int toluaI_get_object_o_cnt(lua_State* tolua_S)
{
  tolua_pushnumber(tolua_S,(long)o_cnt);
 return 1;
}

/* set function: o_cnt */
static int toluaI_set_object_o_cnt(lua_State* tolua_S)
{
  if (!tolua_istype(tolua_S,1,LUA_TNUMBER,0))
   TOLUA_ERR_ASSIGN;
  o_cnt = ((s16b)  tolua_getnumber(tolua_S,1,0));
 return 0;
}

/* get function: o_list */
static int toluaI_get_object_o_list(lua_State* tolua_S)
{
 int toluaI_index;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
  tolua_error(tolua_S,"invalid type in array indexing.");
 toluaI_index = (int)tolua_getnumber(tolua_S,2,0);
 if (toluaI_index<0 || toluaI_index>=o_max)
  tolua_error(tolua_S,"array: o_list indexing out of range.");
 tolua_pushusertype(tolua_S,(void*)&o_list[toluaI_index],tolua_tag(tolua_S,"object_type"));
 return 1;
}

/* set function: o_list */
static int toluaI_set_object_o_list(lua_State* tolua_S)
{
 int toluaI_index;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
  tolua_error(tolua_S,"invalid type in array indexing.");
 toluaI_index = (int)tolua_getnumber(tolua_S,2,0);
 if (toluaI_index<0 || toluaI_index>=o_max)
  tolua_error(tolua_S,"array: o_list indexing out of range.");
  o_list[toluaI_index] = *((object_type*)  tolua_getusertype(tolua_S,3,0));
 return 0;
}

/* get function: k_info */
static int toluaI_get_object_k_info(lua_State* tolua_S)
{
 int toluaI_index;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
  tolua_error(tolua_S,"invalid type in array indexing.");
 toluaI_index = (int)tolua_getnumber(tolua_S,2,0);
 if (toluaI_index<0 || toluaI_index>=z_info->k_max)
  tolua_error(tolua_S,"array: k_info indexing out of range.");
 tolua_pushusertype(tolua_S,(void*)&k_info[toluaI_index],tolua_tag(tolua_S,"object_kind"));
 return 1;
}

/* set function: k_info */
static int toluaI_set_object_k_info(lua_State* tolua_S)
{
 int toluaI_index;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
  tolua_error(tolua_S,"invalid type in array indexing.");
 toluaI_index = (int)tolua_getnumber(tolua_S,2,0);
 if (toluaI_index<0 || toluaI_index>=z_info->k_max)
  tolua_error(tolua_S,"array: k_info indexing out of range.");
  k_info[toluaI_index] = *((object_kind*)  tolua_getusertype(tolua_S,3,0));
 return 0;
}

/* get function: k_name */
static int toluaI_get_object_k_name(lua_State* tolua_S)
{
  tolua_pushstring(tolua_S,(const char*)k_name);
 return 1;
}

/* set function: k_name */
static int toluaI_set_object_k_name(lua_State* tolua_S)
{
  if (!tolua_istype(tolua_S,1,LUA_TSTRING,0))
   TOLUA_ERR_ASSIGN;
  k_name = ((char*)  tolua_getstring(tolua_S,1,0));
 return 0;
}

/* get function: k_text */
static int toluaI_get_object_k_text(lua_State* tolua_S)
{
  tolua_pushstring(tolua_S,(const char*)k_text);
 return 1;
}

/* set function: k_text */
static int toluaI_set_object_k_text(lua_State* tolua_S)
{
  if (!tolua_istype(tolua_S,1,LUA_TSTRING,0))
   TOLUA_ERR_ASSIGN;
  k_text = ((char*)  tolua_getstring(tolua_S,1,0));
 return 0;
}

/* get function: a_name */
static int toluaI_get_object_a_name(lua_State* tolua_S)
{
  tolua_pushstring(tolua_S,(const char*)a_name);
 return 1;
}

/* set function: a_name */
static int toluaI_set_object_a_name(lua_State* tolua_S)
{
  if (!tolua_istype(tolua_S,1,LUA_TSTRING,0))
   TOLUA_ERR_ASSIGN;
  a_name = ((char*)  tolua_getstring(tolua_S,1,0));
 return 0;
}

/* get function: a_text */
static int toluaI_get_object_a_text(lua_State* tolua_S)
{
  tolua_pushstring(tolua_S,(const char*)a_text);
 return 1;
}

/* set function: a_text */
static int toluaI_set_object_a_text(lua_State* tolua_S)
{
  if (!tolua_istype(tolua_S,1,LUA_TSTRING,0))
   TOLUA_ERR_ASSIGN;
  a_text = ((char*)  tolua_getstring(tolua_S,1,0));
 return 0;
}

/* function: reset_visuals */
static int toluaI_object_reset_visuals00(lua_State* tolua_S)
{
 if (
     !tolua_isnoobj(tolua_S,1)
 )
 {
  tolua_error(tolua_S,"#ferror in function 'reset_visuals'.");
  return 0;
 }
 else
 {
  {
   reset_visuals();
  }
 }
 return 0;
}

/* function: identify_fully_aux */
static int toluaI_object_identify_fully_aux00(lua_State* tolua_S)
{
 if (
     !tolua_istype(tolua_S,1,tolua_tag(tolua_S,"const object_type"),0) ||
     !tolua_isnoobj(tolua_S,2)
 )
 {
  tolua_error(tolua_S,"#ferror in function 'identify_fully_aux'.");
  return 0;
 }
 else
 {
  const object_type* o_ptr = ((const object_type*)  tolua_getusertype(tolua_S,1,0));
  {
   bool toluaI_ret = (bool)  identify_fully_aux(o_ptr);
   tolua_pushbool(tolua_S,(int)toluaI_ret);
  }
 }
 return 1;
}

/* function: wield_slot */
static int toluaI_object_wield_slot00(lua_State* tolua_S)
{
 if (
     !tolua_istype(tolua_S,1,tolua_tag(tolua_S,"const object_type"),0) ||
     !tolua_isnoobj(tolua_S,2)
 )
 {
  tolua_error(tolua_S,"#ferror in function 'wield_slot'.");
  return 0;
 }
 else
 {
  const object_type* o_ptr = ((const object_type*)  tolua_getusertype(tolua_S,1,0));
  {
   s16b toluaI_ret = (s16b)  wield_slot(o_ptr);
   tolua_pushnumber(tolua_S,(long)toluaI_ret);
  }
 }
 return 1;
}

/* function: mention_use */
static int toluaI_object_mention_use00(lua_State* tolua_S)
{
 if (
     !tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
     !tolua_isnoobj(tolua_S,2)
 )
 {
  tolua_error(tolua_S,"#ferror in function 'mention_use'.");
  return 0;
 }
 else
 {
  int i = ((int)  tolua_getnumber(tolua_S,1,0));
  {
   cptr toluaI_ret = (cptr)  mention_use(i);
   tolua_pushstring(tolua_S,(const char*)toluaI_ret);
  }
 }
 return 1;
}

/* function: describe_use */
static int toluaI_object_describe_use00(lua_State* tolua_S)
{
 if (
     !tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
     !tolua_isnoobj(tolua_S,2)
 )
 {
  tolua_error(tolua_S,"#ferror in function 'describe_use'.");
  return 0;
 }
 else
 {
  int i = ((int)  tolua_getnumber(tolua_S,1,0));
  {
   cptr toluaI_ret = (cptr)  describe_use(i);
   tolua_pushstring(tolua_S,(const char*)toluaI_ret);
  }
 }
 return 1;
}

/* function: item_charges */
static int toluaI_object_item_charges00(lua_State* tolua_S)
{
 if (
     !tolua_istype(tolua_S,1,tolua_tag(tolua_S,"object_type"),0) ||
     !tolua_isnoobj(tolua_S,2)
 )
 {
  tolua_error(tolua_S,"#ferror in function 'item_charges'.");
  return 0;
 }
 else
 {
  object_type* o_ptr = ((object_type*)  tolua_getusertype(tolua_S,1,0));
  {
   item_charges(o_ptr);
  }
 }
 return 0;
}

/* function: item_describe */
static int toluaI_object_item_describe00(lua_State* tolua_S)
{
 if (
     !tolua_istype(tolua_S,1,tolua_tag(tolua_S,"object_type"),0) ||
     !tolua_isnoobj(tolua_S,2)
 )
 {
  tolua_error(tolua_S,"#ferror in function 'item_describe'.");
  return 0;
 }
 else
 {
  object_type* o_ptr = ((object_type*)  tolua_getusertype(tolua_S,1,0));
  {
   item_describe(o_ptr);
  }
 }
 return 0;
}

/* function: item_split */
static int toluaI_object_item_split00(lua_State* tolua_S)
{
 if (
     !tolua_istype(tolua_S,1,tolua_tag(tolua_S,"object_type"),0) ||
     !tolua_istype(tolua_S,2,LUA_TNUMBER,0) ||
     !tolua_isnoobj(tolua_S,3)
 )
 {
  tolua_error(tolua_S,"#ferror in function 'item_split'.");
  return 0;
 }
 else
 {
  object_type* o_ptr = ((object_type*)  tolua_getusertype(tolua_S,1,0));
  int num = ((int)  tolua_getnumber(tolua_S,2,0));
  {
   object_type* toluaI_ret = (object_type*)  item_split(o_ptr,num);
   tolua_pushusertype(tolua_S,(void*)toluaI_ret,tolua_tag(tolua_S,"object_type"));
  }
 }
 return 1;
}

/* function: item_increase */
static int toluaI_object_item_increase00(lua_State* tolua_S)
{
 if (
     !tolua_istype(tolua_S,1,tolua_tag(tolua_S,"object_type"),0) ||
     !tolua_istype(tolua_S,2,LUA_TNUMBER,0) ||
     !tolua_isnoobj(tolua_S,3)
 )
 {
  tolua_error(tolua_S,"#ferror in function 'item_increase'.");
  return 0;
 }
 else
 {
  object_type* o_ptr = ((object_type*)  tolua_getusertype(tolua_S,1,0));
  int num = ((int)  tolua_getnumber(tolua_S,2,0));
  {
   item_increase(o_ptr,num);
  }
 }
 return 0;
}

/* function: inven_carry_okay */
static int toluaI_object_inven_carry_okay00(lua_State* tolua_S)
{
 if (
     !tolua_istype(tolua_S,1,tolua_tag(tolua_S,"const object_type"),0) ||
     !tolua_isnoobj(tolua_S,2)
 )
 {
  tolua_error(tolua_S,"#ferror in function 'inven_carry_okay'.");
  return 0;
 }
 else
 {
  const object_type* o_ptr = ((const object_type*)  tolua_getusertype(tolua_S,1,0));
  {
   bool toluaI_ret = (bool)  inven_carry_okay(o_ptr);
   tolua_pushbool(tolua_S,(int)toluaI_ret);
  }
 }
 return 1;
}

/* function: inven_carry */
static int toluaI_object_inven_carry00(lua_State* tolua_S)
{
 if (
     !tolua_istype(tolua_S,1,tolua_tag(tolua_S,"object_type"),0) ||
     !tolua_isnoobj(tolua_S,2)
 )
 {
  tolua_error(tolua_S,"#ferror in function 'inven_carry'.");
  return 0;
 }
 else
 {
  object_type* o_ptr = ((object_type*)  tolua_getusertype(tolua_S,1,0));
  {
   object_type* toluaI_ret = (object_type*)  inven_carry(o_ptr);
   tolua_pushusertype(tolua_S,(void*)toluaI_ret,tolua_tag(tolua_S,"object_type"));
  }
 }
 return 1;
}

/* function: inven_takeoff */
static int toluaI_object_inven_takeoff00(lua_State* tolua_S)
{
 if (
     !tolua_istype(tolua_S,1,tolua_tag(tolua_S,"object_type"),0) ||
     !tolua_isnoobj(tolua_S,2)
 )
 {
  tolua_error(tolua_S,"#ferror in function 'inven_takeoff'.");
  return 0;
 }
 else
 {
  object_type* o_ptr = ((object_type*)  tolua_getusertype(tolua_S,1,0));
  {
   object_type* toluaI_ret = (object_type*)  inven_takeoff(o_ptr);
   tolua_pushusertype(tolua_S,(void*)toluaI_ret,tolua_tag(tolua_S,"object_type"));
  }
 }
 return 1;
}

/* function: inven_drop */
static int toluaI_object_inven_drop00(lua_State* tolua_S)
{
 if (
     !tolua_istype(tolua_S,1,tolua_tag(tolua_S,"object_type"),0) ||
     !tolua_istype(tolua_S,2,LUA_TNUMBER,0) ||
     !tolua_isnoobj(tolua_S,3)
 )
 {
  tolua_error(tolua_S,"#ferror in function 'inven_drop'.");
  return 0;
 }
 else
 {
  object_type* o_ptr = ((object_type*)  tolua_getusertype(tolua_S,1,0));
  int amt = ((int)  tolua_getnumber(tolua_S,2,0));
  {
   inven_drop(o_ptr,amt);
  }
 }
 return 0;
}

/* function: item_tester_hook_weapon */
static int toluaI_object_item_tester_hook_weapon00(lua_State* tolua_S)
{
 if (
     !tolua_istype(tolua_S,1,tolua_tag(tolua_S,"const object_type"),0) ||
     !tolua_isnoobj(tolua_S,2)
 )
 {
  tolua_error(tolua_S,"#ferror in function 'item_tester_hook_weapon'.");
  return 0;
 }
 else
 {
  const object_type* o_ptr = ((const object_type*)  tolua_getusertype(tolua_S,1,0));
  {
   bool toluaI_ret = (bool)  item_tester_hook_weapon(o_ptr);
   tolua_pushbool(tolua_S,(int)toluaI_ret);
  }
 }
 return 1;
}

/* function: item_tester_hook_melee_weapon */
static int toluaI_object_item_tester_hook_melee_weapon00(lua_State* tolua_S)
{
 if (
     !tolua_istype(tolua_S,1,tolua_tag(tolua_S,"const object_type"),0) ||
     !tolua_isnoobj(tolua_S,2)
 )
 {
  tolua_error(tolua_S,"#ferror in function 'item_tester_hook_melee_weapon'.");
  return 0;
 }
 else
 {
  const object_type* o_ptr = ((const object_type*)  tolua_getusertype(tolua_S,1,0));
  {
   bool toluaI_ret = (bool)  item_tester_hook_melee_weapon(o_ptr);
   tolua_pushbool(tolua_S,(int)toluaI_ret);
  }
 }
 return 1;
}

/* function: item_tester_hook_nonsword */
static int toluaI_object_item_tester_hook_nonsword00(lua_State* tolua_S)
{
 if (
     !tolua_istype(tolua_S,1,tolua_tag(tolua_S,"const object_type"),0) ||
     !tolua_isnoobj(tolua_S,2)
 )
 {
  tolua_error(tolua_S,"#ferror in function 'item_tester_hook_nonsword'.");
  return 0;
 }
 else
 {
  const object_type* o_ptr = ((const object_type*)  tolua_getusertype(tolua_S,1,0));
  {
   bool toluaI_ret = (bool)  item_tester_hook_nonsword(o_ptr);
   tolua_pushbool(tolua_S,(int)toluaI_ret);
  }
 }
 return 1;
}

/* function: item_tester_hook_ammo */
static int toluaI_object_item_tester_hook_ammo00(lua_State* tolua_S)
{
 if (
     !tolua_istype(tolua_S,1,tolua_tag(tolua_S,"const object_type"),0) ||
     !tolua_isnoobj(tolua_S,2)
 )
 {
  tolua_error(tolua_S,"#ferror in function 'item_tester_hook_ammo'.");
  return 0;
 }
 else
 {
  const object_type* o_ptr = ((const object_type*)  tolua_getusertype(tolua_S,1,0));
  {
   bool toluaI_ret = (bool)  item_tester_hook_ammo(o_ptr);
   tolua_pushbool(tolua_S,(int)toluaI_ret);
  }
 }
 return 1;
}

/* function: item_tester_hook_fletcher */
static int toluaI_object_item_tester_hook_fletcher00(lua_State* tolua_S)
{
 if (
     !tolua_istype(tolua_S,1,tolua_tag(tolua_S,"const object_type"),0) ||
     !tolua_isnoobj(tolua_S,2)
 )
 {
  tolua_error(tolua_S,"#ferror in function 'item_tester_hook_fletcher'.");
  return 0;
 }
 else
 {
  const object_type* o_ptr = ((const object_type*)  tolua_getusertype(tolua_S,1,0));
  {
   bool toluaI_ret = (bool)  item_tester_hook_fletcher(o_ptr);
   tolua_pushbool(tolua_S,(int)toluaI_ret);
  }
 }
 return 1;
}

/* function: item_tester_hook_armour */
static int toluaI_object_item_tester_hook_armour00(lua_State* tolua_S)
{
 if (
     !tolua_istype(tolua_S,1,tolua_tag(tolua_S,"const object_type"),0) ||
     !tolua_isnoobj(tolua_S,2)
 )
 {
  tolua_error(tolua_S,"#ferror in function 'item_tester_hook_armour'.");
  return 0;
 }
 else
 {
  const object_type* o_ptr = ((const object_type*)  tolua_getusertype(tolua_S,1,0));
  {
   bool toluaI_ret = (bool)  item_tester_hook_armour(o_ptr);
   tolua_pushbool(tolua_S,(int)toluaI_ret);
  }
 }
 return 1;
}

/* function: item_tester_hook_soft_armour */
static int toluaI_object_item_tester_hook_soft_armour00(lua_State* tolua_S)
{
 if (
     !tolua_istype(tolua_S,1,tolua_tag(tolua_S,"const object_type"),0) ||
     !tolua_isnoobj(tolua_S,2)
 )
 {
  tolua_error(tolua_S,"#ferror in function 'item_tester_hook_soft_armour'.");
  return 0;
 }
 else
 {
  const object_type* o_ptr = ((const object_type*)  tolua_getusertype(tolua_S,1,0));
  {
   bool toluaI_ret = (bool)  item_tester_hook_soft_armour(o_ptr);
   tolua_pushbool(tolua_S,(int)toluaI_ret);
  }
 }
 return 1;
}

/* function: item_tester_hook_hard_armour */
static int toluaI_object_item_tester_hook_hard_armour00(lua_State* tolua_S)
{
 if (
     !tolua_istype(tolua_S,1,tolua_tag(tolua_S,"const object_type"),0) ||
     !tolua_isnoobj(tolua_S,2)
 )
 {
  tolua_error(tolua_S,"#ferror in function 'item_tester_hook_hard_armour'.");
  return 0;
 }
 else
 {
  const object_type* o_ptr = ((const object_type*)  tolua_getusertype(tolua_S,1,0));
  {
   bool toluaI_ret = (bool)  item_tester_hook_hard_armour(o_ptr);
   tolua_pushbool(tolua_S,(int)toluaI_ret);
  }
 }
 return 1;
}

/* function: item_tester_hook_helm */
static int toluaI_object_item_tester_hook_helm00(lua_State* tolua_S)
{
 if (
     !tolua_istype(tolua_S,1,tolua_tag(tolua_S,"const object_type"),0) ||
     !tolua_isnoobj(tolua_S,2)
 )
 {
  tolua_error(tolua_S,"#ferror in function 'item_tester_hook_helm'.");
  return 0;
 }
 else
 {
  const object_type* o_ptr = ((const object_type*)  tolua_getusertype(tolua_S,1,0));
  {
   bool toluaI_ret = (bool)  item_tester_hook_helm(o_ptr);
   tolua_pushbool(tolua_S,(int)toluaI_ret);
  }
 }
 return 1;
}

/* function: item_tester_hook_pure_hard_armour */
static int toluaI_object_item_tester_hook_pure_hard_armour00(lua_State* tolua_S)
{
 if (
     !tolua_istype(tolua_S,1,tolua_tag(tolua_S,"const object_type"),0) ||
     !tolua_isnoobj(tolua_S,2)
 )
 {
  tolua_error(tolua_S,"#ferror in function 'item_tester_hook_pure_hard_armour'.");
  return 0;
 }
 else
 {
  const object_type* o_ptr = ((const object_type*)  tolua_getusertype(tolua_S,1,0));
  {
   bool toluaI_ret = (bool)  item_tester_hook_pure_hard_armour(o_ptr);
   tolua_pushbool(tolua_S,(int)toluaI_ret);
  }
 }
 return 1;
}

/* function: item_tester_hook_weapon_armour */
static int toluaI_object_item_tester_hook_weapon_armour00(lua_State* tolua_S)
{
 if (
     !tolua_istype(tolua_S,1,tolua_tag(tolua_S,"const object_type"),0) ||
     !tolua_isnoobj(tolua_S,2)
 )
 {
  tolua_error(tolua_S,"#ferror in function 'item_tester_hook_weapon_armour'.");
  return 0;
 }
 else
 {
  const object_type* o_ptr = ((const object_type*)  tolua_getusertype(tolua_S,1,0));
  {
   bool toluaI_ret = (bool)  item_tester_hook_weapon_armour(o_ptr);
   tolua_pushbool(tolua_S,(int)toluaI_ret);
  }
 }
 return 1;
}

/* function: item_tester_hook_wear */
static int toluaI_object_item_tester_hook_wear00(lua_State* tolua_S)
{
 if (
     !tolua_istype(tolua_S,1,tolua_tag(tolua_S,"const object_type"),0) ||
     !tolua_isnoobj(tolua_S,2)
 )
 {
  tolua_error(tolua_S,"#ferror in function 'item_tester_hook_wear'.");
  return 0;
 }
 else
 {
  const object_type* o_ptr = ((const object_type*)  tolua_getusertype(tolua_S,1,0));
  {
   bool toluaI_ret = (bool)  item_tester_hook_wear(o_ptr);
   tolua_pushbool(tolua_S,(int)toluaI_ret);
  }
 }
 return 1;
}

/* function: item_tester_hook_recharge */
static int toluaI_object_item_tester_hook_recharge00(lua_State* tolua_S)
{
 if (
     !tolua_istype(tolua_S,1,tolua_tag(tolua_S,"const object_type"),0) ||
     !tolua_isnoobj(tolua_S,2)
 )
 {
  tolua_error(tolua_S,"#ferror in function 'item_tester_hook_recharge'.");
  return 0;
 }
 else
 {
  const object_type* o_ptr = ((const object_type*)  tolua_getusertype(tolua_S,1,0));
  {
   bool toluaI_ret = (bool)  item_tester_hook_recharge(o_ptr);
   tolua_pushbool(tolua_S,(int)toluaI_ret);
  }
 }
 return 1;
}

/* function: item_tester_hook_jewel */
static int toluaI_object_item_tester_hook_jewel00(lua_State* tolua_S)
{
 if (
     !tolua_istype(tolua_S,1,tolua_tag(tolua_S,"const object_type"),0) ||
     !tolua_isnoobj(tolua_S,2)
 )
 {
  tolua_error(tolua_S,"#ferror in function 'item_tester_hook_jewel'.");
  return 0;
 }
 else
 {
  const object_type* o_ptr = ((const object_type*)  tolua_getusertype(tolua_S,1,0));
  {
   bool toluaI_ret = (bool)  item_tester_hook_jewel(o_ptr);
   tolua_pushbool(tolua_S,(int)toluaI_ret);
  }
 }
 return 1;
}

/* function: item_tester_hook_tval */
static int toluaI_object_item_tester_hook_tval00(lua_State* tolua_S)
{
 if (
     !tolua_istype(tolua_S,1,tolua_tag(tolua_S,"const object_type"),0) ||
     !tolua_isnoobj(tolua_S,2)
 )
 {
  tolua_error(tolua_S,"#ferror in function 'item_tester_hook_tval'.");
  return 0;
 }
 else
 {
  const object_type* o_ptr = ((const object_type*)  tolua_getusertype(tolua_S,1,0));
  {
   bool toluaI_ret = (bool)  item_tester_hook_tval(o_ptr);
   tolua_pushbool(tolua_S,(int)toluaI_ret);
  }
 }
 return 1;
}

/* function: item_tester_hook_is_blessed */
static int toluaI_object_item_tester_hook_is_blessed00(lua_State* tolua_S)
{
 if (
     !tolua_istype(tolua_S,1,tolua_tag(tolua_S,"const object_type"),0) ||
     !tolua_isnoobj(tolua_S,2)
 )
 {
  tolua_error(tolua_S,"#ferror in function 'item_tester_hook_is_blessed'.");
  return 0;
 }
 else
 {
  const object_type* o_ptr = ((const object_type*)  tolua_getusertype(tolua_S,1,0));
  {
   bool toluaI_ret = (bool)  item_tester_hook_is_blessed(o_ptr);
   tolua_pushbool(tolua_S,(int)toluaI_ret);
  }
 }
 return 1;
}

/* function: item_tester_hook_is_good */
static int toluaI_object_item_tester_hook_is_good00(lua_State* tolua_S)
{
 if (
     !tolua_istype(tolua_S,1,tolua_tag(tolua_S,"const object_type"),0) ||
     !tolua_isnoobj(tolua_S,2)
 )
 {
  tolua_error(tolua_S,"#ferror in function 'item_tester_hook_is_good'.");
  return 0;
 }
 else
 {
  const object_type* o_ptr = ((const object_type*)  tolua_getusertype(tolua_S,1,0));
  {
   bool toluaI_ret = (bool)  item_tester_hook_is_good(o_ptr);
   tolua_pushbool(tolua_S,(int)toluaI_ret);
  }
 }
 return 1;
}

/* function: item_tester_hook_is_great */
static int toluaI_object_item_tester_hook_is_great00(lua_State* tolua_S)
{
 if (
     !tolua_istype(tolua_S,1,tolua_tag(tolua_S,"const object_type"),0) ||
     !tolua_isnoobj(tolua_S,2)
 )
 {
  tolua_error(tolua_S,"#ferror in function 'item_tester_hook_is_great'.");
  return 0;
 }
 else
 {
  const object_type* o_ptr = ((const object_type*)  tolua_getusertype(tolua_S,1,0));
  {
   bool toluaI_ret = (bool)  item_tester_hook_is_great(o_ptr);
   tolua_pushbool(tolua_S,(int)toluaI_ret);
  }
 }
 return 1;
}

/* function: item_tester_hook_is_book */
static int toluaI_object_item_tester_hook_is_book00(lua_State* tolua_S)
{
 if (
     !tolua_istype(tolua_S,1,tolua_tag(tolua_S,"const object_type"),0) ||
     !tolua_isnoobj(tolua_S,2)
 )
 {
  tolua_error(tolua_S,"#ferror in function 'item_tester_hook_is_book'.");
  return 0;
 }
 else
 {
  const object_type* o_ptr = ((const object_type*)  tolua_getusertype(tolua_S,1,0));
  {
   bool toluaI_ret = (bool)  item_tester_hook_is_book(o_ptr);
   tolua_pushbool(tolua_S,(int)toluaI_ret);
  }
 }
 return 1;
}

/* function: item_tester_okay */
static int toluaI_object_item_tester_okay00(lua_State* tolua_S)
{
 if (
     !tolua_istype(tolua_S,1,tolua_tag(tolua_S,"const object_type"),0) ||
     !tolua_isnoobj(tolua_S,2)
 )
 {
  tolua_error(tolua_S,"#ferror in function 'item_tester_okay'.");
  return 0;
 }
 else
 {
  const object_type* o_ptr = ((const object_type*)  tolua_getusertype(tolua_S,1,0));
  {
   bool toluaI_ret = (bool)  item_tester_okay(o_ptr);
   tolua_pushbool(tolua_S,(int)toluaI_ret);
  }
 }
 return 1;
}

/* function: display_inven */
static int toluaI_object_display_inven00(lua_State* tolua_S)
{
 if (
     !tolua_isnoobj(tolua_S,1)
 )
 {
  tolua_error(tolua_S,"#ferror in function 'display_inven'.");
  return 0;
 }
 else
 {
  {
   display_inven();
  }
 }
 return 0;
}

/* function: display_equip */
static int toluaI_object_display_equip00(lua_State* tolua_S)
{
 if (
     !tolua_isnoobj(tolua_S,1)
 )
 {
  tolua_error(tolua_S,"#ferror in function 'display_equip'.");
  return 0;
 }
 else
 {
  {
   display_equip();
  }
 }
 return 0;
}

/* function: show_list */
static int toluaI_object_show_list00(lua_State* tolua_S)
{
 if (
     !tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
     !tolua_isnoobj(tolua_S,2)
 )
 {
  tolua_error(tolua_S,"#ferror in function 'show_list'.");
  return 0;
 }
 else
 {
  s16b o_list_ptr = ((s16b)  tolua_getnumber(tolua_S,1,0));
  {
   show_list(o_list_ptr);
  }
 }
 return 0;
}

/* function: show_equip */
static int toluaI_object_show_equip00(lua_State* tolua_S)
{
 if (
     !tolua_isnoobj(tolua_S,1)
 )
 {
  tolua_error(tolua_S,"#ferror in function 'show_equip'.");
  return 0;
 }
 else
 {
  {
   show_equip();
  }
 }
 return 0;
}

/* function: toggle_inven_equip */
static int toluaI_object_toggle_inven_equip00(lua_State* tolua_S)
{
 if (
     !tolua_isnoobj(tolua_S,1)
 )
 {
  tolua_error(tolua_S,"#ferror in function 'toggle_inven_equip'.");
  return 0;
 }
 else
 {
  {
   toggle_inven_equip();
  }
 }
 return 0;
}

/* function: get_item */
static int toluaI_object_get_item00(lua_State* tolua_S)
{
 if (
     !tolua_istype(tolua_S,1,LUA_TSTRING,0) ||
     !tolua_istype(tolua_S,2,LUA_TSTRING,0) ||
     !tolua_istype(tolua_S,3,LUA_TNUMBER,0) ||
     !tolua_isnoobj(tolua_S,4)
 )
 {
  tolua_error(tolua_S,"#ferror in function 'get_item'.");
  return 0;
 }
 else
 {
  cptr pmt = ((cptr)  tolua_getstring(tolua_S,1,0));
  cptr str = ((cptr)  tolua_getstring(tolua_S,2,0));
  int mode = ((int)  tolua_getnumber(tolua_S,3,0));
  {
   object_type* toluaI_ret = (object_type*)  get_item(pmt,str,mode);
   tolua_pushusertype(tolua_S,(void*)toluaI_ret,tolua_tag(tolua_S,"object_type"));
  }
 }
 return 1;
}

/* function: delete_dungeon_object */
static int toluaI_object_delete_dungeon_object00(lua_State* tolua_S)
{
 if (
     !tolua_istype(tolua_S,1,tolua_tag(tolua_S,"object_type"),0) ||
     !tolua_isnoobj(tolua_S,2)
 )
 {
  tolua_error(tolua_S,"#ferror in function 'delete_dungeon_object'.");
  return 0;
 }
 else
 {
  object_type* o_ptr = ((object_type*)  tolua_getusertype(tolua_S,1,0));
  {
   delete_dungeon_object(o_ptr);
  }
 }
 return 0;
}

/* function: delete_object */
static int toluaI_object_delete_object00(lua_State* tolua_S)
{
 if (
     !tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
     !tolua_istype(tolua_S,2,LUA_TNUMBER,0) ||
     !tolua_isnoobj(tolua_S,3)
 )
 {
  tolua_error(tolua_S,"#ferror in function 'delete_object'.");
  return 0;
 }
 else
 {
  int x = ((int)  tolua_getnumber(tolua_S,1,0));
  int y = ((int)  tolua_getnumber(tolua_S,2,0));
  {
   delete_object(x,y);
  }
 }
 return 0;
}

/* function: delete_object_list */
static int toluaI_object_delete_object_list00(lua_State* tolua_S)
{
 if (
     !tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
     !tolua_isnoobj(tolua_S,2)
 )
 {
  tolua_error(tolua_S,"#ferror in function 'delete_object_list'.");
  return 0;
 }
 else
 {
  s16b o_idx_ptr = ((s16b)  tolua_getnumber(tolua_S,1,0));
  {
   delete_object_list(&o_idx_ptr);
   tolua_pushnumber(tolua_S,(long)o_idx_ptr);
  }
 }
 return 1;
}

/* function: drop_object_list */
static int toluaI_object_drop_object_list00(lua_State* tolua_S)
{
 if (
     !tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
     !tolua_istype(tolua_S,2,LUA_TNUMBER,0) ||
     !tolua_istype(tolua_S,3,LUA_TNUMBER,0) ||
     !tolua_isnoobj(tolua_S,4)
 )
 {
  tolua_error(tolua_S,"#ferror in function 'drop_object_list'.");
  return 0;
 }
 else
 {
  s16b o_idx_ptr = ((s16b)  tolua_getnumber(tolua_S,1,0));
  int x = ((int)  tolua_getnumber(tolua_S,2,0));
  int y = ((int)  tolua_getnumber(tolua_S,3,0));
  {
   drop_object_list(&o_idx_ptr,x,y);
   tolua_pushnumber(tolua_S,(long)o_idx_ptr);
  }
 }
 return 1;
}

/* function: object_known */
static int toluaI_object_object_known00(lua_State* tolua_S)
{
 if (
     !tolua_istype(tolua_S,1,tolua_tag(tolua_S,"object_type"),0) ||
     !tolua_isnoobj(tolua_S,2)
 )
 {
  tolua_error(tolua_S,"#ferror in function 'object_known'.");
  return 0;
 }
 else
 {
  object_type* o_ptr = ((object_type*)  tolua_getusertype(tolua_S,1,0));
  {
   object_known(o_ptr);
  }
 }
 return 0;
}

/* function: object_aware */
static int toluaI_object_object_aware00(lua_State* tolua_S)
{
 if (
     !tolua_istype(tolua_S,1,tolua_tag(tolua_S,"object_type"),0) ||
     !tolua_isnoobj(tolua_S,2)
 )
 {
  tolua_error(tolua_S,"#ferror in function 'object_aware'.");
  return 0;
 }
 else
 {
  object_type* o_ptr = ((object_type*)  tolua_getusertype(tolua_S,1,0));
  {
   object_aware(o_ptr);
  }
 }
 return 0;
}

/* function: object_tried */
static int toluaI_object_object_tried00(lua_State* tolua_S)
{
 if (
     !tolua_istype(tolua_S,1,tolua_tag(tolua_S,"object_type"),0) ||
     !tolua_isnoobj(tolua_S,2)
 )
 {
  tolua_error(tolua_S,"#ferror in function 'object_tried'.");
  return 0;
 }
 else
 {
  object_type* o_ptr = ((object_type*)  tolua_getusertype(tolua_S,1,0));
  {
   object_tried(o_ptr);
  }
 }
 return 0;
}

/* function: object_mental */
static int toluaI_object_object_mental00(lua_State* tolua_S)
{
 if (
     !tolua_istype(tolua_S,1,tolua_tag(tolua_S,"object_type"),0) ||
     !tolua_isnoobj(tolua_S,2)
 )
 {
  tolua_error(tolua_S,"#ferror in function 'object_mental'.");
  return 0;
 }
 else
 {
  object_type* o_ptr = ((object_type*)  tolua_getusertype(tolua_S,1,0));
  {
   object_mental(o_ptr);
  }
 }
 return 0;
}

/* function: flag_cost */
static int toluaI_object_flag_cost00(lua_State* tolua_S)
{
 if (
     !tolua_istype(tolua_S,1,tolua_tag(tolua_S,"const object_type"),0) ||
     !tolua_istype(tolua_S,2,LUA_TNUMBER,0) ||
     !tolua_isnoobj(tolua_S,3)
 )
 {
  tolua_error(tolua_S,"#ferror in function 'flag_cost'.");
  return 0;
 }
 else
 {
  const object_type* o_ptr = ((const object_type*)  tolua_getusertype(tolua_S,1,0));
  int plusses = ((int)  tolua_getnumber(tolua_S,2,0));
  {
   s32b toluaI_ret = (s32b)  flag_cost(o_ptr,plusses);
   tolua_pushnumber(tolua_S,(long)toluaI_ret);
  }
 }
 return 1;
}

/* function: object_value */
static int toluaI_object_object_value00(lua_State* tolua_S)
{
 if (
     !tolua_istype(tolua_S,1,tolua_tag(tolua_S,"const object_type"),0) ||
     !tolua_isnoobj(tolua_S,2)
 )
 {
  tolua_error(tolua_S,"#ferror in function 'object_value'.");
  return 0;
 }
 else
 {
  const object_type* o_ptr = ((const object_type*)  tolua_getusertype(tolua_S,1,0));
  {
   s32b toluaI_ret = (s32b)  object_value(o_ptr);
   tolua_pushnumber(tolua_S,(long)toluaI_ret);
  }
 }
 return 1;
}

/* function: object_value_real */
static int toluaI_object_object_value_real00(lua_State* tolua_S)
{
 if (
     !tolua_istype(tolua_S,1,tolua_tag(tolua_S,"const object_type"),0) ||
     !tolua_isnoobj(tolua_S,2)
 )
 {
  tolua_error(tolua_S,"#ferror in function 'object_value_real'.");
  return 0;
 }
 else
 {
  const object_type* o_ptr = ((const object_type*)  tolua_getusertype(tolua_S,1,0));
  {
   s32b toluaI_ret = (s32b)  object_value_real(o_ptr);
   tolua_pushnumber(tolua_S,(long)toluaI_ret);
  }
 }
 return 1;
}

/* function: distribute_charges */
static int toluaI_object_distribute_charges00(lua_State* tolua_S)
{
 if (
     !tolua_istype(tolua_S,1,tolua_tag(tolua_S,"object_type"),0) ||
     !tolua_istype(tolua_S,2,tolua_tag(tolua_S,"object_type"),0) ||
     !tolua_istype(tolua_S,3,LUA_TNUMBER,0) ||
     !tolua_isnoobj(tolua_S,4)
 )
 {
  tolua_error(tolua_S,"#ferror in function 'distribute_charges'.");
  return 0;
 }
 else
 {
  object_type* o_ptr = ((object_type*)  tolua_getusertype(tolua_S,1,0));
  object_type* q_ptr = ((object_type*)  tolua_getusertype(tolua_S,2,0));
  int amt = ((int)  tolua_getnumber(tolua_S,3,0));
  {
   distribute_charges(o_ptr,q_ptr,amt);
  }
 }
 return 0;
}

/* function: reduce_charges */
static int toluaI_object_reduce_charges00(lua_State* tolua_S)
{
 if (
     !tolua_istype(tolua_S,1,tolua_tag(tolua_S,"object_type"),0) ||
     !tolua_istype(tolua_S,2,LUA_TNUMBER,0) ||
     !tolua_isnoobj(tolua_S,3)
 )
 {
  tolua_error(tolua_S,"#ferror in function 'reduce_charges'.");
  return 0;
 }
 else
 {
  object_type* o_ptr = ((object_type*)  tolua_getusertype(tolua_S,1,0));
  int amt = ((int)  tolua_getnumber(tolua_S,2,0));
  {
   reduce_charges(o_ptr,amt);
  }
 }
 return 0;
}

/* function: object_similar */
static int toluaI_object_object_similar00(lua_State* tolua_S)
{
 if (
     !tolua_istype(tolua_S,1,tolua_tag(tolua_S,"const object_type"),0) ||
     !tolua_istype(tolua_S,2,tolua_tag(tolua_S,"const object_type"),0) ||
     !tolua_isnoobj(tolua_S,3)
 )
 {
  tolua_error(tolua_S,"#ferror in function 'object_similar'.");
  return 0;
 }
 else
 {
  const object_type* o_ptr = ((const object_type*)  tolua_getusertype(tolua_S,1,0));
  const object_type* j_ptr = ((const object_type*)  tolua_getusertype(tolua_S,2,0));
  {
   bool toluaI_ret = (bool)  object_similar(o_ptr,j_ptr);
   tolua_pushbool(tolua_S,(int)toluaI_ret);
  }
 }
 return 1;
}

/* function: object_absorb */
static int toluaI_object_object_absorb00(lua_State* tolua_S)
{
 if (
     !tolua_istype(tolua_S,1,tolua_tag(tolua_S,"object_type"),0) ||
     !tolua_istype(tolua_S,2,tolua_tag(tolua_S,"object_type"),0) ||
     !tolua_isnoobj(tolua_S,3)
 )
 {
  tolua_error(tolua_S,"#ferror in function 'object_absorb'.");
  return 0;
 }
 else
 {
  object_type* o_ptr = ((object_type*)  tolua_getusertype(tolua_S,1,0));
  object_type* j_ptr = ((object_type*)  tolua_getusertype(tolua_S,2,0));
  {
   object_absorb(o_ptr,j_ptr);
  }
 }
 return 0;
}

/* function: lookup_kind */
static int toluaI_object_lookup_kind00(lua_State* tolua_S)
{
 if (
     !tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
     !tolua_istype(tolua_S,2,LUA_TNUMBER,0) ||
     !tolua_isnoobj(tolua_S,3)
 )
 {
  tolua_error(tolua_S,"#ferror in function 'lookup_kind'.");
  return 0;
 }
 else
 {
  int tval = ((int)  tolua_getnumber(tolua_S,1,0));
  int sval = ((int)  tolua_getnumber(tolua_S,2,0));
  {
   s16b toluaI_ret = (s16b)  lookup_kind(tval,sval);
   tolua_pushnumber(tolua_S,(long)toluaI_ret);
  }
 }
 return 1;
}

/* function: object_wipe */
static int toluaI_object_object_wipe00(lua_State* tolua_S)
{
 if (
     !tolua_istype(tolua_S,1,tolua_tag(tolua_S,"object_type"),0) ||
     !tolua_isnoobj(tolua_S,2)
 )
 {
  tolua_error(tolua_S,"#ferror in function 'object_wipe'.");
  return 0;
 }
 else
 {
  object_type* o_ptr = ((object_type*)  tolua_getusertype(tolua_S,1,0));
  {
   object_wipe(o_ptr);
  }
 }
 return 0;
}

/* function: object_prep */
static int toluaI_object_object_prep00(lua_State* tolua_S)
{
 if (
     !tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
     !tolua_isnoobj(tolua_S,2)
 )
 {
  tolua_error(tolua_S,"#ferror in function 'object_prep'.");
  return 0;
 }
 else
 {
  int k_idx = ((int)  tolua_getnumber(tolua_S,1,0));
  {
   object_type* toluaI_ret = (object_type*)  object_prep(k_idx);
   tolua_pushusertype(tolua_S,(void*)toluaI_ret,tolua_tag(tolua_S,"object_type"));
  }
 }
 return 1;
}

/* function: add_ego_flags */
static int toluaI_object_add_ego_flags00(lua_State* tolua_S)
{
 if (
     !tolua_istype(tolua_S,1,tolua_tag(tolua_S,"object_type"),0) ||
     !tolua_istype(tolua_S,2,LUA_TNUMBER,0) ||
     !tolua_isnoobj(tolua_S,3)
 )
 {
  tolua_error(tolua_S,"#ferror in function 'add_ego_flags'.");
  return 0;
 }
 else
 {
  object_type* o_ptr = ((object_type*)  tolua_getusertype(tolua_S,1,0));
  byte ego = ((byte)  tolua_getnumber(tolua_S,2,0));
  {
   add_ego_flags(o_ptr,ego);
  }
 }
 return 0;
}

/* function: add_ego_power */
static int toluaI_object_add_ego_power00(lua_State* tolua_S)
{
 if (
     !tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
     !tolua_istype(tolua_S,2,tolua_tag(tolua_S,"object_type"),0) ||
     !tolua_isnoobj(tolua_S,3)
 )
 {
  tolua_error(tolua_S,"#ferror in function 'add_ego_power'.");
  return 0;
 }
 else
 {
  int power = ((int)  tolua_getnumber(tolua_S,1,0));
  object_type* o_ptr = ((object_type*)  tolua_getusertype(tolua_S,2,0));
  {
   add_ego_power(power,o_ptr);
  }
 }
 return 0;
}

/* function: m_bonus */
static int toluaI_object_m_bonus00(lua_State* tolua_S)
{
 if (
     !tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
     !tolua_istype(tolua_S,2,LUA_TNUMBER,0) ||
     !tolua_isnoobj(tolua_S,3)
 )
 {
  tolua_error(tolua_S,"#ferror in function 'm_bonus'.");
  return 0;
 }
 else
 {
  int max = ((int)  tolua_getnumber(tolua_S,1,0));
  int level = ((int)  tolua_getnumber(tolua_S,2,0));
  {
   s16b toluaI_ret = (s16b)  m_bonus(max,level);
   tolua_pushnumber(tolua_S,(long)toluaI_ret);
  }
 }
 return 1;
}

/* function: apply_magic */
static int toluaI_object_apply_magic00(lua_State* tolua_S)
{
 if (
     !tolua_istype(tolua_S,1,tolua_tag(tolua_S,"object_type"),0) ||
     !tolua_istype(tolua_S,2,LUA_TNUMBER,0) ||
     !tolua_istype(tolua_S,3,LUA_TNUMBER,0) ||
     !tolua_istype(tolua_S,4,LUA_TNUMBER,0) ||
     !tolua_isnoobj(tolua_S,5)
 )
 {
  tolua_error(tolua_S,"#ferror in function 'apply_magic'.");
  return 0;
 }
 else
 {
  object_type* o_ptr = ((object_type*)  tolua_getusertype(tolua_S,1,0));
  int lev = ((int)  tolua_getnumber(tolua_S,2,0));
  int lev_dif = ((int)  tolua_getnumber(tolua_S,3,0));
  byte flags = ((byte)  tolua_getnumber(tolua_S,4,0));
  {
   apply_magic(o_ptr,lev,lev_dif,flags);
  }
 }
 return 0;
}

/* function: init_match_hook */
static int toluaI_object_init_match_hook00(lua_State* tolua_S)
{
 if (
     !tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
     !tolua_istype(tolua_S,2,LUA_TNUMBER,0) ||
     !tolua_isnoobj(tolua_S,3)
 )
 {
  tolua_error(tolua_S,"#ferror in function 'init_match_hook'.");
  return 0;
 }
 else
 {
  byte tval = ((byte)  tolua_getnumber(tolua_S,1,0));
  byte sval = ((byte)  tolua_getnumber(tolua_S,2,0));
  {
   init_match_hook(tval,sval);
  }
 }
 return 0;
}

/* function: kind_is_match */
static int toluaI_object_kind_is_match00(lua_State* tolua_S)
{
 if (
     !tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
     !tolua_isnoobj(tolua_S,2)
 )
 {
  tolua_error(tolua_S,"#ferror in function 'kind_is_match'.");
  return 0;
 }
 else
 {
  int k_idx = ((int)  tolua_getnumber(tolua_S,1,0));
  {
   byte toluaI_ret = (byte)  kind_is_match(k_idx);
   tolua_pushnumber(tolua_S,(long)toluaI_ret);
  }
 }
 return 1;
}

/* function: init_match_theme */
static int toluaI_object_init_match_theme00(lua_State* tolua_S)
{
 if (
     !tolua_istype(tolua_S,1,tolua_tag(tolua_S,"obj_theme"),0) ||
     !tolua_isnoobj(tolua_S,2)
 )
 {
  tolua_error(tolua_S,"#ferror in function 'init_match_theme'.");
  return 0;
 }
 else
 {
  obj_theme theme = *((obj_theme*)  tolua_getusertype(tolua_S,1,0));
  {
   init_match_theme(theme);
  }
 }
 return 0;
}

/* function: kind_is_theme */
static int toluaI_object_kind_is_theme00(lua_State* tolua_S)
{
 if (
     !tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
     !tolua_isnoobj(tolua_S,2)
 )
 {
  tolua_error(tolua_S,"#ferror in function 'kind_is_theme'.");
  return 0;
 }
 else
 {
  int k_idx = ((int)  tolua_getnumber(tolua_S,1,0));
  {
   byte toluaI_ret = (byte)  kind_is_theme(k_idx);
   tolua_pushnumber(tolua_S,(long)toluaI_ret);
  }
 }
 return 1;
}

/* function: make_object */
static int toluaI_object_make_object00(lua_State* tolua_S)
{
 if (
     !tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
     !tolua_istype(tolua_S,2,LUA_TNUMBER,0) ||
     !tolua_istype(tolua_S,3,tolua_tag(tolua_S,"obj_theme"),0) ||
     !tolua_isnoobj(tolua_S,4)
 )
 {
  tolua_error(tolua_S,"#ferror in function 'make_object'.");
  return 0;
 }
 else
 {
  int level = ((int)  tolua_getnumber(tolua_S,1,0));
  int delta_level = ((int)  tolua_getnumber(tolua_S,2,0));
  obj_theme* theme = ((obj_theme*)  tolua_getusertype(tolua_S,3,0));
  {
   object_type* toluaI_ret = (object_type*)  make_object(level,delta_level,theme);
   tolua_pushusertype(tolua_S,(void*)toluaI_ret,tolua_tag(tolua_S,"object_type"));
  }
 }
 return 1;
}

/* function: place_object */
static int toluaI_object_place_object00(lua_State* tolua_S)
{
 if (
     !tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
     !tolua_istype(tolua_S,2,LUA_TNUMBER,0) ||
     !tolua_istype(tolua_S,3,tolua_tag(tolua_S,"bool"),0) ||
     !tolua_istype(tolua_S,4,tolua_tag(tolua_S,"bool"),0) ||
     !tolua_istype(tolua_S,5,LUA_TNUMBER,0) ||
     !tolua_isnoobj(tolua_S,6)
 )
 {
  tolua_error(tolua_S,"#ferror in function 'place_object'.");
  return 0;
 }
 else
 {
  int x = ((int)  tolua_getnumber(tolua_S,1,0));
  int y = ((int)  tolua_getnumber(tolua_S,2,0));
  bool good = ((bool)  tolua_getbool(tolua_S,3,0));
  bool great = ((bool)  tolua_getbool(tolua_S,4,0));
  int delta_level = ((int)  tolua_getnumber(tolua_S,5,0));
  {
   place_object(x,y,good,great,delta_level);
  }
 }
 return 0;
}

/* function: make_gold */
static int toluaI_object_make_gold00(lua_State* tolua_S)
{
 if (
     !tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
     !tolua_istype(tolua_S,2,LUA_TNUMBER,0) ||
     !tolua_isnoobj(tolua_S,3)
 )
 {
  tolua_error(tolua_S,"#ferror in function 'make_gold'.");
  return 0;
 }
 else
 {
  int level = ((int)  tolua_getnumber(tolua_S,1,0));
  int coin_type = ((int)  tolua_getnumber(tolua_S,2,0));
  {
   object_type* toluaI_ret = (object_type*)  make_gold(level,coin_type);
   tolua_pushusertype(tolua_S,(void*)toluaI_ret,tolua_tag(tolua_S,"object_type"));
  }
 }
 return 1;
}

/* function: place_gold */
static int toluaI_object_place_gold00(lua_State* tolua_S)
{
 if (
     !tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
     !tolua_istype(tolua_S,2,LUA_TNUMBER,0) ||
     !tolua_isnoobj(tolua_S,3)
 )
 {
  tolua_error(tolua_S,"#ferror in function 'place_gold'.");
  return 0;
 }
 else
 {
  int x = ((int)  tolua_getnumber(tolua_S,1,0));
  int y = ((int)  tolua_getnumber(tolua_S,2,0));
  {
   place_gold(x,y);
  }
 }
 return 0;
}

/* function: drop_near */
static int toluaI_object_drop_near00(lua_State* tolua_S)
{
 if (
     !tolua_istype(tolua_S,1,tolua_tag(tolua_S,"object_type"),0) ||
     !tolua_istype(tolua_S,2,LUA_TNUMBER,0) ||
     !tolua_istype(tolua_S,3,LUA_TNUMBER,0) ||
     !tolua_istype(tolua_S,4,LUA_TNUMBER,0) ||
     !tolua_isnoobj(tolua_S,5)
 )
 {
  tolua_error(tolua_S,"#ferror in function 'drop_near'.");
  return 0;
 }
 else
 {
  object_type* o_ptr = ((object_type*)  tolua_getusertype(tolua_S,1,0));
  int chance = ((int)  tolua_getnumber(tolua_S,2,0));
  int x = ((int)  tolua_getnumber(tolua_S,3,0));
  int y = ((int)  tolua_getnumber(tolua_S,4,0));
  {
   drop_near(o_ptr,chance,x,y);
  }
 }
 return 0;
}

/* function: acquirement */
static int toluaI_object_acquirement00(lua_State* tolua_S)
{
 if (
     !tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
     !tolua_istype(tolua_S,2,LUA_TNUMBER,0) ||
     !tolua_istype(tolua_S,3,LUA_TNUMBER,0) ||
     !tolua_istype(tolua_S,4,tolua_tag(tolua_S,"bool"),0) ||
     !tolua_istype(tolua_S,5,tolua_tag(tolua_S,"bool"),0) ||
     !tolua_isnoobj(tolua_S,6)
 )
 {
  tolua_error(tolua_S,"#ferror in function 'acquirement'.");
  return 0;
 }
 else
 {
  int x1 = ((int)  tolua_getnumber(tolua_S,1,0));
  int y1 = ((int)  tolua_getnumber(tolua_S,2,0));
  int num = ((int)  tolua_getnumber(tolua_S,3,0));
  bool great = ((bool)  tolua_getbool(tolua_S,4,0));
  bool known = ((bool)  tolua_getbool(tolua_S,5,0));
  {
   acquirement(x1,y1,num,great,known);
  }
 }
 return 0;
}

/* function: item_activation */
static int toluaI_object_item_activation00(lua_State* tolua_S)
{
 if (
     !tolua_istype(tolua_S,1,tolua_tag(tolua_S,"const object_type"),0) ||
     !tolua_isnoobj(tolua_S,2)
 )
 {
  tolua_error(tolua_S,"#ferror in function 'item_activation'.");
  return 0;
 }
 else
 {
  const object_type* o_ptr = ((const object_type*)  tolua_getusertype(tolua_S,1,0));
  {
   cptr toluaI_ret = (cptr)  item_activation(o_ptr);
   tolua_pushstring(tolua_S,(const char*)toluaI_ret);
  }
 }
 return 1;
}

/* function: can_player_destroy_object */
static int toluaI_object_can_player_destroy_object00(lua_State* tolua_S)
{
 if (
     !tolua_istype(tolua_S,1,tolua_tag(tolua_S,"object_type"),0) ||
     !tolua_isnoobj(tolua_S,2)
 )
 {
  tolua_error(tolua_S,"#ferror in function 'can_player_destroy_object'.");
  return 0;
 }
 else
 {
  object_type* o_ptr = ((object_type*)  tolua_getusertype(tolua_S,1,0));
  {
   bool toluaI_ret = (bool)  can_player_destroy_object(o_ptr);
   tolua_pushbool(tolua_S,(int)toluaI_ret);
  }
 }
 return 1;
}

/* function: display_koff */
static int toluaI_object_display_koff00(lua_State* tolua_S)
{
 if (
     !tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
     !tolua_isnoobj(tolua_S,2)
 )
 {
  tolua_error(tolua_S,"#ferror in function 'display_koff'.");
  return 0;
 }
 else
 {
  int k_idx = ((int)  tolua_getnumber(tolua_S,1,0));
  {
   display_koff(k_idx);
  }
 }
 return 0;
}

/* function: create_artifact */
static int toluaI_object_create_artifact00(lua_State* tolua_S)
{
 if (
     !tolua_istype(tolua_S,1,tolua_tag(tolua_S,"object_type"),0) ||
     !tolua_istype(tolua_S,2,LUA_TNUMBER,0) ||
     !tolua_istype(tolua_S,3,tolua_tag(tolua_S,"bool"),0) ||
     !tolua_isnoobj(tolua_S,4)
 )
 {
  tolua_error(tolua_S,"#ferror in function 'create_artifact'.");
  return 0;
 }
 else
 {
  object_type* o_ptr = ((object_type*)  tolua_getusertype(tolua_S,1,0));
  int level = ((int)  tolua_getnumber(tolua_S,2,0));
  bool a_scroll = ((bool)  tolua_getbool(tolua_S,3,0));
  {
   bool toluaI_ret = (bool)  create_artifact(o_ptr,level,a_scroll);
   tolua_pushbool(tolua_S,(int)toluaI_ret);
  }
 }
 return 1;
}

/* function: create_named_art */
static int toluaI_object_create_named_art00(lua_State* tolua_S)
{
 if (
     !tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
     !tolua_istype(tolua_S,2,LUA_TNUMBER,0) ||
     !tolua_istype(tolua_S,3,LUA_TNUMBER,0) ||
     !tolua_isnoobj(tolua_S,4)
 )
 {
  tolua_error(tolua_S,"#ferror in function 'create_named_art'.");
  return 0;
 }
 else
 {
  int a_idx = ((int)  tolua_getnumber(tolua_S,1,0));
  int x = ((int)  tolua_getnumber(tolua_S,2,0));
  int y = ((int)  tolua_getnumber(tolua_S,3,0));
  {
   create_named_art(a_idx,x,y);
  }
 }
 return 0;
}

/* function: k_info_alloc */
static int toluaI_object_k_info_alloc00(lua_State* tolua_S)
{
 if (
     !tolua_isnoobj(tolua_S,1)
 )
 {
  tolua_error(tolua_S,"#ferror in function 'k_info_alloc'.");
  return 0;
 }
 else
 {
  {
   errr toluaI_ret = (errr)  k_info_alloc();
   tolua_pushnumber(tolua_S,(long)toluaI_ret);
  }
 }
 return 1;
}

/* function: k_info_free */
static int toluaI_object_k_info_free00(lua_State* tolua_S)
{
 if (
     !tolua_isnoobj(tolua_S,1)
 )
 {
  tolua_error(tolua_S,"#ferror in function 'k_info_free'.");
  return 0;
 }
 else
 {
  {
   errr toluaI_ret = (errr)  k_info_free();
   tolua_pushnumber(tolua_S,(long)toluaI_ret);
  }
 }
 return 1;
}

/* function: k_info_add */
static int toluaI_object_k_info_add00(lua_State* tolua_S)
{
 if (
     !tolua_istype(tolua_S,1,tolua_tag(tolua_S,"object_kind"),0) ||
     !tolua_isnoobj(tolua_S,2)
 )
 {
  tolua_error(tolua_S,"#ferror in function 'k_info_add'.");
  return 0;
 }
 else
 {
  object_kind* k_info_entry = ((object_kind*)  tolua_getusertype(tolua_S,1,0));
  {
   object_kind* toluaI_ret = (object_kind*)  k_info_add(k_info_entry);
   tolua_pushusertype(tolua_S,(void*)toluaI_ret,tolua_tag(tolua_S,"object_kind"));
  }
 }
 return 1;
}

/* function: get_object_level */
static int toluaI_object_get_object_level00(lua_State* tolua_S)
{
 if (
     !tolua_istype(tolua_S,1,tolua_tag(tolua_S,"const object_type"),0) ||
     !tolua_isnoobj(tolua_S,2)
 )
 {
  tolua_error(tolua_S,"#ferror in function 'get_object_level'.");
  return 0;
 }
 else
 {
  const object_type* o_ptr = ((const object_type*)  tolua_getusertype(tolua_S,1,0));
  {
   byte toluaI_ret = (byte)  get_object_level(o_ptr);
   tolua_pushnumber(tolua_S,(long)toluaI_ret);
  }
 }
 return 1;
}

/* function: get_object_name */
static int toluaI_object_get_object_name00(lua_State* tolua_S)
{
 if (
     !tolua_istype(tolua_S,1,tolua_tag(tolua_S,"const object_type"),0) ||
     !tolua_isnoobj(tolua_S,2)
 )
 {
  tolua_error(tolua_S,"#ferror in function 'get_object_name'.");
  return 0;
 }
 else
 {
  const object_type* o_ptr = ((const object_type*)  tolua_getusertype(tolua_S,1,0));
  {
   cptr toluaI_ret = (cptr)  get_object_name(o_ptr);
   tolua_pushstring(tolua_S,(const char*)toluaI_ret);
  }
 }
 return 1;
}

/* function: get_object_d_attr */
static int toluaI_object_get_object_d_attr00(lua_State* tolua_S)
{
 if (
     !tolua_istype(tolua_S,1,tolua_tag(tolua_S,"const object_type"),0) ||
     !tolua_isnoobj(tolua_S,2)
 )
 {
  tolua_error(tolua_S,"#ferror in function 'get_object_d_attr'.");
  return 0;
 }
 else
 {
  const object_type* o_ptr = ((const object_type*)  tolua_getusertype(tolua_S,1,0));
  {
   byte toluaI_ret = (byte)  get_object_d_attr(o_ptr);
   tolua_pushnumber(tolua_S,(long)toluaI_ret);
  }
 }
 return 1;
}

/* function: get_object_x_attr */
static int toluaI_object_get_object_x_attr00(lua_State* tolua_S)
{
 if (
     !tolua_istype(tolua_S,1,tolua_tag(tolua_S,"const object_type"),0) ||
     !tolua_isnoobj(tolua_S,2)
 )
 {
  tolua_error(tolua_S,"#ferror in function 'get_object_x_attr'.");
  return 0;
 }
 else
 {
  const object_type* o_ptr = ((const object_type*)  tolua_getusertype(tolua_S,1,0));
  {
   byte toluaI_ret = (byte)  get_object_x_attr(o_ptr);
   tolua_pushnumber(tolua_S,(long)toluaI_ret);
  }
 }
 return 1;
}

/* function: get_object_d_char */
static int toluaI_object_get_object_d_char00(lua_State* tolua_S)
{
 if (
     !tolua_istype(tolua_S,1,tolua_tag(tolua_S,"const object_type"),0) ||
     !tolua_isnoobj(tolua_S,2)
 )
 {
  tolua_error(tolua_S,"#ferror in function 'get_object_d_char'.");
  return 0;
 }
 else
 {
  const object_type* o_ptr = ((const object_type*)  tolua_getusertype(tolua_S,1,0));
  {
   char toluaI_ret = (char)  get_object_d_char(o_ptr);
   tolua_pushnumber(tolua_S,(long)toluaI_ret);
  }
 }
 return 1;
}

/* function: get_object_x_char */
static int toluaI_object_get_object_x_char00(lua_State* tolua_S)
{
 if (
     !tolua_istype(tolua_S,1,tolua_tag(tolua_S,"const object_type"),0) ||
     !tolua_isnoobj(tolua_S,2)
 )
 {
  tolua_error(tolua_S,"#ferror in function 'get_object_x_char'.");
  return 0;
 }
 else
 {
  const object_type* o_ptr = ((const object_type*)  tolua_getusertype(tolua_S,1,0));
  {
   char toluaI_ret = (char)  get_object_x_char(o_ptr);
   tolua_pushnumber(tolua_S,(long)toluaI_ret);
  }
 }
 return 1;
}

/* function: get_object_aware */
static int toluaI_object_get_object_aware00(lua_State* tolua_S)
{
 if (
     !tolua_istype(tolua_S,1,tolua_tag(tolua_S,"const object_type"),0) ||
     !tolua_isnoobj(tolua_S,2)
 )
 {
  tolua_error(tolua_S,"#ferror in function 'get_object_aware'.");
  return 0;
 }
 else
 {
  const object_type* o_ptr = ((const object_type*)  tolua_getusertype(tolua_S,1,0));
  {
   bool toluaI_ret = (bool)  get_object_aware(o_ptr);
   tolua_pushbool(tolua_S,(int)toluaI_ret);
  }
 }
 return 1;
}

/* function: get_object_tried */
static int toluaI_object_get_object_tried00(lua_State* tolua_S)
{
 if (
     !tolua_istype(tolua_S,1,tolua_tag(tolua_S,"const object_type"),0) ||
     !tolua_isnoobj(tolua_S,2)
 )
 {
  tolua_error(tolua_S,"#ferror in function 'get_object_tried'.");
  return 0;
 }
 else
 {
  const object_type* o_ptr = ((const object_type*)  tolua_getusertype(tolua_S,1,0));
  {
   bool toluaI_ret = (bool)  get_object_tried(o_ptr);
   tolua_pushbool(tolua_S,(int)toluaI_ret);
  }
 }
 return 1;
}

/* function: object_is_potion */
static int toluaI_object_object_is_potion00(lua_State* tolua_S)
{
 if (
     !tolua_istype(tolua_S,1,tolua_tag(tolua_S,"const object_type"),0) ||
     !tolua_isnoobj(tolua_S,2)
 )
 {
  tolua_error(tolua_S,"#ferror in function 'object_is_potion'.");
  return 0;
 }
 else
 {
  const object_type* o_ptr = ((const object_type*)  tolua_getusertype(tolua_S,1,0));
  {
   bool toluaI_ret = (bool)  object_is_potion(o_ptr);
   tolua_pushbool(tolua_S,(int)toluaI_ret);
  }
 }
 return 1;
}

/* function: init_object_alloc */
static int toluaI_object_init_object_alloc00(lua_State* tolua_S)
{
 if (
     !tolua_isnoobj(tolua_S,1)
 )
 {
  tolua_error(tolua_S,"#ferror in function 'init_object_alloc'.");
  return 0;
 }
 else
 {
  {
   errr toluaI_ret = (errr)  init_object_alloc();
   tolua_pushnumber(tolua_S,(long)toluaI_ret);
  }
 }
 return 1;
}

/* function: k_info_reset */
static int toluaI_object_k_info_reset00(lua_State* tolua_S)
{
 if (
     !tolua_isnoobj(tolua_S,1)
 )
 {
  tolua_error(tolua_S,"#ferror in function 'k_info_reset'.");
  return 0;
 }
 else
 {
  {
   k_info_reset();
  }
 }
 return 0;
}

/* Open function */
int tolua_object_open (lua_State* tolua_S)
{
 tolua_open(tolua_S);
 toluaI_reg_types(tolua_S);
 tolua_constant(tolua_S,NULL,"MAX_TRIGGER",MAX_TRIGGER);
 tolua_constant(tolua_S,NULL,"TRIGGER_USE",TRIGGER_USE);
 tolua_constant(tolua_S,NULL,"TRIGGER_MAKE",TRIGGER_MAKE);
 tolua_constant(tolua_S,NULL,"TRIGGER_BONUS",TRIGGER_BONUS);
 tolua_constant(tolua_S,NULL,"TRIGGER_SMASH",TRIGGER_SMASH);
 tolua_constant(tolua_S,NULL,"TRIGGER_DESC",TRIGGER_DESC);
 tolua_constant(tolua_S,NULL,"EGO_XTRA_SUSTAIN",EGO_XTRA_SUSTAIN);
 tolua_constant(tolua_S,NULL,"EGO_XTRA_LO_RESIST",EGO_XTRA_LO_RESIST);
 tolua_constant(tolua_S,NULL,"EGO_XTRA_HI_RESIST",EGO_XTRA_HI_RESIST);
 tolua_constant(tolua_S,NULL,"EGO_XTRA_ANY_RESIST",EGO_XTRA_ANY_RESIST);
 tolua_constant(tolua_S,NULL,"EGO_XTRA_ABILITY",EGO_XTRA_ABILITY);
 tolua_constant(tolua_S,NULL,"EGO_XTRA_POWER",EGO_XTRA_POWER);
 tolua_constant(tolua_S,NULL,"TV_ANY",TV_ANY);
 tolua_constant(tolua_S,NULL,"TV_SKELETON",TV_SKELETON);
 tolua_constant(tolua_S,NULL,"TV_BOTTLE",TV_BOTTLE);
 tolua_constant(tolua_S,NULL,"TV_JUNK",TV_JUNK);
 tolua_constant(tolua_S,NULL,"TV_SPIKE",TV_SPIKE);
 tolua_constant(tolua_S,NULL,"TV_CHEST",TV_CHEST);
 tolua_constant(tolua_S,NULL,"TV_FIGURINE",TV_FIGURINE);
 tolua_constant(tolua_S,NULL,"TV_STATUE",TV_STATUE);
 tolua_constant(tolua_S,NULL,"TV_SHOT",TV_SHOT);
 tolua_constant(tolua_S,NULL,"TV_ARROW",TV_ARROW);
 tolua_constant(tolua_S,NULL,"TV_BOLT",TV_BOLT);
 tolua_constant(tolua_S,NULL,"TV_BOW",TV_BOW);
 tolua_constant(tolua_S,NULL,"TV_DIGGING",TV_DIGGING);
 tolua_constant(tolua_S,NULL,"TV_HAFTED",TV_HAFTED);
 tolua_constant(tolua_S,NULL,"TV_POLEARM",TV_POLEARM);
 tolua_constant(tolua_S,NULL,"TV_SWORD",TV_SWORD);
 tolua_constant(tolua_S,NULL,"TV_BOOTS",TV_BOOTS);
 tolua_constant(tolua_S,NULL,"TV_GLOVES",TV_GLOVES);
 tolua_constant(tolua_S,NULL,"TV_HELM",TV_HELM);
 tolua_constant(tolua_S,NULL,"TV_CROWN",TV_CROWN);
 tolua_constant(tolua_S,NULL,"TV_SHIELD",TV_SHIELD);
 tolua_constant(tolua_S,NULL,"TV_CLOAK",TV_CLOAK);
 tolua_constant(tolua_S,NULL,"TV_SOFT_ARMOR",TV_SOFT_ARMOR);
 tolua_constant(tolua_S,NULL,"TV_HARD_ARMOR",TV_HARD_ARMOR);
 tolua_constant(tolua_S,NULL,"TV_DRAG_ARMOR",TV_DRAG_ARMOR);
 tolua_constant(tolua_S,NULL,"TV_LITE",TV_LITE);
 tolua_constant(tolua_S,NULL,"TV_AMULET",TV_AMULET);
 tolua_constant(tolua_S,NULL,"TV_RING",TV_RING);
 tolua_constant(tolua_S,NULL,"TV_STAFF",TV_STAFF);
 tolua_constant(tolua_S,NULL,"TV_WAND",TV_WAND);
 tolua_constant(tolua_S,NULL,"TV_ROD",TV_ROD);
 tolua_constant(tolua_S,NULL,"TV_SCROLL",TV_SCROLL);
 tolua_constant(tolua_S,NULL,"TV_POTION",TV_POTION);
 tolua_constant(tolua_S,NULL,"TV_FLASK",TV_FLASK);
 tolua_constant(tolua_S,NULL,"TV_FOOD",TV_FOOD);
 tolua_constant(tolua_S,NULL,"TV_LIFE_BOOK",TV_LIFE_BOOK);
 tolua_constant(tolua_S,NULL,"TV_SORCERY_BOOK",TV_SORCERY_BOOK);
 tolua_constant(tolua_S,NULL,"TV_NATURE_BOOK",TV_NATURE_BOOK);
 tolua_constant(tolua_S,NULL,"TV_CHAOS_BOOK",TV_CHAOS_BOOK);
 tolua_constant(tolua_S,NULL,"TV_DEATH_BOOK",TV_DEATH_BOOK);
 tolua_constant(tolua_S,NULL,"TV_TRUMP_BOOK",TV_TRUMP_BOOK);
 tolua_constant(tolua_S,NULL,"TV_ARCANE_BOOK",TV_ARCANE_BOOK);
 tolua_constant(tolua_S,NULL,"TV_GOLD",TV_GOLD);
 tolua_constant(tolua_S,NULL,"TV_BOOKS_MIN",TV_BOOKS_MIN);
 tolua_constant(tolua_S,NULL,"TV_BOOKS_MAX",TV_BOOKS_MAX);
 tolua_constant(tolua_S,NULL,"SV_WAND_HEAL_MONSTER",SV_WAND_HEAL_MONSTER);
 tolua_constant(tolua_S,NULL,"SV_WAND_HASTE_MONSTER",SV_WAND_HASTE_MONSTER);
 tolua_constant(tolua_S,NULL,"SV_WAND_CLONE_MONSTER",SV_WAND_CLONE_MONSTER);
 tolua_constant(tolua_S,NULL,"SV_WAND_TELEPORT_AWAY",SV_WAND_TELEPORT_AWAY);
 tolua_constant(tolua_S,NULL,"SV_WAND_DISARMING",SV_WAND_DISARMING);
 tolua_constant(tolua_S,NULL,"SV_WAND_TRAP_DOOR_DEST",SV_WAND_TRAP_DOOR_DEST);
 tolua_constant(tolua_S,NULL,"SV_WAND_STONE_TO_MUD",SV_WAND_STONE_TO_MUD);
 tolua_constant(tolua_S,NULL,"SV_WAND_LITE",SV_WAND_LITE);
 tolua_constant(tolua_S,NULL,"SV_WAND_SLEEP_MONSTER",SV_WAND_SLEEP_MONSTER);
 tolua_constant(tolua_S,NULL,"SV_WAND_SLOW_MONSTER",SV_WAND_SLOW_MONSTER);
 tolua_constant(tolua_S,NULL,"SV_WAND_CONFUSE_MONSTER",SV_WAND_CONFUSE_MONSTER);
 tolua_constant(tolua_S,NULL,"SV_WAND_FEAR_MONSTER",SV_WAND_FEAR_MONSTER);
 tolua_constant(tolua_S,NULL,"SV_WAND_DRAIN_LIFE",SV_WAND_DRAIN_LIFE);
 tolua_constant(tolua_S,NULL,"SV_WAND_POLYMORPH",SV_WAND_POLYMORPH);
 tolua_constant(tolua_S,NULL,"SV_WAND_STINKING_CLOUD",SV_WAND_STINKING_CLOUD);
 tolua_constant(tolua_S,NULL,"SV_WAND_MAGIC_MISSILE",SV_WAND_MAGIC_MISSILE);
 tolua_constant(tolua_S,NULL,"SV_WAND_ACID_BOLT",SV_WAND_ACID_BOLT);
 tolua_constant(tolua_S,NULL,"SV_WAND_CHARM_MONSTER",SV_WAND_CHARM_MONSTER);
 tolua_constant(tolua_S,NULL,"SV_WAND_FIRE_BOLT",SV_WAND_FIRE_BOLT);
 tolua_constant(tolua_S,NULL,"SV_WAND_COLD_BOLT",SV_WAND_COLD_BOLT);
 tolua_constant(tolua_S,NULL,"SV_WAND_ACID_BALL",SV_WAND_ACID_BALL);
 tolua_constant(tolua_S,NULL,"SV_WAND_ELEC_BALL",SV_WAND_ELEC_BALL);
 tolua_constant(tolua_S,NULL,"SV_WAND_FIRE_BALL",SV_WAND_FIRE_BALL);
 tolua_constant(tolua_S,NULL,"SV_WAND_COLD_BALL",SV_WAND_COLD_BALL);
 tolua_constant(tolua_S,NULL,"SV_WAND_WONDER",SV_WAND_WONDER);
 tolua_constant(tolua_S,NULL,"OBJ_GOLD_LIST",OBJ_GOLD_LIST);
 tolua_constant(tolua_S,NULL,"MAX_GOLD",MAX_GOLD);
 tolua_constant(tolua_S,NULL,"OC_NONE",OC_NONE);
 tolua_constant(tolua_S,NULL,"OC_NORMAL",OC_NORMAL);
 tolua_constant(tolua_S,NULL,"OC_FORCE_BAD",OC_FORCE_BAD);
 tolua_constant(tolua_S,NULL,"OC_FORCE_GOOD",OC_FORCE_GOOD);
 tolua_constant(tolua_S,NULL,"TR0_STR",TR0_STR);
 tolua_constant(tolua_S,NULL,"TR0_INT",TR0_INT);
 tolua_constant(tolua_S,NULL,"TR0_WIS",TR0_WIS);
 tolua_constant(tolua_S,NULL,"TR0_DEX",TR0_DEX);
 tolua_constant(tolua_S,NULL,"TR0_CON",TR0_CON);
 tolua_constant(tolua_S,NULL,"TR0_CHR",TR0_CHR);
 tolua_constant(tolua_S,NULL,"TR0_XXX1",TR0_XXX1);
 tolua_constant(tolua_S,NULL,"TR0_SP",TR0_SP);
 tolua_constant(tolua_S,NULL,"TR0_STEALTH",TR0_STEALTH);
 tolua_constant(tolua_S,NULL,"TR0_SEARCH",TR0_SEARCH);
 tolua_constant(tolua_S,NULL,"TR0_INFRA",TR0_INFRA);
 tolua_constant(tolua_S,NULL,"TR0_TUNNEL",TR0_TUNNEL);
 tolua_constant(tolua_S,NULL,"TR0_SPEED",TR0_SPEED);
 tolua_constant(tolua_S,NULL,"TR0_BLOWS",TR0_BLOWS);
 tolua_constant(tolua_S,NULL,"TR0_CHAOTIC",TR0_CHAOTIC);
 tolua_constant(tolua_S,NULL,"TR0_VAMPIRIC",TR0_VAMPIRIC);
 tolua_constant(tolua_S,NULL,"TR0_SLAY_ANIMAL",TR0_SLAY_ANIMAL);
 tolua_constant(tolua_S,NULL,"TR0_SLAY_EVIL",TR0_SLAY_EVIL);
 tolua_constant(tolua_S,NULL,"TR0_SLAY_UNDEAD",TR0_SLAY_UNDEAD);
 tolua_constant(tolua_S,NULL,"TR0_SLAY_DEMON",TR0_SLAY_DEMON);
 tolua_constant(tolua_S,NULL,"TR0_SLAY_ORC",TR0_SLAY_ORC);
 tolua_constant(tolua_S,NULL,"TR0_SLAY_TROLL",TR0_SLAY_TROLL);
 tolua_constant(tolua_S,NULL,"TR0_SLAY_GIANT",TR0_SLAY_GIANT);
 tolua_constant(tolua_S,NULL,"TR0_SLAY_DRAGON",TR0_SLAY_DRAGON);
 tolua_constant(tolua_S,NULL,"TR0_KILL_DRAGON",TR0_KILL_DRAGON);
 tolua_constant(tolua_S,NULL,"TR0_VORPAL",TR0_VORPAL);
 tolua_constant(tolua_S,NULL,"TR0_IMPACT",TR0_IMPACT);
 tolua_constant(tolua_S,NULL,"TR0_BRAND_POIS",TR0_BRAND_POIS);
 tolua_constant(tolua_S,NULL,"TR0_BRAND_ACID",TR0_BRAND_ACID);
 tolua_constant(tolua_S,NULL,"TR0_BRAND_ELEC",TR0_BRAND_ELEC);
 tolua_constant(tolua_S,NULL,"TR0_BRAND_FIRE",TR0_BRAND_FIRE);
 tolua_constant(tolua_S,NULL,"TR0_BRAND_COLD",TR0_BRAND_COLD);
 tolua_constant(tolua_S,NULL,"TR1_SUST_STR",TR1_SUST_STR);
 tolua_constant(tolua_S,NULL,"TR1_SUST_INT",TR1_SUST_INT);
 tolua_constant(tolua_S,NULL,"TR1_SUST_WIS",TR1_SUST_WIS);
 tolua_constant(tolua_S,NULL,"TR1_SUST_DEX",TR1_SUST_DEX);
 tolua_constant(tolua_S,NULL,"TR1_SUST_CON",TR1_SUST_CON);
 tolua_constant(tolua_S,NULL,"TR1_SUST_CHR",TR1_SUST_CHR);
 tolua_constant(tolua_S,NULL,"TR1_XXX1",TR1_XXX1);
 tolua_constant(tolua_S,NULL,"TR1_IM_POIS",TR1_IM_POIS);
 tolua_constant(tolua_S,NULL,"TR1_IM_ACID",TR1_IM_ACID);
 tolua_constant(tolua_S,NULL,"TR1_IM_ELEC",TR1_IM_ELEC);
 tolua_constant(tolua_S,NULL,"TR1_IM_FIRE",TR1_IM_FIRE);
 tolua_constant(tolua_S,NULL,"TR1_IM_COLD",TR1_IM_COLD);
 tolua_constant(tolua_S,NULL,"TR1_THROW",TR1_THROW);
 tolua_constant(tolua_S,NULL,"TR1_REFLECT",TR1_REFLECT);
 tolua_constant(tolua_S,NULL,"TR1_FREE_ACT",TR1_FREE_ACT);
 tolua_constant(tolua_S,NULL,"TR1_HOLD_LIFE",TR1_HOLD_LIFE);
 tolua_constant(tolua_S,NULL,"TR1_RES_ACID",TR1_RES_ACID);
 tolua_constant(tolua_S,NULL,"TR1_RES_ELEC",TR1_RES_ELEC);
 tolua_constant(tolua_S,NULL,"TR1_RES_FIRE",TR1_RES_FIRE);
 tolua_constant(tolua_S,NULL,"TR1_RES_COLD",TR1_RES_COLD);
 tolua_constant(tolua_S,NULL,"TR1_RES_POIS",TR1_RES_POIS);
 tolua_constant(tolua_S,NULL,"TR1_RES_FEAR",TR1_RES_FEAR);
 tolua_constant(tolua_S,NULL,"TR1_RES_LITE",TR1_RES_LITE);
 tolua_constant(tolua_S,NULL,"TR1_RES_DARK",TR1_RES_DARK);
 tolua_constant(tolua_S,NULL,"TR1_RES_BLIND",TR1_RES_BLIND);
 tolua_constant(tolua_S,NULL,"TR1_RES_CONF",TR1_RES_CONF);
 tolua_constant(tolua_S,NULL,"TR1_RES_SOUND",TR1_RES_SOUND);
 tolua_constant(tolua_S,NULL,"TR1_RES_SHARDS",TR1_RES_SHARDS);
 tolua_constant(tolua_S,NULL,"TR1_RES_NETHER",TR1_RES_NETHER);
 tolua_constant(tolua_S,NULL,"TR1_RES_NEXUS",TR1_RES_NEXUS);
 tolua_constant(tolua_S,NULL,"TR1_RES_CHAOS",TR1_RES_CHAOS);
 tolua_constant(tolua_S,NULL,"TR1_RES_DISEN",TR1_RES_DISEN);
 tolua_constant(tolua_S,NULL,"TR2_SH_FIRE",TR2_SH_FIRE);
 tolua_constant(tolua_S,NULL,"TR2_SH_ELEC",TR2_SH_ELEC);
 tolua_constant(tolua_S,NULL,"TR2_QUESTITEM",TR2_QUESTITEM);
 tolua_constant(tolua_S,NULL,"TR2_XXX4",TR2_XXX4);
 tolua_constant(tolua_S,NULL,"TR2_NO_TELE",TR2_NO_TELE);
 tolua_constant(tolua_S,NULL,"TR2_NO_MAGIC",TR2_NO_MAGIC);
 tolua_constant(tolua_S,NULL,"TR2_XXX7",TR2_XXX7);
 tolua_constant(tolua_S,NULL,"TR2_TY_CURSE",TR2_TY_CURSE);
 tolua_constant(tolua_S,NULL,"TR2_EASY_KNOW",TR2_EASY_KNOW);
 tolua_constant(tolua_S,NULL,"TR2_HIDE_TYPE",TR2_HIDE_TYPE);
 tolua_constant(tolua_S,NULL,"TR2_SHOW_MODS",TR2_SHOW_MODS);
 tolua_constant(tolua_S,NULL,"TR2_INSTA_ART",TR2_INSTA_ART);
 tolua_constant(tolua_S,NULL,"TR2_FEATHER",TR2_FEATHER);
 tolua_constant(tolua_S,NULL,"TR2_LITE",TR2_LITE);
 tolua_constant(tolua_S,NULL,"TR2_SEE_INVIS",TR2_SEE_INVIS);
 tolua_constant(tolua_S,NULL,"TR2_TELEPATHY",TR2_TELEPATHY);
 tolua_constant(tolua_S,NULL,"TR2_SLOW_DIGEST",TR2_SLOW_DIGEST);
 tolua_constant(tolua_S,NULL,"TR2_REGEN",TR2_REGEN);
 tolua_constant(tolua_S,NULL,"TR2_XTRA_MIGHT",TR2_XTRA_MIGHT);
 tolua_constant(tolua_S,NULL,"TR2_XTRA_SHOTS",TR2_XTRA_SHOTS);
 tolua_constant(tolua_S,NULL,"TR2_IGNORE_ACID",TR2_IGNORE_ACID);
 tolua_constant(tolua_S,NULL,"TR2_IGNORE_ELEC",TR2_IGNORE_ELEC);
 tolua_constant(tolua_S,NULL,"TR2_IGNORE_FIRE",TR2_IGNORE_FIRE);
 tolua_constant(tolua_S,NULL,"TR2_IGNORE_COLD",TR2_IGNORE_COLD);
 tolua_constant(tolua_S,NULL,"TR2_ACTIVATE",TR2_ACTIVATE);
 tolua_constant(tolua_S,NULL,"TR2_DRAIN_EXP",TR2_DRAIN_EXP);
 tolua_constant(tolua_S,NULL,"TR2_TELEPORT",TR2_TELEPORT);
 tolua_constant(tolua_S,NULL,"TR2_AGGRAVATE",TR2_AGGRAVATE);
 tolua_constant(tolua_S,NULL,"TR2_BLESSED",TR2_BLESSED);
 tolua_constant(tolua_S,NULL,"TR2_CURSED",TR2_CURSED);
 tolua_constant(tolua_S,NULL,"TR2_HEAVY_CURSE",TR2_HEAVY_CURSE);
 tolua_constant(tolua_S,NULL,"TR2_PERMA_CURSE",TR2_PERMA_CURSE);
 tolua_constant(tolua_S,NULL,"TR3_LUCK_10",TR3_LUCK_10);
 tolua_constant(tolua_S,NULL,"TR3_XXX2",TR3_XXX2);
 tolua_constant(tolua_S,NULL,"TR3_XXX3",TR3_XXX3);
 tolua_constant(tolua_S,NULL,"TR3_XXX4",TR3_XXX4);
 tolua_constant(tolua_S,NULL,"TR3_XXX5",TR3_XXX5);
 tolua_constant(tolua_S,NULL,"TR3_XXX6",TR3_XXX6);
 tolua_constant(tolua_S,NULL,"TR3_XXX7",TR3_XXX7);
 tolua_constant(tolua_S,NULL,"TR3_XXX8",TR3_XXX8);
 tolua_constant(tolua_S,NULL,"TR3_IM_LITE",TR3_IM_LITE);
 tolua_constant(tolua_S,NULL,"TR3_IM_DARK",TR3_IM_DARK);
 tolua_constant(tolua_S,NULL,"TR3_SH_ACID",TR3_SH_ACID);
 tolua_constant(tolua_S,NULL,"TR3_SH_COLD",TR3_SH_COLD);
 tolua_constant(tolua_S,NULL,"TR3_MUTATE",TR3_MUTATE);
 tolua_constant(tolua_S,NULL,"TR3_PATRON",TR3_PATRON);
 tolua_constant(tolua_S,NULL,"TR3_STRANGE_LUCK",TR3_STRANGE_LUCK);
 tolua_constant(tolua_S,NULL,"TR3_PASS_WALL",TR3_PASS_WALL);
 tolua_constant(tolua_S,NULL,"TR3_GHOUL_TOUCH",TR3_GHOUL_TOUCH);
 tolua_constant(tolua_S,NULL,"TR3_PSI_CRIT",TR3_PSI_CRIT);
 tolua_constant(tolua_S,NULL,"TR3_RETURN",TR3_RETURN);
 tolua_constant(tolua_S,NULL,"TR3_EXPLODE",TR3_EXPLODE);
 tolua_constant(tolua_S,NULL,"TR3_HURT_ACID",TR3_HURT_ACID);
 tolua_constant(tolua_S,NULL,"TR3_HURT_ELEC",TR3_HURT_ELEC);
 tolua_constant(tolua_S,NULL,"TR3_HURT_FIRE",TR3_HURT_FIRE);
 tolua_constant(tolua_S,NULL,"TR3_HURT_COLD",TR3_HURT_COLD);
 tolua_constant(tolua_S,NULL,"TR3_HURT_LITE",TR3_HURT_LITE);
 tolua_constant(tolua_S,NULL,"TR3_HURT_DARK",TR3_HURT_DARK);
 tolua_constant(tolua_S,NULL,"TR3_XXX27",TR3_XXX27);
 tolua_constant(tolua_S,NULL,"TR3_XXX28",TR3_XXX28);
 tolua_constant(tolua_S,NULL,"TR3_AUTO_CURSE",TR3_AUTO_CURSE);
 tolua_constant(tolua_S,NULL,"TR3_DRAIN_STATS",TR3_DRAIN_STATS);
 tolua_constant(tolua_S,NULL,"TR3_CANT_EAT",TR3_CANT_EAT);
 tolua_constant(tolua_S,NULL,"TR3_SLOW_HEAL",TR3_SLOW_HEAL);
 tolua_cclass(tolua_S,"object_kind","");
 tolua_tablevar(tolua_S,"object_kind","name",toluaI_get_object_object_kind_name,toluaI_set_object_object_kind_name);
 tolua_tablevar(tolua_S,"object_kind","text",toluaI_get_object_object_kind_text,toluaI_set_object_object_kind_text);
 tolua_tablevar(tolua_S,"object_kind","tval",toluaI_get_object_object_kind_tval,toluaI_set_object_object_kind_tval);
 tolua_tablevar(tolua_S,"object_kind","sval",toluaI_get_object_object_kind_sval,toluaI_set_object_object_kind_sval);
 tolua_tablevar(tolua_S,"object_kind","pval",toluaI_get_object_object_kind_pval,toluaI_set_object_object_kind_pval);
 tolua_tablevar(tolua_S,"object_kind","to_h",toluaI_get_object_object_kind_to_h,toluaI_set_object_object_kind_to_h);
 tolua_tablevar(tolua_S,"object_kind","to_d",toluaI_get_object_object_kind_to_d,toluaI_set_object_object_kind_to_d);
 tolua_tablevar(tolua_S,"object_kind","to_a",toluaI_get_object_object_kind_to_a,toluaI_set_object_object_kind_to_a);
 tolua_tablevar(tolua_S,"object_kind","ac",toluaI_get_object_object_kind_ac,toluaI_set_object_object_kind_ac);
 tolua_tablevar(tolua_S,"object_kind","dd",toluaI_get_object_object_kind_dd,toluaI_set_object_object_kind_dd);
 tolua_tablevar(tolua_S,"object_kind","ds",toluaI_get_object_object_kind_ds,toluaI_set_object_object_kind_ds);
 tolua_tablevar(tolua_S,"object_kind","weight",toluaI_get_object_object_kind_weight,toluaI_set_object_object_kind_weight);
 tolua_tablevar(tolua_S,"object_kind","cost",toluaI_get_object_object_kind_cost,toluaI_set_object_object_kind_cost);
 tolua_tablearray(tolua_S,"object_kind","flags",toluaI_get_object_object_kind_flags,toluaI_set_object_object_kind_flags);
 tolua_tablearray(tolua_S,"object_kind","locale",toluaI_get_object_object_kind_locale,toluaI_set_object_object_kind_locale);
 tolua_tablearray(tolua_S,"object_kind","chance",toluaI_get_object_object_kind_chance,toluaI_set_object_object_kind_chance);
 tolua_tablevar(tolua_S,"object_kind","level",toluaI_get_object_object_kind_level,toluaI_set_object_object_kind_level);
 tolua_tablevar(tolua_S,"object_kind","extra",toluaI_get_object_object_kind_extra,toluaI_set_object_object_kind_extra);
 tolua_tablevar(tolua_S,"object_kind","d_attr",toluaI_get_object_object_kind_d_attr,toluaI_set_object_object_kind_d_attr);
 tolua_tablevar(tolua_S,"object_kind","d_char",toluaI_get_object_object_kind_d_char,toluaI_set_object_object_kind_d_char);
 tolua_tablevar(tolua_S,"object_kind","x_attr",toluaI_get_object_object_kind_x_attr,toluaI_set_object_object_kind_x_attr);
 tolua_tablevar(tolua_S,"object_kind","x_char",toluaI_get_object_object_kind_x_char,toluaI_set_object_object_kind_x_char);
 tolua_tablevar(tolua_S,"object_kind","flavor",toluaI_get_object_object_kind_flavor,toluaI_set_object_object_kind_flavor);
 tolua_tablevar(tolua_S,"object_kind","easy_know",toluaI_get_object_object_kind_easy_know,toluaI_set_object_object_kind_easy_know);
 tolua_tablevar(tolua_S,"object_kind","aware",toluaI_get_object_object_kind_aware,toluaI_set_object_object_kind_aware);
 tolua_tablevar(tolua_S,"object_kind","tried",toluaI_get_object_object_kind_tried,toluaI_set_object_object_kind_tried);
 tolua_cclass(tolua_S,"obj_theme","");
 tolua_tablevar(tolua_S,"obj_theme","treasure",toluaI_get_object_obj_theme_treasure,toluaI_set_object_obj_theme_treasure);
 tolua_tablevar(tolua_S,"obj_theme","combat",toluaI_get_object_obj_theme_combat,toluaI_set_object_obj_theme_combat);
 tolua_tablevar(tolua_S,"obj_theme","magic",toluaI_get_object_obj_theme_magic,toluaI_set_object_obj_theme_magic);
 tolua_tablevar(tolua_S,"obj_theme","tools",toluaI_get_object_obj_theme_tools,toluaI_set_object_obj_theme_tools);
 tolua_cclass(tolua_S,"object_type","");
 tolua_tablevar(tolua_S,"object_type","k_idx",toluaI_get_object_object_type_k_idx,toluaI_set_object_object_type_k_idx);
 tolua_tablevar(tolua_S,"object_type","iy",toluaI_get_object_object_type_iy,toluaI_set_object_object_type_iy);
 tolua_tablevar(tolua_S,"object_type","ix",toluaI_get_object_object_type_ix,toluaI_set_object_object_type_ix);
 tolua_tablevar(tolua_S,"object_type","tval",toluaI_get_object_object_type_tval,toluaI_set_object_object_type_tval);
 tolua_tablevar(tolua_S,"object_type","sval",toluaI_get_object_object_type_sval,toluaI_set_object_object_type_sval);
 tolua_tablevar(tolua_S,"object_type","pval",toluaI_get_object_object_type_pval,toluaI_set_object_object_type_pval);
 tolua_tablevar(tolua_S,"object_type","discount",toluaI_get_object_object_type_discount,toluaI_set_object_object_type_discount);
 tolua_tablevar(tolua_S,"object_type","number",toluaI_get_object_object_type_number,toluaI_set_object_object_type_number);
 tolua_tablevar(tolua_S,"object_type","weight",toluaI_get_object_object_type_weight,toluaI_set_object_object_type_weight);
 tolua_tablevar(tolua_S,"object_type","to_h",toluaI_get_object_object_type_to_h,toluaI_set_object_object_type_to_h);
 tolua_tablevar(tolua_S,"object_type","to_d",toluaI_get_object_object_type_to_d,toluaI_set_object_object_type_to_d);
 tolua_tablevar(tolua_S,"object_type","to_a",toluaI_get_object_object_type_to_a,toluaI_set_object_object_type_to_a);
 tolua_tablevar(tolua_S,"object_type","ac",toluaI_get_object_object_type_ac,toluaI_set_object_object_type_ac);
 tolua_tablevar(tolua_S,"object_type","timeout",toluaI_get_object_object_type_timeout,toluaI_set_object_object_type_timeout);
 tolua_tablevar(tolua_S,"object_type","dd",toluaI_get_object_object_type_dd,toluaI_set_object_object_type_dd);
 tolua_tablevar(tolua_S,"object_type","ds",toluaI_get_object_object_type_ds,toluaI_set_object_object_type_ds);
 tolua_tablevar(tolua_S,"object_type","inscription",toluaI_get_object_object_type_inscription,toluaI_set_object_object_type_inscription);
 tolua_tablevar(tolua_S,"object_type","xtra_name",toluaI_get_object_object_type_xtra_name,toluaI_set_object_object_type_xtra_name);
 tolua_tablearray(tolua_S,"object_type","flags",toluaI_get_object_object_type_flags,toluaI_set_object_object_type_flags);
 tolua_tablearray(tolua_S,"object_type","kn_flags",toluaI_get_object_object_type_kn_flags,toluaI_set_object_object_type_kn_flags);
 tolua_tablevar(tolua_S,"object_type","next_o_idx",toluaI_get_object_object_type_next_o_idx,toluaI_set_object_object_type_next_o_idx);
 tolua_tablevar(tolua_S,"object_type","cost",toluaI_get_object_object_type_cost,toluaI_set_object_object_type_cost);
 tolua_tablevar(tolua_S,"object_type","feeling",toluaI_get_object_object_type_feeling,toluaI_set_object_object_type_feeling);
 tolua_tablevar(tolua_S,"object_type","a_idx",toluaI_get_object_object_type_a_idx,toluaI_set_object_object_type_a_idx);
 tolua_tablevar(tolua_S,"object_type","info",toluaI_get_object_object_type_info,toluaI_set_object_object_type_info);
 tolua_tablevar(tolua_S,"object_type","allocated",toluaI_get_object_object_type_allocated,toluaI_set_object_object_type_allocated);
 tolua_globalvar(tolua_S,"o_max",toluaI_get_object_o_max,toluaI_set_object_o_max);
 tolua_globalvar(tolua_S,"o_cnt",toluaI_get_object_o_cnt,toluaI_set_object_o_cnt);
 tolua_globalarray(tolua_S,"o_list",toluaI_get_object_o_list,toluaI_set_object_o_list);
 tolua_globalarray(tolua_S,"k_info",toluaI_get_object_k_info,toluaI_set_object_k_info);
 tolua_globalvar(tolua_S,"k_name",toluaI_get_object_k_name,toluaI_set_object_k_name);
 tolua_globalvar(tolua_S,"k_text",toluaI_get_object_k_text,toluaI_set_object_k_text);
 tolua_globalvar(tolua_S,"a_name",toluaI_get_object_a_name,toluaI_set_object_a_name);
 tolua_globalvar(tolua_S,"a_text",toluaI_get_object_a_text,toluaI_set_object_a_text);
 tolua_function(tolua_S,NULL,"reset_visuals",toluaI_object_reset_visuals00);
 tolua_function(tolua_S,NULL,"identify_fully_aux",toluaI_object_identify_fully_aux00);
 tolua_function(tolua_S,NULL,"wield_slot",toluaI_object_wield_slot00);
 tolua_function(tolua_S,NULL,"mention_use",toluaI_object_mention_use00);
 tolua_function(tolua_S,NULL,"describe_use",toluaI_object_describe_use00);
 tolua_function(tolua_S,NULL,"item_charges",toluaI_object_item_charges00);
 tolua_function(tolua_S,NULL,"item_describe",toluaI_object_item_describe00);
 tolua_function(tolua_S,NULL,"item_split",toluaI_object_item_split00);
 tolua_function(tolua_S,NULL,"item_increase",toluaI_object_item_increase00);
 tolua_function(tolua_S,NULL,"inven_carry_okay",toluaI_object_inven_carry_okay00);
 tolua_function(tolua_S,NULL,"inven_carry",toluaI_object_inven_carry00);
 tolua_function(tolua_S,NULL,"inven_takeoff",toluaI_object_inven_takeoff00);
 tolua_function(tolua_S,NULL,"inven_drop",toluaI_object_inven_drop00);
 tolua_function(tolua_S,NULL,"item_tester_hook_weapon",toluaI_object_item_tester_hook_weapon00);
 tolua_function(tolua_S,NULL,"item_tester_hook_melee_weapon",toluaI_object_item_tester_hook_melee_weapon00);
 tolua_function(tolua_S,NULL,"item_tester_hook_nonsword",toluaI_object_item_tester_hook_nonsword00);
 tolua_function(tolua_S,NULL,"item_tester_hook_ammo",toluaI_object_item_tester_hook_ammo00);
 tolua_function(tolua_S,NULL,"item_tester_hook_fletcher",toluaI_object_item_tester_hook_fletcher00);
 tolua_function(tolua_S,NULL,"item_tester_hook_armour",toluaI_object_item_tester_hook_armour00);
 tolua_function(tolua_S,NULL,"item_tester_hook_soft_armour",toluaI_object_item_tester_hook_soft_armour00);
 tolua_function(tolua_S,NULL,"item_tester_hook_hard_armour",toluaI_object_item_tester_hook_hard_armour00);
 tolua_function(tolua_S,NULL,"item_tester_hook_helm",toluaI_object_item_tester_hook_helm00);
 tolua_function(tolua_S,NULL,"item_tester_hook_pure_hard_armour",toluaI_object_item_tester_hook_pure_hard_armour00);
 tolua_function(tolua_S,NULL,"item_tester_hook_weapon_armour",toluaI_object_item_tester_hook_weapon_armour00);
 tolua_function(tolua_S,NULL,"item_tester_hook_wear",toluaI_object_item_tester_hook_wear00);
 tolua_function(tolua_S,NULL,"item_tester_hook_recharge",toluaI_object_item_tester_hook_recharge00);
 tolua_function(tolua_S,NULL,"item_tester_hook_jewel",toluaI_object_item_tester_hook_jewel00);
 tolua_function(tolua_S,NULL,"item_tester_hook_tval",toluaI_object_item_tester_hook_tval00);
 tolua_function(tolua_S,NULL,"item_tester_hook_is_blessed",toluaI_object_item_tester_hook_is_blessed00);
 tolua_function(tolua_S,NULL,"item_tester_hook_is_good",toluaI_object_item_tester_hook_is_good00);
 tolua_function(tolua_S,NULL,"item_tester_hook_is_great",toluaI_object_item_tester_hook_is_great00);
 tolua_function(tolua_S,NULL,"item_tester_hook_is_book",toluaI_object_item_tester_hook_is_book00);
 tolua_function(tolua_S,NULL,"item_tester_okay",toluaI_object_item_tester_okay00);
 tolua_function(tolua_S,NULL,"display_inven",toluaI_object_display_inven00);
 tolua_function(tolua_S,NULL,"display_equip",toluaI_object_display_equip00);
 tolua_function(tolua_S,NULL,"show_list",toluaI_object_show_list00);
 tolua_function(tolua_S,NULL,"show_equip",toluaI_object_show_equip00);
 tolua_function(tolua_S,NULL,"toggle_inven_equip",toluaI_object_toggle_inven_equip00);
 tolua_function(tolua_S,NULL,"get_item",toluaI_object_get_item00);
 tolua_function(tolua_S,NULL,"delete_dungeon_object",toluaI_object_delete_dungeon_object00);
 tolua_function(tolua_S,NULL,"delete_object",toluaI_object_delete_object00);
 tolua_function(tolua_S,NULL,"delete_object_list",toluaI_object_delete_object_list00);
 tolua_function(tolua_S,NULL,"drop_object_list",toluaI_object_drop_object_list00);
 tolua_function(tolua_S,NULL,"object_known",toluaI_object_object_known00);
 tolua_function(tolua_S,NULL,"object_aware",toluaI_object_object_aware00);
 tolua_function(tolua_S,NULL,"object_tried",toluaI_object_object_tried00);
 tolua_function(tolua_S,NULL,"object_mental",toluaI_object_object_mental00);
 tolua_function(tolua_S,NULL,"flag_cost",toluaI_object_flag_cost00);
 tolua_function(tolua_S,NULL,"object_value",toluaI_object_object_value00);
 tolua_function(tolua_S,NULL,"object_value_real",toluaI_object_object_value_real00);
 tolua_function(tolua_S,NULL,"distribute_charges",toluaI_object_distribute_charges00);
 tolua_function(tolua_S,NULL,"reduce_charges",toluaI_object_reduce_charges00);
 tolua_function(tolua_S,NULL,"object_similar",toluaI_object_object_similar00);
 tolua_function(tolua_S,NULL,"object_absorb",toluaI_object_object_absorb00);
 tolua_function(tolua_S,NULL,"lookup_kind",toluaI_object_lookup_kind00);
 tolua_function(tolua_S,NULL,"object_wipe",toluaI_object_object_wipe00);
 tolua_function(tolua_S,NULL,"object_prep",toluaI_object_object_prep00);
 tolua_function(tolua_S,NULL,"add_ego_flags",toluaI_object_add_ego_flags00);
 tolua_function(tolua_S,NULL,"add_ego_power",toluaI_object_add_ego_power00);
 tolua_function(tolua_S,NULL,"m_bonus",toluaI_object_m_bonus00);
 tolua_function(tolua_S,NULL,"apply_magic",toluaI_object_apply_magic00);
 tolua_function(tolua_S,NULL,"init_match_hook",toluaI_object_init_match_hook00);
 tolua_function(tolua_S,NULL,"kind_is_match",toluaI_object_kind_is_match00);
 tolua_function(tolua_S,NULL,"init_match_theme",toluaI_object_init_match_theme00);
 tolua_function(tolua_S,NULL,"kind_is_theme",toluaI_object_kind_is_theme00);
 tolua_function(tolua_S,NULL,"make_object",toluaI_object_make_object00);
 tolua_function(tolua_S,NULL,"place_object",toluaI_object_place_object00);
 tolua_function(tolua_S,NULL,"make_gold",toluaI_object_make_gold00);
 tolua_function(tolua_S,NULL,"place_gold",toluaI_object_place_gold00);
 tolua_function(tolua_S,NULL,"drop_near",toluaI_object_drop_near00);
 tolua_function(tolua_S,NULL,"acquirement",toluaI_object_acquirement00);
 tolua_function(tolua_S,NULL,"item_activation",toluaI_object_item_activation00);
 tolua_function(tolua_S,NULL,"can_player_destroy_object",toluaI_object_can_player_destroy_object00);
 tolua_function(tolua_S,NULL,"display_koff",toluaI_object_display_koff00);
 tolua_function(tolua_S,NULL,"create_artifact",toluaI_object_create_artifact00);
 tolua_function(tolua_S,NULL,"create_named_art",toluaI_object_create_named_art00);
 tolua_function(tolua_S,NULL,"k_info_alloc",toluaI_object_k_info_alloc00);
 tolua_function(tolua_S,NULL,"k_info_free",toluaI_object_k_info_free00);
 tolua_function(tolua_S,NULL,"k_info_add",toluaI_object_k_info_add00);
 tolua_function(tolua_S,NULL,"get_object_level",toluaI_object_get_object_level00);
 tolua_function(tolua_S,NULL,"get_object_name",toluaI_object_get_object_name00);
 tolua_function(tolua_S,NULL,"get_object_d_attr",toluaI_object_get_object_d_attr00);
 tolua_function(tolua_S,NULL,"get_object_x_attr",toluaI_object_get_object_x_attr00);
 tolua_function(tolua_S,NULL,"get_object_d_char",toluaI_object_get_object_d_char00);
 tolua_function(tolua_S,NULL,"get_object_x_char",toluaI_object_get_object_x_char00);
 tolua_function(tolua_S,NULL,"get_object_aware",toluaI_object_get_object_aware00);
 tolua_function(tolua_S,NULL,"get_object_tried",toluaI_object_get_object_tried00);
 tolua_function(tolua_S,NULL,"object_is_potion",toluaI_object_object_is_potion00);
 tolua_function(tolua_S,NULL,"init_object_alloc",toluaI_object_init_object_alloc00);
 tolua_function(tolua_S,NULL,"k_info_reset",toluaI_object_k_info_reset00);
 return 1;
}
/* Close function */
void tolua_object_close (lua_State* tolua_S)
{
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"MAX_TRIGGER");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TRIGGER_USE");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TRIGGER_MAKE");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TRIGGER_BONUS");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TRIGGER_SMASH");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TRIGGER_DESC");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"EGO_XTRA_SUSTAIN");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"EGO_XTRA_LO_RESIST");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"EGO_XTRA_HI_RESIST");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"EGO_XTRA_ANY_RESIST");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"EGO_XTRA_ABILITY");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"EGO_XTRA_POWER");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TV_ANY");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TV_SKELETON");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TV_BOTTLE");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TV_JUNK");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TV_SPIKE");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TV_CHEST");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TV_FIGURINE");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TV_STATUE");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TV_SHOT");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TV_ARROW");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TV_BOLT");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TV_BOW");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TV_DIGGING");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TV_HAFTED");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TV_POLEARM");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TV_SWORD");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TV_BOOTS");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TV_GLOVES");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TV_HELM");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TV_CROWN");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TV_SHIELD");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TV_CLOAK");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TV_SOFT_ARMOR");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TV_HARD_ARMOR");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TV_DRAG_ARMOR");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TV_LITE");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TV_AMULET");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TV_RING");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TV_STAFF");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TV_WAND");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TV_ROD");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TV_SCROLL");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TV_POTION");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TV_FLASK");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TV_FOOD");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TV_LIFE_BOOK");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TV_SORCERY_BOOK");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TV_NATURE_BOOK");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TV_CHAOS_BOOK");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TV_DEATH_BOOK");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TV_TRUMP_BOOK");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TV_ARCANE_BOOK");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TV_GOLD");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TV_BOOKS_MIN");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TV_BOOKS_MAX");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_WAND_HEAL_MONSTER");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_WAND_HASTE_MONSTER");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_WAND_CLONE_MONSTER");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_WAND_TELEPORT_AWAY");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_WAND_DISARMING");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_WAND_TRAP_DOOR_DEST");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_WAND_STONE_TO_MUD");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_WAND_LITE");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_WAND_SLEEP_MONSTER");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_WAND_SLOW_MONSTER");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_WAND_CONFUSE_MONSTER");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_WAND_FEAR_MONSTER");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_WAND_DRAIN_LIFE");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_WAND_POLYMORPH");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_WAND_STINKING_CLOUD");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_WAND_MAGIC_MISSILE");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_WAND_ACID_BOLT");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_WAND_CHARM_MONSTER");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_WAND_FIRE_BOLT");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_WAND_COLD_BOLT");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_WAND_ACID_BALL");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_WAND_ELEC_BALL");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_WAND_FIRE_BALL");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_WAND_COLD_BALL");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_WAND_WONDER");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"OBJ_GOLD_LIST");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"MAX_GOLD");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"OC_NONE");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"OC_NORMAL");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"OC_FORCE_BAD");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"OC_FORCE_GOOD");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TR0_STR");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TR0_INT");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TR0_WIS");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TR0_DEX");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TR0_CON");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TR0_CHR");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TR0_XXX1");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TR0_SP");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TR0_STEALTH");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TR0_SEARCH");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TR0_INFRA");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TR0_TUNNEL");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TR0_SPEED");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TR0_BLOWS");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TR0_CHAOTIC");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TR0_VAMPIRIC");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TR0_SLAY_ANIMAL");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TR0_SLAY_EVIL");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TR0_SLAY_UNDEAD");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TR0_SLAY_DEMON");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TR0_SLAY_ORC");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TR0_SLAY_TROLL");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TR0_SLAY_GIANT");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TR0_SLAY_DRAGON");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TR0_KILL_DRAGON");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TR0_VORPAL");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TR0_IMPACT");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TR0_BRAND_POIS");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TR0_BRAND_ACID");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TR0_BRAND_ELEC");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TR0_BRAND_FIRE");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TR0_BRAND_COLD");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TR1_SUST_STR");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TR1_SUST_INT");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TR1_SUST_WIS");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TR1_SUST_DEX");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TR1_SUST_CON");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TR1_SUST_CHR");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TR1_XXX1");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TR1_IM_POIS");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TR1_IM_ACID");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TR1_IM_ELEC");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TR1_IM_FIRE");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TR1_IM_COLD");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TR1_THROW");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TR1_REFLECT");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TR1_FREE_ACT");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TR1_HOLD_LIFE");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TR1_RES_ACID");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TR1_RES_ELEC");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TR1_RES_FIRE");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TR1_RES_COLD");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TR1_RES_POIS");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TR1_RES_FEAR");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TR1_RES_LITE");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TR1_RES_DARK");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TR1_RES_BLIND");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TR1_RES_CONF");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TR1_RES_SOUND");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TR1_RES_SHARDS");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TR1_RES_NETHER");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TR1_RES_NEXUS");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TR1_RES_CHAOS");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TR1_RES_DISEN");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TR2_SH_FIRE");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TR2_SH_ELEC");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TR2_QUESTITEM");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TR2_XXX4");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TR2_NO_TELE");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TR2_NO_MAGIC");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TR2_XXX7");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TR2_TY_CURSE");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TR2_EASY_KNOW");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TR2_HIDE_TYPE");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TR2_SHOW_MODS");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TR2_INSTA_ART");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TR2_FEATHER");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TR2_LITE");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TR2_SEE_INVIS");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TR2_TELEPATHY");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TR2_SLOW_DIGEST");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TR2_REGEN");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TR2_XTRA_MIGHT");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TR2_XTRA_SHOTS");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TR2_IGNORE_ACID");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TR2_IGNORE_ELEC");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TR2_IGNORE_FIRE");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TR2_IGNORE_COLD");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TR2_ACTIVATE");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TR2_DRAIN_EXP");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TR2_TELEPORT");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TR2_AGGRAVATE");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TR2_BLESSED");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TR2_CURSED");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TR2_HEAVY_CURSE");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TR2_PERMA_CURSE");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TR3_LUCK_10");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TR3_XXX2");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TR3_XXX3");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TR3_XXX4");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TR3_XXX5");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TR3_XXX6");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TR3_XXX7");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TR3_XXX8");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TR3_IM_LITE");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TR3_IM_DARK");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TR3_SH_ACID");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TR3_SH_COLD");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TR3_MUTATE");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TR3_PATRON");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TR3_STRANGE_LUCK");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TR3_PASS_WALL");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TR3_GHOUL_TOUCH");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TR3_PSI_CRIT");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TR3_RETURN");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TR3_EXPLODE");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TR3_HURT_ACID");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TR3_HURT_ELEC");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TR3_HURT_FIRE");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TR3_HURT_COLD");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TR3_HURT_LITE");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TR3_HURT_DARK");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TR3_XXX27");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TR3_XXX28");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TR3_AUTO_CURSE");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TR3_DRAIN_STATS");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TR3_CANT_EAT");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TR3_SLOW_HEAL");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"object_kind");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"obj_theme");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"object_type");
 lua_getglobals(tolua_S);
 lua_pushstring(tolua_S,"o_max"); lua_pushnil(tolua_S); lua_rawset(tolua_S,-3);
 lua_pop(tolua_S,1);
 lua_getglobals(tolua_S);
 lua_pushstring(tolua_S,"o_cnt"); lua_pushnil(tolua_S); lua_rawset(tolua_S,-3);
 lua_pop(tolua_S,1);
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"o_list");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"k_info");
 lua_getglobals(tolua_S);
 lua_pushstring(tolua_S,"k_name"); lua_pushnil(tolua_S); lua_rawset(tolua_S,-3);
 lua_pop(tolua_S,1);
 lua_getglobals(tolua_S);
 lua_pushstring(tolua_S,"k_text"); lua_pushnil(tolua_S); lua_rawset(tolua_S,-3);
 lua_pop(tolua_S,1);
 lua_getglobals(tolua_S);
 lua_pushstring(tolua_S,"a_name"); lua_pushnil(tolua_S); lua_rawset(tolua_S,-3);
 lua_pop(tolua_S,1);
 lua_getglobals(tolua_S);
 lua_pushstring(tolua_S,"a_text"); lua_pushnil(tolua_S); lua_rawset(tolua_S,-3);
 lua_pop(tolua_S,1);
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"reset_visuals");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"identify_fully_aux");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"wield_slot");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"mention_use");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"describe_use");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"item_charges");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"item_describe");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"item_split");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"item_increase");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"inven_carry_okay");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"inven_carry");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"inven_takeoff");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"inven_drop");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"item_tester_hook_weapon");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"item_tester_hook_melee_weapon");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"item_tester_hook_nonsword");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"item_tester_hook_ammo");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"item_tester_hook_fletcher");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"item_tester_hook_armour");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"item_tester_hook_soft_armour");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"item_tester_hook_hard_armour");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"item_tester_hook_helm");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"item_tester_hook_pure_hard_armour");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"item_tester_hook_weapon_armour");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"item_tester_hook_wear");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"item_tester_hook_recharge");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"item_tester_hook_jewel");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"item_tester_hook_tval");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"item_tester_hook_is_blessed");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"item_tester_hook_is_good");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"item_tester_hook_is_great");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"item_tester_hook_is_book");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"item_tester_okay");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"display_inven");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"display_equip");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"show_list");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"show_equip");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"toggle_inven_equip");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"get_item");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"delete_dungeon_object");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"delete_object");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"delete_object_list");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"drop_object_list");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"object_known");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"object_aware");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"object_tried");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"object_mental");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"flag_cost");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"object_value");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"object_value_real");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"distribute_charges");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"reduce_charges");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"object_similar");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"object_absorb");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"lookup_kind");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"object_wipe");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"object_prep");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"add_ego_flags");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"add_ego_power");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"m_bonus");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"apply_magic");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"init_match_hook");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"kind_is_match");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"init_match_theme");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"kind_is_theme");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"make_object");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"place_object");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"make_gold");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"place_gold");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"drop_near");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"acquirement");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"item_activation");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"can_player_destroy_object");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"display_koff");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"create_artifact");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"create_named_art");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"k_info_alloc");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"k_info_free");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"k_info_add");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"get_object_level");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"get_object_name");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"get_object_d_attr");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"get_object_x_attr");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"get_object_d_char");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"get_object_x_char");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"get_object_aware");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"get_object_tried");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"object_is_potion");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"init_object_alloc");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"k_info_reset");
}
