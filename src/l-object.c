/*
** Lua binding: object
** Generated automatically by tolua 4.0a - angband.
*/

#include "lua/tolua.h"

/* Exported function */
int  tolua_object_open (lua_State* tolua_S);
void tolua_object_close (lua_State* tolua_S);

#include "angband.h"

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

/* get function: flags1 of class  object_kind */
static int toluaI_get_object_object_kind_flags1(lua_State* tolua_S)
{
  object_kind* self = (object_kind*)  tolua_getusertype(tolua_S,1,0);
  if (!self) TOLUA_ERR_SELF;
  tolua_pushnumber(tolua_S,(long)self->flags1);
 return 1;
}

/* set function: flags1 of class  object_kind */
static int toluaI_set_object_object_kind_flags1(lua_State* tolua_S)
{
  object_kind* self = (object_kind*)  tolua_getusertype(tolua_S,1,0);
  if (!self) TOLUA_ERR_SELF;
  if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
   TOLUA_ERR_ASSIGN;
  self->flags1 = ((u32b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: flags2 of class  object_kind */
static int toluaI_get_object_object_kind_flags2(lua_State* tolua_S)
{
  object_kind* self = (object_kind*)  tolua_getusertype(tolua_S,1,0);
  if (!self) TOLUA_ERR_SELF;
  tolua_pushnumber(tolua_S,(long)self->flags2);
 return 1;
}

/* set function: flags2 of class  object_kind */
static int toluaI_set_object_object_kind_flags2(lua_State* tolua_S)
{
  object_kind* self = (object_kind*)  tolua_getusertype(tolua_S,1,0);
  if (!self) TOLUA_ERR_SELF;
  if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
   TOLUA_ERR_ASSIGN;
  self->flags2 = ((u32b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: flags3 of class  object_kind */
static int toluaI_get_object_object_kind_flags3(lua_State* tolua_S)
{
  object_kind* self = (object_kind*)  tolua_getusertype(tolua_S,1,0);
  if (!self) TOLUA_ERR_SELF;
  tolua_pushnumber(tolua_S,(long)self->flags3);
 return 1;
}

/* set function: flags3 of class  object_kind */
static int toluaI_set_object_object_kind_flags3(lua_State* tolua_S)
{
  object_kind* self = (object_kind*)  tolua_getusertype(tolua_S,1,0);
  if (!self) TOLUA_ERR_SELF;
  if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
   TOLUA_ERR_ASSIGN;
  self->flags3 = ((u32b)  tolua_getnumber(tolua_S,2,0));
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
 toluaI_index = (int)tolua_getnumber(tolua_S,2,0)-1;
 if (toluaI_index<0 || toluaI_index>=4)
  tolua_error(tolua_S,"array indexing out of range.");
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
 toluaI_index = (int)tolua_getnumber(tolua_S,2,0)-1;
 if (toluaI_index<0 || toluaI_index>=4)
  tolua_error(tolua_S,"array indexing out of range.");
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
 toluaI_index = (int)tolua_getnumber(tolua_S,2,0)-1;
 if (toluaI_index<0 || toluaI_index>=4)
  tolua_error(tolua_S,"array indexing out of range.");
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
 toluaI_index = (int)tolua_getnumber(tolua_S,2,0)-1;
 if (toluaI_index<0 || toluaI_index>=4)
  tolua_error(tolua_S,"array indexing out of range.");
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

/* get function: flags1 of class  object_type */
static int toluaI_get_object_object_type_flags1(lua_State* tolua_S)
{
  object_type* self = (object_type*)  tolua_getusertype(tolua_S,1,0);
  if (!self) TOLUA_ERR_SELF;
  tolua_pushnumber(tolua_S,(long)self->flags1);
 return 1;
}

/* set function: flags1 of class  object_type */
static int toluaI_set_object_object_type_flags1(lua_State* tolua_S)
{
  object_type* self = (object_type*)  tolua_getusertype(tolua_S,1,0);
  if (!self) TOLUA_ERR_SELF;
  if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
   TOLUA_ERR_ASSIGN;
  self->flags1 = ((u32b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: flags2 of class  object_type */
static int toluaI_get_object_object_type_flags2(lua_State* tolua_S)
{
  object_type* self = (object_type*)  tolua_getusertype(tolua_S,1,0);
  if (!self) TOLUA_ERR_SELF;
  tolua_pushnumber(tolua_S,(long)self->flags2);
 return 1;
}

/* set function: flags2 of class  object_type */
static int toluaI_set_object_object_type_flags2(lua_State* tolua_S)
{
  object_type* self = (object_type*)  tolua_getusertype(tolua_S,1,0);
  if (!self) TOLUA_ERR_SELF;
  if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
   TOLUA_ERR_ASSIGN;
  self->flags2 = ((u32b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: flags3 of class  object_type */
static int toluaI_get_object_object_type_flags3(lua_State* tolua_S)
{
  object_type* self = (object_type*)  tolua_getusertype(tolua_S,1,0);
  if (!self) TOLUA_ERR_SELF;
  tolua_pushnumber(tolua_S,(long)self->flags3);
 return 1;
}

/* set function: flags3 of class  object_type */
static int toluaI_set_object_object_type_flags3(lua_State* tolua_S)
{
  object_type* self = (object_type*)  tolua_getusertype(tolua_S,1,0);
  if (!self) TOLUA_ERR_SELF;
  if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
   TOLUA_ERR_ASSIGN;
  self->flags3 = ((u32b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: kn_flags1 of class  object_type */
static int toluaI_get_object_object_type_kn_flags1(lua_State* tolua_S)
{
  object_type* self = (object_type*)  tolua_getusertype(tolua_S,1,0);
  if (!self) TOLUA_ERR_SELF;
  tolua_pushnumber(tolua_S,(long)self->kn_flags1);
 return 1;
}

/* set function: kn_flags1 of class  object_type */
static int toluaI_set_object_object_type_kn_flags1(lua_State* tolua_S)
{
  object_type* self = (object_type*)  tolua_getusertype(tolua_S,1,0);
  if (!self) TOLUA_ERR_SELF;
  if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
   TOLUA_ERR_ASSIGN;
  self->kn_flags1 = ((u32b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: kn_flags2 of class  object_type */
static int toluaI_get_object_object_type_kn_flags2(lua_State* tolua_S)
{
  object_type* self = (object_type*)  tolua_getusertype(tolua_S,1,0);
  if (!self) TOLUA_ERR_SELF;
  tolua_pushnumber(tolua_S,(long)self->kn_flags2);
 return 1;
}

/* set function: kn_flags2 of class  object_type */
static int toluaI_set_object_object_type_kn_flags2(lua_State* tolua_S)
{
  object_type* self = (object_type*)  tolua_getusertype(tolua_S,1,0);
  if (!self) TOLUA_ERR_SELF;
  if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
   TOLUA_ERR_ASSIGN;
  self->kn_flags2 = ((u32b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: kn_flags3 of class  object_type */
static int toluaI_get_object_object_type_kn_flags3(lua_State* tolua_S)
{
  object_type* self = (object_type*)  tolua_getusertype(tolua_S,1,0);
  if (!self) TOLUA_ERR_SELF;
  tolua_pushnumber(tolua_S,(long)self->kn_flags3);
 return 1;
}

/* set function: kn_flags3 of class  object_type */
static int toluaI_set_object_object_type_kn_flags3(lua_State* tolua_S)
{
  object_type* self = (object_type*)  tolua_getusertype(tolua_S,1,0);
  if (!self) TOLUA_ERR_SELF;
  if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
   TOLUA_ERR_ASSIGN;
  self->kn_flags3 = ((u32b)  tolua_getnumber(tolua_S,2,0));
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

/* get function: activate of class  object_type */
static int toluaI_get_object_object_type_activate(lua_State* tolua_S)
{
  object_type* self = (object_type*)  tolua_getusertype(tolua_S,1,0);
  if (!self) TOLUA_ERR_SELF;
  tolua_pushnumber(tolua_S,(long)self->activate);
 return 1;
}

/* set function: activate of class  object_type */
static int toluaI_set_object_object_type_activate(lua_State* tolua_S)
{
  object_type* self = (object_type*)  tolua_getusertype(tolua_S,1,0);
  if (!self) TOLUA_ERR_SELF;
  if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
   TOLUA_ERR_ASSIGN;
  self->activate = ((byte)  tolua_getnumber(tolua_S,2,0));
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
 toluaI_index = (int)tolua_getnumber(tolua_S,2,0)-1;
 if (toluaI_index<0 || toluaI_index>=o_max)
  tolua_error(tolua_S,"array indexing out of range.");
 tolua_pushusertype(tolua_S,(void*)&o_list[toluaI_index],tolua_tag(tolua_S,"object_type"));
 return 1;
}

/* set function: o_list */
static int toluaI_set_object_o_list(lua_State* tolua_S)
{
 int toluaI_index;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
  tolua_error(tolua_S,"invalid type in array indexing.");
 toluaI_index = (int)tolua_getnumber(tolua_S,2,0)-1;
 if (toluaI_index<0 || toluaI_index>=o_max)
  tolua_error(tolua_S,"array indexing out of range.");
  o_list[toluaI_index] = *((object_type*)  tolua_getusertype(tolua_S,3,0));
 return 0;
}

/* get function: k_info */
static int toluaI_get_object_k_info(lua_State* tolua_S)
{
 int toluaI_index;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
  tolua_error(tolua_S,"invalid type in array indexing.");
 toluaI_index = (int)tolua_getnumber(tolua_S,2,0)-1;
 if (toluaI_index<0 || toluaI_index>=z_info->k_max)
  tolua_error(tolua_S,"array indexing out of range.");
 tolua_pushusertype(tolua_S,(void*)&k_info[toluaI_index],tolua_tag(tolua_S,"object_kind"));
 return 1;
}

/* set function: k_info */
static int toluaI_set_object_k_info(lua_State* tolua_S)
{
 int toluaI_index;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
  tolua_error(tolua_S,"invalid type in array indexing.");
 toluaI_index = (int)tolua_getnumber(tolua_S,2,0)-1;
 if (toluaI_index<0 || toluaI_index>=z_info->k_max)
  tolua_error(tolua_S,"array indexing out of range.");
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

/* function: random_resistance */
static int toluaI_object_random_resistance00(lua_State* tolua_S)
{
 if (
     !tolua_istype(tolua_S,1,tolua_tag(tolua_S,"object_type"),0) ||
     !tolua_istype(tolua_S,2,LUA_TNUMBER,0) ||
     !tolua_isnoobj(tolua_S,3)
 )
 {
  tolua_error(tolua_S,"#ferror in function 'random_resistance'.");
  return 0;
 }
 else
 {
  object_type* o_ptr = ((object_type*)  tolua_getusertype(tolua_S,1,0));
  int specific = ((int)  tolua_getnumber(tolua_S,2,0));
  {
   random_resistance(o_ptr,specific);
  }
 }
 return 0;
}

/* function: activate_effect */
static int toluaI_object_activate_effect00(lua_State* tolua_S)
{
 if (
     !tolua_istype(tolua_S,1,tolua_tag(tolua_S,"object_type"),0) ||
     !tolua_isnoobj(tolua_S,2)
 )
 {
  tolua_error(tolua_S,"#ferror in function 'activate_effect'.");
  return 0;
 }
 else
 {
  object_type* o_ptr = ((object_type*)  tolua_getusertype(tolua_S,1,0));
  {
   bool toluaI_ret = (bool)  activate_effect(o_ptr);
   tolua_pushbool(tolua_S,(int)toluaI_ret);
  }
 }
 return 1;
}

/* function: random_artifact_resistance */
static int toluaI_object_random_artifact_resistance00(lua_State* tolua_S)
{
 if (
     !tolua_istype(tolua_S,1,tolua_tag(tolua_S,"object_type"),0) ||
     !tolua_isnoobj(tolua_S,2)
 )
 {
  tolua_error(tolua_S,"#ferror in function 'random_artifact_resistance'.");
  return 0;
 }
 else
 {
  object_type* o_ptr = ((object_type*)  tolua_getusertype(tolua_S,1,0));
  {
   random_artifact_resistance(o_ptr);
  }
 }
 return 0;
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
 tolua_constant(tolua_S,NULL,"ACT_SUNLIGHT",ACT_SUNLIGHT);
 tolua_constant(tolua_S,NULL,"ACT_BO_MISS_1",ACT_BO_MISS_1);
 tolua_constant(tolua_S,NULL,"ACT_BA_POIS_1",ACT_BA_POIS_1);
 tolua_constant(tolua_S,NULL,"ACT_BO_ELEC_1",ACT_BO_ELEC_1);
 tolua_constant(tolua_S,NULL,"ACT_BO_ACID_1",ACT_BO_ACID_1);
 tolua_constant(tolua_S,NULL,"ACT_BO_COLD_1",ACT_BO_COLD_1);
 tolua_constant(tolua_S,NULL,"ACT_BO_FIRE_1",ACT_BO_FIRE_1);
 tolua_constant(tolua_S,NULL,"ACT_BA_COLD_1",ACT_BA_COLD_1);
 tolua_constant(tolua_S,NULL,"ACT_BA_FIRE_1",ACT_BA_FIRE_1);
 tolua_constant(tolua_S,NULL,"ACT_DRAIN_1",ACT_DRAIN_1);
 tolua_constant(tolua_S,NULL,"ACT_BA_COLD_2",ACT_BA_COLD_2);
 tolua_constant(tolua_S,NULL,"ACT_BA_ELEC_2",ACT_BA_ELEC_2);
 tolua_constant(tolua_S,NULL,"ACT_DRAIN_2",ACT_DRAIN_2);
 tolua_constant(tolua_S,NULL,"ACT_VAMPIRE_1",ACT_VAMPIRE_1);
 tolua_constant(tolua_S,NULL,"ACT_BO_MISS_2",ACT_BO_MISS_2);
 tolua_constant(tolua_S,NULL,"ACT_BA_FIRE_2",ACT_BA_FIRE_2);
 tolua_constant(tolua_S,NULL,"ACT_BA_COLD_3",ACT_BA_COLD_3);
 tolua_constant(tolua_S,NULL,"ACT_BA_ELEC_3",ACT_BA_ELEC_3);
 tolua_constant(tolua_S,NULL,"ACT_WHIRLWIND",ACT_WHIRLWIND);
 tolua_constant(tolua_S,NULL,"ACT_VAMPIRE_2",ACT_VAMPIRE_2);
 tolua_constant(tolua_S,NULL,"ACT_CALL_CHAOS",ACT_CALL_CHAOS);
 tolua_constant(tolua_S,NULL,"ACT_ROCKET",ACT_ROCKET);
 tolua_constant(tolua_S,NULL,"ACT_DISP_EVIL",ACT_DISP_EVIL);
 tolua_constant(tolua_S,NULL,"ACT_BA_MISS_3",ACT_BA_MISS_3);
 tolua_constant(tolua_S,NULL,"ACT_DISP_GOOD",ACT_DISP_GOOD);
 tolua_constant(tolua_S,NULL,"ACT_CONFUSE",ACT_CONFUSE);
 tolua_constant(tolua_S,NULL,"ACT_SLEEP",ACT_SLEEP);
 tolua_constant(tolua_S,NULL,"ACT_QUAKE",ACT_QUAKE);
 tolua_constant(tolua_S,NULL,"ACT_TERROR",ACT_TERROR);
 tolua_constant(tolua_S,NULL,"ACT_TELE_AWAY",ACT_TELE_AWAY);
 tolua_constant(tolua_S,NULL,"ACT_BANISH_EVIL",ACT_BANISH_EVIL);
 tolua_constant(tolua_S,NULL,"ACT_GENOCIDE",ACT_GENOCIDE);
 tolua_constant(tolua_S,NULL,"ACT_MASS_GENO",ACT_MASS_GENO);
 tolua_constant(tolua_S,NULL,"ACT_CHARM_ANIMAL",ACT_CHARM_ANIMAL);
 tolua_constant(tolua_S,NULL,"ACT_CHARM_UNDEAD",ACT_CHARM_UNDEAD);
 tolua_constant(tolua_S,NULL,"ACT_CHARM_OTHER",ACT_CHARM_OTHER);
 tolua_constant(tolua_S,NULL,"ACT_CHARM_ANIMALS",ACT_CHARM_ANIMALS);
 tolua_constant(tolua_S,NULL,"ACT_CHARM_OTHERS",ACT_CHARM_OTHERS);
 tolua_constant(tolua_S,NULL,"ACT_SUMMON_ANIMAL",ACT_SUMMON_ANIMAL);
 tolua_constant(tolua_S,NULL,"ACT_SUMMON_PHANTOM",ACT_SUMMON_PHANTOM);
 tolua_constant(tolua_S,NULL,"ACT_SUMMON_ELEMENTAL",ACT_SUMMON_ELEMENTAL);
 tolua_constant(tolua_S,NULL,"ACT_SUMMON_DEMON",ACT_SUMMON_DEMON);
 tolua_constant(tolua_S,NULL,"ACT_SUMMON_UNDEAD",ACT_SUMMON_UNDEAD);
 tolua_constant(tolua_S,NULL,"ACT_CURE_LW",ACT_CURE_LW);
 tolua_constant(tolua_S,NULL,"ACT_CURE_MW",ACT_CURE_MW);
 tolua_constant(tolua_S,NULL,"ACT_CURE_POISON",ACT_CURE_POISON);
 tolua_constant(tolua_S,NULL,"ACT_REST_LIFE",ACT_REST_LIFE);
 tolua_constant(tolua_S,NULL,"ACT_REST_ALL",ACT_REST_ALL);
 tolua_constant(tolua_S,NULL,"ACT_CURE_700",ACT_CURE_700);
 tolua_constant(tolua_S,NULL,"ACT_CURE_1000",ACT_CURE_1000);
 tolua_constant(tolua_S,NULL,"ACT_ESP",ACT_ESP);
 tolua_constant(tolua_S,NULL,"ACT_BERSERK",ACT_BERSERK);
 tolua_constant(tolua_S,NULL,"ACT_PROT_EVIL",ACT_PROT_EVIL);
 tolua_constant(tolua_S,NULL,"ACT_RESIST_ALL",ACT_RESIST_ALL);
 tolua_constant(tolua_S,NULL,"ACT_SPEED",ACT_SPEED);
 tolua_constant(tolua_S,NULL,"ACT_XTRA_SPEED",ACT_XTRA_SPEED);
 tolua_constant(tolua_S,NULL,"ACT_WRAITH",ACT_WRAITH);
 tolua_constant(tolua_S,NULL,"ACT_INVULN",ACT_INVULN);
 tolua_constant(tolua_S,NULL,"ACT_TELEPORT_1",ACT_TELEPORT_1);
 tolua_constant(tolua_S,NULL,"ACT_LIGHT",ACT_LIGHT);
 tolua_constant(tolua_S,NULL,"ACT_MAP_LIGHT",ACT_MAP_LIGHT);
 tolua_constant(tolua_S,NULL,"ACT_DETECT_ALL",ACT_DETECT_ALL);
 tolua_constant(tolua_S,NULL,"ACT_DETECT_XTRA",ACT_DETECT_XTRA);
 tolua_constant(tolua_S,NULL,"ACT_ID_FULL",ACT_ID_FULL);
 tolua_constant(tolua_S,NULL,"ACT_ID_PLAIN",ACT_ID_PLAIN);
 tolua_constant(tolua_S,NULL,"ACT_RUNE_EXPLO",ACT_RUNE_EXPLO);
 tolua_constant(tolua_S,NULL,"ACT_RUNE_PROT",ACT_RUNE_PROT);
 tolua_constant(tolua_S,NULL,"ACT_SATIATE",ACT_SATIATE);
 tolua_constant(tolua_S,NULL,"ACT_DEST_DOOR",ACT_DEST_DOOR);
 tolua_constant(tolua_S,NULL,"ACT_STONE_MUD",ACT_STONE_MUD);
 tolua_constant(tolua_S,NULL,"ACT_RECHARGE",ACT_RECHARGE);
 tolua_constant(tolua_S,NULL,"ACT_ALCHEMY",ACT_ALCHEMY);
 tolua_constant(tolua_S,NULL,"ACT_DIM_DOOR",ACT_DIM_DOOR);
 tolua_constant(tolua_S,NULL,"ACT_TELEPORT_2",ACT_TELEPORT_2);
 tolua_constant(tolua_S,NULL,"ACT_RECALL",ACT_RECALL);
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
 tolua_constant(tolua_S,NULL,"SV_ANY",SV_ANY);
 tolua_constant(tolua_S,NULL,"SV_FIGURINE_NORMAL",SV_FIGURINE_NORMAL);
 tolua_constant(tolua_S,NULL,"SV_WOODEN_STATUE",SV_WOODEN_STATUE);
 tolua_constant(tolua_S,NULL,"SV_CLAY_STATUE",SV_CLAY_STATUE);
 tolua_constant(tolua_S,NULL,"SV_STONE_STATUE",SV_STONE_STATUE);
 tolua_constant(tolua_S,NULL,"SV_IRON_STATUE",SV_IRON_STATUE);
 tolua_constant(tolua_S,NULL,"SV_COPPER_STATUE",SV_COPPER_STATUE);
 tolua_constant(tolua_S,NULL,"SV_SILVER_STATUE",SV_SILVER_STATUE);
 tolua_constant(tolua_S,NULL,"SV_GOLDEN_STATUE",SV_GOLDEN_STATUE);
 tolua_constant(tolua_S,NULL,"SV_IVORY_STATUE",SV_IVORY_STATUE);
 tolua_constant(tolua_S,NULL,"SV_MITHRIL_STATUE",SV_MITHRIL_STATUE);
 tolua_constant(tolua_S,NULL,"SV_ORNATE_STATUE",SV_ORNATE_STATUE);
 tolua_constant(tolua_S,NULL,"SV_AMMO_LIGHT",SV_AMMO_LIGHT);
 tolua_constant(tolua_S,NULL,"SV_AMMO_NORMAL",SV_AMMO_NORMAL);
 tolua_constant(tolua_S,NULL,"SV_AMMO_HEAVY",SV_AMMO_HEAVY);
 tolua_constant(tolua_S,NULL,"SV_SLING",SV_SLING);
 tolua_constant(tolua_S,NULL,"SV_SHORT_BOW",SV_SHORT_BOW);
 tolua_constant(tolua_S,NULL,"SV_LONG_BOW",SV_LONG_BOW);
 tolua_constant(tolua_S,NULL,"SV_LIGHT_XBOW",SV_LIGHT_XBOW);
 tolua_constant(tolua_S,NULL,"SV_HEAVY_XBOW",SV_HEAVY_XBOW);
 tolua_constant(tolua_S,NULL,"SV_SHOVEL",SV_SHOVEL);
 tolua_constant(tolua_S,NULL,"SV_GNOMISH_SHOVEL",SV_GNOMISH_SHOVEL);
 tolua_constant(tolua_S,NULL,"SV_DWARVEN_SHOVEL",SV_DWARVEN_SHOVEL);
 tolua_constant(tolua_S,NULL,"SV_PICK",SV_PICK);
 tolua_constant(tolua_S,NULL,"SV_ORCISH_PICK",SV_ORCISH_PICK);
 tolua_constant(tolua_S,NULL,"SV_DWARVEN_PICK",SV_DWARVEN_PICK);
 tolua_constant(tolua_S,NULL,"SV_MATTOCK",SV_MATTOCK);
 tolua_constant(tolua_S,NULL,"SV_CLUB",SV_CLUB);
 tolua_constant(tolua_S,NULL,"SV_WHIP",SV_WHIP);
 tolua_constant(tolua_S,NULL,"SV_QUARTERSTAFF",SV_QUARTERSTAFF);
 tolua_constant(tolua_S,NULL,"SV_NUNCHAKU",SV_NUNCHAKU);
 tolua_constant(tolua_S,NULL,"SV_MACE",SV_MACE);
 tolua_constant(tolua_S,NULL,"SV_BALL_AND_CHAIN",SV_BALL_AND_CHAIN);
 tolua_constant(tolua_S,NULL,"SV_JO_STAFF",SV_JO_STAFF);
 tolua_constant(tolua_S,NULL,"SV_WAR_HAMMER",SV_WAR_HAMMER);
 tolua_constant(tolua_S,NULL,"SV_THREE_PIECE_ROD",SV_THREE_PIECE_ROD);
 tolua_constant(tolua_S,NULL,"SV_MORNING_STAR",SV_MORNING_STAR);
 tolua_constant(tolua_S,NULL,"SV_FLAIL",SV_FLAIL);
 tolua_constant(tolua_S,NULL,"SV_BO_STAFF",SV_BO_STAFF);
 tolua_constant(tolua_S,NULL,"SV_LEAD_FILLED_MACE",SV_LEAD_FILLED_MACE);
 tolua_constant(tolua_S,NULL,"SV_TETSUBO",SV_TETSUBO);
 tolua_constant(tolua_S,NULL,"SV_TWO_HANDED_FLAIL",SV_TWO_HANDED_FLAIL);
 tolua_constant(tolua_S,NULL,"SV_GREAT_HAMMER",SV_GREAT_HAMMER);
 tolua_constant(tolua_S,NULL,"SV_MACE_OF_DISRUPTION",SV_MACE_OF_DISRUPTION);
 tolua_constant(tolua_S,NULL,"SV_GROND",SV_GROND);
 tolua_constant(tolua_S,NULL,"SV_HATCHET",SV_HATCHET);
 tolua_constant(tolua_S,NULL,"SV_SPEAR",SV_SPEAR);
 tolua_constant(tolua_S,NULL,"SV_SICKLE",SV_SICKLE);
 tolua_constant(tolua_S,NULL,"SV_AWL_PIKE",SV_AWL_PIKE);
 tolua_constant(tolua_S,NULL,"SV_TRIDENT",SV_TRIDENT);
 tolua_constant(tolua_S,NULL,"SV_FAUCHARD",SV_FAUCHARD);
 tolua_constant(tolua_S,NULL,"SV_BROAD_SPEAR",SV_BROAD_SPEAR);
 tolua_constant(tolua_S,NULL,"SV_PIKE",SV_PIKE);
 tolua_constant(tolua_S,NULL,"SV_NAGINATA",SV_NAGINATA);
 tolua_constant(tolua_S,NULL,"SV_BEAKED_AXE",SV_BEAKED_AXE);
 tolua_constant(tolua_S,NULL,"SV_BROAD_AXE",SV_BROAD_AXE);
 tolua_constant(tolua_S,NULL,"SV_LUCERNE_HAMMER",SV_LUCERNE_HAMMER);
 tolua_constant(tolua_S,NULL,"SV_GLAIVE",SV_GLAIVE);
 tolua_constant(tolua_S,NULL,"SV_LAJATANG",SV_LAJATANG);
 tolua_constant(tolua_S,NULL,"SV_HALBERD",SV_HALBERD);
 tolua_constant(tolua_S,NULL,"SV_GUISARME",SV_GUISARME);
 tolua_constant(tolua_S,NULL,"SV_SCYTHE",SV_SCYTHE);
 tolua_constant(tolua_S,NULL,"SV_LANCE",SV_LANCE);
 tolua_constant(tolua_S,NULL,"SV_BATTLE_AXE",SV_BATTLE_AXE);
 tolua_constant(tolua_S,NULL,"SV_GREAT_AXE",SV_GREAT_AXE);
 tolua_constant(tolua_S,NULL,"SV_TRIFURCATE_SPEAR",SV_TRIFURCATE_SPEAR);
 tolua_constant(tolua_S,NULL,"SV_LOCHABER_AXE",SV_LOCHABER_AXE);
 tolua_constant(tolua_S,NULL,"SV_HEAVY_LANCE",SV_HEAVY_LANCE);
 tolua_constant(tolua_S,NULL,"SV_SCYTHE_OF_SLICING",SV_SCYTHE_OF_SLICING);
 tolua_constant(tolua_S,NULL,"SV_BROKEN_DAGGER",SV_BROKEN_DAGGER);
 tolua_constant(tolua_S,NULL,"SV_BROKEN_SWORD",SV_BROKEN_SWORD);
 tolua_constant(tolua_S,NULL,"SV_DAGGER",SV_DAGGER);
 tolua_constant(tolua_S,NULL,"SV_MAIN_GAUCHE",SV_MAIN_GAUCHE);
 tolua_constant(tolua_S,NULL,"SV_TANTO",SV_TANTO);
 tolua_constant(tolua_S,NULL,"SV_RAPIER",SV_RAPIER);
 tolua_constant(tolua_S,NULL,"SV_SMALL_SWORD",SV_SMALL_SWORD);
 tolua_constant(tolua_S,NULL,"SV_BASILLARD",SV_BASILLARD);
 tolua_constant(tolua_S,NULL,"SV_SHORT_SWORD",SV_SHORT_SWORD);
 tolua_constant(tolua_S,NULL,"SV_SABRE",SV_SABRE);
 tolua_constant(tolua_S,NULL,"SV_CUTLASS",SV_CUTLASS);
 tolua_constant(tolua_S,NULL,"SV_WAKIZASHI",SV_WAKIZASHI);
 tolua_constant(tolua_S,NULL,"SV_KHOPESH",SV_KHOPESH);
 tolua_constant(tolua_S,NULL,"SV_TULWAR",SV_TULWAR);
 tolua_constant(tolua_S,NULL,"SV_BROAD_SWORD",SV_BROAD_SWORD);
 tolua_constant(tolua_S,NULL,"SV_LONG_SWORD",SV_LONG_SWORD);
 tolua_constant(tolua_S,NULL,"SV_SCIMITAR",SV_SCIMITAR);
 tolua_constant(tolua_S,NULL,"SV_NINJATO",SV_NINJATO);
 tolua_constant(tolua_S,NULL,"SV_KATANA",SV_KATANA);
 tolua_constant(tolua_S,NULL,"SV_BASTARD_SWORD",SV_BASTARD_SWORD);
 tolua_constant(tolua_S,NULL,"SV_GREAT_SCIMITAR",SV_GREAT_SCIMITAR);
 tolua_constant(tolua_S,NULL,"SV_CLAYMORE",SV_CLAYMORE);
 tolua_constant(tolua_S,NULL,"SV_ESPADON",SV_ESPADON);
 tolua_constant(tolua_S,NULL,"SV_TWO_HANDED_SWORD",SV_TWO_HANDED_SWORD);
 tolua_constant(tolua_S,NULL,"SV_FLAMBERGE",SV_FLAMBERGE);
 tolua_constant(tolua_S,NULL,"SV_NO_DACHI",SV_NO_DACHI);
 tolua_constant(tolua_S,NULL,"SV_EXECUTIONERS_SWORD",SV_EXECUTIONERS_SWORD);
 tolua_constant(tolua_S,NULL,"SV_ZWEIHANDER",SV_ZWEIHANDER);
 tolua_constant(tolua_S,NULL,"SV_BLADE_OF_CHAOS",SV_BLADE_OF_CHAOS);
 tolua_constant(tolua_S,NULL,"SV_DIAMOND_EDGE",SV_DIAMOND_EDGE);
 tolua_constant(tolua_S,NULL,"SV_SMALL_LEATHER_SHIELD",SV_SMALL_LEATHER_SHIELD);
 tolua_constant(tolua_S,NULL,"SV_SMALL_METAL_SHIELD",SV_SMALL_METAL_SHIELD);
 tolua_constant(tolua_S,NULL,"SV_LARGE_LEATHER_SHIELD",SV_LARGE_LEATHER_SHIELD);
 tolua_constant(tolua_S,NULL,"SV_LARGE_METAL_SHIELD",SV_LARGE_METAL_SHIELD);
 tolua_constant(tolua_S,NULL,"SV_DRAGON_SHIELD",SV_DRAGON_SHIELD);
 tolua_constant(tolua_S,NULL,"SV_SHIELD_OF_DEFLECTION",SV_SHIELD_OF_DEFLECTION);
 tolua_constant(tolua_S,NULL,"SV_HARD_LEATHER_CAP",SV_HARD_LEATHER_CAP);
 tolua_constant(tolua_S,NULL,"SV_METAL_CAP",SV_METAL_CAP);
 tolua_constant(tolua_S,NULL,"SV_JINGASA",SV_JINGASA);
 tolua_constant(tolua_S,NULL,"SV_IRON_HELM",SV_IRON_HELM);
 tolua_constant(tolua_S,NULL,"SV_STEEL_HELM",SV_STEEL_HELM);
 tolua_constant(tolua_S,NULL,"SV_DRAGON_HELM",SV_DRAGON_HELM);
 tolua_constant(tolua_S,NULL,"SV_KABUTO",SV_KABUTO);
 tolua_constant(tolua_S,NULL,"SV_IRON_CROWN",SV_IRON_CROWN);
 tolua_constant(tolua_S,NULL,"SV_GOLDEN_CROWN",SV_GOLDEN_CROWN);
 tolua_constant(tolua_S,NULL,"SV_JEWELED_CROWN",SV_JEWELED_CROWN);
 tolua_constant(tolua_S,NULL,"SV_MORGOTH",SV_MORGOTH);
 tolua_constant(tolua_S,NULL,"SV_PAIR_OF_SOFT_LEATHER_BOOTS",SV_PAIR_OF_SOFT_LEATHER_BOOTS);
 tolua_constant(tolua_S,NULL,"SV_PAIR_OF_HARD_LEATHER_BOOTS",SV_PAIR_OF_HARD_LEATHER_BOOTS);
 tolua_constant(tolua_S,NULL,"SV_PAIR_OF_METAL_SHOD_BOOTS",SV_PAIR_OF_METAL_SHOD_BOOTS);
 tolua_constant(tolua_S,NULL,"SV_CLOAK",SV_CLOAK);
 tolua_constant(tolua_S,NULL,"SV_ELVEN_CLOAK",SV_ELVEN_CLOAK);
 tolua_constant(tolua_S,NULL,"SV_FUR_CLOAK",SV_FUR_CLOAK);
 tolua_constant(tolua_S,NULL,"SV_SHADOW_CLOAK",SV_SHADOW_CLOAK);
 tolua_constant(tolua_S,NULL,"SV_SET_OF_LEATHER_GLOVES",SV_SET_OF_LEATHER_GLOVES);
 tolua_constant(tolua_S,NULL,"SV_SET_OF_GAUNTLETS",SV_SET_OF_GAUNTLETS);
 tolua_constant(tolua_S,NULL,"SV_SET_OF_CESTI",SV_SET_OF_CESTI);
 tolua_constant(tolua_S,NULL,"SV_T_SHIRT",SV_T_SHIRT);
 tolua_constant(tolua_S,NULL,"SV_FILTHY_RAG",SV_FILTHY_RAG);
 tolua_constant(tolua_S,NULL,"SV_ROBE",SV_ROBE);
 tolua_constant(tolua_S,NULL,"SV_PAPER_ARMOR",SV_PAPER_ARMOR);
 tolua_constant(tolua_S,NULL,"SV_SOFT_LEATHER_ARMOR",SV_SOFT_LEATHER_ARMOR);
 tolua_constant(tolua_S,NULL,"SV_SOFT_STUDDED_LEATHER",SV_SOFT_STUDDED_LEATHER);
 tolua_constant(tolua_S,NULL,"SV_HARD_LEATHER_ARMOR",SV_HARD_LEATHER_ARMOR);
 tolua_constant(tolua_S,NULL,"SV_HARD_STUDDED_LEATHER",SV_HARD_STUDDED_LEATHER);
 tolua_constant(tolua_S,NULL,"SV_RHINO_HIDE_ARMOR",SV_RHINO_HIDE_ARMOR);
 tolua_constant(tolua_S,NULL,"SV_CORD_ARMOR",SV_CORD_ARMOR);
 tolua_constant(tolua_S,NULL,"SV_PADDED_ARMOR",SV_PADDED_ARMOR);
 tolua_constant(tolua_S,NULL,"SV_LEATHER_SCALE_MAIL",SV_LEATHER_SCALE_MAIL);
 tolua_constant(tolua_S,NULL,"SV_LEATHER_JACK",SV_LEATHER_JACK);
 tolua_constant(tolua_S,NULL,"SV_STONE_AND_HIDE_ARMOR",SV_STONE_AND_HIDE_ARMOR);
 tolua_constant(tolua_S,NULL,"SV_RUSTY_CHAIN_MAIL",SV_RUSTY_CHAIN_MAIL);
 tolua_constant(tolua_S,NULL,"SV_RING_MAIL",SV_RING_MAIL);
 tolua_constant(tolua_S,NULL,"SV_METAL_SCALE_MAIL",SV_METAL_SCALE_MAIL);
 tolua_constant(tolua_S,NULL,"SV_CHAIN_MAIL",SV_CHAIN_MAIL);
 tolua_constant(tolua_S,NULL,"SV_DOUBLE_RING_MAIL",SV_DOUBLE_RING_MAIL);
 tolua_constant(tolua_S,NULL,"SV_AUGMENTED_CHAIN_MAIL",SV_AUGMENTED_CHAIN_MAIL);
 tolua_constant(tolua_S,NULL,"SV_DOUBLE_CHAIN_MAIL",SV_DOUBLE_CHAIN_MAIL);
 tolua_constant(tolua_S,NULL,"SV_BAR_CHAIN_MAIL",SV_BAR_CHAIN_MAIL);
 tolua_constant(tolua_S,NULL,"SV_METAL_BRIGANDINE_ARMOUR",SV_METAL_BRIGANDINE_ARMOUR);
 tolua_constant(tolua_S,NULL,"SV_SPLINT_MAIL",SV_SPLINT_MAIL);
 tolua_constant(tolua_S,NULL,"SV_DO_MARU",SV_DO_MARU);
 tolua_constant(tolua_S,NULL,"SV_PARTIAL_PLATE_ARMOUR",SV_PARTIAL_PLATE_ARMOUR);
 tolua_constant(tolua_S,NULL,"SV_METAL_LAMELLAR_ARMOUR",SV_METAL_LAMELLAR_ARMOUR);
 tolua_constant(tolua_S,NULL,"SV_HARAMAKIDO",SV_HARAMAKIDO);
 tolua_constant(tolua_S,NULL,"SV_FULL_PLATE_ARMOUR",SV_FULL_PLATE_ARMOUR);
 tolua_constant(tolua_S,NULL,"SV_O_YOROI",SV_O_YOROI);
 tolua_constant(tolua_S,NULL,"SV_RIBBED_PLATE_ARMOUR",SV_RIBBED_PLATE_ARMOUR);
 tolua_constant(tolua_S,NULL,"SV_MITHRIL_CHAIN_MAIL",SV_MITHRIL_CHAIN_MAIL);
 tolua_constant(tolua_S,NULL,"SV_MITHRIL_PLATE_MAIL",SV_MITHRIL_PLATE_MAIL);
 tolua_constant(tolua_S,NULL,"SV_ADAMANTITE_PLATE_MAIL",SV_ADAMANTITE_PLATE_MAIL);
 tolua_constant(tolua_S,NULL,"SV_DRAGON_BLACK",SV_DRAGON_BLACK);
 tolua_constant(tolua_S,NULL,"SV_DRAGON_BLUE",SV_DRAGON_BLUE);
 tolua_constant(tolua_S,NULL,"SV_DRAGON_WHITE",SV_DRAGON_WHITE);
 tolua_constant(tolua_S,NULL,"SV_DRAGON_RED",SV_DRAGON_RED);
 tolua_constant(tolua_S,NULL,"SV_DRAGON_GREEN",SV_DRAGON_GREEN);
 tolua_constant(tolua_S,NULL,"SV_DRAGON_MULTIHUED",SV_DRAGON_MULTIHUED);
 tolua_constant(tolua_S,NULL,"SV_DRAGON_SHINING",SV_DRAGON_SHINING);
 tolua_constant(tolua_S,NULL,"SV_DRAGON_LAW",SV_DRAGON_LAW);
 tolua_constant(tolua_S,NULL,"SV_DRAGON_BRONZE",SV_DRAGON_BRONZE);
 tolua_constant(tolua_S,NULL,"SV_DRAGON_GOLD",SV_DRAGON_GOLD);
 tolua_constant(tolua_S,NULL,"SV_DRAGON_CHAOS",SV_DRAGON_CHAOS);
 tolua_constant(tolua_S,NULL,"SV_DRAGON_BALANCE",SV_DRAGON_BALANCE);
 tolua_constant(tolua_S,NULL,"SV_DRAGON_POWER",SV_DRAGON_POWER);
 tolua_constant(tolua_S,NULL,"SV_LITE_TORCH",SV_LITE_TORCH);
 tolua_constant(tolua_S,NULL,"SV_LITE_LANTERN",SV_LITE_LANTERN);
 tolua_constant(tolua_S,NULL,"SV_LITE_GALADRIEL",SV_LITE_GALADRIEL);
 tolua_constant(tolua_S,NULL,"SV_LITE_ELENDIL",SV_LITE_ELENDIL);
 tolua_constant(tolua_S,NULL,"SV_LITE_THRAIN",SV_LITE_THRAIN);
 tolua_constant(tolua_S,NULL,"SV_AMULET_DOOM",SV_AMULET_DOOM);
 tolua_constant(tolua_S,NULL,"SV_AMULET_TELEPORT",SV_AMULET_TELEPORT);
 tolua_constant(tolua_S,NULL,"SV_AMULET_BERSERK",SV_AMULET_BERSERK);
 tolua_constant(tolua_S,NULL,"SV_AMULET_SLOW_DIGEST",SV_AMULET_SLOW_DIGEST);
 tolua_constant(tolua_S,NULL,"SV_AMULET_RESIST_ACID",SV_AMULET_RESIST_ACID);
 tolua_constant(tolua_S,NULL,"SV_AMULET_SEARCHING",SV_AMULET_SEARCHING);
 tolua_constant(tolua_S,NULL,"SV_AMULET_WISDOM",SV_AMULET_WISDOM);
 tolua_constant(tolua_S,NULL,"SV_AMULET_CHARISMA",SV_AMULET_CHARISMA);
 tolua_constant(tolua_S,NULL,"SV_AMULET_THE_MAGI",SV_AMULET_THE_MAGI);
 tolua_constant(tolua_S,NULL,"SV_AMULET_REFLECTION",SV_AMULET_REFLECTION);
 tolua_constant(tolua_S,NULL,"SV_AMULET_CARLAMMAS",SV_AMULET_CARLAMMAS);
 tolua_constant(tolua_S,NULL,"SV_AMULET_INGWE",SV_AMULET_INGWE);
 tolua_constant(tolua_S,NULL,"SV_AMULET_DWARVES",SV_AMULET_DWARVES);
 tolua_constant(tolua_S,NULL,"SV_AMULET_NO_MAGIC",SV_AMULET_NO_MAGIC);
 tolua_constant(tolua_S,NULL,"SV_AMULET_NO_TELE",SV_AMULET_NO_TELE);
 tolua_constant(tolua_S,NULL,"SV_AMULET_RESISTANCE",SV_AMULET_RESISTANCE);
 tolua_constant(tolua_S,NULL,"SV_RING_WOE",SV_RING_WOE);
 tolua_constant(tolua_S,NULL,"SV_RING_AGGRAVATION",SV_RING_AGGRAVATION);
 tolua_constant(tolua_S,NULL,"SV_RING_WEAKNESS",SV_RING_WEAKNESS);
 tolua_constant(tolua_S,NULL,"SV_RING_STUPIDITY",SV_RING_STUPIDITY);
 tolua_constant(tolua_S,NULL,"SV_RING_TELEPORTATION",SV_RING_TELEPORTATION);
 tolua_constant(tolua_S,NULL,"SV_RING_SLOW_DIGESTION",SV_RING_SLOW_DIGESTION);
 tolua_constant(tolua_S,NULL,"SV_RING_FEATHER_FALL",SV_RING_FEATHER_FALL);
 tolua_constant(tolua_S,NULL,"SV_RING_RESIST_FIRE",SV_RING_RESIST_FIRE);
 tolua_constant(tolua_S,NULL,"SV_RING_RESIST_COLD",SV_RING_RESIST_COLD);
 tolua_constant(tolua_S,NULL,"SV_RING_SUSTAIN_STR",SV_RING_SUSTAIN_STR);
 tolua_constant(tolua_S,NULL,"SV_RING_SUSTAIN_INT",SV_RING_SUSTAIN_INT);
 tolua_constant(tolua_S,NULL,"SV_RING_SUSTAIN_WIS",SV_RING_SUSTAIN_WIS);
 tolua_constant(tolua_S,NULL,"SV_RING_SUSTAIN_DEX",SV_RING_SUSTAIN_DEX);
 tolua_constant(tolua_S,NULL,"SV_RING_SUSTAIN_CON",SV_RING_SUSTAIN_CON);
 tolua_constant(tolua_S,NULL,"SV_RING_SUSTAIN_CHR",SV_RING_SUSTAIN_CHR);
 tolua_constant(tolua_S,NULL,"SV_RING_PROTECTION",SV_RING_PROTECTION);
 tolua_constant(tolua_S,NULL,"SV_RING_ACID",SV_RING_ACID);
 tolua_constant(tolua_S,NULL,"SV_RING_FLAMES",SV_RING_FLAMES);
 tolua_constant(tolua_S,NULL,"SV_RING_ICE",SV_RING_ICE);
 tolua_constant(tolua_S,NULL,"SV_RING_RESIST_POIS",SV_RING_RESIST_POIS);
 tolua_constant(tolua_S,NULL,"SV_RING_FREE_ACTION",SV_RING_FREE_ACTION);
 tolua_constant(tolua_S,NULL,"SV_RING_SEE_INVIS",SV_RING_SEE_INVIS);
 tolua_constant(tolua_S,NULL,"SV_RING_SEARCHING",SV_RING_SEARCHING);
 tolua_constant(tolua_S,NULL,"SV_RING_STR",SV_RING_STR);
 tolua_constant(tolua_S,NULL,"SV_RING_INT",SV_RING_INT);
 tolua_constant(tolua_S,NULL,"SV_RING_DEX",SV_RING_DEX);
 tolua_constant(tolua_S,NULL,"SV_RING_CON",SV_RING_CON);
 tolua_constant(tolua_S,NULL,"SV_RING_ACCURACY",SV_RING_ACCURACY);
 tolua_constant(tolua_S,NULL,"SV_RING_DAMAGE",SV_RING_DAMAGE);
 tolua_constant(tolua_S,NULL,"SV_RING_SLAYING",SV_RING_SLAYING);
 tolua_constant(tolua_S,NULL,"SV_RING_SPEED",SV_RING_SPEED);
 tolua_constant(tolua_S,NULL,"SV_RING_BARAHIR",SV_RING_BARAHIR);
 tolua_constant(tolua_S,NULL,"SV_RING_TULKAS",SV_RING_TULKAS);
 tolua_constant(tolua_S,NULL,"SV_RING_NARYA",SV_RING_NARYA);
 tolua_constant(tolua_S,NULL,"SV_RING_NENYA",SV_RING_NENYA);
 tolua_constant(tolua_S,NULL,"SV_RING_VILYA",SV_RING_VILYA);
 tolua_constant(tolua_S,NULL,"SV_RING_POWER",SV_RING_POWER);
 tolua_constant(tolua_S,NULL,"SV_RING_RES_FEAR",SV_RING_RES_FEAR);
 tolua_constant(tolua_S,NULL,"SV_RING_RES_LD",SV_RING_RES_LD);
 tolua_constant(tolua_S,NULL,"SV_RING_RES_NETHER",SV_RING_RES_NETHER);
 tolua_constant(tolua_S,NULL,"SV_RING_RES_NEXUS",SV_RING_RES_NEXUS);
 tolua_constant(tolua_S,NULL,"SV_RING_RES_SOUND",SV_RING_RES_SOUND);
 tolua_constant(tolua_S,NULL,"SV_RING_RES_CONFUSION",SV_RING_RES_CONFUSION);
 tolua_constant(tolua_S,NULL,"SV_RING_RES_SHARDS",SV_RING_RES_SHARDS);
 tolua_constant(tolua_S,NULL,"SV_RING_RES_DISENCHANT",SV_RING_RES_DISENCHANT);
 tolua_constant(tolua_S,NULL,"SV_RING_RES_CHAOS",SV_RING_RES_CHAOS);
 tolua_constant(tolua_S,NULL,"SV_RING_RES_BLINDNESS",SV_RING_RES_BLINDNESS);
 tolua_constant(tolua_S,NULL,"SV_RING_LORDLY",SV_RING_LORDLY);
 tolua_constant(tolua_S,NULL,"SV_RING_ATTACKS",SV_RING_ATTACKS);
 tolua_constant(tolua_S,NULL,"SV_STAFF_DARKNESS",SV_STAFF_DARKNESS);
 tolua_constant(tolua_S,NULL,"SV_STAFF_SLOWNESS",SV_STAFF_SLOWNESS);
 tolua_constant(tolua_S,NULL,"SV_STAFF_HASTE_MONSTERS",SV_STAFF_HASTE_MONSTERS);
 tolua_constant(tolua_S,NULL,"SV_STAFF_SUMMONING",SV_STAFF_SUMMONING);
 tolua_constant(tolua_S,NULL,"SV_STAFF_TELEPORTATION",SV_STAFF_TELEPORTATION);
 tolua_constant(tolua_S,NULL,"SV_STAFF_IDENTIFY",SV_STAFF_IDENTIFY);
 tolua_constant(tolua_S,NULL,"SV_STAFF_REMOVE_CURSE",SV_STAFF_REMOVE_CURSE);
 tolua_constant(tolua_S,NULL,"SV_STAFF_STARLITE",SV_STAFF_STARLITE);
 tolua_constant(tolua_S,NULL,"SV_STAFF_LITE",SV_STAFF_LITE);
 tolua_constant(tolua_S,NULL,"SV_STAFF_MAPPING",SV_STAFF_MAPPING);
 tolua_constant(tolua_S,NULL,"SV_STAFF_DETECT_GOLD",SV_STAFF_DETECT_GOLD);
 tolua_constant(tolua_S,NULL,"SV_STAFF_DETECT_ITEM",SV_STAFF_DETECT_ITEM);
 tolua_constant(tolua_S,NULL,"SV_STAFF_DETECT_TRAP",SV_STAFF_DETECT_TRAP);
 tolua_constant(tolua_S,NULL,"SV_STAFF_DETECT_DOOR",SV_STAFF_DETECT_DOOR);
 tolua_constant(tolua_S,NULL,"SV_STAFF_DETECT_INVIS",SV_STAFF_DETECT_INVIS);
 tolua_constant(tolua_S,NULL,"SV_STAFF_DETECT_EVIL",SV_STAFF_DETECT_EVIL);
 tolua_constant(tolua_S,NULL,"SV_STAFF_CURE_LIGHT",SV_STAFF_CURE_LIGHT);
 tolua_constant(tolua_S,NULL,"SV_STAFF_CURING",SV_STAFF_CURING);
 tolua_constant(tolua_S,NULL,"SV_STAFF_HEALING",SV_STAFF_HEALING);
 tolua_constant(tolua_S,NULL,"SV_STAFF_THE_MAGI",SV_STAFF_THE_MAGI);
 tolua_constant(tolua_S,NULL,"SV_STAFF_SLEEP_MONSTERS",SV_STAFF_SLEEP_MONSTERS);
 tolua_constant(tolua_S,NULL,"SV_STAFF_SLOW_MONSTERS",SV_STAFF_SLOW_MONSTERS);
 tolua_constant(tolua_S,NULL,"SV_STAFF_SPEED",SV_STAFF_SPEED);
 tolua_constant(tolua_S,NULL,"SV_STAFF_PROBING",SV_STAFF_PROBING);
 tolua_constant(tolua_S,NULL,"SV_STAFF_DISPEL_EVIL",SV_STAFF_DISPEL_EVIL);
 tolua_constant(tolua_S,NULL,"SV_STAFF_POWER",SV_STAFF_POWER);
 tolua_constant(tolua_S,NULL,"SV_STAFF_HOLINESS",SV_STAFF_HOLINESS);
 tolua_constant(tolua_S,NULL,"SV_STAFF_GENOCIDE",SV_STAFF_GENOCIDE);
 tolua_constant(tolua_S,NULL,"SV_STAFF_EARTHQUAKES",SV_STAFF_EARTHQUAKES);
 tolua_constant(tolua_S,NULL,"SV_STAFF_DESTRUCTION",SV_STAFF_DESTRUCTION);
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
 tolua_constant(tolua_S,NULL,"SV_WAND_ANNIHILATION",SV_WAND_ANNIHILATION);
 tolua_constant(tolua_S,NULL,"SV_WAND_DRAGON_FIRE",SV_WAND_DRAGON_FIRE);
 tolua_constant(tolua_S,NULL,"SV_WAND_DRAGON_COLD",SV_WAND_DRAGON_COLD);
 tolua_constant(tolua_S,NULL,"SV_WAND_DRAGON_BREATH",SV_WAND_DRAGON_BREATH);
 tolua_constant(tolua_S,NULL,"SV_WAND_ROCKETS",SV_WAND_ROCKETS);
 tolua_constant(tolua_S,NULL,"SV_ROD_DETECT_TRAP",SV_ROD_DETECT_TRAP);
 tolua_constant(tolua_S,NULL,"SV_ROD_DETECT_DOOR",SV_ROD_DETECT_DOOR);
 tolua_constant(tolua_S,NULL,"SV_ROD_IDENTIFY",SV_ROD_IDENTIFY);
 tolua_constant(tolua_S,NULL,"SV_ROD_RECALL",SV_ROD_RECALL);
 tolua_constant(tolua_S,NULL,"SV_ROD_ILLUMINATION",SV_ROD_ILLUMINATION);
 tolua_constant(tolua_S,NULL,"SV_ROD_MAPPING",SV_ROD_MAPPING);
 tolua_constant(tolua_S,NULL,"SV_ROD_DETECTION",SV_ROD_DETECTION);
 tolua_constant(tolua_S,NULL,"SV_ROD_PROBING",SV_ROD_PROBING);
 tolua_constant(tolua_S,NULL,"SV_ROD_CURING",SV_ROD_CURING);
 tolua_constant(tolua_S,NULL,"SV_ROD_HEALING",SV_ROD_HEALING);
 tolua_constant(tolua_S,NULL,"SV_ROD_RESTORATION",SV_ROD_RESTORATION);
 tolua_constant(tolua_S,NULL,"SV_ROD_SPEED",SV_ROD_SPEED);
 tolua_constant(tolua_S,NULL,"SV_ROD_PESTICIDE",SV_ROD_PESTICIDE);
 tolua_constant(tolua_S,NULL,"SV_ROD_TELEPORT_AWAY",SV_ROD_TELEPORT_AWAY);
 tolua_constant(tolua_S,NULL,"SV_ROD_DISARMING",SV_ROD_DISARMING);
 tolua_constant(tolua_S,NULL,"SV_ROD_LITE",SV_ROD_LITE);
 tolua_constant(tolua_S,NULL,"SV_ROD_SLEEP_MONSTER",SV_ROD_SLEEP_MONSTER);
 tolua_constant(tolua_S,NULL,"SV_ROD_SLOW_MONSTER",SV_ROD_SLOW_MONSTER);
 tolua_constant(tolua_S,NULL,"SV_ROD_DRAIN_LIFE",SV_ROD_DRAIN_LIFE);
 tolua_constant(tolua_S,NULL,"SV_ROD_POLYMORPH",SV_ROD_POLYMORPH);
 tolua_constant(tolua_S,NULL,"SV_ROD_ACID_BOLT",SV_ROD_ACID_BOLT);
 tolua_constant(tolua_S,NULL,"SV_ROD_ELEC_BOLT",SV_ROD_ELEC_BOLT);
 tolua_constant(tolua_S,NULL,"SV_ROD_FIRE_BOLT",SV_ROD_FIRE_BOLT);
 tolua_constant(tolua_S,NULL,"SV_ROD_COLD_BOLT",SV_ROD_COLD_BOLT);
 tolua_constant(tolua_S,NULL,"SV_ROD_ACID_BALL",SV_ROD_ACID_BALL);
 tolua_constant(tolua_S,NULL,"SV_ROD_ELEC_BALL",SV_ROD_ELEC_BALL);
 tolua_constant(tolua_S,NULL,"SV_ROD_FIRE_BALL",SV_ROD_FIRE_BALL);
 tolua_constant(tolua_S,NULL,"SV_ROD_COLD_BALL",SV_ROD_COLD_BALL);
 tolua_constant(tolua_S,NULL,"SV_ROD_HAVOC",SV_ROD_HAVOC);
 tolua_constant(tolua_S,NULL,"SV_SCROLL_DARKNESS",SV_SCROLL_DARKNESS);
 tolua_constant(tolua_S,NULL,"SV_SCROLL_AGGRAVATE_MONSTER",SV_SCROLL_AGGRAVATE_MONSTER);
 tolua_constant(tolua_S,NULL,"SV_SCROLL_CURSE_ARMOR",SV_SCROLL_CURSE_ARMOR);
 tolua_constant(tolua_S,NULL,"SV_SCROLL_CURSE_WEAPON",SV_SCROLL_CURSE_WEAPON);
 tolua_constant(tolua_S,NULL,"SV_SCROLL_SUMMON_MONSTER",SV_SCROLL_SUMMON_MONSTER);
 tolua_constant(tolua_S,NULL,"SV_SCROLL_SUMMON_UNDEAD",SV_SCROLL_SUMMON_UNDEAD);
 tolua_constant(tolua_S,NULL,"SV_SCROLL_TRAP_CREATION",SV_SCROLL_TRAP_CREATION);
 tolua_constant(tolua_S,NULL,"SV_SCROLL_PHASE_DOOR",SV_SCROLL_PHASE_DOOR);
 tolua_constant(tolua_S,NULL,"SV_SCROLL_TELEPORT",SV_SCROLL_TELEPORT);
 tolua_constant(tolua_S,NULL,"SV_SCROLL_TELEPORT_LEVEL",SV_SCROLL_TELEPORT_LEVEL);
 tolua_constant(tolua_S,NULL,"SV_SCROLL_WORD_OF_RECALL",SV_SCROLL_WORD_OF_RECALL);
 tolua_constant(tolua_S,NULL,"SV_SCROLL_IDENTIFY",SV_SCROLL_IDENTIFY);
 tolua_constant(tolua_S,NULL,"SV_SCROLL_STAR_IDENTIFY",SV_SCROLL_STAR_IDENTIFY);
 tolua_constant(tolua_S,NULL,"SV_SCROLL_REMOVE_CURSE",SV_SCROLL_REMOVE_CURSE);
 tolua_constant(tolua_S,NULL,"SV_SCROLL_STAR_REMOVE_CURSE",SV_SCROLL_STAR_REMOVE_CURSE);
 tolua_constant(tolua_S,NULL,"SV_SCROLL_ENCHANT_ARMOR",SV_SCROLL_ENCHANT_ARMOR);
 tolua_constant(tolua_S,NULL,"SV_SCROLL_ENCHANT_WEAPON_TO_HIT",SV_SCROLL_ENCHANT_WEAPON_TO_HIT);
 tolua_constant(tolua_S,NULL,"SV_SCROLL_ENCHANT_WEAPON_TO_DAM",SV_SCROLL_ENCHANT_WEAPON_TO_DAM);
 tolua_constant(tolua_S,NULL,"SV_SCROLL_STAR_ENCHANT_ARMOR",SV_SCROLL_STAR_ENCHANT_ARMOR);
 tolua_constant(tolua_S,NULL,"SV_SCROLL_STAR_ENCHANT_WEAPON",SV_SCROLL_STAR_ENCHANT_WEAPON);
 tolua_constant(tolua_S,NULL,"SV_SCROLL_RECHARGING",SV_SCROLL_RECHARGING);
 tolua_constant(tolua_S,NULL,"SV_SCROLL_MUNDANITY",SV_SCROLL_MUNDANITY);
 tolua_constant(tolua_S,NULL,"SV_SCROLL_LIGHT",SV_SCROLL_LIGHT);
 tolua_constant(tolua_S,NULL,"SV_SCROLL_MAPPING",SV_SCROLL_MAPPING);
 tolua_constant(tolua_S,NULL,"SV_SCROLL_DETECT_GOLD",SV_SCROLL_DETECT_GOLD);
 tolua_constant(tolua_S,NULL,"SV_SCROLL_DETECT_ITEM",SV_SCROLL_DETECT_ITEM);
 tolua_constant(tolua_S,NULL,"SV_SCROLL_DETECT_TRAP",SV_SCROLL_DETECT_TRAP);
 tolua_constant(tolua_S,NULL,"SV_SCROLL_DETECT_DOOR",SV_SCROLL_DETECT_DOOR);
 tolua_constant(tolua_S,NULL,"SV_SCROLL_DETECT_INVIS",SV_SCROLL_DETECT_INVIS);
 tolua_constant(tolua_S,NULL,"SV_SCROLL_SATISFY_HUNGER",SV_SCROLL_SATISFY_HUNGER);
 tolua_constant(tolua_S,NULL,"SV_SCROLL_BLESSING",SV_SCROLL_BLESSING);
 tolua_constant(tolua_S,NULL,"SV_SCROLL_HOLY_CHANT",SV_SCROLL_HOLY_CHANT);
 tolua_constant(tolua_S,NULL,"SV_SCROLL_HOLY_PRAYER",SV_SCROLL_HOLY_PRAYER);
 tolua_constant(tolua_S,NULL,"SV_SCROLL_MONSTER_CONFUSION",SV_SCROLL_MONSTER_CONFUSION);
 tolua_constant(tolua_S,NULL,"SV_SCROLL_PROTECTION_FROM_EVIL",SV_SCROLL_PROTECTION_FROM_EVIL);
 tolua_constant(tolua_S,NULL,"SV_SCROLL_RUNE_OF_PROTECTION",SV_SCROLL_RUNE_OF_PROTECTION);
 tolua_constant(tolua_S,NULL,"SV_SCROLL_TRAP_DOOR_DESTRUCTION",SV_SCROLL_TRAP_DOOR_DESTRUCTION);
 tolua_constant(tolua_S,NULL,"SV_SCROLL_STAR_DESTRUCTION",SV_SCROLL_STAR_DESTRUCTION);
 tolua_constant(tolua_S,NULL,"SV_SCROLL_DISPEL_UNDEAD",SV_SCROLL_DISPEL_UNDEAD);
 tolua_constant(tolua_S,NULL,"SV_SCROLL_GENOCIDE",SV_SCROLL_GENOCIDE);
 tolua_constant(tolua_S,NULL,"SV_SCROLL_MASS_GENOCIDE",SV_SCROLL_MASS_GENOCIDE);
 tolua_constant(tolua_S,NULL,"SV_SCROLL_ACQUIREMENT",SV_SCROLL_ACQUIREMENT);
 tolua_constant(tolua_S,NULL,"SV_SCROLL_STAR_ACQUIREMENT",SV_SCROLL_STAR_ACQUIREMENT);
 tolua_constant(tolua_S,NULL,"SV_SCROLL_FIRE",SV_SCROLL_FIRE);
 tolua_constant(tolua_S,NULL,"SV_SCROLL_ICE",SV_SCROLL_ICE);
 tolua_constant(tolua_S,NULL,"SV_SCROLL_CHAOS",SV_SCROLL_CHAOS);
 tolua_constant(tolua_S,NULL,"SV_SCROLL_RUMOR",SV_SCROLL_RUMOR);
 tolua_constant(tolua_S,NULL,"SV_SCROLL_ARTIFACT",SV_SCROLL_ARTIFACT);
 tolua_constant(tolua_S,NULL,"SV_POTION_WATER",SV_POTION_WATER);
 tolua_constant(tolua_S,NULL,"SV_POTION_APPLE_JUICE",SV_POTION_APPLE_JUICE);
 tolua_constant(tolua_S,NULL,"SV_POTION_SLIME_MOLD",SV_POTION_SLIME_MOLD);
 tolua_constant(tolua_S,NULL,"SV_POTION_SLOWNESS",SV_POTION_SLOWNESS);
 tolua_constant(tolua_S,NULL,"SV_POTION_SALT_WATER",SV_POTION_SALT_WATER);
 tolua_constant(tolua_S,NULL,"SV_POTION_POISON",SV_POTION_POISON);
 tolua_constant(tolua_S,NULL,"SV_POTION_BLINDNESS",SV_POTION_BLINDNESS);
 tolua_constant(tolua_S,NULL,"SV_POTION_CONFUSION",SV_POTION_CONFUSION);
 tolua_constant(tolua_S,NULL,"SV_POTION_SLEEP",SV_POTION_SLEEP);
 tolua_constant(tolua_S,NULL,"SV_POTION_LOSE_MEMORIES",SV_POTION_LOSE_MEMORIES);
 tolua_constant(tolua_S,NULL,"SV_POTION_RUINATION",SV_POTION_RUINATION);
 tolua_constant(tolua_S,NULL,"SV_POTION_DEC_STR",SV_POTION_DEC_STR);
 tolua_constant(tolua_S,NULL,"SV_POTION_DEC_INT",SV_POTION_DEC_INT);
 tolua_constant(tolua_S,NULL,"SV_POTION_DEC_WIS",SV_POTION_DEC_WIS);
 tolua_constant(tolua_S,NULL,"SV_POTION_DEC_DEX",SV_POTION_DEC_DEX);
 tolua_constant(tolua_S,NULL,"SV_POTION_DEC_CON",SV_POTION_DEC_CON);
 tolua_constant(tolua_S,NULL,"SV_POTION_DEC_CHR",SV_POTION_DEC_CHR);
 tolua_constant(tolua_S,NULL,"SV_POTION_DETONATIONS",SV_POTION_DETONATIONS);
 tolua_constant(tolua_S,NULL,"SV_POTION_DEATH",SV_POTION_DEATH);
 tolua_constant(tolua_S,NULL,"SV_POTION_INFRAVISION",SV_POTION_INFRAVISION);
 tolua_constant(tolua_S,NULL,"SV_POTION_DETECT_INVIS",SV_POTION_DETECT_INVIS);
 tolua_constant(tolua_S,NULL,"SV_POTION_SLOW_POISON",SV_POTION_SLOW_POISON);
 tolua_constant(tolua_S,NULL,"SV_POTION_CURE_POISON",SV_POTION_CURE_POISON);
 tolua_constant(tolua_S,NULL,"SV_POTION_BOLDNESS",SV_POTION_BOLDNESS);
 tolua_constant(tolua_S,NULL,"SV_POTION_SPEED",SV_POTION_SPEED);
 tolua_constant(tolua_S,NULL,"SV_POTION_RESIST_HEAT",SV_POTION_RESIST_HEAT);
 tolua_constant(tolua_S,NULL,"SV_POTION_RESIST_COLD",SV_POTION_RESIST_COLD);
 tolua_constant(tolua_S,NULL,"SV_POTION_HEROISM",SV_POTION_HEROISM);
 tolua_constant(tolua_S,NULL,"SV_POTION_BERSERK_STRENGTH",SV_POTION_BERSERK_STRENGTH);
 tolua_constant(tolua_S,NULL,"SV_POTION_CURE_LIGHT",SV_POTION_CURE_LIGHT);
 tolua_constant(tolua_S,NULL,"SV_POTION_CURE_SERIOUS",SV_POTION_CURE_SERIOUS);
 tolua_constant(tolua_S,NULL,"SV_POTION_CURE_CRITICAL",SV_POTION_CURE_CRITICAL);
 tolua_constant(tolua_S,NULL,"SV_POTION_HEALING",SV_POTION_HEALING);
 tolua_constant(tolua_S,NULL,"SV_POTION_STAR_HEALING",SV_POTION_STAR_HEALING);
 tolua_constant(tolua_S,NULL,"SV_POTION_LIFE",SV_POTION_LIFE);
 tolua_constant(tolua_S,NULL,"SV_POTION_RESTORE_MANA",SV_POTION_RESTORE_MANA);
 tolua_constant(tolua_S,NULL,"SV_POTION_RESTORE_EXP",SV_POTION_RESTORE_EXP);
 tolua_constant(tolua_S,NULL,"SV_POTION_RES_STR",SV_POTION_RES_STR);
 tolua_constant(tolua_S,NULL,"SV_POTION_RES_INT",SV_POTION_RES_INT);
 tolua_constant(tolua_S,NULL,"SV_POTION_RES_WIS",SV_POTION_RES_WIS);
 tolua_constant(tolua_S,NULL,"SV_POTION_RES_DEX",SV_POTION_RES_DEX);
 tolua_constant(tolua_S,NULL,"SV_POTION_RES_CON",SV_POTION_RES_CON);
 tolua_constant(tolua_S,NULL,"SV_POTION_RES_CHR",SV_POTION_RES_CHR);
 tolua_constant(tolua_S,NULL,"SV_POTION_INC_STR",SV_POTION_INC_STR);
 tolua_constant(tolua_S,NULL,"SV_POTION_INC_INT",SV_POTION_INC_INT);
 tolua_constant(tolua_S,NULL,"SV_POTION_INC_WIS",SV_POTION_INC_WIS);
 tolua_constant(tolua_S,NULL,"SV_POTION_INC_DEX",SV_POTION_INC_DEX);
 tolua_constant(tolua_S,NULL,"SV_POTION_INC_CON",SV_POTION_INC_CON);
 tolua_constant(tolua_S,NULL,"SV_POTION_INC_CHR",SV_POTION_INC_CHR);
 tolua_constant(tolua_S,NULL,"SV_POTION_AUGMENTATION",SV_POTION_AUGMENTATION);
 tolua_constant(tolua_S,NULL,"SV_POTION_ENLIGHTENMENT",SV_POTION_ENLIGHTENMENT);
 tolua_constant(tolua_S,NULL,"SV_POTION_STAR_ENLIGHTENMENT",SV_POTION_STAR_ENLIGHTENMENT);
 tolua_constant(tolua_S,NULL,"SV_POTION_SELF_KNOWLEDGE",SV_POTION_SELF_KNOWLEDGE);
 tolua_constant(tolua_S,NULL,"SV_POTION_EXPERIENCE",SV_POTION_EXPERIENCE);
 tolua_constant(tolua_S,NULL,"SV_POTION_RESISTANCE",SV_POTION_RESISTANCE);
 tolua_constant(tolua_S,NULL,"SV_POTION_CURING",SV_POTION_CURING);
 tolua_constant(tolua_S,NULL,"SV_POTION_INVULNERABILITY",SV_POTION_INVULNERABILITY);
 tolua_constant(tolua_S,NULL,"SV_POTION_NEW_LIFE",SV_POTION_NEW_LIFE);
 tolua_constant(tolua_S,NULL,"SV_FOOD_POISON",SV_FOOD_POISON);
 tolua_constant(tolua_S,NULL,"SV_FOOD_BLINDNESS",SV_FOOD_BLINDNESS);
 tolua_constant(tolua_S,NULL,"SV_FOOD_PARANOIA",SV_FOOD_PARANOIA);
 tolua_constant(tolua_S,NULL,"SV_FOOD_CONFUSION",SV_FOOD_CONFUSION);
 tolua_constant(tolua_S,NULL,"SV_FOOD_HALLUCINATION",SV_FOOD_HALLUCINATION);
 tolua_constant(tolua_S,NULL,"SV_FOOD_PARALYSIS",SV_FOOD_PARALYSIS);
 tolua_constant(tolua_S,NULL,"SV_FOOD_WEAKNESS",SV_FOOD_WEAKNESS);
 tolua_constant(tolua_S,NULL,"SV_FOOD_SICKNESS",SV_FOOD_SICKNESS);
 tolua_constant(tolua_S,NULL,"SV_FOOD_STUPIDITY",SV_FOOD_STUPIDITY);
 tolua_constant(tolua_S,NULL,"SV_FOOD_NAIVETY",SV_FOOD_NAIVETY);
 tolua_constant(tolua_S,NULL,"SV_FOOD_UNHEALTH",SV_FOOD_UNHEALTH);
 tolua_constant(tolua_S,NULL,"SV_FOOD_DISEASE",SV_FOOD_DISEASE);
 tolua_constant(tolua_S,NULL,"SV_FOOD_CURE_POISON",SV_FOOD_CURE_POISON);
 tolua_constant(tolua_S,NULL,"SV_FOOD_CURE_BLINDNESS",SV_FOOD_CURE_BLINDNESS);
 tolua_constant(tolua_S,NULL,"SV_FOOD_CURE_PARANOIA",SV_FOOD_CURE_PARANOIA);
 tolua_constant(tolua_S,NULL,"SV_FOOD_CURE_CONFUSION",SV_FOOD_CURE_CONFUSION);
 tolua_constant(tolua_S,NULL,"SV_FOOD_CURE_SERIOUS",SV_FOOD_CURE_SERIOUS);
 tolua_constant(tolua_S,NULL,"SV_FOOD_RESTORE_STR",SV_FOOD_RESTORE_STR);
 tolua_constant(tolua_S,NULL,"SV_FOOD_RESTORE_CON",SV_FOOD_RESTORE_CON);
 tolua_constant(tolua_S,NULL,"SV_FOOD_RESTORING",SV_FOOD_RESTORING);
 tolua_constant(tolua_S,NULL,"SV_FOOD_BISCUIT",SV_FOOD_BISCUIT);
 tolua_constant(tolua_S,NULL,"SV_FOOD_JERKY",SV_FOOD_JERKY);
 tolua_constant(tolua_S,NULL,"SV_FOOD_RATION",SV_FOOD_RATION);
 tolua_constant(tolua_S,NULL,"SV_FOOD_SLIME_MOLD",SV_FOOD_SLIME_MOLD);
 tolua_constant(tolua_S,NULL,"SV_FOOD_WAYBREAD",SV_FOOD_WAYBREAD);
 tolua_constant(tolua_S,NULL,"SV_FOOD_PINT_OF_ALE",SV_FOOD_PINT_OF_ALE);
 tolua_constant(tolua_S,NULL,"SV_FOOD_PINT_OF_WINE",SV_FOOD_PINT_OF_WINE);
 tolua_constant(tolua_S,NULL,"SV_FOOD_MIN_FOOD",SV_FOOD_MIN_FOOD);
 tolua_constant(tolua_S,NULL,"SV_ROD_MIN_DIRECTION",SV_ROD_MIN_DIRECTION);
 tolua_constant(tolua_S,NULL,"SV_CHEST_MIN_LARGE",SV_CHEST_MIN_LARGE);
 tolua_constant(tolua_S,NULL,"SV_BOOK_MIN_GOOD",SV_BOOK_MIN_GOOD);
 tolua_constant(tolua_S,NULL,"OBJ_GOLD_LIST",OBJ_GOLD_LIST);
 tolua_constant(tolua_S,NULL,"MAX_GOLD",MAX_GOLD);
 tolua_constant(tolua_S,NULL,"OC_NONE",OC_NONE);
 tolua_constant(tolua_S,NULL,"OC_NORMAL",OC_NORMAL);
 tolua_constant(tolua_S,NULL,"OC_FORCE_BAD",OC_FORCE_BAD);
 tolua_constant(tolua_S,NULL,"OC_FORCE_GOOD",OC_FORCE_GOOD);
 tolua_constant(tolua_S,NULL,"TR1_STR",TR1_STR);
 tolua_constant(tolua_S,NULL,"TR1_INT",TR1_INT);
 tolua_constant(tolua_S,NULL,"TR1_WIS",TR1_WIS);
 tolua_constant(tolua_S,NULL,"TR1_DEX",TR1_DEX);
 tolua_constant(tolua_S,NULL,"TR1_CON",TR1_CON);
 tolua_constant(tolua_S,NULL,"TR1_CHR",TR1_CHR);
 tolua_constant(tolua_S,NULL,"TR1_XXX1",TR1_XXX1);
 tolua_constant(tolua_S,NULL,"TR1_XXX2",TR1_XXX2);
 tolua_constant(tolua_S,NULL,"TR1_STEALTH",TR1_STEALTH);
 tolua_constant(tolua_S,NULL,"TR1_SEARCH",TR1_SEARCH);
 tolua_constant(tolua_S,NULL,"TR1_INFRA",TR1_INFRA);
 tolua_constant(tolua_S,NULL,"TR1_TUNNEL",TR1_TUNNEL);
 tolua_constant(tolua_S,NULL,"TR1_SPEED",TR1_SPEED);
 tolua_constant(tolua_S,NULL,"TR1_BLOWS",TR1_BLOWS);
 tolua_constant(tolua_S,NULL,"TR1_CHAOTIC",TR1_CHAOTIC);
 tolua_constant(tolua_S,NULL,"TR1_VAMPIRIC",TR1_VAMPIRIC);
 tolua_constant(tolua_S,NULL,"TR1_SLAY_ANIMAL",TR1_SLAY_ANIMAL);
 tolua_constant(tolua_S,NULL,"TR1_SLAY_EVIL",TR1_SLAY_EVIL);
 tolua_constant(tolua_S,NULL,"TR1_SLAY_UNDEAD",TR1_SLAY_UNDEAD);
 tolua_constant(tolua_S,NULL,"TR1_SLAY_DEMON",TR1_SLAY_DEMON);
 tolua_constant(tolua_S,NULL,"TR1_SLAY_ORC",TR1_SLAY_ORC);
 tolua_constant(tolua_S,NULL,"TR1_SLAY_TROLL",TR1_SLAY_TROLL);
 tolua_constant(tolua_S,NULL,"TR1_SLAY_GIANT",TR1_SLAY_GIANT);
 tolua_constant(tolua_S,NULL,"TR1_SLAY_DRAGON",TR1_SLAY_DRAGON);
 tolua_constant(tolua_S,NULL,"TR1_KILL_DRAGON",TR1_KILL_DRAGON);
 tolua_constant(tolua_S,NULL,"TR1_VORPAL",TR1_VORPAL);
 tolua_constant(tolua_S,NULL,"TR1_IMPACT",TR1_IMPACT);
 tolua_constant(tolua_S,NULL,"TR1_BRAND_POIS",TR1_BRAND_POIS);
 tolua_constant(tolua_S,NULL,"TR1_BRAND_ACID",TR1_BRAND_ACID);
 tolua_constant(tolua_S,NULL,"TR1_BRAND_ELEC",TR1_BRAND_ELEC);
 tolua_constant(tolua_S,NULL,"TR1_BRAND_FIRE",TR1_BRAND_FIRE);
 tolua_constant(tolua_S,NULL,"TR1_BRAND_COLD",TR1_BRAND_COLD);
 tolua_constant(tolua_S,NULL,"TR2_SUST_STR",TR2_SUST_STR);
 tolua_constant(tolua_S,NULL,"TR2_SUST_INT",TR2_SUST_INT);
 tolua_constant(tolua_S,NULL,"TR2_SUST_WIS",TR2_SUST_WIS);
 tolua_constant(tolua_S,NULL,"TR2_SUST_DEX",TR2_SUST_DEX);
 tolua_constant(tolua_S,NULL,"TR2_SUST_CON",TR2_SUST_CON);
 tolua_constant(tolua_S,NULL,"TR2_SUST_CHR",TR2_SUST_CHR);
 tolua_constant(tolua_S,NULL,"TR2_XXX1",TR2_XXX1);
 tolua_constant(tolua_S,NULL,"TR2_XXX2",TR2_XXX2);
 tolua_constant(tolua_S,NULL,"TR2_IM_ACID",TR2_IM_ACID);
 tolua_constant(tolua_S,NULL,"TR2_IM_ELEC",TR2_IM_ELEC);
 tolua_constant(tolua_S,NULL,"TR2_IM_FIRE",TR2_IM_FIRE);
 tolua_constant(tolua_S,NULL,"TR2_IM_COLD",TR2_IM_COLD);
 tolua_constant(tolua_S,NULL,"TR2_THROW",TR2_THROW);
 tolua_constant(tolua_S,NULL,"TR2_REFLECT",TR2_REFLECT);
 tolua_constant(tolua_S,NULL,"TR2_FREE_ACT",TR2_FREE_ACT);
 tolua_constant(tolua_S,NULL,"TR2_HOLD_LIFE",TR2_HOLD_LIFE);
 tolua_constant(tolua_S,NULL,"TR2_RES_ACID",TR2_RES_ACID);
 tolua_constant(tolua_S,NULL,"TR2_RES_ELEC",TR2_RES_ELEC);
 tolua_constant(tolua_S,NULL,"TR2_RES_FIRE",TR2_RES_FIRE);
 tolua_constant(tolua_S,NULL,"TR2_RES_COLD",TR2_RES_COLD);
 tolua_constant(tolua_S,NULL,"TR2_RES_POIS",TR2_RES_POIS);
 tolua_constant(tolua_S,NULL,"TR2_RES_FEAR",TR2_RES_FEAR);
 tolua_constant(tolua_S,NULL,"TR2_RES_LITE",TR2_RES_LITE);
 tolua_constant(tolua_S,NULL,"TR2_RES_DARK",TR2_RES_DARK);
 tolua_constant(tolua_S,NULL,"TR2_RES_BLIND",TR2_RES_BLIND);
 tolua_constant(tolua_S,NULL,"TR2_RES_CONF",TR2_RES_CONF);
 tolua_constant(tolua_S,NULL,"TR2_RES_SOUND",TR2_RES_SOUND);
 tolua_constant(tolua_S,NULL,"TR2_RES_SHARDS",TR2_RES_SHARDS);
 tolua_constant(tolua_S,NULL,"TR2_RES_NETHER",TR2_RES_NETHER);
 tolua_constant(tolua_S,NULL,"TR2_RES_NEXUS",TR2_RES_NEXUS);
 tolua_constant(tolua_S,NULL,"TR2_RES_CHAOS",TR2_RES_CHAOS);
 tolua_constant(tolua_S,NULL,"TR2_RES_DISEN",TR2_RES_DISEN);
 tolua_constant(tolua_S,NULL,"TR3_SH_FIRE",TR3_SH_FIRE);
 tolua_constant(tolua_S,NULL,"TR3_SH_ELEC",TR3_SH_ELEC);
 tolua_constant(tolua_S,NULL,"TR3_QUESTITEM",TR3_QUESTITEM);
 tolua_constant(tolua_S,NULL,"TR3_XXX4",TR3_XXX4);
 tolua_constant(tolua_S,NULL,"TR3_NO_TELE",TR3_NO_TELE);
 tolua_constant(tolua_S,NULL,"TR3_NO_MAGIC",TR3_NO_MAGIC);
 tolua_constant(tolua_S,NULL,"TR3_XXX7",TR3_XXX7);
 tolua_constant(tolua_S,NULL,"TR3_TY_CURSE",TR3_TY_CURSE);
 tolua_constant(tolua_S,NULL,"TR3_EASY_KNOW",TR3_EASY_KNOW);
 tolua_constant(tolua_S,NULL,"TR3_HIDE_TYPE",TR3_HIDE_TYPE);
 tolua_constant(tolua_S,NULL,"TR3_SHOW_MODS",TR3_SHOW_MODS);
 tolua_constant(tolua_S,NULL,"TR3_INSTA_ART",TR3_INSTA_ART);
 tolua_constant(tolua_S,NULL,"TR3_FEATHER",TR3_FEATHER);
 tolua_constant(tolua_S,NULL,"TR3_LITE",TR3_LITE);
 tolua_constant(tolua_S,NULL,"TR3_SEE_INVIS",TR3_SEE_INVIS);
 tolua_constant(tolua_S,NULL,"TR3_TELEPATHY",TR3_TELEPATHY);
 tolua_constant(tolua_S,NULL,"TR3_SLOW_DIGEST",TR3_SLOW_DIGEST);
 tolua_constant(tolua_S,NULL,"TR3_REGEN",TR3_REGEN);
 tolua_constant(tolua_S,NULL,"TR3_XTRA_MIGHT",TR3_XTRA_MIGHT);
 tolua_constant(tolua_S,NULL,"TR3_XTRA_SHOTS",TR3_XTRA_SHOTS);
 tolua_constant(tolua_S,NULL,"TR3_IGNORE_ACID",TR3_IGNORE_ACID);
 tolua_constant(tolua_S,NULL,"TR3_IGNORE_ELEC",TR3_IGNORE_ELEC);
 tolua_constant(tolua_S,NULL,"TR3_IGNORE_FIRE",TR3_IGNORE_FIRE);
 tolua_constant(tolua_S,NULL,"TR3_IGNORE_COLD",TR3_IGNORE_COLD);
 tolua_constant(tolua_S,NULL,"TR3_ACTIVATE",TR3_ACTIVATE);
 tolua_constant(tolua_S,NULL,"TR3_DRAIN_EXP",TR3_DRAIN_EXP);
 tolua_constant(tolua_S,NULL,"TR3_TELEPORT",TR3_TELEPORT);
 tolua_constant(tolua_S,NULL,"TR3_AGGRAVATE",TR3_AGGRAVATE);
 tolua_constant(tolua_S,NULL,"TR3_BLESSED",TR3_BLESSED);
 tolua_constant(tolua_S,NULL,"TR3_CURSED",TR3_CURSED);
 tolua_constant(tolua_S,NULL,"TR3_HEAVY_CURSE",TR3_HEAVY_CURSE);
 tolua_constant(tolua_S,NULL,"TR3_PERMA_CURSE",TR3_PERMA_CURSE);
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
 tolua_tablevar(tolua_S,"object_kind","flags1",toluaI_get_object_object_kind_flags1,toluaI_set_object_object_kind_flags1);
 tolua_tablevar(tolua_S,"object_kind","flags2",toluaI_get_object_object_kind_flags2,toluaI_set_object_object_kind_flags2);
 tolua_tablevar(tolua_S,"object_kind","flags3",toluaI_get_object_object_kind_flags3,toluaI_set_object_object_kind_flags3);
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
 tolua_tablevar(tolua_S,"object_type","flags1",toluaI_get_object_object_type_flags1,toluaI_set_object_object_type_flags1);
 tolua_tablevar(tolua_S,"object_type","flags2",toluaI_get_object_object_type_flags2,toluaI_set_object_object_type_flags2);
 tolua_tablevar(tolua_S,"object_type","flags3",toluaI_get_object_object_type_flags3,toluaI_set_object_object_type_flags3);
 tolua_tablevar(tolua_S,"object_type","kn_flags1",toluaI_get_object_object_type_kn_flags1,toluaI_set_object_object_type_kn_flags1);
 tolua_tablevar(tolua_S,"object_type","kn_flags2",toluaI_get_object_object_type_kn_flags2,toluaI_set_object_object_type_kn_flags2);
 tolua_tablevar(tolua_S,"object_type","kn_flags3",toluaI_get_object_object_type_kn_flags3,toluaI_set_object_object_type_kn_flags3);
 tolua_tablevar(tolua_S,"object_type","next_o_idx",toluaI_get_object_object_type_next_o_idx,toluaI_set_object_object_type_next_o_idx);
 tolua_tablevar(tolua_S,"object_type","cost",toluaI_get_object_object_type_cost,toluaI_set_object_object_type_cost);
 tolua_tablevar(tolua_S,"object_type","feeling",toluaI_get_object_object_type_feeling,toluaI_set_object_object_type_feeling);
 tolua_tablevar(tolua_S,"object_type","activate",toluaI_get_object_object_type_activate,toluaI_set_object_object_type_activate);
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
 tolua_function(tolua_S,NULL,"random_resistance",toluaI_object_random_resistance00);
 tolua_function(tolua_S,NULL,"activate_effect",toluaI_object_activate_effect00);
 tolua_function(tolua_S,NULL,"random_artifact_resistance",toluaI_object_random_artifact_resistance00);
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
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"ACT_SUNLIGHT");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"ACT_BO_MISS_1");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"ACT_BA_POIS_1");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"ACT_BO_ELEC_1");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"ACT_BO_ACID_1");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"ACT_BO_COLD_1");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"ACT_BO_FIRE_1");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"ACT_BA_COLD_1");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"ACT_BA_FIRE_1");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"ACT_DRAIN_1");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"ACT_BA_COLD_2");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"ACT_BA_ELEC_2");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"ACT_DRAIN_2");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"ACT_VAMPIRE_1");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"ACT_BO_MISS_2");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"ACT_BA_FIRE_2");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"ACT_BA_COLD_3");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"ACT_BA_ELEC_3");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"ACT_WHIRLWIND");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"ACT_VAMPIRE_2");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"ACT_CALL_CHAOS");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"ACT_ROCKET");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"ACT_DISP_EVIL");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"ACT_BA_MISS_3");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"ACT_DISP_GOOD");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"ACT_CONFUSE");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"ACT_SLEEP");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"ACT_QUAKE");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"ACT_TERROR");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"ACT_TELE_AWAY");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"ACT_BANISH_EVIL");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"ACT_GENOCIDE");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"ACT_MASS_GENO");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"ACT_CHARM_ANIMAL");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"ACT_CHARM_UNDEAD");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"ACT_CHARM_OTHER");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"ACT_CHARM_ANIMALS");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"ACT_CHARM_OTHERS");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"ACT_SUMMON_ANIMAL");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"ACT_SUMMON_PHANTOM");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"ACT_SUMMON_ELEMENTAL");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"ACT_SUMMON_DEMON");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"ACT_SUMMON_UNDEAD");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"ACT_CURE_LW");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"ACT_CURE_MW");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"ACT_CURE_POISON");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"ACT_REST_LIFE");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"ACT_REST_ALL");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"ACT_CURE_700");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"ACT_CURE_1000");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"ACT_ESP");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"ACT_BERSERK");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"ACT_PROT_EVIL");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"ACT_RESIST_ALL");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"ACT_SPEED");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"ACT_XTRA_SPEED");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"ACT_WRAITH");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"ACT_INVULN");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"ACT_TELEPORT_1");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"ACT_LIGHT");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"ACT_MAP_LIGHT");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"ACT_DETECT_ALL");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"ACT_DETECT_XTRA");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"ACT_ID_FULL");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"ACT_ID_PLAIN");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"ACT_RUNE_EXPLO");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"ACT_RUNE_PROT");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"ACT_SATIATE");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"ACT_DEST_DOOR");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"ACT_STONE_MUD");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"ACT_RECHARGE");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"ACT_ALCHEMY");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"ACT_DIM_DOOR");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"ACT_TELEPORT_2");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"ACT_RECALL");
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
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_ANY");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_FIGURINE_NORMAL");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_WOODEN_STATUE");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_CLAY_STATUE");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_STONE_STATUE");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_IRON_STATUE");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_COPPER_STATUE");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_SILVER_STATUE");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_GOLDEN_STATUE");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_IVORY_STATUE");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_MITHRIL_STATUE");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_ORNATE_STATUE");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_AMMO_LIGHT");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_AMMO_NORMAL");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_AMMO_HEAVY");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_SLING");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_SHORT_BOW");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_LONG_BOW");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_LIGHT_XBOW");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_HEAVY_XBOW");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_SHOVEL");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_GNOMISH_SHOVEL");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_DWARVEN_SHOVEL");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_PICK");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_ORCISH_PICK");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_DWARVEN_PICK");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_MATTOCK");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_CLUB");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_WHIP");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_QUARTERSTAFF");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_NUNCHAKU");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_MACE");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_BALL_AND_CHAIN");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_JO_STAFF");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_WAR_HAMMER");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_THREE_PIECE_ROD");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_MORNING_STAR");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_FLAIL");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_BO_STAFF");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_LEAD_FILLED_MACE");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_TETSUBO");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_TWO_HANDED_FLAIL");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_GREAT_HAMMER");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_MACE_OF_DISRUPTION");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_GROND");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_HATCHET");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_SPEAR");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_SICKLE");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_AWL_PIKE");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_TRIDENT");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_FAUCHARD");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_BROAD_SPEAR");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_PIKE");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_NAGINATA");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_BEAKED_AXE");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_BROAD_AXE");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_LUCERNE_HAMMER");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_GLAIVE");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_LAJATANG");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_HALBERD");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_GUISARME");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_SCYTHE");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_LANCE");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_BATTLE_AXE");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_GREAT_AXE");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_TRIFURCATE_SPEAR");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_LOCHABER_AXE");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_HEAVY_LANCE");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_SCYTHE_OF_SLICING");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_BROKEN_DAGGER");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_BROKEN_SWORD");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_DAGGER");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_MAIN_GAUCHE");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_TANTO");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_RAPIER");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_SMALL_SWORD");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_BASILLARD");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_SHORT_SWORD");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_SABRE");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_CUTLASS");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_WAKIZASHI");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_KHOPESH");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_TULWAR");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_BROAD_SWORD");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_LONG_SWORD");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_SCIMITAR");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_NINJATO");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_KATANA");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_BASTARD_SWORD");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_GREAT_SCIMITAR");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_CLAYMORE");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_ESPADON");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_TWO_HANDED_SWORD");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_FLAMBERGE");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_NO_DACHI");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_EXECUTIONERS_SWORD");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_ZWEIHANDER");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_BLADE_OF_CHAOS");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_DIAMOND_EDGE");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_SMALL_LEATHER_SHIELD");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_SMALL_METAL_SHIELD");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_LARGE_LEATHER_SHIELD");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_LARGE_METAL_SHIELD");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_DRAGON_SHIELD");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_SHIELD_OF_DEFLECTION");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_HARD_LEATHER_CAP");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_METAL_CAP");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_JINGASA");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_IRON_HELM");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_STEEL_HELM");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_DRAGON_HELM");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_KABUTO");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_IRON_CROWN");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_GOLDEN_CROWN");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_JEWELED_CROWN");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_MORGOTH");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_PAIR_OF_SOFT_LEATHER_BOOTS");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_PAIR_OF_HARD_LEATHER_BOOTS");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_PAIR_OF_METAL_SHOD_BOOTS");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_CLOAK");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_ELVEN_CLOAK");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_FUR_CLOAK");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_SHADOW_CLOAK");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_SET_OF_LEATHER_GLOVES");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_SET_OF_GAUNTLETS");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_SET_OF_CESTI");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_T_SHIRT");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_FILTHY_RAG");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_ROBE");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_PAPER_ARMOR");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_SOFT_LEATHER_ARMOR");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_SOFT_STUDDED_LEATHER");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_HARD_LEATHER_ARMOR");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_HARD_STUDDED_LEATHER");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_RHINO_HIDE_ARMOR");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_CORD_ARMOR");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_PADDED_ARMOR");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_LEATHER_SCALE_MAIL");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_LEATHER_JACK");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_STONE_AND_HIDE_ARMOR");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_RUSTY_CHAIN_MAIL");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_RING_MAIL");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_METAL_SCALE_MAIL");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_CHAIN_MAIL");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_DOUBLE_RING_MAIL");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_AUGMENTED_CHAIN_MAIL");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_DOUBLE_CHAIN_MAIL");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_BAR_CHAIN_MAIL");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_METAL_BRIGANDINE_ARMOUR");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_SPLINT_MAIL");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_DO_MARU");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_PARTIAL_PLATE_ARMOUR");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_METAL_LAMELLAR_ARMOUR");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_HARAMAKIDO");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_FULL_PLATE_ARMOUR");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_O_YOROI");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_RIBBED_PLATE_ARMOUR");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_MITHRIL_CHAIN_MAIL");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_MITHRIL_PLATE_MAIL");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_ADAMANTITE_PLATE_MAIL");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_DRAGON_BLACK");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_DRAGON_BLUE");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_DRAGON_WHITE");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_DRAGON_RED");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_DRAGON_GREEN");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_DRAGON_MULTIHUED");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_DRAGON_SHINING");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_DRAGON_LAW");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_DRAGON_BRONZE");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_DRAGON_GOLD");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_DRAGON_CHAOS");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_DRAGON_BALANCE");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_DRAGON_POWER");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_LITE_TORCH");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_LITE_LANTERN");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_LITE_GALADRIEL");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_LITE_ELENDIL");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_LITE_THRAIN");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_AMULET_DOOM");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_AMULET_TELEPORT");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_AMULET_BERSERK");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_AMULET_SLOW_DIGEST");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_AMULET_RESIST_ACID");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_AMULET_SEARCHING");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_AMULET_WISDOM");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_AMULET_CHARISMA");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_AMULET_THE_MAGI");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_AMULET_REFLECTION");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_AMULET_CARLAMMAS");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_AMULET_INGWE");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_AMULET_DWARVES");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_AMULET_NO_MAGIC");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_AMULET_NO_TELE");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_AMULET_RESISTANCE");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_RING_WOE");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_RING_AGGRAVATION");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_RING_WEAKNESS");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_RING_STUPIDITY");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_RING_TELEPORTATION");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_RING_SLOW_DIGESTION");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_RING_FEATHER_FALL");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_RING_RESIST_FIRE");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_RING_RESIST_COLD");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_RING_SUSTAIN_STR");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_RING_SUSTAIN_INT");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_RING_SUSTAIN_WIS");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_RING_SUSTAIN_DEX");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_RING_SUSTAIN_CON");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_RING_SUSTAIN_CHR");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_RING_PROTECTION");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_RING_ACID");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_RING_FLAMES");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_RING_ICE");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_RING_RESIST_POIS");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_RING_FREE_ACTION");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_RING_SEE_INVIS");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_RING_SEARCHING");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_RING_STR");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_RING_INT");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_RING_DEX");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_RING_CON");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_RING_ACCURACY");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_RING_DAMAGE");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_RING_SLAYING");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_RING_SPEED");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_RING_BARAHIR");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_RING_TULKAS");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_RING_NARYA");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_RING_NENYA");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_RING_VILYA");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_RING_POWER");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_RING_RES_FEAR");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_RING_RES_LD");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_RING_RES_NETHER");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_RING_RES_NEXUS");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_RING_RES_SOUND");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_RING_RES_CONFUSION");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_RING_RES_SHARDS");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_RING_RES_DISENCHANT");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_RING_RES_CHAOS");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_RING_RES_BLINDNESS");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_RING_LORDLY");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_RING_ATTACKS");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_STAFF_DARKNESS");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_STAFF_SLOWNESS");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_STAFF_HASTE_MONSTERS");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_STAFF_SUMMONING");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_STAFF_TELEPORTATION");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_STAFF_IDENTIFY");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_STAFF_REMOVE_CURSE");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_STAFF_STARLITE");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_STAFF_LITE");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_STAFF_MAPPING");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_STAFF_DETECT_GOLD");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_STAFF_DETECT_ITEM");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_STAFF_DETECT_TRAP");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_STAFF_DETECT_DOOR");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_STAFF_DETECT_INVIS");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_STAFF_DETECT_EVIL");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_STAFF_CURE_LIGHT");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_STAFF_CURING");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_STAFF_HEALING");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_STAFF_THE_MAGI");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_STAFF_SLEEP_MONSTERS");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_STAFF_SLOW_MONSTERS");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_STAFF_SPEED");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_STAFF_PROBING");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_STAFF_DISPEL_EVIL");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_STAFF_POWER");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_STAFF_HOLINESS");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_STAFF_GENOCIDE");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_STAFF_EARTHQUAKES");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_STAFF_DESTRUCTION");
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
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_WAND_ANNIHILATION");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_WAND_DRAGON_FIRE");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_WAND_DRAGON_COLD");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_WAND_DRAGON_BREATH");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_WAND_ROCKETS");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_ROD_DETECT_TRAP");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_ROD_DETECT_DOOR");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_ROD_IDENTIFY");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_ROD_RECALL");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_ROD_ILLUMINATION");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_ROD_MAPPING");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_ROD_DETECTION");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_ROD_PROBING");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_ROD_CURING");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_ROD_HEALING");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_ROD_RESTORATION");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_ROD_SPEED");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_ROD_PESTICIDE");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_ROD_TELEPORT_AWAY");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_ROD_DISARMING");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_ROD_LITE");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_ROD_SLEEP_MONSTER");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_ROD_SLOW_MONSTER");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_ROD_DRAIN_LIFE");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_ROD_POLYMORPH");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_ROD_ACID_BOLT");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_ROD_ELEC_BOLT");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_ROD_FIRE_BOLT");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_ROD_COLD_BOLT");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_ROD_ACID_BALL");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_ROD_ELEC_BALL");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_ROD_FIRE_BALL");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_ROD_COLD_BALL");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_ROD_HAVOC");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_SCROLL_DARKNESS");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_SCROLL_AGGRAVATE_MONSTER");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_SCROLL_CURSE_ARMOR");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_SCROLL_CURSE_WEAPON");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_SCROLL_SUMMON_MONSTER");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_SCROLL_SUMMON_UNDEAD");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_SCROLL_TRAP_CREATION");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_SCROLL_PHASE_DOOR");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_SCROLL_TELEPORT");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_SCROLL_TELEPORT_LEVEL");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_SCROLL_WORD_OF_RECALL");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_SCROLL_IDENTIFY");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_SCROLL_STAR_IDENTIFY");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_SCROLL_REMOVE_CURSE");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_SCROLL_STAR_REMOVE_CURSE");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_SCROLL_ENCHANT_ARMOR");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_SCROLL_ENCHANT_WEAPON_TO_HIT");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_SCROLL_ENCHANT_WEAPON_TO_DAM");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_SCROLL_STAR_ENCHANT_ARMOR");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_SCROLL_STAR_ENCHANT_WEAPON");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_SCROLL_RECHARGING");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_SCROLL_MUNDANITY");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_SCROLL_LIGHT");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_SCROLL_MAPPING");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_SCROLL_DETECT_GOLD");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_SCROLL_DETECT_ITEM");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_SCROLL_DETECT_TRAP");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_SCROLL_DETECT_DOOR");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_SCROLL_DETECT_INVIS");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_SCROLL_SATISFY_HUNGER");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_SCROLL_BLESSING");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_SCROLL_HOLY_CHANT");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_SCROLL_HOLY_PRAYER");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_SCROLL_MONSTER_CONFUSION");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_SCROLL_PROTECTION_FROM_EVIL");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_SCROLL_RUNE_OF_PROTECTION");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_SCROLL_TRAP_DOOR_DESTRUCTION");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_SCROLL_STAR_DESTRUCTION");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_SCROLL_DISPEL_UNDEAD");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_SCROLL_GENOCIDE");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_SCROLL_MASS_GENOCIDE");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_SCROLL_ACQUIREMENT");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_SCROLL_STAR_ACQUIREMENT");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_SCROLL_FIRE");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_SCROLL_ICE");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_SCROLL_CHAOS");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_SCROLL_RUMOR");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_SCROLL_ARTIFACT");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_POTION_WATER");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_POTION_APPLE_JUICE");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_POTION_SLIME_MOLD");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_POTION_SLOWNESS");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_POTION_SALT_WATER");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_POTION_POISON");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_POTION_BLINDNESS");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_POTION_CONFUSION");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_POTION_SLEEP");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_POTION_LOSE_MEMORIES");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_POTION_RUINATION");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_POTION_DEC_STR");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_POTION_DEC_INT");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_POTION_DEC_WIS");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_POTION_DEC_DEX");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_POTION_DEC_CON");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_POTION_DEC_CHR");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_POTION_DETONATIONS");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_POTION_DEATH");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_POTION_INFRAVISION");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_POTION_DETECT_INVIS");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_POTION_SLOW_POISON");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_POTION_CURE_POISON");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_POTION_BOLDNESS");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_POTION_SPEED");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_POTION_RESIST_HEAT");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_POTION_RESIST_COLD");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_POTION_HEROISM");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_POTION_BERSERK_STRENGTH");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_POTION_CURE_LIGHT");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_POTION_CURE_SERIOUS");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_POTION_CURE_CRITICAL");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_POTION_HEALING");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_POTION_STAR_HEALING");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_POTION_LIFE");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_POTION_RESTORE_MANA");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_POTION_RESTORE_EXP");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_POTION_RES_STR");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_POTION_RES_INT");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_POTION_RES_WIS");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_POTION_RES_DEX");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_POTION_RES_CON");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_POTION_RES_CHR");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_POTION_INC_STR");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_POTION_INC_INT");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_POTION_INC_WIS");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_POTION_INC_DEX");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_POTION_INC_CON");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_POTION_INC_CHR");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_POTION_AUGMENTATION");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_POTION_ENLIGHTENMENT");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_POTION_STAR_ENLIGHTENMENT");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_POTION_SELF_KNOWLEDGE");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_POTION_EXPERIENCE");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_POTION_RESISTANCE");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_POTION_CURING");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_POTION_INVULNERABILITY");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_POTION_NEW_LIFE");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_FOOD_POISON");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_FOOD_BLINDNESS");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_FOOD_PARANOIA");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_FOOD_CONFUSION");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_FOOD_HALLUCINATION");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_FOOD_PARALYSIS");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_FOOD_WEAKNESS");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_FOOD_SICKNESS");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_FOOD_STUPIDITY");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_FOOD_NAIVETY");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_FOOD_UNHEALTH");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_FOOD_DISEASE");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_FOOD_CURE_POISON");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_FOOD_CURE_BLINDNESS");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_FOOD_CURE_PARANOIA");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_FOOD_CURE_CONFUSION");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_FOOD_CURE_SERIOUS");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_FOOD_RESTORE_STR");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_FOOD_RESTORE_CON");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_FOOD_RESTORING");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_FOOD_BISCUIT");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_FOOD_JERKY");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_FOOD_RATION");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_FOOD_SLIME_MOLD");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_FOOD_WAYBREAD");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_FOOD_PINT_OF_ALE");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_FOOD_PINT_OF_WINE");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_FOOD_MIN_FOOD");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_ROD_MIN_DIRECTION");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_CHEST_MIN_LARGE");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_BOOK_MIN_GOOD");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"OBJ_GOLD_LIST");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"MAX_GOLD");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"OC_NONE");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"OC_NORMAL");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"OC_FORCE_BAD");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"OC_FORCE_GOOD");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TR1_STR");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TR1_INT");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TR1_WIS");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TR1_DEX");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TR1_CON");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TR1_CHR");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TR1_XXX1");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TR1_XXX2");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TR1_STEALTH");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TR1_SEARCH");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TR1_INFRA");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TR1_TUNNEL");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TR1_SPEED");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TR1_BLOWS");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TR1_CHAOTIC");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TR1_VAMPIRIC");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TR1_SLAY_ANIMAL");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TR1_SLAY_EVIL");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TR1_SLAY_UNDEAD");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TR1_SLAY_DEMON");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TR1_SLAY_ORC");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TR1_SLAY_TROLL");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TR1_SLAY_GIANT");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TR1_SLAY_DRAGON");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TR1_KILL_DRAGON");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TR1_VORPAL");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TR1_IMPACT");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TR1_BRAND_POIS");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TR1_BRAND_ACID");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TR1_BRAND_ELEC");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TR1_BRAND_FIRE");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TR1_BRAND_COLD");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TR2_SUST_STR");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TR2_SUST_INT");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TR2_SUST_WIS");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TR2_SUST_DEX");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TR2_SUST_CON");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TR2_SUST_CHR");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TR2_XXX1");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TR2_XXX2");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TR2_IM_ACID");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TR2_IM_ELEC");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TR2_IM_FIRE");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TR2_IM_COLD");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TR2_THROW");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TR2_REFLECT");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TR2_FREE_ACT");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TR2_HOLD_LIFE");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TR2_RES_ACID");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TR2_RES_ELEC");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TR2_RES_FIRE");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TR2_RES_COLD");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TR2_RES_POIS");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TR2_RES_FEAR");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TR2_RES_LITE");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TR2_RES_DARK");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TR2_RES_BLIND");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TR2_RES_CONF");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TR2_RES_SOUND");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TR2_RES_SHARDS");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TR2_RES_NETHER");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TR2_RES_NEXUS");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TR2_RES_CHAOS");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TR2_RES_DISEN");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TR3_SH_FIRE");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TR3_SH_ELEC");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TR3_QUESTITEM");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TR3_XXX4");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TR3_NO_TELE");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TR3_NO_MAGIC");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TR3_XXX7");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TR3_TY_CURSE");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TR3_EASY_KNOW");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TR3_HIDE_TYPE");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TR3_SHOW_MODS");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TR3_INSTA_ART");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TR3_FEATHER");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TR3_LITE");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TR3_SEE_INVIS");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TR3_TELEPATHY");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TR3_SLOW_DIGEST");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TR3_REGEN");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TR3_XTRA_MIGHT");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TR3_XTRA_SHOTS");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TR3_IGNORE_ACID");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TR3_IGNORE_ELEC");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TR3_IGNORE_FIRE");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TR3_IGNORE_COLD");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TR3_ACTIVATE");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TR3_DRAIN_EXP");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TR3_TELEPORT");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TR3_AGGRAVATE");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TR3_BLESSED");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TR3_CURSED");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TR3_HEAVY_CURSE");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TR3_PERMA_CURSE");
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
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"random_resistance");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"activate_effect");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"random_artifact_resistance");
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
