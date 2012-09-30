/*
** Lua binding: object
** Generated automatically by tolua 4.0a - angband on Tue Oct  7 23:02:04 2003.
*/

#include "lua/tolua.h"

/* Exported function */
int tolua_object_open (lua_State* tolua_S);
void tolua_object_close (lua_State* tolua_S);

#include "angband.h"
static object_type lua_obj_forge;
static obj_theme lua_obj_theme;
static bool lua_is_artifact(object_type *o_ptr) { return artifact_p(o_ptr); }
static bool lua_is_aware(object_type *o_ptr) { return object_aware_p(o_ptr); }
static bool lua_is_known(object_type *o_ptr) { return object_known_p(o_ptr); }
static void lua_set_aware(object_type *o_ptr) { object_aware(o_ptr); }
static void lua_set_known(object_type *o_ptr) { object_known(o_ptr); }

/* function to register type */
static void toluaI_reg_types (lua_State* tolua_S)
{
 tolua_usertype(tolua_S,"FILE");
 tolua_usertype(tolua_S,"object_type");
 tolua_usertype(tolua_S,"ego_item_type");
 tolua_usertype(tolua_S,"header");
 tolua_usertype(tolua_S,"obj_theme");
 tolua_usertype(tolua_S,"artifact_type");
 tolua_usertype(tolua_S,"object_kind");
}

/* error messages */
#define TOLUA_ERR_SELF tolua_error(tolua_S,"invalid 'self'")
#define TOLUA_ERR_ASSIGN tolua_error(tolua_S,"#vinvalid type in variable assignment.")

/* get function: lua_obj_forge */
static int toluaI_get_object_obj_forge(lua_State* tolua_S)
{
 tolua_pushusertype(tolua_S,(void*)&lua_obj_forge,tolua_tag(tolua_S,"object_type"));
 return 1;
}

/* set function: lua_obj_forge */
static int toluaI_set_object_obj_forge(lua_State* tolua_S)
{
 if (!tolua_istype(tolua_S,2,tolua_tag(tolua_S,"object_type"),0))
 TOLUA_ERR_ASSIGN;
  lua_obj_forge = *((object_type*)  tolua_getusertype(tolua_S,2,0));
 return 0;
}

/* get function: lua_obj_theme */
static int toluaI_get_object_theme_forge(lua_State* tolua_S)
{
 tolua_pushusertype(tolua_S,(void*)&lua_obj_theme,tolua_tag(tolua_S,"obj_theme"));
 return 1;
}

/* set function: lua_obj_theme */
static int toluaI_set_object_theme_forge(lua_State* tolua_S)
{
 if (!tolua_istype(tolua_S,2,tolua_tag(tolua_S,"obj_theme"),0))
 TOLUA_ERR_ASSIGN;
  lua_obj_theme = *((obj_theme*)  tolua_getusertype(tolua_S,2,0));
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
  self->pval = ((s32b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: pval2 of class  object_kind */
static int toluaI_get_object_object_kind_pval2(lua_State* tolua_S)
{
  object_kind* self = (object_kind*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->pval2);
 return 1;
}

/* set function: pval2 of class  object_kind */
static int toluaI_set_object_object_kind_pval2(lua_State* tolua_S)
{
  object_kind* self = (object_kind*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->pval2 = ((s32b)  tolua_getnumber(tolua_S,2,0));
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
  self->weight = ((s32b)  tolua_getnumber(tolua_S,2,0));
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

/* get function: flags4 of class  object_kind */
static int toluaI_get_object_object_kind_flags4(lua_State* tolua_S)
{
  object_kind* self = (object_kind*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->flags4);
 return 1;
}

/* set function: flags4 of class  object_kind */
static int toluaI_set_object_object_kind_flags4(lua_State* tolua_S)
{
  object_kind* self = (object_kind*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->flags4 = ((u32b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: flags5 of class  object_kind */
static int toluaI_get_object_object_kind_flags5(lua_State* tolua_S)
{
  object_kind* self = (object_kind*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->flags5);
 return 1;
}

/* set function: flags5 of class  object_kind */
static int toluaI_set_object_object_kind_flags5(lua_State* tolua_S)
{
  object_kind* self = (object_kind*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->flags5 = ((u32b)  tolua_getnumber(tolua_S,2,0));
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
 tolua_pushnumber(tolua_S,(long)self->easy_know);
 return 1;
}

/* set function: easy_know of class  object_kind */
static int toluaI_set_object_object_kind_easy_know(lua_State* tolua_S)
{
  object_kind* self = (object_kind*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->easy_know = ((bool)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: aware of class  object_kind */
static int toluaI_get_object_object_kind_aware(lua_State* tolua_S)
{
  object_kind* self = (object_kind*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->aware);
 return 1;
}

/* set function: aware of class  object_kind */
static int toluaI_set_object_object_kind_aware(lua_State* tolua_S)
{
  object_kind* self = (object_kind*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->aware = ((bool)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: tried of class  object_kind */
static int toluaI_get_object_object_kind_tried(lua_State* tolua_S)
{
  object_kind* self = (object_kind*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->tried);
 return 1;
}

/* set function: tried of class  object_kind */
static int toluaI_set_object_object_kind_tried(lua_State* tolua_S)
{
  object_kind* self = (object_kind*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->tried = ((bool)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: know of class  object_kind */
static int toluaI_get_object_object_kind_know(lua_State* tolua_S)
{
  object_kind* self = (object_kind*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->know);
 return 1;
}

/* set function: know of class  object_kind */
static int toluaI_set_object_object_kind_know(lua_State* tolua_S)
{
  object_kind* self = (object_kind*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->know = ((bool)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: esp of class  object_kind */
static int toluaI_get_object_object_kind_esp(lua_State* tolua_S)
{
  object_kind* self = (object_kind*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->esp);
 return 1;
}

/* set function: esp of class  object_kind */
static int toluaI_set_object_object_kind_esp(lua_State* tolua_S)
{
  object_kind* self = (object_kind*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->esp = ((u32b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: btval of class  object_kind */
static int toluaI_get_object_object_kind_btval(lua_State* tolua_S)
{
  object_kind* self = (object_kind*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->btval);
 return 1;
}

/* set function: btval of class  object_kind */
static int toluaI_set_object_object_kind_btval(lua_State* tolua_S)
{
  object_kind* self = (object_kind*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->btval = ((byte)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: bsval of class  object_kind */
static int toluaI_get_object_object_kind_bsval(lua_State* tolua_S)
{
  object_kind* self = (object_kind*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->bsval);
 return 1;
}

/* set function: bsval of class  object_kind */
static int toluaI_set_object_object_kind_bsval(lua_State* tolua_S)
{
  object_kind* self = (object_kind*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->bsval = ((byte)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: artifact of class  object_kind */
static int toluaI_get_object_object_kind_artifact(lua_State* tolua_S)
{
  object_kind* self = (object_kind*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->artifact);
 return 1;
}

/* set function: artifact of class  object_kind */
static int toluaI_set_object_object_kind_artifact(lua_State* tolua_S)
{
  object_kind* self = (object_kind*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->artifact = ((bool)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: power of class  object_kind */
static int toluaI_get_object_object_kind_power(lua_State* tolua_S)
{
  object_kind* self = (object_kind*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->power);
 return 1;
}

/* set function: power of class  object_kind */
static int toluaI_set_object_object_kind_power(lua_State* tolua_S)
{
  object_kind* self = (object_kind*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->power = ((s16b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: name of class  artifact_type */
static int toluaI_get_object_artifact_type_name(lua_State* tolua_S)
{
  artifact_type* self = (artifact_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->name);
 return 1;
}

/* set function: name of class  artifact_type */
static int toluaI_set_object_artifact_type_name(lua_State* tolua_S)
{
  artifact_type* self = (artifact_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->name = ((u32b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: text of class  artifact_type */
static int toluaI_get_object_artifact_type_text(lua_State* tolua_S)
{
  artifact_type* self = (artifact_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->text);
 return 1;
}

/* set function: text of class  artifact_type */
static int toluaI_set_object_artifact_type_text(lua_State* tolua_S)
{
  artifact_type* self = (artifact_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->text = ((u32b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: tval of class  artifact_type */
static int toluaI_get_object_artifact_type_tval(lua_State* tolua_S)
{
  artifact_type* self = (artifact_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->tval);
 return 1;
}

/* set function: tval of class  artifact_type */
static int toluaI_set_object_artifact_type_tval(lua_State* tolua_S)
{
  artifact_type* self = (artifact_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->tval = ((byte)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: sval of class  artifact_type */
static int toluaI_get_object_artifact_type_sval(lua_State* tolua_S)
{
  artifact_type* self = (artifact_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->sval);
 return 1;
}

/* set function: sval of class  artifact_type */
static int toluaI_set_object_artifact_type_sval(lua_State* tolua_S)
{
  artifact_type* self = (artifact_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->sval = ((byte)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: pval of class  artifact_type */
static int toluaI_get_object_artifact_type_pval(lua_State* tolua_S)
{
  artifact_type* self = (artifact_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->pval);
 return 1;
}

/* set function: pval of class  artifact_type */
static int toluaI_set_object_artifact_type_pval(lua_State* tolua_S)
{
  artifact_type* self = (artifact_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->pval = ((s16b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: to_h of class  artifact_type */
static int toluaI_get_object_artifact_type_to_h(lua_State* tolua_S)
{
  artifact_type* self = (artifact_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->to_h);
 return 1;
}

/* set function: to_h of class  artifact_type */
static int toluaI_set_object_artifact_type_to_h(lua_State* tolua_S)
{
  artifact_type* self = (artifact_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->to_h = ((s16b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: to_d of class  artifact_type */
static int toluaI_get_object_artifact_type_to_d(lua_State* tolua_S)
{
  artifact_type* self = (artifact_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->to_d);
 return 1;
}

/* set function: to_d of class  artifact_type */
static int toluaI_set_object_artifact_type_to_d(lua_State* tolua_S)
{
  artifact_type* self = (artifact_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->to_d = ((s16b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: to_a of class  artifact_type */
static int toluaI_get_object_artifact_type_to_a(lua_State* tolua_S)
{
  artifact_type* self = (artifact_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->to_a);
 return 1;
}

/* set function: to_a of class  artifact_type */
static int toluaI_set_object_artifact_type_to_a(lua_State* tolua_S)
{
  artifact_type* self = (artifact_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->to_a = ((s16b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: ac of class  artifact_type */
static int toluaI_get_object_artifact_type_ac(lua_State* tolua_S)
{
  artifact_type* self = (artifact_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->ac);
 return 1;
}

/* set function: ac of class  artifact_type */
static int toluaI_set_object_artifact_type_ac(lua_State* tolua_S)
{
  artifact_type* self = (artifact_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->ac = ((s16b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: dd of class  artifact_type */
static int toluaI_get_object_artifact_type_dd(lua_State* tolua_S)
{
  artifact_type* self = (artifact_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->dd);
 return 1;
}

/* set function: dd of class  artifact_type */
static int toluaI_set_object_artifact_type_dd(lua_State* tolua_S)
{
  artifact_type* self = (artifact_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->dd = ((byte)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: ds of class  artifact_type */
static int toluaI_get_object_artifact_type_ds(lua_State* tolua_S)
{
  artifact_type* self = (artifact_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->ds);
 return 1;
}

/* set function: ds of class  artifact_type */
static int toluaI_set_object_artifact_type_ds(lua_State* tolua_S)
{
  artifact_type* self = (artifact_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->ds = ((byte)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: weight of class  artifact_type */
static int toluaI_get_object_artifact_type_weight(lua_State* tolua_S)
{
  artifact_type* self = (artifact_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->weight);
 return 1;
}

/* set function: weight of class  artifact_type */
static int toluaI_set_object_artifact_type_weight(lua_State* tolua_S)
{
  artifact_type* self = (artifact_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->weight = ((s16b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: cost of class  artifact_type */
static int toluaI_get_object_artifact_type_cost(lua_State* tolua_S)
{
  artifact_type* self = (artifact_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->cost);
 return 1;
}

/* set function: cost of class  artifact_type */
static int toluaI_set_object_artifact_type_cost(lua_State* tolua_S)
{
  artifact_type* self = (artifact_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->cost = ((s32b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: flags1 of class  artifact_type */
static int toluaI_get_object_artifact_type_flags1(lua_State* tolua_S)
{
  artifact_type* self = (artifact_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->flags1);
 return 1;
}

/* set function: flags1 of class  artifact_type */
static int toluaI_set_object_artifact_type_flags1(lua_State* tolua_S)
{
  artifact_type* self = (artifact_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->flags1 = ((u32b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: flags2 of class  artifact_type */
static int toluaI_get_object_artifact_type_flags2(lua_State* tolua_S)
{
  artifact_type* self = (artifact_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->flags2);
 return 1;
}

/* set function: flags2 of class  artifact_type */
static int toluaI_set_object_artifact_type_flags2(lua_State* tolua_S)
{
  artifact_type* self = (artifact_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->flags2 = ((u32b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: flags3 of class  artifact_type */
static int toluaI_get_object_artifact_type_flags3(lua_State* tolua_S)
{
  artifact_type* self = (artifact_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->flags3);
 return 1;
}

/* set function: flags3 of class  artifact_type */
static int toluaI_set_object_artifact_type_flags3(lua_State* tolua_S)
{
  artifact_type* self = (artifact_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->flags3 = ((u32b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: flags4 of class  artifact_type */
static int toluaI_get_object_artifact_type_flags4(lua_State* tolua_S)
{
  artifact_type* self = (artifact_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->flags4);
 return 1;
}

/* set function: flags4 of class  artifact_type */
static int toluaI_set_object_artifact_type_flags4(lua_State* tolua_S)
{
  artifact_type* self = (artifact_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->flags4 = ((u32b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: flags5 of class  artifact_type */
static int toluaI_get_object_artifact_type_flags5(lua_State* tolua_S)
{
  artifact_type* self = (artifact_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->flags5);
 return 1;
}

/* set function: flags5 of class  artifact_type */
static int toluaI_set_object_artifact_type_flags5(lua_State* tolua_S)
{
  artifact_type* self = (artifact_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->flags5 = ((u32b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: level of class  artifact_type */
static int toluaI_get_object_artifact_type_level(lua_State* tolua_S)
{
  artifact_type* self = (artifact_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->level);
 return 1;
}

/* set function: level of class  artifact_type */
static int toluaI_set_object_artifact_type_level(lua_State* tolua_S)
{
  artifact_type* self = (artifact_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->level = ((byte)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: rarity of class  artifact_type */
static int toluaI_get_object_artifact_type_rarity(lua_State* tolua_S)
{
  artifact_type* self = (artifact_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->rarity);
 return 1;
}

/* set function: rarity of class  artifact_type */
static int toluaI_set_object_artifact_type_rarity(lua_State* tolua_S)
{
  artifact_type* self = (artifact_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->rarity = ((byte)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: cur_num of class  artifact_type */
static int toluaI_get_object_artifact_type_cur_num(lua_State* tolua_S)
{
  artifact_type* self = (artifact_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->cur_num);
 return 1;
}

/* set function: cur_num of class  artifact_type */
static int toluaI_set_object_artifact_type_cur_num(lua_State* tolua_S)
{
  artifact_type* self = (artifact_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->cur_num = ((byte)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: max_num of class  artifact_type */
static int toluaI_get_object_artifact_type_max_num(lua_State* tolua_S)
{
  artifact_type* self = (artifact_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->max_num);
 return 1;
}

/* set function: max_num of class  artifact_type */
static int toluaI_set_object_artifact_type_max_num(lua_State* tolua_S)
{
  artifact_type* self = (artifact_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->max_num = ((byte)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: esp of class  artifact_type */
static int toluaI_get_object_artifact_type_esp(lua_State* tolua_S)
{
  artifact_type* self = (artifact_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->esp);
 return 1;
}

/* set function: esp of class  artifact_type */
static int toluaI_set_object_artifact_type_esp(lua_State* tolua_S)
{
  artifact_type* self = (artifact_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->esp = ((u32b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: power of class  artifact_type */
static int toluaI_get_object_artifact_type_power(lua_State* tolua_S)
{
  artifact_type* self = (artifact_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->power);
 return 1;
}

/* set function: power of class  artifact_type */
static int toluaI_set_object_artifact_type_power(lua_State* tolua_S)
{
  artifact_type* self = (artifact_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->power = ((s16b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: name of class  ego_item_type */
static int toluaI_get_object_ego_item_type_name(lua_State* tolua_S)
{
  ego_item_type* self = (ego_item_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->name);
 return 1;
}

/* set function: name of class  ego_item_type */
static int toluaI_set_object_ego_item_type_name(lua_State* tolua_S)
{
  ego_item_type* self = (ego_item_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->name = ((u32b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: text of class  ego_item_type */
static int toluaI_get_object_ego_item_type_text(lua_State* tolua_S)
{
  ego_item_type* self = (ego_item_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->text);
 return 1;
}

/* set function: text of class  ego_item_type */
static int toluaI_set_object_ego_item_type_text(lua_State* tolua_S)
{
  ego_item_type* self = (ego_item_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->text = ((u32b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: before of class  ego_item_type */
static int toluaI_get_object_ego_item_type_before(lua_State* tolua_S)
{
  ego_item_type* self = (ego_item_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->before);
 return 1;
}

/* set function: before of class  ego_item_type */
static int toluaI_set_object_ego_item_type_before(lua_State* tolua_S)
{
  ego_item_type* self = (ego_item_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->before = ((bool)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: tval of class  ego_item_type */
static int toluaI_get_object_ego_item_type_tval(lua_State* tolua_S)
{
 int toluaI_index;
  ego_item_type* self;
 lua_pushstring(tolua_S,".self");
 lua_rawget(tolua_S,1);
 self = (ego_item_type*)  lua_touserdata(tolua_S,-1);
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 tolua_error(tolua_S,"invalid type in array indexing.");
 toluaI_index = (int)tolua_getnumber(tolua_S,2,0)-1;
 if (toluaI_index<0 || toluaI_index>=6)
 tolua_error(tolua_S,"array indexing out of range.");
 tolua_pushnumber(tolua_S,(long)self->tval[toluaI_index]);
 return 1;
}

/* set function: tval of class  ego_item_type */
static int toluaI_set_object_ego_item_type_tval(lua_State* tolua_S)
{
 int toluaI_index;
  ego_item_type* self;
 lua_pushstring(tolua_S,".self");
 lua_rawget(tolua_S,1);
 self = (ego_item_type*)  lua_touserdata(tolua_S,-1);
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 tolua_error(tolua_S,"invalid type in array indexing.");
 toluaI_index = (int)tolua_getnumber(tolua_S,2,0)-1;
 if (toluaI_index<0 || toluaI_index>=6)
 tolua_error(tolua_S,"array indexing out of range.");
  self->tval[toluaI_index] = ((byte)  tolua_getnumber(tolua_S,3,0));
 return 0;
}

/* get function: min_sval of class  ego_item_type */
static int toluaI_get_object_ego_item_type_min_sval(lua_State* tolua_S)
{
 int toluaI_index;
  ego_item_type* self;
 lua_pushstring(tolua_S,".self");
 lua_rawget(tolua_S,1);
 self = (ego_item_type*)  lua_touserdata(tolua_S,-1);
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 tolua_error(tolua_S,"invalid type in array indexing.");
 toluaI_index = (int)tolua_getnumber(tolua_S,2,0)-1;
 if (toluaI_index<0 || toluaI_index>=6)
 tolua_error(tolua_S,"array indexing out of range.");
 tolua_pushnumber(tolua_S,(long)self->min_sval[toluaI_index]);
 return 1;
}

/* set function: min_sval of class  ego_item_type */
static int toluaI_set_object_ego_item_type_min_sval(lua_State* tolua_S)
{
 int toluaI_index;
  ego_item_type* self;
 lua_pushstring(tolua_S,".self");
 lua_rawget(tolua_S,1);
 self = (ego_item_type*)  lua_touserdata(tolua_S,-1);
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 tolua_error(tolua_S,"invalid type in array indexing.");
 toluaI_index = (int)tolua_getnumber(tolua_S,2,0)-1;
 if (toluaI_index<0 || toluaI_index>=6)
 tolua_error(tolua_S,"array indexing out of range.");
  self->min_sval[toluaI_index] = ((byte)  tolua_getnumber(tolua_S,3,0));
 return 0;
}

/* get function: max_sval of class  ego_item_type */
static int toluaI_get_object_ego_item_type_max_sval(lua_State* tolua_S)
{
 int toluaI_index;
  ego_item_type* self;
 lua_pushstring(tolua_S,".self");
 lua_rawget(tolua_S,1);
 self = (ego_item_type*)  lua_touserdata(tolua_S,-1);
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 tolua_error(tolua_S,"invalid type in array indexing.");
 toluaI_index = (int)tolua_getnumber(tolua_S,2,0)-1;
 if (toluaI_index<0 || toluaI_index>=6)
 tolua_error(tolua_S,"array indexing out of range.");
 tolua_pushnumber(tolua_S,(long)self->max_sval[toluaI_index]);
 return 1;
}

/* set function: max_sval of class  ego_item_type */
static int toluaI_set_object_ego_item_type_max_sval(lua_State* tolua_S)
{
 int toluaI_index;
  ego_item_type* self;
 lua_pushstring(tolua_S,".self");
 lua_rawget(tolua_S,1);
 self = (ego_item_type*)  lua_touserdata(tolua_S,-1);
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 tolua_error(tolua_S,"invalid type in array indexing.");
 toluaI_index = (int)tolua_getnumber(tolua_S,2,0)-1;
 if (toluaI_index<0 || toluaI_index>=6)
 tolua_error(tolua_S,"array indexing out of range.");
  self->max_sval[toluaI_index] = ((byte)  tolua_getnumber(tolua_S,3,0));
 return 0;
}

/* get function: rating of class  ego_item_type */
static int toluaI_get_object_ego_item_type_rating(lua_State* tolua_S)
{
  ego_item_type* self = (ego_item_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->rating);
 return 1;
}

/* set function: rating of class  ego_item_type */
static int toluaI_set_object_ego_item_type_rating(lua_State* tolua_S)
{
  ego_item_type* self = (ego_item_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->rating = ((byte)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: level of class  ego_item_type */
static int toluaI_get_object_ego_item_type_level(lua_State* tolua_S)
{
  ego_item_type* self = (ego_item_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->level);
 return 1;
}

/* set function: level of class  ego_item_type */
static int toluaI_set_object_ego_item_type_level(lua_State* tolua_S)
{
  ego_item_type* self = (ego_item_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->level = ((byte)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: rarity of class  ego_item_type */
static int toluaI_get_object_ego_item_type_rarity(lua_State* tolua_S)
{
  ego_item_type* self = (ego_item_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->rarity);
 return 1;
}

/* set function: rarity of class  ego_item_type */
static int toluaI_set_object_ego_item_type_rarity(lua_State* tolua_S)
{
  ego_item_type* self = (ego_item_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->rarity = ((byte)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: mrarity of class  ego_item_type */
static int toluaI_get_object_ego_item_type_mrarity(lua_State* tolua_S)
{
  ego_item_type* self = (ego_item_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->mrarity);
 return 1;
}

/* set function: mrarity of class  ego_item_type */
static int toluaI_set_object_ego_item_type_mrarity(lua_State* tolua_S)
{
  ego_item_type* self = (ego_item_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->mrarity = ((byte)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: max_to_h of class  ego_item_type */
static int toluaI_get_object_ego_item_type_max_to_h(lua_State* tolua_S)
{
  ego_item_type* self = (ego_item_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->max_to_h);
 return 1;
}

/* set function: max_to_h of class  ego_item_type */
static int toluaI_set_object_ego_item_type_max_to_h(lua_State* tolua_S)
{
  ego_item_type* self = (ego_item_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->max_to_h = ((s16b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: max_to_d of class  ego_item_type */
static int toluaI_get_object_ego_item_type_max_to_d(lua_State* tolua_S)
{
  ego_item_type* self = (ego_item_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->max_to_d);
 return 1;
}

/* set function: max_to_d of class  ego_item_type */
static int toluaI_set_object_ego_item_type_max_to_d(lua_State* tolua_S)
{
  ego_item_type* self = (ego_item_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->max_to_d = ((s16b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: max_to_a of class  ego_item_type */
static int toluaI_get_object_ego_item_type_max_to_a(lua_State* tolua_S)
{
  ego_item_type* self = (ego_item_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->max_to_a);
 return 1;
}

/* set function: max_to_a of class  ego_item_type */
static int toluaI_set_object_ego_item_type_max_to_a(lua_State* tolua_S)
{
  ego_item_type* self = (ego_item_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->max_to_a = ((s16b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: max_pval of class  ego_item_type */
static int toluaI_get_object_ego_item_type_max_pval(lua_State* tolua_S)
{
  ego_item_type* self = (ego_item_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->max_pval);
 return 1;
}

/* set function: max_pval of class  ego_item_type */
static int toluaI_set_object_ego_item_type_max_pval(lua_State* tolua_S)
{
  ego_item_type* self = (ego_item_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->max_pval = ((s32b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: cost of class  ego_item_type */
static int toluaI_get_object_ego_item_type_cost(lua_State* tolua_S)
{
  ego_item_type* self = (ego_item_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->cost);
 return 1;
}

/* set function: cost of class  ego_item_type */
static int toluaI_set_object_ego_item_type_cost(lua_State* tolua_S)
{
  ego_item_type* self = (ego_item_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->cost = ((s32b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: rar of class  ego_item_type */
static int toluaI_get_object_ego_item_type_rar(lua_State* tolua_S)
{
 int toluaI_index;
  ego_item_type* self;
 lua_pushstring(tolua_S,".self");
 lua_rawget(tolua_S,1);
 self = (ego_item_type*)  lua_touserdata(tolua_S,-1);
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 tolua_error(tolua_S,"invalid type in array indexing.");
 toluaI_index = (int)tolua_getnumber(tolua_S,2,0)-1;
 if (toluaI_index<0 || toluaI_index>=5)
 tolua_error(tolua_S,"array indexing out of range.");
 tolua_pushnumber(tolua_S,(long)self->rar[toluaI_index]);
 return 1;
}

/* set function: rar of class  ego_item_type */
static int toluaI_set_object_ego_item_type_rar(lua_State* tolua_S)
{
 int toluaI_index;
  ego_item_type* self;
 lua_pushstring(tolua_S,".self");
 lua_rawget(tolua_S,1);
 self = (ego_item_type*)  lua_touserdata(tolua_S,-1);
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 tolua_error(tolua_S,"invalid type in array indexing.");
 toluaI_index = (int)tolua_getnumber(tolua_S,2,0)-1;
 if (toluaI_index<0 || toluaI_index>=5)
 tolua_error(tolua_S,"array indexing out of range.");
  self->rar[toluaI_index] = ((byte)  tolua_getnumber(tolua_S,3,0));
 return 0;
}

/* get function: flags1 of class  ego_item_type */
static int toluaI_get_object_ego_item_type_flags1(lua_State* tolua_S)
{
 int toluaI_index;
  ego_item_type* self;
 lua_pushstring(tolua_S,".self");
 lua_rawget(tolua_S,1);
 self = (ego_item_type*)  lua_touserdata(tolua_S,-1);
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 tolua_error(tolua_S,"invalid type in array indexing.");
 toluaI_index = (int)tolua_getnumber(tolua_S,2,0)-1;
 if (toluaI_index<0 || toluaI_index>=5)
 tolua_error(tolua_S,"array indexing out of range.");
 tolua_pushnumber(tolua_S,(long)self->flags1[toluaI_index]);
 return 1;
}

/* set function: flags1 of class  ego_item_type */
static int toluaI_set_object_ego_item_type_flags1(lua_State* tolua_S)
{
 int toluaI_index;
  ego_item_type* self;
 lua_pushstring(tolua_S,".self");
 lua_rawget(tolua_S,1);
 self = (ego_item_type*)  lua_touserdata(tolua_S,-1);
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 tolua_error(tolua_S,"invalid type in array indexing.");
 toluaI_index = (int)tolua_getnumber(tolua_S,2,0)-1;
 if (toluaI_index<0 || toluaI_index>=5)
 tolua_error(tolua_S,"array indexing out of range.");
  self->flags1[toluaI_index] = ((u32b)  tolua_getnumber(tolua_S,3,0));
 return 0;
}

/* get function: flags2 of class  ego_item_type */
static int toluaI_get_object_ego_item_type_flags2(lua_State* tolua_S)
{
 int toluaI_index;
  ego_item_type* self;
 lua_pushstring(tolua_S,".self");
 lua_rawget(tolua_S,1);
 self = (ego_item_type*)  lua_touserdata(tolua_S,-1);
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 tolua_error(tolua_S,"invalid type in array indexing.");
 toluaI_index = (int)tolua_getnumber(tolua_S,2,0)-1;
 if (toluaI_index<0 || toluaI_index>=5)
 tolua_error(tolua_S,"array indexing out of range.");
 tolua_pushnumber(tolua_S,(long)self->flags2[toluaI_index]);
 return 1;
}

/* set function: flags2 of class  ego_item_type */
static int toluaI_set_object_ego_item_type_flags2(lua_State* tolua_S)
{
 int toluaI_index;
  ego_item_type* self;
 lua_pushstring(tolua_S,".self");
 lua_rawget(tolua_S,1);
 self = (ego_item_type*)  lua_touserdata(tolua_S,-1);
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 tolua_error(tolua_S,"invalid type in array indexing.");
 toluaI_index = (int)tolua_getnumber(tolua_S,2,0)-1;
 if (toluaI_index<0 || toluaI_index>=5)
 tolua_error(tolua_S,"array indexing out of range.");
  self->flags2[toluaI_index] = ((u32b)  tolua_getnumber(tolua_S,3,0));
 return 0;
}

/* get function: flags3 of class  ego_item_type */
static int toluaI_get_object_ego_item_type_flags3(lua_State* tolua_S)
{
 int toluaI_index;
  ego_item_type* self;
 lua_pushstring(tolua_S,".self");
 lua_rawget(tolua_S,1);
 self = (ego_item_type*)  lua_touserdata(tolua_S,-1);
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 tolua_error(tolua_S,"invalid type in array indexing.");
 toluaI_index = (int)tolua_getnumber(tolua_S,2,0)-1;
 if (toluaI_index<0 || toluaI_index>=5)
 tolua_error(tolua_S,"array indexing out of range.");
 tolua_pushnumber(tolua_S,(long)self->flags3[toluaI_index]);
 return 1;
}

/* set function: flags3 of class  ego_item_type */
static int toluaI_set_object_ego_item_type_flags3(lua_State* tolua_S)
{
 int toluaI_index;
  ego_item_type* self;
 lua_pushstring(tolua_S,".self");
 lua_rawget(tolua_S,1);
 self = (ego_item_type*)  lua_touserdata(tolua_S,-1);
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 tolua_error(tolua_S,"invalid type in array indexing.");
 toluaI_index = (int)tolua_getnumber(tolua_S,2,0)-1;
 if (toluaI_index<0 || toluaI_index>=5)
 tolua_error(tolua_S,"array indexing out of range.");
  self->flags3[toluaI_index] = ((u32b)  tolua_getnumber(tolua_S,3,0));
 return 0;
}

/* get function: flags4 of class  ego_item_type */
static int toluaI_get_object_ego_item_type_flags4(lua_State* tolua_S)
{
 int toluaI_index;
  ego_item_type* self;
 lua_pushstring(tolua_S,".self");
 lua_rawget(tolua_S,1);
 self = (ego_item_type*)  lua_touserdata(tolua_S,-1);
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 tolua_error(tolua_S,"invalid type in array indexing.");
 toluaI_index = (int)tolua_getnumber(tolua_S,2,0)-1;
 if (toluaI_index<0 || toluaI_index>=5)
 tolua_error(tolua_S,"array indexing out of range.");
 tolua_pushnumber(tolua_S,(long)self->flags4[toluaI_index]);
 return 1;
}

/* set function: flags4 of class  ego_item_type */
static int toluaI_set_object_ego_item_type_flags4(lua_State* tolua_S)
{
 int toluaI_index;
  ego_item_type* self;
 lua_pushstring(tolua_S,".self");
 lua_rawget(tolua_S,1);
 self = (ego_item_type*)  lua_touserdata(tolua_S,-1);
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 tolua_error(tolua_S,"invalid type in array indexing.");
 toluaI_index = (int)tolua_getnumber(tolua_S,2,0)-1;
 if (toluaI_index<0 || toluaI_index>=5)
 tolua_error(tolua_S,"array indexing out of range.");
  self->flags4[toluaI_index] = ((u32b)  tolua_getnumber(tolua_S,3,0));
 return 0;
}

/* get function: flags5 of class  ego_item_type */
static int toluaI_get_object_ego_item_type_flags5(lua_State* tolua_S)
{
 int toluaI_index;
  ego_item_type* self;
 lua_pushstring(tolua_S,".self");
 lua_rawget(tolua_S,1);
 self = (ego_item_type*)  lua_touserdata(tolua_S,-1);
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 tolua_error(tolua_S,"invalid type in array indexing.");
 toluaI_index = (int)tolua_getnumber(tolua_S,2,0)-1;
 if (toluaI_index<0 || toluaI_index>=5)
 tolua_error(tolua_S,"array indexing out of range.");
 tolua_pushnumber(tolua_S,(long)self->flags5[toluaI_index]);
 return 1;
}

/* set function: flags5 of class  ego_item_type */
static int toluaI_set_object_ego_item_type_flags5(lua_State* tolua_S)
{
 int toluaI_index;
  ego_item_type* self;
 lua_pushstring(tolua_S,".self");
 lua_rawget(tolua_S,1);
 self = (ego_item_type*)  lua_touserdata(tolua_S,-1);
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 tolua_error(tolua_S,"invalid type in array indexing.");
 toluaI_index = (int)tolua_getnumber(tolua_S,2,0)-1;
 if (toluaI_index<0 || toluaI_index>=5)
 tolua_error(tolua_S,"array indexing out of range.");
  self->flags5[toluaI_index] = ((u32b)  tolua_getnumber(tolua_S,3,0));
 return 0;
}

/* get function: esp of class  ego_item_type */
static int toluaI_get_object_ego_item_type_esp(lua_State* tolua_S)
{
 int toluaI_index;
  ego_item_type* self;
 lua_pushstring(tolua_S,".self");
 lua_rawget(tolua_S,1);
 self = (ego_item_type*)  lua_touserdata(tolua_S,-1);
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 tolua_error(tolua_S,"invalid type in array indexing.");
 toluaI_index = (int)tolua_getnumber(tolua_S,2,0)-1;
 if (toluaI_index<0 || toluaI_index>=5)
 tolua_error(tolua_S,"array indexing out of range.");
 tolua_pushnumber(tolua_S,(long)self->esp[toluaI_index]);
 return 1;
}

/* set function: esp of class  ego_item_type */
static int toluaI_set_object_ego_item_type_esp(lua_State* tolua_S)
{
 int toluaI_index;
  ego_item_type* self;
 lua_pushstring(tolua_S,".self");
 lua_rawget(tolua_S,1);
 self = (ego_item_type*)  lua_touserdata(tolua_S,-1);
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 tolua_error(tolua_S,"invalid type in array indexing.");
 toluaI_index = (int)tolua_getnumber(tolua_S,2,0)-1;
 if (toluaI_index<0 || toluaI_index>=5)
 tolua_error(tolua_S,"array indexing out of range.");
  self->esp[toluaI_index] = ((u32b)  tolua_getnumber(tolua_S,3,0));
 return 0;
}

/* get function: fego of class  ego_item_type */
static int toluaI_get_object_ego_item_type_fego(lua_State* tolua_S)
{
 int toluaI_index;
  ego_item_type* self;
 lua_pushstring(tolua_S,".self");
 lua_rawget(tolua_S,1);
 self = (ego_item_type*)  lua_touserdata(tolua_S,-1);
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 tolua_error(tolua_S,"invalid type in array indexing.");
 toluaI_index = (int)tolua_getnumber(tolua_S,2,0)-1;
 if (toluaI_index<0 || toluaI_index>=5)
 tolua_error(tolua_S,"array indexing out of range.");
 tolua_pushnumber(tolua_S,(long)self->fego[toluaI_index]);
 return 1;
}

/* set function: fego of class  ego_item_type */
static int toluaI_set_object_ego_item_type_fego(lua_State* tolua_S)
{
 int toluaI_index;
  ego_item_type* self;
 lua_pushstring(tolua_S,".self");
 lua_rawget(tolua_S,1);
 self = (ego_item_type*)  lua_touserdata(tolua_S,-1);
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 tolua_error(tolua_S,"invalid type in array indexing.");
 toluaI_index = (int)tolua_getnumber(tolua_S,2,0)-1;
 if (toluaI_index<0 || toluaI_index>=5)
 tolua_error(tolua_S,"array indexing out of range.");
  self->fego[toluaI_index] = ((u32b)  tolua_getnumber(tolua_S,3,0));
 return 0;
}

/* get function: power of class  ego_item_type */
static int toluaI_get_object_ego_item_type_power(lua_State* tolua_S)
{
  ego_item_type* self = (ego_item_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->power);
 return 1;
}

/* set function: power of class  ego_item_type */
static int toluaI_set_object_ego_item_type_power(lua_State* tolua_S)
{
  ego_item_type* self = (ego_item_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->power = ((s16b)  tolua_getnumber(tolua_S,2,0));
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
  self->iy = ((byte)  tolua_getnumber(tolua_S,2,0));
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
  self->ix = ((byte)  tolua_getnumber(tolua_S,2,0));
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
  self->pval = ((s32b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: pval2 of class  object_type */
static int toluaI_get_object_object_type_pval2(lua_State* tolua_S)
{
  object_type* self = (object_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->pval2);
 return 1;
}

/* set function: pval2 of class  object_type */
static int toluaI_set_object_object_type_pval2(lua_State* tolua_S)
{
  object_type* self = (object_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->pval2 = ((s16b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: pval3 of class  object_type */
static int toluaI_get_object_object_type_pval3(lua_State* tolua_S)
{
  object_type* self = (object_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->pval3);
 return 1;
}

/* set function: pval3 of class  object_type */
static int toluaI_set_object_object_type_pval3(lua_State* tolua_S)
{
  object_type* self = (object_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->pval3 = ((s32b)  tolua_getnumber(tolua_S,2,0));
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
  self->weight = ((s32b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: elevel of class  object_type */
static int toluaI_get_object_object_type_elevel(lua_State* tolua_S)
{
  object_type* self = (object_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->elevel);
 return 1;
}

/* set function: elevel of class  object_type */
static int toluaI_set_object_object_type_elevel(lua_State* tolua_S)
{
  object_type* self = (object_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->elevel = ((byte)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: exp of class  object_type */
static int toluaI_get_object_object_type_exp(lua_State* tolua_S)
{
  object_type* self = (object_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->exp);
 return 1;
}

/* set function: exp of class  object_type */
static int toluaI_set_object_object_type_exp(lua_State* tolua_S)
{
  object_type* self = (object_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->exp = ((s32b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: name1 of class  object_type */
static int toluaI_get_object_object_type_name1(lua_State* tolua_S)
{
  object_type* self = (object_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->name1);
 return 1;
}

/* set function: name1 of class  object_type */
static int toluaI_set_object_object_type_name1(lua_State* tolua_S)
{
  object_type* self = (object_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->name1 = ((byte)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: name2 of class  object_type */
static int toluaI_get_object_object_type_name2(lua_State* tolua_S)
{
  object_type* self = (object_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->name2);
 return 1;
}

/* set function: name2 of class  object_type */
static int toluaI_set_object_object_type_name2(lua_State* tolua_S)
{
  object_type* self = (object_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->name2 = ((s16b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: name2b of class  object_type */
static int toluaI_get_object_object_type_name2b(lua_State* tolua_S)
{
  object_type* self = (object_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->name2b);
 return 1;
}

/* set function: name2b of class  object_type */
static int toluaI_set_object_object_type_name2b(lua_State* tolua_S)
{
  object_type* self = (object_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->name2b = ((s16b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: xtra1 of class  object_type */
static int toluaI_get_object_object_type_xtra1(lua_State* tolua_S)
{
  object_type* self = (object_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->xtra1);
 return 1;
}

/* set function: xtra1 of class  object_type */
static int toluaI_set_object_object_type_xtra1(lua_State* tolua_S)
{
  object_type* self = (object_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->xtra1 = ((byte)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: xtra2 of class  object_type */
static int toluaI_get_object_object_type_xtra2(lua_State* tolua_S)
{
  object_type* self = (object_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->xtra2);
 return 1;
}

/* set function: xtra2 of class  object_type */
static int toluaI_set_object_object_type_xtra2(lua_State* tolua_S)
{
  object_type* self = (object_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->xtra2 = ((s16b)  tolua_getnumber(tolua_S,2,0));
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

/* get function: ident of class  object_type */
static int toluaI_get_object_object_type_ident(lua_State* tolua_S)
{
  object_type* self = (object_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->ident);
 return 1;
}

/* set function: ident of class  object_type */
static int toluaI_set_object_object_type_ident(lua_State* tolua_S)
{
  object_type* self = (object_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->ident = ((byte)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: marked of class  object_type */
static int toluaI_get_object_object_type_marked(lua_State* tolua_S)
{
  object_type* self = (object_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->marked);
 return 1;
}

/* set function: marked of class  object_type */
static int toluaI_set_object_object_type_marked(lua_State* tolua_S)
{
  object_type* self = (object_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->marked = ((byte)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: note of class  object_type */
static int toluaI_get_object_object_type_note(lua_State* tolua_S)
{
  object_type* self = (object_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->note);
 return 1;
}

/* set function: note of class  object_type */
static int toluaI_set_object_object_type_note(lua_State* tolua_S)
{
  object_type* self = (object_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->note = ((u16b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: art_name of class  object_type */
static int toluaI_get_object_object_type_art_name(lua_State* tolua_S)
{
  object_type* self = (object_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->art_name);
 return 1;
}

/* set function: art_name of class  object_type */
static int toluaI_set_object_object_type_art_name(lua_State* tolua_S)
{
  object_type* self = (object_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->art_name = ((u16b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: art_flags1 of class  object_type */
static int toluaI_get_object_object_type_art_flags1(lua_State* tolua_S)
{
  object_type* self = (object_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->art_flags1);
 return 1;
}

/* set function: art_flags1 of class  object_type */
static int toluaI_set_object_object_type_art_flags1(lua_State* tolua_S)
{
  object_type* self = (object_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->art_flags1 = ((u32b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: art_flags2 of class  object_type */
static int toluaI_get_object_object_type_art_flags2(lua_State* tolua_S)
{
  object_type* self = (object_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->art_flags2);
 return 1;
}

/* set function: art_flags2 of class  object_type */
static int toluaI_set_object_object_type_art_flags2(lua_State* tolua_S)
{
  object_type* self = (object_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->art_flags2 = ((u32b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: art_flags3 of class  object_type */
static int toluaI_get_object_object_type_art_flags3(lua_State* tolua_S)
{
  object_type* self = (object_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->art_flags3);
 return 1;
}

/* set function: art_flags3 of class  object_type */
static int toluaI_set_object_object_type_art_flags3(lua_State* tolua_S)
{
  object_type* self = (object_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->art_flags3 = ((u32b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: art_flags4 of class  object_type */
static int toluaI_get_object_object_type_art_flags4(lua_State* tolua_S)
{
  object_type* self = (object_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->art_flags4);
 return 1;
}

/* set function: art_flags4 of class  object_type */
static int toluaI_set_object_object_type_art_flags4(lua_State* tolua_S)
{
  object_type* self = (object_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->art_flags4 = ((u32b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: art_flags5 of class  object_type */
static int toluaI_get_object_object_type_art_flags5(lua_State* tolua_S)
{
  object_type* self = (object_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->art_flags5);
 return 1;
}

/* set function: art_flags5 of class  object_type */
static int toluaI_set_object_object_type_art_flags5(lua_State* tolua_S)
{
  object_type* self = (object_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->art_flags5 = ((u32b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: art_esp of class  object_type */
static int toluaI_get_object_object_type_art_esp(lua_State* tolua_S)
{
  object_type* self = (object_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->art_esp);
 return 1;
}

/* set function: art_esp of class  object_type */
static int toluaI_set_object_object_type_art_esp(lua_State* tolua_S)
{
  object_type* self = (object_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->art_esp = ((u32b)  tolua_getnumber(tolua_S,2,0));
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

/* get function: held_m_idx of class  object_type */
static int toluaI_get_object_object_type_held_m_idx(lua_State* tolua_S)
{
  object_type* self = (object_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->held_m_idx);
 return 1;
}

/* set function: held_m_idx of class  object_type */
static int toluaI_set_object_object_type_held_m_idx(lua_State* tolua_S)
{
  object_type* self = (object_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->held_m_idx = ((s16b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: sense of class  object_type */
static int toluaI_get_object_object_type_sense(lua_State* tolua_S)
{
  object_type* self = (object_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->sense);
 return 1;
}

/* set function: sense of class  object_type */
static int toluaI_set_object_object_type_sense(lua_State* tolua_S)
{
  object_type* self = (object_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->sense = ((byte)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: found of class  object_type */
static int toluaI_get_object_object_type_found(lua_State* tolua_S)
{
  object_type* self = (object_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->found);
 return 1;
}

/* set function: found of class  object_type */
static int toluaI_set_object_object_type_found(lua_State* tolua_S)
{
  object_type* self = (object_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->found = ((byte)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: found_aux1 of class  object_type */
static int toluaI_get_object_object_type_found_aux1(lua_State* tolua_S)
{
  object_type* self = (object_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->found_aux1);
 return 1;
}

/* set function: found_aux1 of class  object_type */
static int toluaI_set_object_object_type_found_aux1(lua_State* tolua_S)
{
  object_type* self = (object_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->found_aux1 = ((s16b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: found_aux2 of class  object_type */
static int toluaI_get_object_object_type_found_aux2(lua_State* tolua_S)
{
  object_type* self = (object_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->found_aux2);
 return 1;
}

/* set function: found_aux2 of class  object_type */
static int toluaI_set_object_object_type_found_aux2(lua_State* tolua_S)
{
  object_type* self = (object_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->found_aux2 = ((s16b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: o_list */
static int toluaI_get_object_o_list(lua_State* tolua_S)
{
 int toluaI_index;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 tolua_error(tolua_S,"invalid type in array indexing.");
 toluaI_index = (int)tolua_getnumber(tolua_S,2,0)-1;
 if (toluaI_index<0 || toluaI_index>=max_o_idx)
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
 if (toluaI_index<0 || toluaI_index>=max_o_idx)
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
 if (toluaI_index<0 || toluaI_index>=max_k_idx)
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
 if (toluaI_index<0 || toluaI_index>=max_k_idx)
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

/* get function: a_info */
static int toluaI_get_object_a_info(lua_State* tolua_S)
{
 int toluaI_index;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 tolua_error(tolua_S,"invalid type in array indexing.");
 toluaI_index = (int)tolua_getnumber(tolua_S,2,0)-1;
 if (toluaI_index<0 || toluaI_index>=max_a_idx)
 tolua_error(tolua_S,"array indexing out of range.");
 tolua_pushusertype(tolua_S,(void*)&a_info[toluaI_index],tolua_tag(tolua_S,"artifact_type"));
 return 1;
}

/* set function: a_info */
static int toluaI_set_object_a_info(lua_State* tolua_S)
{
 int toluaI_index;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 tolua_error(tolua_S,"invalid type in array indexing.");
 toluaI_index = (int)tolua_getnumber(tolua_S,2,0)-1;
 if (toluaI_index<0 || toluaI_index>=max_a_idx)
 tolua_error(tolua_S,"array indexing out of range.");
  a_info[toluaI_index] = *((artifact_type*)  tolua_getusertype(tolua_S,3,0));
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

/* get function: e_head */
static int toluaI_get_object_e_head(lua_State* tolua_S)
{
 tolua_pushusertype(tolua_S,(void*)e_head,tolua_tag(tolua_S,"header"));
 return 1;
}

/* set function: e_head */
static int toluaI_set_object_e_head(lua_State* tolua_S)
{
 if (!tolua_istype(tolua_S,1,tolua_tag(tolua_S,"header"),0))
 TOLUA_ERR_ASSIGN;
  e_head = ((header*)  tolua_getusertype(tolua_S,1,0));
 return 0;
}

/* get function: e_info */
static int toluaI_get_object_e_info(lua_State* tolua_S)
{
 int toluaI_index;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 tolua_error(tolua_S,"invalid type in array indexing.");
 toluaI_index = (int)tolua_getnumber(tolua_S,2,0)-1;
 if (toluaI_index<0 || toluaI_index>=max_e_idx)
 tolua_error(tolua_S,"array indexing out of range.");
 tolua_pushusertype(tolua_S,(void*)&e_info[toluaI_index],tolua_tag(tolua_S,"ego_item_type"));
 return 1;
}

/* set function: e_info */
static int toluaI_set_object_e_info(lua_State* tolua_S)
{
 int toluaI_index;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 tolua_error(tolua_S,"invalid type in array indexing.");
 toluaI_index = (int)tolua_getnumber(tolua_S,2,0)-1;
 if (toluaI_index<0 || toluaI_index>=max_e_idx)
 tolua_error(tolua_S,"array indexing out of range.");
  e_info[toluaI_index] = *((ego_item_type*)  tolua_getusertype(tolua_S,3,0));
 return 0;
}

/* get function: e_name */
static int toluaI_get_object_e_name(lua_State* tolua_S)
{
 tolua_pushstring(tolua_S,(const char*)e_name);
 return 1;
}

/* set function: e_name */
static int toluaI_set_object_e_name(lua_State* tolua_S)
{
 if (!tolua_istype(tolua_S,1,LUA_TSTRING,0))
 TOLUA_ERR_ASSIGN;
  e_name = ((char*)  tolua_getstring(tolua_S,1,0));
 return 0;
}

/* get function: e_text */
static int toluaI_get_object_e_text(lua_State* tolua_S)
{
 tolua_pushstring(tolua_S,(const char*)e_text);
 return 1;
}

/* set function: e_text */
static int toluaI_set_object_e_text(lua_State* tolua_S)
{
 if (!tolua_istype(tolua_S,1,LUA_TSTRING,0))
 TOLUA_ERR_ASSIGN;
  e_text = ((char*)  tolua_getstring(tolua_S,1,0));
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
 goto tolua_lerror;
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
tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'm_bonus'.");
 return 0;
}

/* function: wield_slot_ideal */
static int toluaI_object_wield_slot_ideal00(lua_State* tolua_S)
{
 if (
 !tolua_istype(tolua_S,1,tolua_tag(tolua_S,"object_type"),0) ||
 !tolua_istype(tolua_S,2,LUA_TNUMBER,0) ||
 !tolua_isnoobj(tolua_S,3)
 )
 goto tolua_lerror;
 else
 {
  object_type* o_ptr = ((object_type*)  tolua_getusertype(tolua_S,1,0));
  bool ideal = ((bool)  tolua_getnumber(tolua_S,2,0));
 {
  s16b toluaI_ret = (s16b)  wield_slot_ideal(o_ptr,ideal);
 tolua_pushnumber(tolua_S,(long)toluaI_ret);
 }
 }
 return 1;
tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'wield_slot_ideal'.");
 return 0;
}

/* function: wield_slot */
static int toluaI_object_wield_slot00(lua_State* tolua_S)
{
 if (
 !tolua_istype(tolua_S,1,tolua_tag(tolua_S,"object_type"),0) ||
 !tolua_isnoobj(tolua_S,2)
 )
 goto tolua_lerror;
 else
 {
  object_type* o_ptr = ((object_type*)  tolua_getusertype(tolua_S,1,0));
 {
  s16b toluaI_ret = (s16b)  wield_slot(o_ptr);
 tolua_pushnumber(tolua_S,(long)toluaI_ret);
 }
 }
 return 1;
tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'wield_slot'.");
 return 0;
}

/* function: object_flags */
static int toluaI_object_object_flags00(lua_State* tolua_S)
{
 if (
 !tolua_istype(tolua_S,1,tolua_tag(tolua_S,"object_type"),0) ||
 !tolua_istype(tolua_S,2,LUA_TNUMBER,1) ||
 !tolua_istype(tolua_S,3,LUA_TNUMBER,1) ||
 !tolua_istype(tolua_S,4,LUA_TNUMBER,1) ||
 !tolua_istype(tolua_S,5,LUA_TNUMBER,1) ||
 !tolua_istype(tolua_S,6,LUA_TNUMBER,1) ||
 !tolua_istype(tolua_S,7,LUA_TNUMBER,1) ||
 !tolua_isnoobj(tolua_S,8)
 )
 goto tolua_lerror;
 else
 {
  object_type* o_ptr = ((object_type*)  tolua_getusertype(tolua_S,1,0));
  u32b f1 = ((u32b)  tolua_getnumber(tolua_S,2,0));
  u32b f2 = ((u32b)  tolua_getnumber(tolua_S,3,0));
  u32b f3 = ((u32b)  tolua_getnumber(tolua_S,4,0));
  u32b f4 = ((u32b)  tolua_getnumber(tolua_S,5,0));
  u32b f5 = ((u32b)  tolua_getnumber(tolua_S,6,0));
  u32b esp = ((u32b)  tolua_getnumber(tolua_S,7,0));
 {
  object_flags(o_ptr,&f1,&f2,&f3,&f4,&f5,&esp);
 tolua_pushnumber(tolua_S,(long)f1);
 tolua_pushnumber(tolua_S,(long)f2);
 tolua_pushnumber(tolua_S,(long)f3);
 tolua_pushnumber(tolua_S,(long)f4);
 tolua_pushnumber(tolua_S,(long)f5);
 tolua_pushnumber(tolua_S,(long)esp);
 }
 }
 return 6;
tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'object_flags'.");
 return 0;
}

/* function: lua_object_desc */
static int toluaI_object_object_desc00(lua_State* tolua_S)
{
 if (
 !tolua_istype(tolua_S,1,tolua_tag(tolua_S,"object_type"),0) ||
 !tolua_istype(tolua_S,2,LUA_TNUMBER,0) ||
 !tolua_istype(tolua_S,3,LUA_TNUMBER,0) ||
 !tolua_isnoobj(tolua_S,4)
 )
 goto tolua_lerror;
 else
 {
  object_type* o_ptr = ((object_type*)  tolua_getusertype(tolua_S,1,0));
  int pref = ((int)  tolua_getnumber(tolua_S,2,0));
  int mode = ((int)  tolua_getnumber(tolua_S,3,0));
 {
  char* toluaI_ret = (char*)  lua_object_desc(o_ptr,pref,mode);
 tolua_pushstring(tolua_S,(const char*)toluaI_ret);
 }
 }
 return 1;
tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'object_desc'.");
 return 0;
}

/* function: object_out_desc */
static int toluaI_object_object_out_desc00(lua_State* tolua_S)
{
 if (
 !tolua_istype(tolua_S,1,tolua_tag(tolua_S,"object_type"),0) ||
 !tolua_istype(tolua_S,2,tolua_tag(tolua_S,"FILE"),0) ||
 !tolua_istype(tolua_S,3,LUA_TNUMBER,0) ||
 !tolua_istype(tolua_S,4,LUA_TNUMBER,0) ||
 !tolua_isnoobj(tolua_S,5)
 )
 goto tolua_lerror;
 else
 {
  object_type* o_ptr = ((object_type*)  tolua_getusertype(tolua_S,1,0));
  FILE* fff = ((FILE*)  tolua_getusertype(tolua_S,2,0));
  bool trim_down = ((bool)  tolua_getnumber(tolua_S,3,0));
  bool wait_for_it = ((bool)  tolua_getnumber(tolua_S,4,0));
 {
  bool toluaI_ret = (bool)  object_out_desc(o_ptr,fff,trim_down,wait_for_it);
 tolua_pushnumber(tolua_S,(long)toluaI_ret);
 }
 }
 return 1;
tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'object_out_desc'.");
 return 0;
}

/* function: inven_item_describe */
static int toluaI_object_inven_item_describe00(lua_State* tolua_S)
{
 if (
 !tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
 !tolua_isnoobj(tolua_S,2)
 )
 goto tolua_lerror;
 else
 {
  int item = ((int)  tolua_getnumber(tolua_S,1,0));
 {
  inven_item_describe(item);
 }
 }
 return 0;
tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'inven_item_describe'.");
 return 0;
}

/* function: inven_item_increase */
static int toluaI_object_inven_item_increase00(lua_State* tolua_S)
{
 if (
 !tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
 !tolua_istype(tolua_S,2,LUA_TNUMBER,0) ||
 !tolua_isnoobj(tolua_S,3)
 )
 goto tolua_lerror;
 else
 {
  int item = ((int)  tolua_getnumber(tolua_S,1,0));
  int num = ((int)  tolua_getnumber(tolua_S,2,0));
 {
  inven_item_increase(item,num);
 }
 }
 return 0;
tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'inven_item_increase'.");
 return 0;
}

/* function: inven_item_optimize */
static int toluaI_object_inven_item_optimize00(lua_State* tolua_S)
{
 if (
 !tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
 !tolua_isnoobj(tolua_S,2)
 )
 goto tolua_lerror;
 else
 {
  int item = ((int)  tolua_getnumber(tolua_S,1,0));
 {
  bool toluaI_ret = (bool)  inven_item_optimize(item);
 tolua_pushnumber(tolua_S,(long)toluaI_ret);
 }
 }
 return 1;
tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'inven_item_optimize'.");
 return 0;
}

/* function: floor_item_describe */
static int toluaI_object_floor_item_describe00(lua_State* tolua_S)
{
 if (
 !tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
 !tolua_isnoobj(tolua_S,2)
 )
 goto tolua_lerror;
 else
 {
  int item = ((int)  tolua_getnumber(tolua_S,1,0));
 {
  floor_item_describe(item);
 }
 }
 return 0;
tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'floor_item_describe'.");
 return 0;
}

/* function: floor_item_increase */
static int toluaI_object_floor_item_increase00(lua_State* tolua_S)
{
 if (
 !tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
 !tolua_istype(tolua_S,2,LUA_TNUMBER,0) ||
 !tolua_isnoobj(tolua_S,3)
 )
 goto tolua_lerror;
 else
 {
  int item = ((int)  tolua_getnumber(tolua_S,1,0));
  int num = ((int)  tolua_getnumber(tolua_S,2,0));
 {
  floor_item_increase(item,num);
 }
 }
 return 0;
tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'floor_item_increase'.");
 return 0;
}

/* function: floor_item_optimize */
static int toluaI_object_floor_item_optimize00(lua_State* tolua_S)
{
 if (
 !tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
 !tolua_isnoobj(tolua_S,2)
 )
 goto tolua_lerror;
 else
 {
  int item = ((int)  tolua_getnumber(tolua_S,1,0));
 {
  floor_item_optimize(item);
 }
 }
 return 0;
tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'floor_item_optimize'.");
 return 0;
}

/* function: delete_object_idx */
static int toluaI_object_delete_object_idx00(lua_State* tolua_S)
{
 if (
 !tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
 !tolua_isnoobj(tolua_S,2)
 )
 goto tolua_lerror;
 else
 {
  int o_idx = ((int)  tolua_getnumber(tolua_S,1,0));
 {
  delete_object_idx(o_idx);
 }
 }
 return 0;
tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'delete_object_idx'.");
 return 0;
}

/* function: o_pop */
static int toluaI_object_o_pop00(lua_State* tolua_S)
{
 if (
 !tolua_isnoobj(tolua_S,1)
 )
 goto tolua_lerror;
 else
 {
 {
  s16b toluaI_ret = (s16b)  o_pop();
 tolua_pushnumber(tolua_S,(long)toluaI_ret);
 }
 }
 return 1;
tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'o_pop'.");
 return 0;
}

/* function: get_obj_num_prep */
static int toluaI_object_get_obj_num_prep00(lua_State* tolua_S)
{
 if (
 !tolua_isnoobj(tolua_S,1)
 )
 goto tolua_lerror;
 else
 {
 {
  errr toluaI_ret = (errr)  get_obj_num_prep();
 tolua_pushnumber(tolua_S,(long)toluaI_ret);
 }
 }
 return 1;
tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'get_obj_num_prep'.");
 return 0;
}

/* function: ident_all */
static int toluaI_object_ident_all00(lua_State* tolua_S)
{
 if (
 !tolua_isnoobj(tolua_S,1)
 )
 goto tolua_lerror;
 else
 {
 {
  bool toluaI_ret = (bool)  ident_all();
 tolua_pushnumber(tolua_S,(long)toluaI_ret);
 }
 }
 return 1;
tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'ident_all'.");
 return 0;
}

/* function: get_obj_num */
static int toluaI_object_get_obj_num00(lua_State* tolua_S)
{
 if (
 !tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
 !tolua_isnoobj(tolua_S,2)
 )
 goto tolua_lerror;
 else
 {
  int level = ((int)  tolua_getnumber(tolua_S,1,0));
 {
  s16b toluaI_ret = (s16b)  get_obj_num(level);
 tolua_pushnumber(tolua_S,(long)toluaI_ret);
 }
 }
 return 1;
tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'get_obj_num'.");
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
 goto tolua_lerror;
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
tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'lookup_kind'.");
 return 0;
}

/* function: object_wipe */
static int toluaI_object_object_wipe00(lua_State* tolua_S)
{
 if (
 !tolua_istype(tolua_S,1,tolua_tag(tolua_S,"object_type"),0) ||
 !tolua_isnoobj(tolua_S,2)
 )
 goto tolua_lerror;
 else
 {
  object_type* o_ptr = ((object_type*)  tolua_getusertype(tolua_S,1,0));
 {
  object_wipe(o_ptr);
 }
 }
 return 0;
tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'object_wipe'.");
 return 0;
}

/* function: object_prep */
static int toluaI_object_object_prep00(lua_State* tolua_S)
{
 if (
 !tolua_istype(tolua_S,1,tolua_tag(tolua_S,"object_type"),0) ||
 !tolua_istype(tolua_S,2,LUA_TNUMBER,0) ||
 !tolua_isnoobj(tolua_S,3)
 )
 goto tolua_lerror;
 else
 {
  object_type* o_ptr = ((object_type*)  tolua_getusertype(tolua_S,1,0));
  int k_idx = ((int)  tolua_getnumber(tolua_S,2,0));
 {
  object_prep(o_ptr,k_idx);
 }
 }
 return 0;
tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'object_prep'.");
 return 0;
}

/* function: object_copy */
static int toluaI_object_object_copy00(lua_State* tolua_S)
{
 if (
 !tolua_istype(tolua_S,1,tolua_tag(tolua_S,"object_type"),0) ||
 !tolua_istype(tolua_S,2,tolua_tag(tolua_S,"object_type"),0) ||
 !tolua_isnoobj(tolua_S,3)
 )
 goto tolua_lerror;
 else
 {
  object_type* o_ptr = ((object_type*)  tolua_getusertype(tolua_S,1,0));
  object_type* j_ptr = ((object_type*)  tolua_getusertype(tolua_S,2,0));
 {
  object_copy(o_ptr,j_ptr);
 }
 }
 return 0;
tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'object_copy'.");
 return 0;
}

/* function: inven_carry_okay */
static int toluaI_object_inven_carry_okay00(lua_State* tolua_S)
{
 if (
 !tolua_istype(tolua_S,1,tolua_tag(tolua_S,"object_type"),0) ||
 !tolua_isnoobj(tolua_S,2)
 )
 goto tolua_lerror;
 else
 {
  object_type* o_ptr = ((object_type*)  tolua_getusertype(tolua_S,1,0));
 {
  bool toluaI_ret = (bool)  inven_carry_okay(o_ptr);
 tolua_pushnumber(tolua_S,(long)toluaI_ret);
 }
 }
 return 1;
tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'inven_carry_okay'.");
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
 !tolua_istype(tolua_S,5,LUA_TNUMBER,0) ||
 !tolua_isnoobj(tolua_S,6)
 )
 goto tolua_lerror;
 else
 {
  object_type* o_ptr = ((object_type*)  tolua_getusertype(tolua_S,1,0));
  int lev = ((int)  tolua_getnumber(tolua_S,2,0));
  bool okay = ((bool)  tolua_getnumber(tolua_S,3,0));
  bool good = ((bool)  tolua_getnumber(tolua_S,4,0));
  bool great = ((bool)  tolua_getnumber(tolua_S,5,0));
 {
  apply_magic(o_ptr,lev,okay,good,great);
 }
 }
 return 0;
tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'apply_magic'.");
 return 0;
}

/* function: make_object */
static int toluaI_object_make_object00(lua_State* tolua_S)
{
 if (
 !tolua_istype(tolua_S,1,tolua_tag(tolua_S,"object_type"),0) ||
 !tolua_istype(tolua_S,2,LUA_TNUMBER,0) ||
 !tolua_istype(tolua_S,3,LUA_TNUMBER,0) ||
 !tolua_istype(tolua_S,4,tolua_tag(tolua_S,"obj_theme"),0) ||
 !tolua_isnoobj(tolua_S,5)
 )
 goto tolua_lerror;
 else
 {
  object_type* j_ptr = ((object_type*)  tolua_getusertype(tolua_S,1,0));
  bool good = ((bool)  tolua_getnumber(tolua_S,2,0));
  bool great = ((bool)  tolua_getnumber(tolua_S,3,0));
  obj_theme theme = *((obj_theme*)  tolua_getusertype(tolua_S,4,0));
 {
  bool toluaI_ret = (bool)  make_object(j_ptr,good,great,theme);
 tolua_pushnumber(tolua_S,(long)toluaI_ret);
 }
 }
 return 1;
tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'make_object'.");
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
 goto tolua_lerror;
 else
 {
  object_type* o_ptr = ((object_type*)  tolua_getusertype(tolua_S,1,0));
  int chance = ((int)  tolua_getnumber(tolua_S,2,0));
  int y = ((int)  tolua_getnumber(tolua_S,3,0));
  int x = ((int)  tolua_getnumber(tolua_S,4,0));
 {
  s16b toluaI_ret = (s16b)  drop_near(o_ptr,chance,y,x);
 tolua_pushnumber(tolua_S,(long)toluaI_ret);
 }
 }
 return 1;
tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'drop_near'.");
 return 0;
}

/* function: get_object */
static int toluaI_object_get_object00(lua_State* tolua_S)
{
 if (
 !tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
 !tolua_isnoobj(tolua_S,2)
 )
 goto tolua_lerror;
 else
 {
  int item = ((int)  tolua_getnumber(tolua_S,1,0));
 {
  object_type* toluaI_ret = (object_type*)  get_object(item);
 tolua_pushusertype(tolua_S,(void*)toluaI_ret,tolua_tag(tolua_S,"object_type"));
 }
 }
 return 1;
tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'get_object'.");
 return 0;
}

/* function: new_object */
static int toluaI_object_new_object00(lua_State* tolua_S)
{
 if (
 !tolua_isnoobj(tolua_S,1)
 )
 goto tolua_lerror;
 else
 {
 {
  object_type* toluaI_ret = (object_type*)  new_object();
 tolua_pushusertype(tolua_S,(void*)toluaI_ret,tolua_tag(tolua_S,"object_type"));
 }
 }
 return 1;
tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'new_object'.");
 return 0;
}

/* function: end_object */
static int toluaI_object_end_object00(lua_State* tolua_S)
{
 if (
 !tolua_istype(tolua_S,1,tolua_tag(tolua_S,"object_type"),0) ||
 !tolua_isnoobj(tolua_S,2)
 )
 goto tolua_lerror;
 else
 {
  object_type* o_ptr = ((object_type*)  tolua_getusertype(tolua_S,1,0));
 {
  end_object(o_ptr);
 }
 }
 return 0;
tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'end_object'.");
 return 0;
}

/* function: get_item */
static int toluaI_object_get_item_aux00(lua_State* tolua_S)
{
 if (
 !tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
 !tolua_istype(tolua_S,2,LUA_TSTRING,0) ||
 !tolua_istype(tolua_S,3,LUA_TSTRING,0) ||
 !tolua_istype(tolua_S,4,LUA_TNUMBER,0) ||
 !tolua_isnoobj(tolua_S,5)
 )
 goto tolua_lerror;
 else
 {
  int cp = ((int)  tolua_getnumber(tolua_S,1,0));
  cptr pmt = ((cptr)  tolua_getstring(tolua_S,2,0));
  cptr str = ((cptr)  tolua_getstring(tolua_S,3,0));
  int mode = ((int)  tolua_getnumber(tolua_S,4,0));
 {
  bool toluaI_ret = (bool)  get_item(&cp,pmt,str,mode);
 tolua_pushnumber(tolua_S,(long)toluaI_ret);
 tolua_pushnumber(tolua_S,(long)cp);
 }
 }
 return 2;
tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'get_item_aux'.");
 return 0;
}

/* function: lua_set_item_tester */
static int toluaI_object_lua_set_item_tester00(lua_State* tolua_S)
{
 if (
 !tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
 !tolua_istype(tolua_S,2,LUA_TSTRING,0) ||
 !tolua_isnoobj(tolua_S,3)
 )
 goto tolua_lerror;
 else
 {
  int tval = ((int)  tolua_getnumber(tolua_S,1,0));
  char* fct = ((char*)  tolua_getstring(tolua_S,2,0));
 {
  lua_set_item_tester(tval,fct);
 }
 }
 return 0;
tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'lua_set_item_tester'.");
 return 0;
}

/* function: is_magestaff */
static int toluaI_object_is_magestaff00(lua_State* tolua_S)
{
 if (
 !tolua_isnoobj(tolua_S,1)
 )
 goto tolua_lerror;
 else
 {
 {
  bool toluaI_ret = (bool)  is_magestaff();
 tolua_pushnumber(tolua_S,(long)toluaI_ret);
 }
 }
 return 1;
tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'is_magestaff'.");
 return 0;
}

/* function: identify_pack_fully */
static int toluaI_object_identify_pack_fully00(lua_State* tolua_S)
{
 if (
 !tolua_isnoobj(tolua_S,1)
 )
 goto tolua_lerror;
 else
 {
 {
  identify_pack_fully();
 }
 }
 return 0;
tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'identify_pack_fully'.");
 return 0;
}

/* function: inven_carry */
static int toluaI_object_inven_carry00(lua_State* tolua_S)
{
 if (
 !tolua_istype(tolua_S,1,tolua_tag(tolua_S,"object_type"),0) ||
 !tolua_istype(tolua_S,2,LUA_TNUMBER,0) ||
 !tolua_isnoobj(tolua_S,3)
 )
 goto tolua_lerror;
 else
 {
  object_type* o_ptr = ((object_type*)  tolua_getusertype(tolua_S,1,0));
  bool final = ((bool)  tolua_getnumber(tolua_S,2,0));
 {
  s16b toluaI_ret = (s16b)  inven_carry(o_ptr,final);
 tolua_pushnumber(tolua_S,(long)toluaI_ret);
 }
 }
 return 1;
tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'inven_carry'.");
 return 0;
}

/* function: calc_total_weight */
static int toluaI_object_calc_total_weight00(lua_State* tolua_S)
{
 if (
 !tolua_isnoobj(tolua_S,1)
 )
 goto tolua_lerror;
 else
 {
 {
  s32b toluaI_ret = (s32b)  calc_total_weight();
 tolua_pushnumber(tolua_S,(long)toluaI_ret);
 }
 }
 return 1;
tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'calc_total_weight'.");
 return 0;
}

/* function: get_slot */
static int toluaI_object_get_slot00(lua_State* tolua_S)
{
 if (
 !tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
 !tolua_isnoobj(tolua_S,2)
 )
 goto tolua_lerror;
 else
 {
  int slot = ((int)  tolua_getnumber(tolua_S,1,0));
 {
  int toluaI_ret = (int)  get_slot(slot);
 tolua_pushnumber(tolua_S,(long)toluaI_ret);
 }
 }
 return 1;
tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'get_slot'.");
 return 0;
}

/* function: is_blessed */
static int toluaI_object_is_blessed00(lua_State* tolua_S)
{
 if (
 !tolua_istype(tolua_S,1,tolua_tag(tolua_S,"object_type"),0) ||
 !tolua_isnoobj(tolua_S,2)
 )
 goto tolua_lerror;
 else
 {
  object_type* o_ptr = ((object_type*)  tolua_getusertype(tolua_S,1,0));
 {
  bool toluaI_ret = (bool)  is_blessed(o_ptr);
 tolua_pushnumber(tolua_S,(long)toluaI_ret);
 }
 }
 return 1;
tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'is_blessed'.");
 return 0;
}

/* get function: sense_desc */
static int toluaI_get_object_sense_desc(lua_State* tolua_S)
{
 int toluaI_index;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 tolua_error(tolua_S,"invalid type in array indexing.");
 toluaI_index = (int)tolua_getnumber(tolua_S,2,0)-1;
 if (toluaI_index<0 || toluaI_index>=1000)
 tolua_error(tolua_S,"array indexing out of range.");
 tolua_pushstring(tolua_S,(const char*)sense_desc[toluaI_index]);
 return 1;
}

/* set function: sense_desc */
static int toluaI_set_object_sense_desc(lua_State* tolua_S)
{
 int toluaI_index;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 tolua_error(tolua_S,"invalid type in array indexing.");
 toluaI_index = (int)tolua_getnumber(tolua_S,2,0)-1;
 if (toluaI_index<0 || toluaI_index>=1000)
 tolua_error(tolua_S,"array indexing out of range.");
  sense_desc[toluaI_index] = ((cptr)  tolua_getstring(tolua_S,3,0));
 return 0;
}

/* function: object_pickup */
static int toluaI_object_object_pickup00(lua_State* tolua_S)
{
 if (
 !tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
 !tolua_isnoobj(tolua_S,2)
 )
 goto tolua_lerror;
 else
 {
  int this_o_idx = ((int)  tolua_getnumber(tolua_S,1,0));
 {
  object_pickup(this_o_idx);
 }
 }
 return 0;
tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'object_pickup'.");
 return 0;
}

/* function: lua_is_artifact */
static int toluaI_object_is_artifact00(lua_State* tolua_S)
{
 if (
 !tolua_istype(tolua_S,1,tolua_tag(tolua_S,"object_type"),0) ||
 !tolua_isnoobj(tolua_S,2)
 )
 goto tolua_lerror;
 else
 {
  object_type* o_ptr = ((object_type*)  tolua_getusertype(tolua_S,1,0));
 {
  bool toluaI_ret = (bool)  lua_is_artifact(o_ptr);
 tolua_pushnumber(tolua_S,(long)toluaI_ret);
 }
 }
 return 1;
tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'is_artifact'.");
 return 0;
}

/* function: lua_is_aware */
static int toluaI_object_is_aware00(lua_State* tolua_S)
{
 if (
 !tolua_istype(tolua_S,1,tolua_tag(tolua_S,"object_type"),0) ||
 !tolua_isnoobj(tolua_S,2)
 )
 goto tolua_lerror;
 else
 {
  object_type* o_ptr = ((object_type*)  tolua_getusertype(tolua_S,1,0));
 {
  bool toluaI_ret = (bool)  lua_is_aware(o_ptr);
 tolua_pushnumber(tolua_S,(long)toluaI_ret);
 }
 }
 return 1;
tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'is_aware'.");
 return 0;
}

/* function: lua_is_known */
static int toluaI_object_is_known00(lua_State* tolua_S)
{
 if (
 !tolua_istype(tolua_S,1,tolua_tag(tolua_S,"object_type"),0) ||
 !tolua_isnoobj(tolua_S,2)
 )
 goto tolua_lerror;
 else
 {
  object_type* o_ptr = ((object_type*)  tolua_getusertype(tolua_S,1,0));
 {
  bool toluaI_ret = (bool)  lua_is_known(o_ptr);
 tolua_pushnumber(tolua_S,(long)toluaI_ret);
 }
 }
 return 1;
tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'is_known'.");
 return 0;
}

/* function: lua_set_aware */
static int toluaI_object_set_aware00(lua_State* tolua_S)
{
 if (
 !tolua_istype(tolua_S,1,tolua_tag(tolua_S,"object_type"),0) ||
 !tolua_isnoobj(tolua_S,2)
 )
 goto tolua_lerror;
 else
 {
  object_type* o_ptr = ((object_type*)  tolua_getusertype(tolua_S,1,0));
 {
  lua_set_aware(o_ptr);
 }
 }
 return 0;
tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'set_aware'.");
 return 0;
}

/* function: lua_set_known */
static int toluaI_object_set_known00(lua_State* tolua_S)
{
 if (
 !tolua_istype(tolua_S,1,tolua_tag(tolua_S,"object_type"),0) ||
 !tolua_isnoobj(tolua_S,2)
 )
 goto tolua_lerror;
 else
 {
  object_type* o_ptr = ((object_type*)  tolua_getusertype(tolua_S,1,0));
 {
  lua_set_known(o_ptr);
 }
 }
 return 0;
tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'set_known'.");
 return 0;
}

/* function: value_check_aux1 */
static int toluaI_object_value_check_aux100(lua_State* tolua_S)
{
 if (
 !tolua_istype(tolua_S,1,tolua_tag(tolua_S,"object_type"),0) ||
 !tolua_isnoobj(tolua_S,2)
 )
 goto tolua_lerror;
 else
 {
  object_type* o_ptr = ((object_type*)  tolua_getusertype(tolua_S,1,0));
 {
  byte toluaI_ret = (byte)  value_check_aux1(o_ptr);
 tolua_pushnumber(tolua_S,(long)toluaI_ret);
 }
 }
 return 1;
tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'value_check_aux1'.");
 return 0;
}

/* function: value_check_aux1_magic */
static int toluaI_object_value_check_aux1_magic00(lua_State* tolua_S)
{
 if (
 !tolua_istype(tolua_S,1,tolua_tag(tolua_S,"object_type"),0) ||
 !tolua_isnoobj(tolua_S,2)
 )
 goto tolua_lerror;
 else
 {
  object_type* o_ptr = ((object_type*)  tolua_getusertype(tolua_S,1,0));
 {
  byte toluaI_ret = (byte)  value_check_aux1_magic(o_ptr);
 tolua_pushnumber(tolua_S,(long)toluaI_ret);
 }
 }
 return 1;
tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'value_check_aux1_magic'.");
 return 0;
}

/* function: value_check_aux2 */
static int toluaI_object_value_check_aux200(lua_State* tolua_S)
{
 if (
 !tolua_istype(tolua_S,1,tolua_tag(tolua_S,"object_type"),0) ||
 !tolua_isnoobj(tolua_S,2)
 )
 goto tolua_lerror;
 else
 {
  object_type* o_ptr = ((object_type*)  tolua_getusertype(tolua_S,1,0));
 {
  byte toluaI_ret = (byte)  value_check_aux2(o_ptr);
 tolua_pushnumber(tolua_S,(long)toluaI_ret);
 }
 }
 return 1;
tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'value_check_aux2'.");
 return 0;
}

/* function: value_check_aux2_magic */
static int toluaI_object_value_check_aux2_magic00(lua_State* tolua_S)
{
 if (
 !tolua_istype(tolua_S,1,tolua_tag(tolua_S,"object_type"),0) ||
 !tolua_isnoobj(tolua_S,2)
 )
 goto tolua_lerror;
 else
 {
  object_type* o_ptr = ((object_type*)  tolua_getusertype(tolua_S,1,0));
 {
  byte toluaI_ret = (byte)  value_check_aux2_magic(o_ptr);
 tolua_pushnumber(tolua_S,(long)toluaI_ret);
 }
 }
 return 1;
tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'value_check_aux2_magic'.");
 return 0;
}

/* function: select_sense */
static int toluaI_object_select_sense00(lua_State* tolua_S)
{
 if (
 !tolua_istype(tolua_S,1,tolua_tag(tolua_S,"object_type"),0) ||
 !tolua_istype(tolua_S,2,LUA_TNUMBER,0) ||
 !tolua_istype(tolua_S,3,LUA_TNUMBER,0) ||
 !tolua_isnoobj(tolua_S,4)
 )
 goto tolua_lerror;
 else
 {
  object_type* o_ptr = ((object_type*)  tolua_getusertype(tolua_S,1,0));
  bool ok_combat = ((bool)  tolua_getnumber(tolua_S,2,0));
  bool ok_magic = ((bool)  tolua_getnumber(tolua_S,3,0));
 {
  byte toluaI_ret = (byte)  select_sense(o_ptr,ok_combat,ok_magic);
 tolua_pushnumber(tolua_S,(long)toluaI_ret);
 }
 }
 return 1;
tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'select_sense'.");
 return 0;
}

/* function: remove_curse_object */
static int toluaI_object_remove_curse_object00(lua_State* tolua_S)
{
 if (
 !tolua_istype(tolua_S,1,tolua_tag(tolua_S,"object_type"),0) ||
 !tolua_istype(tolua_S,2,LUA_TNUMBER,0) ||
 !tolua_isnoobj(tolua_S,3)
 )
 goto tolua_lerror;
 else
 {
  object_type* o_ptr = ((object_type*)  tolua_getusertype(tolua_S,1,0));
  bool all = ((bool)  tolua_getnumber(tolua_S,2,0));
 {
  bool toluaI_ret = (bool)  remove_curse_object(o_ptr,all);
 tolua_pushnumber(tolua_S,(long)toluaI_ret);
 }
 }
 return 1;
tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'remove_curse_object'.");
 return 0;
}

/* Open function */
int tolua_object_open (lua_State* tolua_S)
{
 tolua_open(tolua_S);
 toluaI_reg_types(tolua_S);
 tolua_globalvar(tolua_S,"obj_forge",toluaI_get_object_obj_forge,toluaI_set_object_obj_forge);
 tolua_globalvar(tolua_S,"theme_forge",toluaI_get_object_theme_forge,toluaI_set_object_theme_forge);
 tolua_constant(tolua_S,NULL,"TR1_STR",TR1_STR);
 tolua_constant(tolua_S,NULL,"TR1_INT",TR1_INT);
 tolua_constant(tolua_S,NULL,"TR1_WIS",TR1_WIS);
 tolua_constant(tolua_S,NULL,"TR1_DEX",TR1_DEX);
 tolua_constant(tolua_S,NULL,"TR1_CON",TR1_CON);
 tolua_constant(tolua_S,NULL,"TR1_CHR",TR1_CHR);
 tolua_constant(tolua_S,NULL,"TR1_MANA",TR1_MANA);
 tolua_constant(tolua_S,NULL,"TR1_SPELL",TR1_SPELL);
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
 tolua_constant(tolua_S,NULL,"TR1_NULL_MASK",TR1_NULL_MASK);
 tolua_constant(tolua_S,NULL,"TR2_SUST_STR",TR2_SUST_STR);
 tolua_constant(tolua_S,NULL,"TR2_SUST_INT",TR2_SUST_INT);
 tolua_constant(tolua_S,NULL,"TR2_SUST_WIS",TR2_SUST_WIS);
 tolua_constant(tolua_S,NULL,"TR2_SUST_DEX",TR2_SUST_DEX);
 tolua_constant(tolua_S,NULL,"TR2_SUST_CON",TR2_SUST_CON);
 tolua_constant(tolua_S,NULL,"TR2_SUST_CHR",TR2_SUST_CHR);
 tolua_constant(tolua_S,NULL,"TR2_INVIS",TR2_INVIS);
 tolua_constant(tolua_S,NULL,"TR2_LIFE",TR2_LIFE);
 tolua_constant(tolua_S,NULL,"TR2_IM_ACID",TR2_IM_ACID);
 tolua_constant(tolua_S,NULL,"TR2_IM_ELEC",TR2_IM_ELEC);
 tolua_constant(tolua_S,NULL,"TR2_IM_FIRE",TR2_IM_FIRE);
 tolua_constant(tolua_S,NULL,"TR2_IM_COLD",TR2_IM_COLD);
 tolua_constant(tolua_S,NULL,"TR2_SENS_FIRE",TR2_SENS_FIRE);
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
 tolua_constant(tolua_S,NULL,"TR2_NULL_MASK",TR2_NULL_MASK);
 tolua_constant(tolua_S,NULL,"TR3_SH_FIRE",TR3_SH_FIRE);
 tolua_constant(tolua_S,NULL,"TR3_SH_ELEC",TR3_SH_ELEC);
 tolua_constant(tolua_S,NULL,"TR3_AUTO_CURSE",TR3_AUTO_CURSE);
 tolua_constant(tolua_S,NULL,"TR3_DECAY",TR3_DECAY);
 tolua_constant(tolua_S,NULL,"TR3_NO_TELE",TR3_NO_TELE);
 tolua_constant(tolua_S,NULL,"TR3_NO_MAGIC",TR3_NO_MAGIC);
 tolua_constant(tolua_S,NULL,"TR3_WRAITH",TR3_WRAITH);
 tolua_constant(tolua_S,NULL,"TR3_TY_CURSE",TR3_TY_CURSE);
 tolua_constant(tolua_S,NULL,"TR3_EASY_KNOW",TR3_EASY_KNOW);
 tolua_constant(tolua_S,NULL,"TR3_HIDE_TYPE",TR3_HIDE_TYPE);
 tolua_constant(tolua_S,NULL,"TR3_SHOW_MODS",TR3_SHOW_MODS);
 tolua_constant(tolua_S,NULL,"TR3_INSTA_ART",TR3_INSTA_ART);
 tolua_constant(tolua_S,NULL,"TR3_FEATHER",TR3_FEATHER);
 tolua_constant(tolua_S,NULL,"TR3_LITE1",TR3_LITE1);
 tolua_constant(tolua_S,NULL,"TR3_SEE_INVIS",TR3_SEE_INVIS);
 tolua_constant(tolua_S,NULL,"TR3_NORM_ART",TR3_NORM_ART);
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
 tolua_constant(tolua_S,NULL,"TR3_NULL_MASK",TR3_NULL_MASK);
 tolua_constant(tolua_S,NULL,"TR4_NEVER_BLOW",TR4_NEVER_BLOW);
 tolua_constant(tolua_S,NULL,"TR4_PRECOGNITION",TR4_PRECOGNITION);
 tolua_constant(tolua_S,NULL,"TR4_BLACK_BREATH",TR4_BLACK_BREATH);
 tolua_constant(tolua_S,NULL,"TR4_RECHARGE",TR4_RECHARGE);
 tolua_constant(tolua_S,NULL,"TR4_FLY",TR4_FLY);
 tolua_constant(tolua_S,NULL,"TR4_DG_CURSE",TR4_DG_CURSE);
 tolua_constant(tolua_S,NULL,"TR4_COULD2H",TR4_COULD2H);
 tolua_constant(tolua_S,NULL,"TR4_MUST2H",TR4_MUST2H);
 tolua_constant(tolua_S,NULL,"TR4_LEVELS",TR4_LEVELS);
 tolua_constant(tolua_S,NULL,"TR4_CLONE",TR4_CLONE);
 tolua_constant(tolua_S,NULL,"TR4_SPECIAL_GENE",TR4_SPECIAL_GENE);
 tolua_constant(tolua_S,NULL,"TR4_CLIMB",TR4_CLIMB);
 tolua_constant(tolua_S,NULL,"TR4_FAST_CAST",TR4_FAST_CAST);
 tolua_constant(tolua_S,NULL,"TR4_CAPACITY",TR4_CAPACITY);
 tolua_constant(tolua_S,NULL,"TR4_CHARGING",TR4_CHARGING);
 tolua_constant(tolua_S,NULL,"TR4_CHEAPNESS",TR4_CHEAPNESS);
 tolua_constant(tolua_S,NULL,"TR4_FOUNTAIN",TR4_FOUNTAIN);
 tolua_constant(tolua_S,NULL,"TR4_ANTIMAGIC_50",TR4_ANTIMAGIC_50);
 tolua_constant(tolua_S,NULL,"TR4_ANTIMAGIC_30",TR4_ANTIMAGIC_30);
 tolua_constant(tolua_S,NULL,"TR4_ANTIMAGIC_20",TR4_ANTIMAGIC_20);
 tolua_constant(tolua_S,NULL,"TR4_ANTIMAGIC_10",TR4_ANTIMAGIC_10);
 tolua_constant(tolua_S,NULL,"TR4_EASY_USE",TR4_EASY_USE);
 tolua_constant(tolua_S,NULL,"TR4_IM_NETHER",TR4_IM_NETHER);
 tolua_constant(tolua_S,NULL,"TR4_RECHARGED",TR4_RECHARGED);
 tolua_constant(tolua_S,NULL,"TR4_ULTIMATE",TR4_ULTIMATE);
 tolua_constant(tolua_S,NULL,"TR4_AUTO_ID",TR4_AUTO_ID);
 tolua_constant(tolua_S,NULL,"TR4_LITE2",TR4_LITE2);
 tolua_constant(tolua_S,NULL,"TR4_LITE3",TR4_LITE3);
 tolua_constant(tolua_S,NULL,"TR4_FUEL_LITE",TR4_FUEL_LITE);
 tolua_constant(tolua_S,NULL,"TR4_ART_EXP",TR4_ART_EXP);
 tolua_constant(tolua_S,NULL,"TR4_CURSE_NO_DROP",TR4_CURSE_NO_DROP);
 tolua_constant(tolua_S,NULL,"TR4_NO_RECHARGE",TR4_NO_RECHARGE);
 tolua_constant(tolua_S,NULL,"TR4_NULL_MASK",TR4_NULL_MASK);
 tolua_constant(tolua_S,NULL,"TR5_TEMPORARY",TR5_TEMPORARY);
 tolua_constant(tolua_S,NULL,"TR5_DRAIN_MANA",TR5_DRAIN_MANA);
 tolua_constant(tolua_S,NULL,"TR5_DRAIN_HP",TR5_DRAIN_HP);
 tolua_constant(tolua_S,NULL,"TR5_KILL_DEMON",TR5_KILL_DEMON);
 tolua_constant(tolua_S,NULL,"TR5_KILL_UNDEAD",TR5_KILL_UNDEAD);
 tolua_constant(tolua_S,NULL,"TR5_CRIT",TR5_CRIT);
 tolua_constant(tolua_S,NULL,"TR5_ATTR_MULTI",TR5_ATTR_MULTI);
 tolua_constant(tolua_S,NULL,"TR5_WOUNDING",TR5_WOUNDING);
 tolua_constant(tolua_S,NULL,"TR5_FULL_NAME",TR5_FULL_NAME);
 tolua_constant(tolua_S,NULL,"TR5_LUCK",TR5_LUCK);
 tolua_constant(tolua_S,NULL,"TR5_IMMOVABLE",TR5_IMMOVABLE);
 tolua_constant(tolua_S,NULL,"TR5_SPELL_CONTAIN",TR5_SPELL_CONTAIN);
 tolua_constant(tolua_S,NULL,"TR5_RES_MORGUL",TR5_RES_MORGUL);
 tolua_constant(tolua_S,NULL,"TR5_ACTIVATE_NO_WIELD",TR5_ACTIVATE_NO_WIELD);
 tolua_constant(tolua_S,NULL,"TR5_MAGIC_BREATH",TR5_MAGIC_BREATH);
 tolua_constant(tolua_S,NULL,"TR5_WATER_BREATH",TR5_WATER_BREATH);
 tolua_constant(tolua_S,NULL,"TR5_WIELD_CAST",TR5_WIELD_CAST);
 tolua_constant(tolua_S,NULL,"ESP_ORC",ESP_ORC);
 tolua_constant(tolua_S,NULL,"ESP_TROLL",ESP_TROLL);
 tolua_constant(tolua_S,NULL,"ESP_DRAGON",ESP_DRAGON);
 tolua_constant(tolua_S,NULL,"ESP_GIANT",ESP_GIANT);
 tolua_constant(tolua_S,NULL,"ESP_DEMON",ESP_DEMON);
 tolua_constant(tolua_S,NULL,"ESP_UNDEAD",ESP_UNDEAD);
 tolua_constant(tolua_S,NULL,"ESP_EVIL",ESP_EVIL);
 tolua_constant(tolua_S,NULL,"ESP_ANIMAL",ESP_ANIMAL);
 tolua_constant(tolua_S,NULL,"ESP_THUNDERLORD",ESP_THUNDERLORD);
 tolua_constant(tolua_S,NULL,"ESP_GOOD",ESP_GOOD);
 tolua_constant(tolua_S,NULL,"ESP_NONLIVING",ESP_NONLIVING);
 tolua_constant(tolua_S,NULL,"ESP_UNIQUE",ESP_UNIQUE);
 tolua_constant(tolua_S,NULL,"ESP_SPIDER",ESP_SPIDER);
 tolua_constant(tolua_S,NULL,"ESP_ALL",ESP_ALL);
 tolua_constant(tolua_S,NULL,"USE_EQUIP",USE_EQUIP);
 tolua_constant(tolua_S,NULL,"USE_INVEN",USE_INVEN);
 tolua_constant(tolua_S,NULL,"USE_FLOOR",USE_FLOOR);
 tolua_constant(tolua_S,NULL,"USE_EXTRA",USE_EXTRA);
 tolua_constant(tolua_S,NULL,"INVEN_WIELD",INVEN_WIELD);
 tolua_constant(tolua_S,NULL,"INVEN_BOW",INVEN_BOW);
 tolua_constant(tolua_S,NULL,"INVEN_RING",INVEN_RING);
 tolua_constant(tolua_S,NULL,"INVEN_NECK",INVEN_NECK);
 tolua_constant(tolua_S,NULL,"INVEN_LITE",INVEN_LITE);
 tolua_constant(tolua_S,NULL,"INVEN_BODY",INVEN_BODY);
 tolua_constant(tolua_S,NULL,"INVEN_OUTER",INVEN_OUTER);
 tolua_constant(tolua_S,NULL,"INVEN_ARM",INVEN_ARM);
 tolua_constant(tolua_S,NULL,"INVEN_HEAD",INVEN_HEAD);
 tolua_constant(tolua_S,NULL,"INVEN_HANDS",INVEN_HANDS);
 tolua_constant(tolua_S,NULL,"INVEN_FEET",INVEN_FEET);
 tolua_constant(tolua_S,NULL,"INVEN_CARRY",INVEN_CARRY);
 tolua_constant(tolua_S,NULL,"INVEN_AMMO",INVEN_AMMO);
 tolua_constant(tolua_S,NULL,"INVEN_TOOL",INVEN_TOOL);
 tolua_constant(tolua_S,NULL,"INVEN_TOTAL",INVEN_TOTAL);
 tolua_constant(tolua_S,NULL,"INVEN_EQ",INVEN_EQ);
 tolua_constant(tolua_S,NULL,"TV_SKELETON",TV_SKELETON);
 tolua_constant(tolua_S,NULL,"TV_BOTTLE",TV_BOTTLE);
 tolua_constant(tolua_S,NULL,"TV_BATERIE",TV_BATERIE);
 tolua_constant(tolua_S,NULL,"TV_SPIKE",TV_SPIKE);
 tolua_constant(tolua_S,NULL,"TV_MSTAFF",TV_MSTAFF);
 tolua_constant(tolua_S,NULL,"TV_CHEST",TV_CHEST);
 tolua_constant(tolua_S,NULL,"TV_PARCHMENT",TV_PARCHMENT);
 tolua_constant(tolua_S,NULL,"TV_PARCHEMENT",TV_PARCHEMENT);
 tolua_constant(tolua_S,NULL,"TV_CORPSE",TV_CORPSE);
 tolua_constant(tolua_S,NULL,"TV_EGG",TV_EGG);
 tolua_constant(tolua_S,NULL,"TV_JUNK",TV_JUNK);
 tolua_constant(tolua_S,NULL,"TV_TOOL",TV_TOOL);
 tolua_constant(tolua_S,NULL,"TV_INSTRUMENT",TV_INSTRUMENT);
 tolua_constant(tolua_S,NULL,"TV_BOOMERANG",TV_BOOMERANG);
 tolua_constant(tolua_S,NULL,"TV_SHOT",TV_SHOT);
 tolua_constant(tolua_S,NULL,"TV_ARROW",TV_ARROW);
 tolua_constant(tolua_S,NULL,"TV_BOLT",TV_BOLT);
 tolua_constant(tolua_S,NULL,"TV_BOW",TV_BOW);
 tolua_constant(tolua_S,NULL,"TV_DIGGING",TV_DIGGING);
 tolua_constant(tolua_S,NULL,"TV_HAFTED",TV_HAFTED);
 tolua_constant(tolua_S,NULL,"TV_POLEARM",TV_POLEARM);
 tolua_constant(tolua_S,NULL,"TV_SWORD",TV_SWORD);
 tolua_constant(tolua_S,NULL,"TV_AXE",TV_AXE);
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
 tolua_constant(tolua_S,NULL,"TV_TRAPKIT",TV_TRAPKIT);
 tolua_constant(tolua_S,NULL,"TV_STAFF",TV_STAFF);
 tolua_constant(tolua_S,NULL,"TV_WAND",TV_WAND);
 tolua_constant(tolua_S,NULL,"TV_ROD",TV_ROD);
 tolua_constant(tolua_S,NULL,"TV_ROD_MAIN",TV_ROD_MAIN);
 tolua_constant(tolua_S,NULL,"TV_SCROLL",TV_SCROLL);
 tolua_constant(tolua_S,NULL,"TV_POTION",TV_POTION);
 tolua_constant(tolua_S,NULL,"TV_POTION2",TV_POTION2);
 tolua_constant(tolua_S,NULL,"TV_FLASK",TV_FLASK);
 tolua_constant(tolua_S,NULL,"TV_FOOD",TV_FOOD);
 tolua_constant(tolua_S,NULL,"TV_HYPNOS",TV_HYPNOS);
 tolua_constant(tolua_S,NULL,"TV_GOLD",TV_GOLD);
 tolua_constant(tolua_S,NULL,"TV_RANDART",TV_RANDART);
 tolua_constant(tolua_S,NULL,"TV_RUNE1",TV_RUNE1);
 tolua_constant(tolua_S,NULL,"TV_RUNE2",TV_RUNE2);
 tolua_constant(tolua_S,NULL,"TV_BOOK",TV_BOOK);
 tolua_constant(tolua_S,NULL,"TV_SYMBIOTIC_BOOK",TV_SYMBIOTIC_BOOK);
 tolua_constant(tolua_S,NULL,"TV_MUSIC_BOOK",TV_MUSIC_BOOK);
 tolua_constant(tolua_S,NULL,"TV_DRUID_BOOK",TV_DRUID_BOOK);
 tolua_constant(tolua_S,NULL,"TV_DAEMON_BOOK",TV_DAEMON_BOOK);
 tolua_constant(tolua_S,NULL,"SV_TOOL_CLIMB",SV_TOOL_CLIMB);
 tolua_constant(tolua_S,NULL,"SV_PORTABLE_HOLE",SV_PORTABLE_HOLE);
 tolua_constant(tolua_S,NULL,"SV_MSTAFF",SV_MSTAFF);
 tolua_constant(tolua_S,NULL,"SV_AMMO_LIGHT",SV_AMMO_LIGHT);
 tolua_constant(tolua_S,NULL,"SV_AMMO_NORMAL",SV_AMMO_NORMAL);
 tolua_constant(tolua_S,NULL,"SV_AMMO_HEAVY",SV_AMMO_HEAVY);
 tolua_constant(tolua_S,NULL,"SV_DRUM",SV_DRUM);
 tolua_constant(tolua_S,NULL,"SV_HARP",SV_HARP);
 tolua_constant(tolua_S,NULL,"SV_HORN",SV_HORN);
 tolua_constant(tolua_S,NULL,"SV_TRAPKIT_SLING",SV_TRAPKIT_SLING);
 tolua_constant(tolua_S,NULL,"SV_TRAPKIT_BOW",SV_TRAPKIT_BOW);
 tolua_constant(tolua_S,NULL,"SV_TRAPKIT_XBOW",SV_TRAPKIT_XBOW);
 tolua_constant(tolua_S,NULL,"SV_TRAPKIT_POTION",SV_TRAPKIT_POTION);
 tolua_constant(tolua_S,NULL,"SV_TRAPKIT_SCROLL",SV_TRAPKIT_SCROLL);
 tolua_constant(tolua_S,NULL,"SV_TRAPKIT_DEVICE",SV_TRAPKIT_DEVICE);
 tolua_constant(tolua_S,NULL,"SV_BOOM_S_WOOD",SV_BOOM_S_WOOD);
 tolua_constant(tolua_S,NULL,"SV_BOOM_WOOD",SV_BOOM_WOOD);
 tolua_constant(tolua_S,NULL,"SV_BOOM_S_METAL",SV_BOOM_S_METAL);
 tolua_constant(tolua_S,NULL,"SV_BOOM_METAL",SV_BOOM_METAL);
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
 tolua_constant(tolua_S,NULL,"SV_WAR_HAMMER",SV_WAR_HAMMER);
 tolua_constant(tolua_S,NULL,"SV_LUCERN_HAMMER",SV_LUCERN_HAMMER);
 tolua_constant(tolua_S,NULL,"SV_THREE_PIECE_ROD",SV_THREE_PIECE_ROD);
 tolua_constant(tolua_S,NULL,"SV_MORNING_STAR",SV_MORNING_STAR);
 tolua_constant(tolua_S,NULL,"SV_FLAIL",SV_FLAIL);
 tolua_constant(tolua_S,NULL,"SV_LEAD_FILLED_MACE",SV_LEAD_FILLED_MACE);
 tolua_constant(tolua_S,NULL,"SV_TWO_HANDED_FLAIL",SV_TWO_HANDED_FLAIL);
 tolua_constant(tolua_S,NULL,"SV_GREAT_HAMMER",SV_GREAT_HAMMER);
 tolua_constant(tolua_S,NULL,"SV_MACE_OF_DISRUPTION",SV_MACE_OF_DISRUPTION);
 tolua_constant(tolua_S,NULL,"SV_GROND",SV_GROND);
 tolua_constant(tolua_S,NULL,"SV_HATCHET",SV_HATCHET);
 tolua_constant(tolua_S,NULL,"SV_CLEAVER",SV_CLEAVER);
 tolua_constant(tolua_S,NULL,"SV_LIGHT_WAR_AXE",SV_LIGHT_WAR_AXE);
 tolua_constant(tolua_S,NULL,"SV_BEAKED_AXE",SV_BEAKED_AXE);
 tolua_constant(tolua_S,NULL,"SV_BROAD_AXE",SV_BROAD_AXE);
 tolua_constant(tolua_S,NULL,"SV_BATTLE_AXE",SV_BATTLE_AXE);
 tolua_constant(tolua_S,NULL,"SV_GREAT_AXE",SV_GREAT_AXE);
 tolua_constant(tolua_S,NULL,"SV_LOCHABER_AXE",SV_LOCHABER_AXE);
 tolua_constant(tolua_S,NULL,"SV_SLAUGHTER_AXE",SV_SLAUGHTER_AXE);
 tolua_constant(tolua_S,NULL,"SV_SPEAR",SV_SPEAR);
 tolua_constant(tolua_S,NULL,"SV_SICKLE",SV_SICKLE);
 tolua_constant(tolua_S,NULL,"SV_AWL_PIKE",SV_AWL_PIKE);
 tolua_constant(tolua_S,NULL,"SV_TRIDENT",SV_TRIDENT);
 tolua_constant(tolua_S,NULL,"SV_FAUCHARD",SV_FAUCHARD);
 tolua_constant(tolua_S,NULL,"SV_BROAD_SPEAR",SV_BROAD_SPEAR);
 tolua_constant(tolua_S,NULL,"SV_PIKE",SV_PIKE);
 tolua_constant(tolua_S,NULL,"SV_GLAIVE",SV_GLAIVE);
 tolua_constant(tolua_S,NULL,"SV_HALBERD",SV_HALBERD);
 tolua_constant(tolua_S,NULL,"SV_GUISARME",SV_GUISARME);
 tolua_constant(tolua_S,NULL,"SV_SCYTHE",SV_SCYTHE);
 tolua_constant(tolua_S,NULL,"SV_LANCE",SV_LANCE);
 tolua_constant(tolua_S,NULL,"SV_TRIFURCATE_SPEAR",SV_TRIFURCATE_SPEAR);
 tolua_constant(tolua_S,NULL,"SV_HEAVY_LANCE",SV_HEAVY_LANCE);
 tolua_constant(tolua_S,NULL,"SV_SCYTHE_OF_SLICING",SV_SCYTHE_OF_SLICING);
 tolua_constant(tolua_S,NULL,"SV_BROKEN_DAGGER",SV_BROKEN_DAGGER);
 tolua_constant(tolua_S,NULL,"SV_BROKEN_SWORD",SV_BROKEN_SWORD);
 tolua_constant(tolua_S,NULL,"SV_DAGGER",SV_DAGGER);
 tolua_constant(tolua_S,NULL,"SV_MAIN_GAUCHE",SV_MAIN_GAUCHE);
 tolua_constant(tolua_S,NULL,"SV_RAPIER",SV_RAPIER);
 tolua_constant(tolua_S,NULL,"SV_SMALL_SWORD",SV_SMALL_SWORD);
 tolua_constant(tolua_S,NULL,"SV_BASILLARD",SV_BASILLARD);
 tolua_constant(tolua_S,NULL,"SV_SHORT_SWORD",SV_SHORT_SWORD);
 tolua_constant(tolua_S,NULL,"SV_SABRE",SV_SABRE);
 tolua_constant(tolua_S,NULL,"SV_CUTLASS",SV_CUTLASS);
 tolua_constant(tolua_S,NULL,"SV_KHOPESH",SV_KHOPESH);
 tolua_constant(tolua_S,NULL,"SV_TULWAR",SV_TULWAR);
 tolua_constant(tolua_S,NULL,"SV_BROAD_SWORD",SV_BROAD_SWORD);
 tolua_constant(tolua_S,NULL,"SV_LONG_SWORD",SV_LONG_SWORD);
 tolua_constant(tolua_S,NULL,"SV_SCIMITAR",SV_SCIMITAR);
 tolua_constant(tolua_S,NULL,"SV_KATANA",SV_KATANA);
 tolua_constant(tolua_S,NULL,"SV_BASTARD_SWORD",SV_BASTARD_SWORD);
 tolua_constant(tolua_S,NULL,"SV_GREAT_SCIMITAR",SV_GREAT_SCIMITAR);
 tolua_constant(tolua_S,NULL,"SV_CLAYMORE",SV_CLAYMORE);
 tolua_constant(tolua_S,NULL,"SV_ESPADON",SV_ESPADON);
 tolua_constant(tolua_S,NULL,"SV_TWO_HANDED_SWORD",SV_TWO_HANDED_SWORD);
 tolua_constant(tolua_S,NULL,"SV_FLAMBERGE",SV_FLAMBERGE);
 tolua_constant(tolua_S,NULL,"SV_EXECUTIONERS_SWORD",SV_EXECUTIONERS_SWORD);
 tolua_constant(tolua_S,NULL,"SV_ZWEIHANDER",SV_ZWEIHANDER);
 tolua_constant(tolua_S,NULL,"SV_BLADE_OF_CHAOS",SV_BLADE_OF_CHAOS);
 tolua_constant(tolua_S,NULL,"SV_BLUESTEEL_BLADE",SV_BLUESTEEL_BLADE);
 tolua_constant(tolua_S,NULL,"SV_SHADOW_BLADE",SV_SHADOW_BLADE);
 tolua_constant(tolua_S,NULL,"SV_DARK_SWORD",SV_DARK_SWORD);
 tolua_constant(tolua_S,NULL,"SV_SMALL_LEATHER_SHIELD",SV_SMALL_LEATHER_SHIELD);
 tolua_constant(tolua_S,NULL,"SV_SMALL_METAL_SHIELD",SV_SMALL_METAL_SHIELD);
 tolua_constant(tolua_S,NULL,"SV_LARGE_LEATHER_SHIELD",SV_LARGE_LEATHER_SHIELD);
 tolua_constant(tolua_S,NULL,"SV_LARGE_METAL_SHIELD",SV_LARGE_METAL_SHIELD);
 tolua_constant(tolua_S,NULL,"SV_DRAGON_SHIELD",SV_DRAGON_SHIELD);
 tolua_constant(tolua_S,NULL,"SV_SHIELD_OF_DEFLECTION",SV_SHIELD_OF_DEFLECTION);
 tolua_constant(tolua_S,NULL,"SV_HARD_LEATHER_CAP",SV_HARD_LEATHER_CAP);
 tolua_constant(tolua_S,NULL,"SV_METAL_CAP",SV_METAL_CAP);
 tolua_constant(tolua_S,NULL,"SV_IRON_HELM",SV_IRON_HELM);
 tolua_constant(tolua_S,NULL,"SV_STEEL_HELM",SV_STEEL_HELM);
 tolua_constant(tolua_S,NULL,"SV_DRAGON_HELM",SV_DRAGON_HELM);
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
 tolua_constant(tolua_S,NULL,"SV_THUNDERLORD_SUIT",SV_THUNDERLORD_SUIT);
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
 tolua_constant(tolua_S,NULL,"SV_PARTIAL_PLATE_ARMOUR",SV_PARTIAL_PLATE_ARMOUR);
 tolua_constant(tolua_S,NULL,"SV_METAL_LAMELLAR_ARMOUR",SV_METAL_LAMELLAR_ARMOUR);
 tolua_constant(tolua_S,NULL,"SV_FULL_PLATE_ARMOUR",SV_FULL_PLATE_ARMOUR);
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
 tolua_constant(tolua_S,NULL,"SV_LITE_TORCH_EVER",SV_LITE_TORCH_EVER);
 tolua_constant(tolua_S,NULL,"SV_LITE_DWARVEN",SV_LITE_DWARVEN);
 tolua_constant(tolua_S,NULL,"SV_LITE_FEANORIAN",SV_LITE_FEANORIAN);
 tolua_constant(tolua_S,NULL,"SV_LITE_GALADRIEL",SV_LITE_GALADRIEL);
 tolua_constant(tolua_S,NULL,"SV_LITE_ELENDIL",SV_LITE_ELENDIL);
 tolua_constant(tolua_S,NULL,"SV_LITE_THRAIN",SV_LITE_THRAIN);
 tolua_constant(tolua_S,NULL,"SV_LITE_UNDEATH",SV_LITE_UNDEATH);
 tolua_constant(tolua_S,NULL,"SV_LITE_PALANTIR",SV_LITE_PALANTIR);
 tolua_constant(tolua_S,NULL,"SV_ANCHOR_SPACETIME",SV_ANCHOR_SPACETIME);
 tolua_constant(tolua_S,NULL,"SV_STONE_LORE",SV_STONE_LORE);
 tolua_constant(tolua_S,NULL,"SV_AMULET_DOOM",SV_AMULET_DOOM);
 tolua_constant(tolua_S,NULL,"SV_AMULET_TELEPORT",SV_AMULET_TELEPORT);
 tolua_constant(tolua_S,NULL,"SV_AMULET_ADORNMENT",SV_AMULET_ADORNMENT);
 tolua_constant(tolua_S,NULL,"SV_AMULET_SLOW_DIGEST",SV_AMULET_SLOW_DIGEST);
 tolua_constant(tolua_S,NULL,"SV_AMULET_RESIST_ACID",SV_AMULET_RESIST_ACID);
 tolua_constant(tolua_S,NULL,"SV_AMULET_SEARCHING",SV_AMULET_SEARCHING);
 tolua_constant(tolua_S,NULL,"SV_AMULET_BRILLANCE",SV_AMULET_BRILLANCE);
 tolua_constant(tolua_S,NULL,"SV_AMULET_CHARISMA",SV_AMULET_CHARISMA);
 tolua_constant(tolua_S,NULL,"SV_AMULET_THE_MAGI",SV_AMULET_THE_MAGI);
 tolua_constant(tolua_S,NULL,"SV_AMULET_REFLECTION",SV_AMULET_REFLECTION);
 tolua_constant(tolua_S,NULL,"SV_AMULET_CARLAMMAS",SV_AMULET_CARLAMMAS);
 tolua_constant(tolua_S,NULL,"SV_AMULET_INGWE",SV_AMULET_INGWE);
 tolua_constant(tolua_S,NULL,"SV_AMULET_DWARVES",SV_AMULET_DWARVES);
 tolua_constant(tolua_S,NULL,"SV_AMULET_NO_MAGIC",SV_AMULET_NO_MAGIC);
 tolua_constant(tolua_S,NULL,"SV_AMULET_NO_TELE",SV_AMULET_NO_TELE);
 tolua_constant(tolua_S,NULL,"SV_AMULET_RESISTANCE",SV_AMULET_RESISTANCE);
 tolua_constant(tolua_S,NULL,"SV_AMULET_NOTHING",SV_AMULET_NOTHING);
 tolua_constant(tolua_S,NULL,"SV_AMULET_SERPENT",SV_AMULET_SERPENT);
 tolua_constant(tolua_S,NULL,"SV_AMULET_TORIS_MEJISTOS",SV_AMULET_TORIS_MEJISTOS);
 tolua_constant(tolua_S,NULL,"SV_AMULET_TRICKERY",SV_AMULET_TRICKERY);
 tolua_constant(tolua_S,NULL,"SV_AMULET_DEVOTION",SV_AMULET_DEVOTION);
 tolua_constant(tolua_S,NULL,"SV_AMULET_WEAPONMASTERY",SV_AMULET_WEAPONMASTERY);
 tolua_constant(tolua_S,NULL,"SV_AMULET_WISDOM",SV_AMULET_WISDOM);
 tolua_constant(tolua_S,NULL,"SV_AMULET_INFRA",SV_AMULET_INFRA);
 tolua_constant(tolua_S,NULL,"SV_AMULET_SPELL",SV_AMULET_SPELL);
 tolua_constant(tolua_S,NULL,"SV_RING_WOE",SV_RING_WOE);
 tolua_constant(tolua_S,NULL,"SV_RING_AGGRAVATION",SV_RING_AGGRAVATION);
 tolua_constant(tolua_S,NULL,"SV_RING_WEAKNESS",SV_RING_WEAKNESS);
 tolua_constant(tolua_S,NULL,"SV_RING_STUPIDITY",SV_RING_STUPIDITY);
 tolua_constant(tolua_S,NULL,"SV_RING_TELEPORTATION",SV_RING_TELEPORTATION);
 tolua_constant(tolua_S,NULL,"SV_RING_SPECIAL",SV_RING_SPECIAL);
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
 tolua_constant(tolua_S,NULL,"SV_RING_NOTHING",SV_RING_NOTHING);
 tolua_constant(tolua_S,NULL,"SV_RING_PRECONITION",SV_RING_PRECONITION);
 tolua_constant(tolua_S,NULL,"SV_RING_FLAR",SV_RING_FLAR);
 tolua_constant(tolua_S,NULL,"SV_RING_INVIS",SV_RING_INVIS);
 tolua_constant(tolua_S,NULL,"SV_RING_FLYING",SV_RING_FLYING);
 tolua_constant(tolua_S,NULL,"SV_RING_WRAITH",SV_RING_WRAITH);
 tolua_constant(tolua_S,NULL,"SV_RING_ELEC",SV_RING_ELEC);
 tolua_constant(tolua_S,NULL,"SV_RING_CRIT",SV_RING_CRIT);
 tolua_constant(tolua_S,NULL,"SV_RING_SPELL",SV_RING_SPELL);
 tolua_constant(tolua_S,NULL,"SV_STAFF_SCHOOL",SV_STAFF_SCHOOL);
 tolua_constant(tolua_S,NULL,"SV_STAFF_NOTHING",SV_STAFF_NOTHING);
 tolua_constant(tolua_S,NULL,"SV_WAND_SCHOOL",SV_WAND_SCHOOL);
 tolua_constant(tolua_S,NULL,"SV_WAND_NOTHING",SV_WAND_NOTHING);
 tolua_constant(tolua_S,NULL,"SV_ROD_NOTHING",SV_ROD_NOTHING);
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
 tolua_constant(tolua_S,NULL,"SV_ROD_DETECT_TRAP",SV_ROD_DETECT_TRAP);
 tolua_constant(tolua_S,NULL,"SV_ROD_HOME",SV_ROD_HOME);
 tolua_constant(tolua_S,NULL,"SV_ROD_WOODEN",SV_ROD_WOODEN);
 tolua_constant(tolua_S,NULL,"SV_ROD_COPPER",SV_ROD_COPPER);
 tolua_constant(tolua_S,NULL,"SV_ROD_IRON",SV_ROD_IRON);
 tolua_constant(tolua_S,NULL,"SV_ROD_ALUMINIUM",SV_ROD_ALUMINIUM);
 tolua_constant(tolua_S,NULL,"SV_ROD_SILVER",SV_ROD_SILVER);
 tolua_constant(tolua_S,NULL,"SV_ROD_GOLDEN",SV_ROD_GOLDEN);
 tolua_constant(tolua_S,NULL,"SV_ROD_MITHRIL",SV_ROD_MITHRIL);
 tolua_constant(tolua_S,NULL,"SV_ROD_ADMANTITE",SV_ROD_ADMANTITE);
 tolua_constant(tolua_S,NULL,"SV_SCROLL_DARKNESS",SV_SCROLL_DARKNESS);
 tolua_constant(tolua_S,NULL,"SV_SCROLL_AGGRAVATE_MONSTER",SV_SCROLL_AGGRAVATE_MONSTER);
 tolua_constant(tolua_S,NULL,"SV_SCROLL_CURSE_ARMOR",SV_SCROLL_CURSE_ARMOR);
 tolua_constant(tolua_S,NULL,"SV_SCROLL_CURSE_WEAPON",SV_SCROLL_CURSE_WEAPON);
 tolua_constant(tolua_S,NULL,"SV_SCROLL_SUMMON_MONSTER",SV_SCROLL_SUMMON_MONSTER);
 tolua_constant(tolua_S,NULL,"SV_SCROLL_SUMMON_UNDEAD",SV_SCROLL_SUMMON_UNDEAD);
 tolua_constant(tolua_S,NULL,"SV_SCROLL_SUMMON_MINE",SV_SCROLL_SUMMON_MINE);
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
 tolua_constant(tolua_S,NULL,"SV_SCROLL_ENCHANT_WEAPON_PVAL",SV_SCROLL_ENCHANT_WEAPON_PVAL);
 tolua_constant(tolua_S,NULL,"SV_SCROLL_STAR_ENCHANT_ARMOR",SV_SCROLL_STAR_ENCHANT_ARMOR);
 tolua_constant(tolua_S,NULL,"SV_SCROLL_STAR_ENCHANT_WEAPON",SV_SCROLL_STAR_ENCHANT_WEAPON);
 tolua_constant(tolua_S,NULL,"SV_SCROLL_RECHARGING",SV_SCROLL_RECHARGING);
 tolua_constant(tolua_S,NULL,"SV_SCROLL_RESET_RECALL",SV_SCROLL_RESET_RECALL);
 tolua_constant(tolua_S,NULL,"SV_SCROLL_LIGHT",SV_SCROLL_LIGHT);
 tolua_constant(tolua_S,NULL,"SV_SCROLL_MAPPING",SV_SCROLL_MAPPING);
 tolua_constant(tolua_S,NULL,"SV_SCROLL_DETECT_GOLD",SV_SCROLL_DETECT_GOLD);
 tolua_constant(tolua_S,NULL,"SV_SCROLL_DETECT_ITEM",SV_SCROLL_DETECT_ITEM);
 tolua_constant(tolua_S,NULL,"SV_SCROLL_DETECT_TRAP",SV_SCROLL_DETECT_TRAP);
 tolua_constant(tolua_S,NULL,"SV_SCROLL_DETECT_DOOR",SV_SCROLL_DETECT_DOOR);
 tolua_constant(tolua_S,NULL,"SV_SCROLL_DETECT_INVIS",SV_SCROLL_DETECT_INVIS);
 tolua_constant(tolua_S,NULL,"SV_SCROLL_DIVINATION",SV_SCROLL_DIVINATION);
 tolua_constant(tolua_S,NULL,"SV_SCROLL_SATISFY_HUNGER",SV_SCROLL_SATISFY_HUNGER);
 tolua_constant(tolua_S,NULL,"SV_SCROLL_BLESSING",SV_SCROLL_BLESSING);
 tolua_constant(tolua_S,NULL,"SV_SCROLL_HOLY_CHANT",SV_SCROLL_HOLY_CHANT);
 tolua_constant(tolua_S,NULL,"SV_SCROLL_HOLY_PRAYER",SV_SCROLL_HOLY_PRAYER);
 tolua_constant(tolua_S,NULL,"SV_SCROLL_MONSTER_CONFUSION",SV_SCROLL_MONSTER_CONFUSION);
 tolua_constant(tolua_S,NULL,"SV_SCROLL_PROTECTION_FROM_EVIL",SV_SCROLL_PROTECTION_FROM_EVIL);
 tolua_constant(tolua_S,NULL,"SV_SCROLL_RUNE_OF_PROTECTION",SV_SCROLL_RUNE_OF_PROTECTION);
 tolua_constant(tolua_S,NULL,"SV_SCROLL_TRAP_DOOR_DESTRUCTION",SV_SCROLL_TRAP_DOOR_DESTRUCTION);
 tolua_constant(tolua_S,NULL,"SV_SCROLL_DEINCARNATION",SV_SCROLL_DEINCARNATION);
 tolua_constant(tolua_S,NULL,"SV_SCROLL_STAR_DESTRUCTION",SV_SCROLL_STAR_DESTRUCTION);
 tolua_constant(tolua_S,NULL,"SV_SCROLL_DISPEL_UNDEAD",SV_SCROLL_DISPEL_UNDEAD);
 tolua_constant(tolua_S,NULL,"SV_SCROLL_MASS_RESURECTION",SV_SCROLL_MASS_RESURECTION);
 tolua_constant(tolua_S,NULL,"SV_SCROLL_GENOCIDE",SV_SCROLL_GENOCIDE);
 tolua_constant(tolua_S,NULL,"SV_SCROLL_MASS_GENOCIDE",SV_SCROLL_MASS_GENOCIDE);
 tolua_constant(tolua_S,NULL,"SV_SCROLL_ACQUIREMENT",SV_SCROLL_ACQUIREMENT);
 tolua_constant(tolua_S,NULL,"SV_SCROLL_STAR_ACQUIREMENT",SV_SCROLL_STAR_ACQUIREMENT);
 tolua_constant(tolua_S,NULL,"SV_SCROLL_FIRE",SV_SCROLL_FIRE);
 tolua_constant(tolua_S,NULL,"SV_SCROLL_ICE",SV_SCROLL_ICE);
 tolua_constant(tolua_S,NULL,"SV_SCROLL_CHAOS",SV_SCROLL_CHAOS);
 tolua_constant(tolua_S,NULL,"SV_SCROLL_RUMOR",SV_SCROLL_RUMOR);
 tolua_constant(tolua_S,NULL,"SV_SCROLL_ARTIFACT",SV_SCROLL_ARTIFACT);
 tolua_constant(tolua_S,NULL,"SV_SCROLL_NOTHING",SV_SCROLL_NOTHING);
 tolua_constant(tolua_S,NULL,"SV_POTION_WATER",SV_POTION_WATER);
 tolua_constant(tolua_S,NULL,"SV_POTION_APPLE_JUICE",SV_POTION_APPLE_JUICE);
 tolua_constant(tolua_S,NULL,"SV_POTION_SLIME_MOLD",SV_POTION_SLIME_MOLD);
 tolua_constant(tolua_S,NULL,"SV_POTION_BLOOD",SV_POTION_BLOOD);
 tolua_constant(tolua_S,NULL,"SV_POTION_SLOWNESS",SV_POTION_SLOWNESS);
 tolua_constant(tolua_S,NULL,"SV_POTION_SALT_WATER",SV_POTION_SALT_WATER);
 tolua_constant(tolua_S,NULL,"SV_POTION_POISON",SV_POTION_POISON);
 tolua_constant(tolua_S,NULL,"SV_POTION_BLINDNESS",SV_POTION_BLINDNESS);
 tolua_constant(tolua_S,NULL,"SV_POTION_INVIS",SV_POTION_INVIS);
 tolua_constant(tolua_S,NULL,"SV_POTION_CONFUSION",SV_POTION_CONFUSION);
 tolua_constant(tolua_S,NULL,"SV_POTION_MUTATION",SV_POTION_MUTATION);
 tolua_constant(tolua_S,NULL,"SV_POTION_SLEEP",SV_POTION_SLEEP);
 tolua_constant(tolua_S,NULL,"SV_POTION_LEARNING",SV_POTION_LEARNING);
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
 tolua_constant(tolua_S,NULL,"SV_POTION_BESERK_STRENGTH",SV_POTION_BESERK_STRENGTH);
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
 tolua_constant(tolua_S,NULL,"SV_POTION_LAST",SV_POTION_LAST);
 tolua_constant(tolua_S,NULL,"SV_POTION2_MIMIC",SV_POTION2_MIMIC);
 tolua_constant(tolua_S,NULL,"SV_POTION2_CURE_LIGHT_SANITY",SV_POTION2_CURE_LIGHT_SANITY);
 tolua_constant(tolua_S,NULL,"SV_POTION2_CURE_SERIOUS_SANITY",SV_POTION2_CURE_SERIOUS_SANITY);
 tolua_constant(tolua_S,NULL,"SV_POTION2_CURE_CRITICAL_SANITY",SV_POTION2_CURE_CRITICAL_SANITY);
 tolua_constant(tolua_S,NULL,"SV_POTION2_CURE_SANITY",SV_POTION2_CURE_SANITY);
 tolua_constant(tolua_S,NULL,"SV_POTION2_CURE_WATER",SV_POTION2_CURE_WATER);
 tolua_constant(tolua_S,NULL,"SV_POTION2_LAST",SV_POTION2_LAST);
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
 tolua_constant(tolua_S,NULL,"SV_FOOD_ATHELAS",SV_FOOD_ATHELAS);
 tolua_constant(tolua_S,NULL,"SV_FOOD_GREAT_HEALTH",SV_FOOD_GREAT_HEALTH);
 tolua_constant(tolua_S,NULL,"SV_FOOD_FORTUNE_COOKIE",SV_FOOD_FORTUNE_COOKIE);
 tolua_constant(tolua_S,NULL,"SV_BATERIE_POISON",SV_BATERIE_POISON);
 tolua_constant(tolua_S,NULL,"SV_BATERIE_EXPLOSION",SV_BATERIE_EXPLOSION);
 tolua_constant(tolua_S,NULL,"SV_BATERIE_TELEPORT",SV_BATERIE_TELEPORT);
 tolua_constant(tolua_S,NULL,"SV_BATERIE_COLD",SV_BATERIE_COLD);
 tolua_constant(tolua_S,NULL,"SV_BATERIE_FIRE",SV_BATERIE_FIRE);
 tolua_constant(tolua_S,NULL,"SV_BATERIE_ACID",SV_BATERIE_ACID);
 tolua_constant(tolua_S,NULL,"SV_BATERIE_LIFE",SV_BATERIE_LIFE);
 tolua_constant(tolua_S,NULL,"SV_BATERIE_CONFUSION",SV_BATERIE_CONFUSION);
 tolua_constant(tolua_S,NULL,"SV_BATERIE_LITE",SV_BATERIE_LITE);
 tolua_constant(tolua_S,NULL,"SV_BATERIE_CHAOS",SV_BATERIE_CHAOS);
 tolua_constant(tolua_S,NULL,"SV_BATERIE_TIME",SV_BATERIE_TIME);
 tolua_constant(tolua_S,NULL,"SV_BATERIE_MAGIC",SV_BATERIE_MAGIC);
 tolua_constant(tolua_S,NULL,"SV_BATERIE_XTRA_LIFE",SV_BATERIE_XTRA_LIFE);
 tolua_constant(tolua_S,NULL,"SV_BATERIE_DARKNESS",SV_BATERIE_DARKNESS);
 tolua_constant(tolua_S,NULL,"SV_BATERIE_KNOWLEDGE",SV_BATERIE_KNOWLEDGE);
 tolua_constant(tolua_S,NULL,"SV_BATERIE_FORCE",SV_BATERIE_FORCE);
 tolua_constant(tolua_S,NULL,"SV_BATERIE_LIGHTNING",SV_BATERIE_LIGHTNING);
 tolua_constant(tolua_S,NULL,"SV_BATERIE_MANA",SV_BATERIE_MANA);
 tolua_constant(tolua_S,NULL,"SV_CORPSE_CORPSE",SV_CORPSE_CORPSE);
 tolua_constant(tolua_S,NULL,"SV_CORPSE_SKELETON",SV_CORPSE_SKELETON);
 tolua_constant(tolua_S,NULL,"SV_CORPSE_HEAD",SV_CORPSE_HEAD);
 tolua_constant(tolua_S,NULL,"SV_CORPSE_SKULL",SV_CORPSE_SKULL);
 tolua_constant(tolua_S,NULL,"SV_CORPSE_MEAT",SV_CORPSE_MEAT);
 tolua_constant(tolua_S,NULL,"SV_DEMONBLADE",SV_DEMONBLADE);
 tolua_constant(tolua_S,NULL,"SV_DEMONSHIELD",SV_DEMONSHIELD);
 tolua_constant(tolua_S,NULL,"SV_DEMONHORN",SV_DEMONHORN);
 tolua_constant(tolua_S,NULL,"IDENT_SENSE",IDENT_SENSE);
 tolua_constant(tolua_S,NULL,"IDENT_FIXED",IDENT_FIXED);
 tolua_constant(tolua_S,NULL,"IDENT_EMPTY",IDENT_EMPTY);
 tolua_constant(tolua_S,NULL,"IDENT_KNOWN",IDENT_KNOWN);
 tolua_constant(tolua_S,NULL,"IDENT_STOREB",IDENT_STOREB);
 tolua_constant(tolua_S,NULL,"IDENT_MENTAL",IDENT_MENTAL);
 tolua_constant(tolua_S,NULL,"IDENT_CURSED",IDENT_CURSED);
 tolua_constant(tolua_S,NULL,"OBJ_FOUND_MONSTER",OBJ_FOUND_MONSTER);
 tolua_constant(tolua_S,NULL,"OBJ_FOUND_FLOOR",OBJ_FOUND_FLOOR);
 tolua_constant(tolua_S,NULL,"OBJ_FOUND_VAULT",OBJ_FOUND_VAULT);
 tolua_constant(tolua_S,NULL,"OBJ_FOUND_SPECIAL",OBJ_FOUND_SPECIAL);
 tolua_constant(tolua_S,NULL,"OBJ_FOUND_RUBBLE",OBJ_FOUND_RUBBLE);
 tolua_constant(tolua_S,NULL,"OBJ_FOUND_REWARD",OBJ_FOUND_REWARD);
 tolua_constant(tolua_S,NULL,"OBJ_FOUND_STORE",OBJ_FOUND_STORE);
 tolua_cclass(tolua_S,"obj_theme","");
 tolua_tablevar(tolua_S,"obj_theme","treasure",toluaI_get_object_obj_theme_treasure,toluaI_set_object_obj_theme_treasure);
 tolua_tablevar(tolua_S,"obj_theme","combat",toluaI_get_object_obj_theme_combat,toluaI_set_object_obj_theme_combat);
 tolua_tablevar(tolua_S,"obj_theme","magic",toluaI_get_object_obj_theme_magic,toluaI_set_object_obj_theme_magic);
 tolua_tablevar(tolua_S,"obj_theme","tools",toluaI_get_object_obj_theme_tools,toluaI_set_object_obj_theme_tools);
 tolua_cclass(tolua_S,"object_kind","");
 tolua_tablevar(tolua_S,"object_kind","name",toluaI_get_object_object_kind_name,toluaI_set_object_object_kind_name);
 tolua_tablevar(tolua_S,"object_kind","text",toluaI_get_object_object_kind_text,toluaI_set_object_object_kind_text);
 tolua_tablevar(tolua_S,"object_kind","tval",toluaI_get_object_object_kind_tval,toluaI_set_object_object_kind_tval);
 tolua_tablevar(tolua_S,"object_kind","sval",toluaI_get_object_object_kind_sval,toluaI_set_object_object_kind_sval);
 tolua_tablevar(tolua_S,"object_kind","pval",toluaI_get_object_object_kind_pval,toluaI_set_object_object_kind_pval);
 tolua_tablevar(tolua_S,"object_kind","pval2",toluaI_get_object_object_kind_pval2,toluaI_set_object_object_kind_pval2);
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
 tolua_tablevar(tolua_S,"object_kind","flags4",toluaI_get_object_object_kind_flags4,toluaI_set_object_object_kind_flags4);
 tolua_tablevar(tolua_S,"object_kind","flags5",toluaI_get_object_object_kind_flags5,toluaI_set_object_object_kind_flags5);
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
 tolua_tablevar(tolua_S,"object_kind","know",toluaI_get_object_object_kind_know,toluaI_set_object_object_kind_know);
 tolua_tablevar(tolua_S,"object_kind","esp",toluaI_get_object_object_kind_esp,toluaI_set_object_object_kind_esp);
 tolua_tablevar(tolua_S,"object_kind","btval",toluaI_get_object_object_kind_btval,toluaI_set_object_object_kind_btval);
 tolua_tablevar(tolua_S,"object_kind","bsval",toluaI_get_object_object_kind_bsval,toluaI_set_object_object_kind_bsval);
 tolua_tablevar(tolua_S,"object_kind","artifact",toluaI_get_object_object_kind_artifact,toluaI_set_object_object_kind_artifact);
 tolua_tablevar(tolua_S,"object_kind","power",toluaI_get_object_object_kind_power,toluaI_set_object_object_kind_power);
 tolua_cclass(tolua_S,"artifact_type","");
 tolua_tablevar(tolua_S,"artifact_type","name",toluaI_get_object_artifact_type_name,toluaI_set_object_artifact_type_name);
 tolua_tablevar(tolua_S,"artifact_type","text",toluaI_get_object_artifact_type_text,toluaI_set_object_artifact_type_text);
 tolua_tablevar(tolua_S,"artifact_type","tval",toluaI_get_object_artifact_type_tval,toluaI_set_object_artifact_type_tval);
 tolua_tablevar(tolua_S,"artifact_type","sval",toluaI_get_object_artifact_type_sval,toluaI_set_object_artifact_type_sval);
 tolua_tablevar(tolua_S,"artifact_type","pval",toluaI_get_object_artifact_type_pval,toluaI_set_object_artifact_type_pval);
 tolua_tablevar(tolua_S,"artifact_type","to_h",toluaI_get_object_artifact_type_to_h,toluaI_set_object_artifact_type_to_h);
 tolua_tablevar(tolua_S,"artifact_type","to_d",toluaI_get_object_artifact_type_to_d,toluaI_set_object_artifact_type_to_d);
 tolua_tablevar(tolua_S,"artifact_type","to_a",toluaI_get_object_artifact_type_to_a,toluaI_set_object_artifact_type_to_a);
 tolua_tablevar(tolua_S,"artifact_type","ac",toluaI_get_object_artifact_type_ac,toluaI_set_object_artifact_type_ac);
 tolua_tablevar(tolua_S,"artifact_type","dd",toluaI_get_object_artifact_type_dd,toluaI_set_object_artifact_type_dd);
 tolua_tablevar(tolua_S,"artifact_type","ds",toluaI_get_object_artifact_type_ds,toluaI_set_object_artifact_type_ds);
 tolua_tablevar(tolua_S,"artifact_type","weight",toluaI_get_object_artifact_type_weight,toluaI_set_object_artifact_type_weight);
 tolua_tablevar(tolua_S,"artifact_type","cost",toluaI_get_object_artifact_type_cost,toluaI_set_object_artifact_type_cost);
 tolua_tablevar(tolua_S,"artifact_type","flags1",toluaI_get_object_artifact_type_flags1,toluaI_set_object_artifact_type_flags1);
 tolua_tablevar(tolua_S,"artifact_type","flags2",toluaI_get_object_artifact_type_flags2,toluaI_set_object_artifact_type_flags2);
 tolua_tablevar(tolua_S,"artifact_type","flags3",toluaI_get_object_artifact_type_flags3,toluaI_set_object_artifact_type_flags3);
 tolua_tablevar(tolua_S,"artifact_type","flags4",toluaI_get_object_artifact_type_flags4,toluaI_set_object_artifact_type_flags4);
 tolua_tablevar(tolua_S,"artifact_type","flags5",toluaI_get_object_artifact_type_flags5,toluaI_set_object_artifact_type_flags5);
 tolua_tablevar(tolua_S,"artifact_type","level",toluaI_get_object_artifact_type_level,toluaI_set_object_artifact_type_level);
 tolua_tablevar(tolua_S,"artifact_type","rarity",toluaI_get_object_artifact_type_rarity,toluaI_set_object_artifact_type_rarity);
 tolua_tablevar(tolua_S,"artifact_type","cur_num",toluaI_get_object_artifact_type_cur_num,toluaI_set_object_artifact_type_cur_num);
 tolua_tablevar(tolua_S,"artifact_type","max_num",toluaI_get_object_artifact_type_max_num,toluaI_set_object_artifact_type_max_num);
 tolua_tablevar(tolua_S,"artifact_type","esp",toluaI_get_object_artifact_type_esp,toluaI_set_object_artifact_type_esp);
 tolua_tablevar(tolua_S,"artifact_type","power",toluaI_get_object_artifact_type_power,toluaI_set_object_artifact_type_power);
 tolua_cclass(tolua_S,"ego_item_type","");
 tolua_tablevar(tolua_S,"ego_item_type","name",toluaI_get_object_ego_item_type_name,toluaI_set_object_ego_item_type_name);
 tolua_tablevar(tolua_S,"ego_item_type","text",toluaI_get_object_ego_item_type_text,toluaI_set_object_ego_item_type_text);
 tolua_tablevar(tolua_S,"ego_item_type","before",toluaI_get_object_ego_item_type_before,toluaI_set_object_ego_item_type_before);
 tolua_tablearray(tolua_S,"ego_item_type","tval",toluaI_get_object_ego_item_type_tval,toluaI_set_object_ego_item_type_tval);
 tolua_tablearray(tolua_S,"ego_item_type","min_sval",toluaI_get_object_ego_item_type_min_sval,toluaI_set_object_ego_item_type_min_sval);
 tolua_tablearray(tolua_S,"ego_item_type","max_sval",toluaI_get_object_ego_item_type_max_sval,toluaI_set_object_ego_item_type_max_sval);
 tolua_tablevar(tolua_S,"ego_item_type","rating",toluaI_get_object_ego_item_type_rating,toluaI_set_object_ego_item_type_rating);
 tolua_tablevar(tolua_S,"ego_item_type","level",toluaI_get_object_ego_item_type_level,toluaI_set_object_ego_item_type_level);
 tolua_tablevar(tolua_S,"ego_item_type","rarity",toluaI_get_object_ego_item_type_rarity,toluaI_set_object_ego_item_type_rarity);
 tolua_tablevar(tolua_S,"ego_item_type","mrarity",toluaI_get_object_ego_item_type_mrarity,toluaI_set_object_ego_item_type_mrarity);
 tolua_tablevar(tolua_S,"ego_item_type","max_to_h",toluaI_get_object_ego_item_type_max_to_h,toluaI_set_object_ego_item_type_max_to_h);
 tolua_tablevar(tolua_S,"ego_item_type","max_to_d",toluaI_get_object_ego_item_type_max_to_d,toluaI_set_object_ego_item_type_max_to_d);
 tolua_tablevar(tolua_S,"ego_item_type","max_to_a",toluaI_get_object_ego_item_type_max_to_a,toluaI_set_object_ego_item_type_max_to_a);
 tolua_tablevar(tolua_S,"ego_item_type","max_pval",toluaI_get_object_ego_item_type_max_pval,toluaI_set_object_ego_item_type_max_pval);
 tolua_tablevar(tolua_S,"ego_item_type","cost",toluaI_get_object_ego_item_type_cost,toluaI_set_object_ego_item_type_cost);
 tolua_tablearray(tolua_S,"ego_item_type","rar",toluaI_get_object_ego_item_type_rar,toluaI_set_object_ego_item_type_rar);
 tolua_tablearray(tolua_S,"ego_item_type","flags1",toluaI_get_object_ego_item_type_flags1,toluaI_set_object_ego_item_type_flags1);
 tolua_tablearray(tolua_S,"ego_item_type","flags2",toluaI_get_object_ego_item_type_flags2,toluaI_set_object_ego_item_type_flags2);
 tolua_tablearray(tolua_S,"ego_item_type","flags3",toluaI_get_object_ego_item_type_flags3,toluaI_set_object_ego_item_type_flags3);
 tolua_tablearray(tolua_S,"ego_item_type","flags4",toluaI_get_object_ego_item_type_flags4,toluaI_set_object_ego_item_type_flags4);
 tolua_tablearray(tolua_S,"ego_item_type","flags5",toluaI_get_object_ego_item_type_flags5,toluaI_set_object_ego_item_type_flags5);
 tolua_tablearray(tolua_S,"ego_item_type","esp",toluaI_get_object_ego_item_type_esp,toluaI_set_object_ego_item_type_esp);
 tolua_tablearray(tolua_S,"ego_item_type","fego",toluaI_get_object_ego_item_type_fego,toluaI_set_object_ego_item_type_fego);
 tolua_tablevar(tolua_S,"ego_item_type","power",toluaI_get_object_ego_item_type_power,toluaI_set_object_ego_item_type_power);
 tolua_cclass(tolua_S,"object_type","");
 tolua_tablevar(tolua_S,"object_type","k_idx",toluaI_get_object_object_type_k_idx,toluaI_set_object_object_type_k_idx);
 tolua_tablevar(tolua_S,"object_type","iy",toluaI_get_object_object_type_iy,toluaI_set_object_object_type_iy);
 tolua_tablevar(tolua_S,"object_type","ix",toluaI_get_object_object_type_ix,toluaI_set_object_object_type_ix);
 tolua_tablevar(tolua_S,"object_type","tval",toluaI_get_object_object_type_tval,toluaI_set_object_object_type_tval);
 tolua_tablevar(tolua_S,"object_type","sval",toluaI_get_object_object_type_sval,toluaI_set_object_object_type_sval);
 tolua_tablevar(tolua_S,"object_type","pval",toluaI_get_object_object_type_pval,toluaI_set_object_object_type_pval);
 tolua_tablevar(tolua_S,"object_type","pval2",toluaI_get_object_object_type_pval2,toluaI_set_object_object_type_pval2);
 tolua_tablevar(tolua_S,"object_type","pval3",toluaI_get_object_object_type_pval3,toluaI_set_object_object_type_pval3);
 tolua_tablevar(tolua_S,"object_type","discount",toluaI_get_object_object_type_discount,toluaI_set_object_object_type_discount);
 tolua_tablevar(tolua_S,"object_type","number",toluaI_get_object_object_type_number,toluaI_set_object_object_type_number);
 tolua_tablevar(tolua_S,"object_type","weight",toluaI_get_object_object_type_weight,toluaI_set_object_object_type_weight);
 tolua_tablevar(tolua_S,"object_type","elevel",toluaI_get_object_object_type_elevel,toluaI_set_object_object_type_elevel);
 tolua_tablevar(tolua_S,"object_type","exp",toluaI_get_object_object_type_exp,toluaI_set_object_object_type_exp);
 tolua_tablevar(tolua_S,"object_type","name1",toluaI_get_object_object_type_name1,toluaI_set_object_object_type_name1);
 tolua_tablevar(tolua_S,"object_type","name2",toluaI_get_object_object_type_name2,toluaI_set_object_object_type_name2);
 tolua_tablevar(tolua_S,"object_type","name2b",toluaI_get_object_object_type_name2b,toluaI_set_object_object_type_name2b);
 tolua_tablevar(tolua_S,"object_type","xtra1",toluaI_get_object_object_type_xtra1,toluaI_set_object_object_type_xtra1);
 tolua_tablevar(tolua_S,"object_type","xtra2",toluaI_get_object_object_type_xtra2,toluaI_set_object_object_type_xtra2);
 tolua_tablevar(tolua_S,"object_type","to_h",toluaI_get_object_object_type_to_h,toluaI_set_object_object_type_to_h);
 tolua_tablevar(tolua_S,"object_type","to_d",toluaI_get_object_object_type_to_d,toluaI_set_object_object_type_to_d);
 tolua_tablevar(tolua_S,"object_type","to_a",toluaI_get_object_object_type_to_a,toluaI_set_object_object_type_to_a);
 tolua_tablevar(tolua_S,"object_type","ac",toluaI_get_object_object_type_ac,toluaI_set_object_object_type_ac);
 tolua_tablevar(tolua_S,"object_type","dd",toluaI_get_object_object_type_dd,toluaI_set_object_object_type_dd);
 tolua_tablevar(tolua_S,"object_type","ds",toluaI_get_object_object_type_ds,toluaI_set_object_object_type_ds);
 tolua_tablevar(tolua_S,"object_type","timeout",toluaI_get_object_object_type_timeout,toluaI_set_object_object_type_timeout);
 tolua_tablevar(tolua_S,"object_type","ident",toluaI_get_object_object_type_ident,toluaI_set_object_object_type_ident);
 tolua_tablevar(tolua_S,"object_type","marked",toluaI_get_object_object_type_marked,toluaI_set_object_object_type_marked);
 tolua_tablevar(tolua_S,"object_type","note",toluaI_get_object_object_type_note,toluaI_set_object_object_type_note);
 tolua_tablevar(tolua_S,"object_type","art_name",toluaI_get_object_object_type_art_name,toluaI_set_object_object_type_art_name);
 tolua_tablevar(tolua_S,"object_type","art_flags1",toluaI_get_object_object_type_art_flags1,toluaI_set_object_object_type_art_flags1);
 tolua_tablevar(tolua_S,"object_type","art_flags2",toluaI_get_object_object_type_art_flags2,toluaI_set_object_object_type_art_flags2);
 tolua_tablevar(tolua_S,"object_type","art_flags3",toluaI_get_object_object_type_art_flags3,toluaI_set_object_object_type_art_flags3);
 tolua_tablevar(tolua_S,"object_type","art_flags4",toluaI_get_object_object_type_art_flags4,toluaI_set_object_object_type_art_flags4);
 tolua_tablevar(tolua_S,"object_type","art_flags5",toluaI_get_object_object_type_art_flags5,toluaI_set_object_object_type_art_flags5);
 tolua_tablevar(tolua_S,"object_type","art_esp",toluaI_get_object_object_type_art_esp,toluaI_set_object_object_type_art_esp);
 tolua_tablevar(tolua_S,"object_type","next_o_idx",toluaI_get_object_object_type_next_o_idx,toluaI_set_object_object_type_next_o_idx);
 tolua_tablevar(tolua_S,"object_type","held_m_idx",toluaI_get_object_object_type_held_m_idx,toluaI_set_object_object_type_held_m_idx);
 tolua_tablevar(tolua_S,"object_type","sense",toluaI_get_object_object_type_sense,toluaI_set_object_object_type_sense);
 tolua_tablevar(tolua_S,"object_type","found",toluaI_get_object_object_type_found,toluaI_set_object_object_type_found);
 tolua_tablevar(tolua_S,"object_type","found_aux1",toluaI_get_object_object_type_found_aux1,toluaI_set_object_object_type_found_aux1);
 tolua_tablevar(tolua_S,"object_type","found_aux2",toluaI_get_object_object_type_found_aux2,toluaI_set_object_object_type_found_aux2);
 tolua_constant(tolua_S,NULL,"SENSE_NONE",SENSE_NONE);
 tolua_constant(tolua_S,NULL,"SENSE_CURSED",SENSE_CURSED);
 tolua_constant(tolua_S,NULL,"SENSE_AVERAGE",SENSE_AVERAGE);
 tolua_constant(tolua_S,NULL,"SENSE_GOOD_LIGHT",SENSE_GOOD_LIGHT);
 tolua_constant(tolua_S,NULL,"SENSE_GOOD_HEAVY",SENSE_GOOD_HEAVY);
 tolua_constant(tolua_S,NULL,"SENSE_EXCELLENT",SENSE_EXCELLENT);
 tolua_constant(tolua_S,NULL,"SENSE_WORTHLESS",SENSE_WORTHLESS);
 tolua_constant(tolua_S,NULL,"SENSE_TERRIBLE",SENSE_TERRIBLE);
 tolua_constant(tolua_S,NULL,"SENSE_SPECIAL",SENSE_SPECIAL);
 tolua_constant(tolua_S,NULL,"SENSE_BROKEN",SENSE_BROKEN);
 tolua_constant(tolua_S,NULL,"SENSE_UNCURSED",SENSE_UNCURSED);
 tolua_globalarray(tolua_S,"o_list",toluaI_get_object_o_list,toluaI_set_object_o_list);
 tolua_globalarray(tolua_S,"k_info",toluaI_get_object_k_info,toluaI_set_object_k_info);
 tolua_globalvar(tolua_S,"k_name",toluaI_get_object_k_name,toluaI_set_object_k_name);
 tolua_globalvar(tolua_S,"k_text",toluaI_get_object_k_text,toluaI_set_object_k_text);
 tolua_globalarray(tolua_S,"a_info",toluaI_get_object_a_info,toluaI_set_object_a_info);
 tolua_globalvar(tolua_S,"a_name",toluaI_get_object_a_name,toluaI_set_object_a_name);
 tolua_globalvar(tolua_S,"a_text",toluaI_get_object_a_text,toluaI_set_object_a_text);
 tolua_globalvar(tolua_S,"e_head",toluaI_get_object_e_head,toluaI_set_object_e_head);
 tolua_globalarray(tolua_S,"e_info",toluaI_get_object_e_info,toluaI_set_object_e_info);
 tolua_globalvar(tolua_S,"e_name",toluaI_get_object_e_name,toluaI_set_object_e_name);
 tolua_globalvar(tolua_S,"e_text",toluaI_get_object_e_text,toluaI_set_object_e_text);
 tolua_function(tolua_S,NULL,"m_bonus",toluaI_object_m_bonus00);
 tolua_function(tolua_S,NULL,"wield_slot_ideal",toluaI_object_wield_slot_ideal00);
 tolua_function(tolua_S,NULL,"wield_slot",toluaI_object_wield_slot00);
 tolua_function(tolua_S,NULL,"object_flags",toluaI_object_object_flags00);
 tolua_function(tolua_S,NULL,"object_desc",toluaI_object_object_desc00);
 tolua_function(tolua_S,NULL,"object_out_desc",toluaI_object_object_out_desc00);
 tolua_function(tolua_S,NULL,"inven_item_describe",toluaI_object_inven_item_describe00);
 tolua_function(tolua_S,NULL,"inven_item_increase",toluaI_object_inven_item_increase00);
 tolua_function(tolua_S,NULL,"inven_item_optimize",toluaI_object_inven_item_optimize00);
 tolua_function(tolua_S,NULL,"floor_item_describe",toluaI_object_floor_item_describe00);
 tolua_function(tolua_S,NULL,"floor_item_increase",toluaI_object_floor_item_increase00);
 tolua_function(tolua_S,NULL,"floor_item_optimize",toluaI_object_floor_item_optimize00);
 tolua_function(tolua_S,NULL,"delete_object_idx",toluaI_object_delete_object_idx00);
 tolua_function(tolua_S,NULL,"o_pop",toluaI_object_o_pop00);
 tolua_function(tolua_S,NULL,"get_obj_num_prep",toluaI_object_get_obj_num_prep00);
 tolua_function(tolua_S,NULL,"ident_all",toluaI_object_ident_all00);
 tolua_function(tolua_S,NULL,"get_obj_num",toluaI_object_get_obj_num00);
 tolua_function(tolua_S,NULL,"lookup_kind",toluaI_object_lookup_kind00);
 tolua_function(tolua_S,NULL,"object_wipe",toluaI_object_object_wipe00);
 tolua_function(tolua_S,NULL,"object_prep",toluaI_object_object_prep00);
 tolua_function(tolua_S,NULL,"object_copy",toluaI_object_object_copy00);
 tolua_function(tolua_S,NULL,"inven_carry_okay",toluaI_object_inven_carry_okay00);
 tolua_function(tolua_S,NULL,"apply_magic",toluaI_object_apply_magic00);
 tolua_function(tolua_S,NULL,"make_object",toluaI_object_make_object00);
 tolua_function(tolua_S,NULL,"drop_near",toluaI_object_drop_near00);
 tolua_function(tolua_S,NULL,"get_object",toluaI_object_get_object00);
 tolua_function(tolua_S,NULL,"new_object",toluaI_object_new_object00);
 tolua_function(tolua_S,NULL,"end_object",toluaI_object_end_object00);
 tolua_function(tolua_S,NULL,"get_item_aux",toluaI_object_get_item_aux00);
 tolua_function(tolua_S,NULL,"lua_set_item_tester",toluaI_object_lua_set_item_tester00);
 tolua_function(tolua_S,NULL,"is_magestaff",toluaI_object_is_magestaff00);
 tolua_function(tolua_S,NULL,"identify_pack_fully",toluaI_object_identify_pack_fully00);
 tolua_function(tolua_S,NULL,"inven_carry",toluaI_object_inven_carry00);
 tolua_function(tolua_S,NULL,"calc_total_weight",toluaI_object_calc_total_weight00);
 tolua_function(tolua_S,NULL,"get_slot",toluaI_object_get_slot00);
 tolua_function(tolua_S,NULL,"is_blessed",toluaI_object_is_blessed00);
 tolua_globalarray(tolua_S,"sense_desc",toluaI_get_object_sense_desc,toluaI_set_object_sense_desc);
 tolua_function(tolua_S,NULL,"object_pickup",toluaI_object_object_pickup00);
 tolua_function(tolua_S,NULL,"is_artifact",toluaI_object_is_artifact00);
 tolua_function(tolua_S,NULL,"is_aware",toluaI_object_is_aware00);
 tolua_function(tolua_S,NULL,"is_known",toluaI_object_is_known00);
 tolua_function(tolua_S,NULL,"set_aware",toluaI_object_set_aware00);
 tolua_function(tolua_S,NULL,"set_known",toluaI_object_set_known00);
 tolua_function(tolua_S,NULL,"value_check_aux1",toluaI_object_value_check_aux100);
 tolua_function(tolua_S,NULL,"value_check_aux1_magic",toluaI_object_value_check_aux1_magic00);
 tolua_function(tolua_S,NULL,"value_check_aux2",toluaI_object_value_check_aux200);
 tolua_function(tolua_S,NULL,"value_check_aux2_magic",toluaI_object_value_check_aux2_magic00);
 tolua_function(tolua_S,NULL,"select_sense",toluaI_object_select_sense00);
 tolua_function(tolua_S,NULL,"remove_curse_object",toluaI_object_remove_curse_object00);
 return 1;
}
/* Close function */
void tolua_object_close (lua_State* tolua_S)
{
 lua_getglobals(tolua_S);
 lua_pushstring(tolua_S,"obj_forge"); lua_pushnil(tolua_S); lua_rawset(tolua_S,-3);
 lua_pop(tolua_S,1);
 lua_getglobals(tolua_S);
 lua_pushstring(tolua_S,"theme_forge"); lua_pushnil(tolua_S); lua_rawset(tolua_S,-3);
 lua_pop(tolua_S,1);
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TR1_STR");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TR1_INT");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TR1_WIS");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TR1_DEX");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TR1_CON");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TR1_CHR");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TR1_MANA");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TR1_SPELL");
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
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TR1_NULL_MASK");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TR2_SUST_STR");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TR2_SUST_INT");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TR2_SUST_WIS");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TR2_SUST_DEX");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TR2_SUST_CON");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TR2_SUST_CHR");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TR2_INVIS");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TR2_LIFE");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TR2_IM_ACID");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TR2_IM_ELEC");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TR2_IM_FIRE");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TR2_IM_COLD");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TR2_SENS_FIRE");
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
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TR2_NULL_MASK");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TR3_SH_FIRE");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TR3_SH_ELEC");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TR3_AUTO_CURSE");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TR3_DECAY");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TR3_NO_TELE");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TR3_NO_MAGIC");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TR3_WRAITH");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TR3_TY_CURSE");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TR3_EASY_KNOW");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TR3_HIDE_TYPE");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TR3_SHOW_MODS");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TR3_INSTA_ART");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TR3_FEATHER");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TR3_LITE1");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TR3_SEE_INVIS");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TR3_NORM_ART");
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
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TR3_NULL_MASK");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TR4_NEVER_BLOW");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TR4_PRECOGNITION");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TR4_BLACK_BREATH");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TR4_RECHARGE");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TR4_FLY");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TR4_DG_CURSE");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TR4_COULD2H");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TR4_MUST2H");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TR4_LEVELS");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TR4_CLONE");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TR4_SPECIAL_GENE");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TR4_CLIMB");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TR4_FAST_CAST");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TR4_CAPACITY");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TR4_CHARGING");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TR4_CHEAPNESS");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TR4_FOUNTAIN");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TR4_ANTIMAGIC_50");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TR4_ANTIMAGIC_30");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TR4_ANTIMAGIC_20");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TR4_ANTIMAGIC_10");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TR4_EASY_USE");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TR4_IM_NETHER");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TR4_RECHARGED");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TR4_ULTIMATE");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TR4_AUTO_ID");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TR4_LITE2");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TR4_LITE3");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TR4_FUEL_LITE");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TR4_ART_EXP");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TR4_CURSE_NO_DROP");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TR4_NO_RECHARGE");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TR4_NULL_MASK");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TR5_TEMPORARY");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TR5_DRAIN_MANA");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TR5_DRAIN_HP");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TR5_KILL_DEMON");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TR5_KILL_UNDEAD");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TR5_CRIT");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TR5_ATTR_MULTI");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TR5_WOUNDING");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TR5_FULL_NAME");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TR5_LUCK");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TR5_IMMOVABLE");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TR5_SPELL_CONTAIN");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TR5_RES_MORGUL");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TR5_ACTIVATE_NO_WIELD");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TR5_MAGIC_BREATH");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TR5_WATER_BREATH");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TR5_WIELD_CAST");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"ESP_ORC");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"ESP_TROLL");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"ESP_DRAGON");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"ESP_GIANT");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"ESP_DEMON");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"ESP_UNDEAD");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"ESP_EVIL");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"ESP_ANIMAL");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"ESP_THUNDERLORD");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"ESP_GOOD");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"ESP_NONLIVING");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"ESP_UNIQUE");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"ESP_SPIDER");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"ESP_ALL");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"USE_EQUIP");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"USE_INVEN");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"USE_FLOOR");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"USE_EXTRA");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"INVEN_WIELD");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"INVEN_BOW");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"INVEN_RING");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"INVEN_NECK");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"INVEN_LITE");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"INVEN_BODY");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"INVEN_OUTER");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"INVEN_ARM");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"INVEN_HEAD");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"INVEN_HANDS");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"INVEN_FEET");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"INVEN_CARRY");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"INVEN_AMMO");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"INVEN_TOOL");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"INVEN_TOTAL");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"INVEN_EQ");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TV_SKELETON");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TV_BOTTLE");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TV_BATERIE");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TV_SPIKE");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TV_MSTAFF");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TV_CHEST");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TV_PARCHMENT");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TV_PARCHEMENT");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TV_CORPSE");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TV_EGG");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TV_JUNK");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TV_TOOL");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TV_INSTRUMENT");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TV_BOOMERANG");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TV_SHOT");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TV_ARROW");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TV_BOLT");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TV_BOW");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TV_DIGGING");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TV_HAFTED");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TV_POLEARM");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TV_SWORD");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TV_AXE");
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
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TV_TRAPKIT");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TV_STAFF");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TV_WAND");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TV_ROD");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TV_ROD_MAIN");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TV_SCROLL");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TV_POTION");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TV_POTION2");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TV_FLASK");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TV_FOOD");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TV_HYPNOS");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TV_GOLD");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TV_RANDART");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TV_RUNE1");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TV_RUNE2");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TV_BOOK");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TV_SYMBIOTIC_BOOK");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TV_MUSIC_BOOK");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TV_DRUID_BOOK");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TV_DAEMON_BOOK");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_TOOL_CLIMB");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_PORTABLE_HOLE");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_MSTAFF");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_AMMO_LIGHT");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_AMMO_NORMAL");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_AMMO_HEAVY");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_DRUM");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_HARP");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_HORN");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_TRAPKIT_SLING");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_TRAPKIT_BOW");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_TRAPKIT_XBOW");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_TRAPKIT_POTION");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_TRAPKIT_SCROLL");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_TRAPKIT_DEVICE");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_BOOM_S_WOOD");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_BOOM_WOOD");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_BOOM_S_METAL");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_BOOM_METAL");
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
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_WAR_HAMMER");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_LUCERN_HAMMER");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_THREE_PIECE_ROD");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_MORNING_STAR");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_FLAIL");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_LEAD_FILLED_MACE");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_TWO_HANDED_FLAIL");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_GREAT_HAMMER");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_MACE_OF_DISRUPTION");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_GROND");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_HATCHET");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_CLEAVER");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_LIGHT_WAR_AXE");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_BEAKED_AXE");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_BROAD_AXE");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_BATTLE_AXE");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_GREAT_AXE");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_LOCHABER_AXE");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_SLAUGHTER_AXE");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_SPEAR");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_SICKLE");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_AWL_PIKE");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_TRIDENT");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_FAUCHARD");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_BROAD_SPEAR");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_PIKE");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_GLAIVE");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_HALBERD");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_GUISARME");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_SCYTHE");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_LANCE");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_TRIFURCATE_SPEAR");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_HEAVY_LANCE");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_SCYTHE_OF_SLICING");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_BROKEN_DAGGER");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_BROKEN_SWORD");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_DAGGER");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_MAIN_GAUCHE");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_RAPIER");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_SMALL_SWORD");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_BASILLARD");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_SHORT_SWORD");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_SABRE");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_CUTLASS");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_KHOPESH");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_TULWAR");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_BROAD_SWORD");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_LONG_SWORD");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_SCIMITAR");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_KATANA");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_BASTARD_SWORD");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_GREAT_SCIMITAR");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_CLAYMORE");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_ESPADON");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_TWO_HANDED_SWORD");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_FLAMBERGE");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_EXECUTIONERS_SWORD");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_ZWEIHANDER");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_BLADE_OF_CHAOS");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_BLUESTEEL_BLADE");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_SHADOW_BLADE");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_DARK_SWORD");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_SMALL_LEATHER_SHIELD");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_SMALL_METAL_SHIELD");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_LARGE_LEATHER_SHIELD");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_LARGE_METAL_SHIELD");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_DRAGON_SHIELD");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_SHIELD_OF_DEFLECTION");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_HARD_LEATHER_CAP");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_METAL_CAP");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_IRON_HELM");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_STEEL_HELM");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_DRAGON_HELM");
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
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_THUNDERLORD_SUIT");
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
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_PARTIAL_PLATE_ARMOUR");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_METAL_LAMELLAR_ARMOUR");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_FULL_PLATE_ARMOUR");
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
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_LITE_TORCH_EVER");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_LITE_DWARVEN");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_LITE_FEANORIAN");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_LITE_GALADRIEL");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_LITE_ELENDIL");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_LITE_THRAIN");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_LITE_UNDEATH");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_LITE_PALANTIR");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_ANCHOR_SPACETIME");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_STONE_LORE");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_AMULET_DOOM");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_AMULET_TELEPORT");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_AMULET_ADORNMENT");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_AMULET_SLOW_DIGEST");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_AMULET_RESIST_ACID");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_AMULET_SEARCHING");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_AMULET_BRILLANCE");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_AMULET_CHARISMA");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_AMULET_THE_MAGI");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_AMULET_REFLECTION");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_AMULET_CARLAMMAS");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_AMULET_INGWE");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_AMULET_DWARVES");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_AMULET_NO_MAGIC");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_AMULET_NO_TELE");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_AMULET_RESISTANCE");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_AMULET_NOTHING");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_AMULET_SERPENT");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_AMULET_TORIS_MEJISTOS");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_AMULET_TRICKERY");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_AMULET_DEVOTION");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_AMULET_WEAPONMASTERY");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_AMULET_WISDOM");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_AMULET_INFRA");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_AMULET_SPELL");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_RING_WOE");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_RING_AGGRAVATION");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_RING_WEAKNESS");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_RING_STUPIDITY");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_RING_TELEPORTATION");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_RING_SPECIAL");
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
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_RING_NOTHING");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_RING_PRECONITION");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_RING_FLAR");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_RING_INVIS");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_RING_FLYING");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_RING_WRAITH");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_RING_ELEC");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_RING_CRIT");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_RING_SPELL");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_STAFF_SCHOOL");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_STAFF_NOTHING");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_WAND_SCHOOL");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_WAND_NOTHING");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_ROD_NOTHING");
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
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_ROD_DETECT_TRAP");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_ROD_HOME");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_ROD_WOODEN");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_ROD_COPPER");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_ROD_IRON");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_ROD_ALUMINIUM");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_ROD_SILVER");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_ROD_GOLDEN");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_ROD_MITHRIL");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_ROD_ADMANTITE");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_SCROLL_DARKNESS");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_SCROLL_AGGRAVATE_MONSTER");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_SCROLL_CURSE_ARMOR");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_SCROLL_CURSE_WEAPON");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_SCROLL_SUMMON_MONSTER");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_SCROLL_SUMMON_UNDEAD");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_SCROLL_SUMMON_MINE");
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
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_SCROLL_ENCHANT_WEAPON_PVAL");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_SCROLL_STAR_ENCHANT_ARMOR");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_SCROLL_STAR_ENCHANT_WEAPON");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_SCROLL_RECHARGING");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_SCROLL_RESET_RECALL");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_SCROLL_LIGHT");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_SCROLL_MAPPING");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_SCROLL_DETECT_GOLD");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_SCROLL_DETECT_ITEM");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_SCROLL_DETECT_TRAP");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_SCROLL_DETECT_DOOR");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_SCROLL_DETECT_INVIS");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_SCROLL_DIVINATION");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_SCROLL_SATISFY_HUNGER");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_SCROLL_BLESSING");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_SCROLL_HOLY_CHANT");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_SCROLL_HOLY_PRAYER");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_SCROLL_MONSTER_CONFUSION");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_SCROLL_PROTECTION_FROM_EVIL");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_SCROLL_RUNE_OF_PROTECTION");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_SCROLL_TRAP_DOOR_DESTRUCTION");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_SCROLL_DEINCARNATION");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_SCROLL_STAR_DESTRUCTION");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_SCROLL_DISPEL_UNDEAD");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_SCROLL_MASS_RESURECTION");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_SCROLL_GENOCIDE");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_SCROLL_MASS_GENOCIDE");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_SCROLL_ACQUIREMENT");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_SCROLL_STAR_ACQUIREMENT");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_SCROLL_FIRE");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_SCROLL_ICE");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_SCROLL_CHAOS");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_SCROLL_RUMOR");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_SCROLL_ARTIFACT");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_SCROLL_NOTHING");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_POTION_WATER");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_POTION_APPLE_JUICE");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_POTION_SLIME_MOLD");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_POTION_BLOOD");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_POTION_SLOWNESS");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_POTION_SALT_WATER");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_POTION_POISON");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_POTION_BLINDNESS");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_POTION_INVIS");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_POTION_CONFUSION");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_POTION_MUTATION");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_POTION_SLEEP");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_POTION_LEARNING");
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
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_POTION_BESERK_STRENGTH");
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
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_POTION_LAST");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_POTION2_MIMIC");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_POTION2_CURE_LIGHT_SANITY");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_POTION2_CURE_SERIOUS_SANITY");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_POTION2_CURE_CRITICAL_SANITY");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_POTION2_CURE_SANITY");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_POTION2_CURE_WATER");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_POTION2_LAST");
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
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_FOOD_ATHELAS");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_FOOD_GREAT_HEALTH");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_FOOD_FORTUNE_COOKIE");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_BATERIE_POISON");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_BATERIE_EXPLOSION");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_BATERIE_TELEPORT");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_BATERIE_COLD");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_BATERIE_FIRE");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_BATERIE_ACID");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_BATERIE_LIFE");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_BATERIE_CONFUSION");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_BATERIE_LITE");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_BATERIE_CHAOS");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_BATERIE_TIME");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_BATERIE_MAGIC");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_BATERIE_XTRA_LIFE");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_BATERIE_DARKNESS");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_BATERIE_KNOWLEDGE");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_BATERIE_FORCE");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_BATERIE_LIGHTNING");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_BATERIE_MANA");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_CORPSE_CORPSE");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_CORPSE_SKELETON");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_CORPSE_HEAD");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_CORPSE_SKULL");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_CORPSE_MEAT");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_DEMONBLADE");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_DEMONSHIELD");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SV_DEMONHORN");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"IDENT_SENSE");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"IDENT_FIXED");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"IDENT_EMPTY");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"IDENT_KNOWN");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"IDENT_STOREB");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"IDENT_MENTAL");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"IDENT_CURSED");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"OBJ_FOUND_MONSTER");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"OBJ_FOUND_FLOOR");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"OBJ_FOUND_VAULT");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"OBJ_FOUND_SPECIAL");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"OBJ_FOUND_RUBBLE");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"OBJ_FOUND_REWARD");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"OBJ_FOUND_STORE");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"obj_theme");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"object_kind");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"artifact_type");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"ego_item_type");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"object_type");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SENSE_NONE");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SENSE_CURSED");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SENSE_AVERAGE");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SENSE_GOOD_LIGHT");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SENSE_GOOD_HEAVY");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SENSE_EXCELLENT");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SENSE_WORTHLESS");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SENSE_TERRIBLE");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SENSE_SPECIAL");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SENSE_BROKEN");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SENSE_UNCURSED");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"o_list");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"k_info");
 lua_getglobals(tolua_S);
 lua_pushstring(tolua_S,"k_name"); lua_pushnil(tolua_S); lua_rawset(tolua_S,-3);
 lua_pop(tolua_S,1);
 lua_getglobals(tolua_S);
 lua_pushstring(tolua_S,"k_text"); lua_pushnil(tolua_S); lua_rawset(tolua_S,-3);
 lua_pop(tolua_S,1);
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"a_info");
 lua_getglobals(tolua_S);
 lua_pushstring(tolua_S,"a_name"); lua_pushnil(tolua_S); lua_rawset(tolua_S,-3);
 lua_pop(tolua_S,1);
 lua_getglobals(tolua_S);
 lua_pushstring(tolua_S,"a_text"); lua_pushnil(tolua_S); lua_rawset(tolua_S,-3);
 lua_pop(tolua_S,1);
 lua_getglobals(tolua_S);
 lua_pushstring(tolua_S,"e_head"); lua_pushnil(tolua_S); lua_rawset(tolua_S,-3);
 lua_pop(tolua_S,1);
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"e_info");
 lua_getglobals(tolua_S);
 lua_pushstring(tolua_S,"e_name"); lua_pushnil(tolua_S); lua_rawset(tolua_S,-3);
 lua_pop(tolua_S,1);
 lua_getglobals(tolua_S);
 lua_pushstring(tolua_S,"e_text"); lua_pushnil(tolua_S); lua_rawset(tolua_S,-3);
 lua_pop(tolua_S,1);
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"m_bonus");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"wield_slot_ideal");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"wield_slot");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"object_flags");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"object_desc");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"object_out_desc");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"inven_item_describe");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"inven_item_increase");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"inven_item_optimize");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"floor_item_describe");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"floor_item_increase");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"floor_item_optimize");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"delete_object_idx");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"o_pop");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"get_obj_num_prep");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"ident_all");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"get_obj_num");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"lookup_kind");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"object_wipe");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"object_prep");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"object_copy");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"inven_carry_okay");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"apply_magic");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"make_object");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"drop_near");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"get_object");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"new_object");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"end_object");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"get_item_aux");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"lua_set_item_tester");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"is_magestaff");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"identify_pack_fully");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"inven_carry");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"calc_total_weight");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"get_slot");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"is_blessed");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"sense_desc");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"object_pickup");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"is_artifact");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"is_aware");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"is_known");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"set_aware");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"set_known");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"value_check_aux1");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"value_check_aux1_magic");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"value_check_aux2");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"value_check_aux2_magic");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"select_sense");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"remove_curse_object");
}
