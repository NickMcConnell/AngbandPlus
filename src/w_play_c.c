/*
** Lua binding: player_c
** Generated automatically by tolua 4.0a - angband on 01/14/02 00:02:52.
*/

#include "lua/tolua.h"

/* Exported function */
int tolua_player_c_open (lua_State* tolua_S);
void tolua_player_c_close (lua_State* tolua_S);

#include "angband.h"

/* function to register type */
static void toluaI_reg_types (lua_State* tolua_S)
{
 tolua_usertype(tolua_S,"player_class");
}

/* error messages */
#define TOLUA_ERR_SELF tolua_error(tolua_S,"invalid 'self'")
#define TOLUA_ERR_ASSIGN tolua_error(tolua_S,"#vinvalid type in variable assignment.")

/* get function: title of class  player_class */
static int toluaI_get_player_c_player_class_title(lua_State* tolua_S)
{
  player_class* self = (player_class*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->title);
 return 1;
}

/* set function: title of class  player_class */
static int toluaI_set_player_c_player_class_title(lua_State* tolua_S)
{
  player_class* self = (player_class*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->title = ((s32b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: desc of class  player_class */
static int toluaI_get_player_c_player_class_desc(lua_State* tolua_S)
{
  player_class* self = (player_class*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->desc);
 return 1;
}

/* set function: desc of class  player_class */
static int toluaI_set_player_c_player_class_desc(lua_State* tolua_S)
{
  player_class* self = (player_class*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->desc = ((s32b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: titles of class  player_class */
static int toluaI_get_player_c_player_class_titles(lua_State* tolua_S)
{
 int toluaI_index;
  player_class* self;
 lua_pushstring(tolua_S,".self");
 lua_rawget(tolua_S,1);
 self = (player_class*)  lua_touserdata(tolua_S,-1);
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 tolua_error(tolua_S,"invalid type in array indexing.");
 toluaI_index = (int)tolua_getnumber(tolua_S,2,0)-1;
 if (toluaI_index<0 || toluaI_index>=10)
 tolua_error(tolua_S,"array indexing out of range.");
 tolua_pushnumber(tolua_S,(long)self->titles[toluaI_index]);
 return 1;
}

/* set function: titles of class  player_class */
static int toluaI_set_player_c_player_class_titles(lua_State* tolua_S)
{
 int toluaI_index;
  player_class* self;
 lua_pushstring(tolua_S,".self");
 lua_rawget(tolua_S,1);
 self = (player_class*)  lua_touserdata(tolua_S,-1);
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 tolua_error(tolua_S,"invalid type in array indexing.");
 toluaI_index = (int)tolua_getnumber(tolua_S,2,0)-1;
 if (toluaI_index<0 || toluaI_index>=10)
 tolua_error(tolua_S,"array indexing out of range.");
  self->titles[toluaI_index] = ((s32b)  tolua_getnumber(tolua_S,3,0));
 return 0;
}

/* get function: c_adj of class  player_class */
static int toluaI_get_player_c_player_class_c_adj(lua_State* tolua_S)
{
 int toluaI_index;
  player_class* self;
 lua_pushstring(tolua_S,".self");
 lua_rawget(tolua_S,1);
 self = (player_class*)  lua_touserdata(tolua_S,-1);
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 tolua_error(tolua_S,"invalid type in array indexing.");
 toluaI_index = (int)tolua_getnumber(tolua_S,2,0)-1;
 if (toluaI_index<0 || toluaI_index>=6)
 tolua_error(tolua_S,"array indexing out of range.");
 tolua_pushnumber(tolua_S,(long)self->c_adj[toluaI_index]);
 return 1;
}

/* set function: c_adj of class  player_class */
static int toluaI_set_player_c_player_class_c_adj(lua_State* tolua_S)
{
 int toluaI_index;
  player_class* self;
 lua_pushstring(tolua_S,".self");
 lua_rawget(tolua_S,1);
 self = (player_class*)  lua_touserdata(tolua_S,-1);
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 tolua_error(tolua_S,"invalid type in array indexing.");
 toluaI_index = (int)tolua_getnumber(tolua_S,2,0)-1;
 if (toluaI_index<0 || toluaI_index>=6)
 tolua_error(tolua_S,"array indexing out of range.");
  self->c_adj[toluaI_index] = ((s16b)  tolua_getnumber(tolua_S,3,0));
 return 0;
}

/* get function: c_dis of class  player_class */
static int toluaI_get_player_c_player_class_c_dis(lua_State* tolua_S)
{
  player_class* self = (player_class*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->c_dis);
 return 1;
}

/* set function: c_dis of class  player_class */
static int toluaI_set_player_c_player_class_c_dis(lua_State* tolua_S)
{
  player_class* self = (player_class*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->c_dis = ((s16b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: c_dev of class  player_class */
static int toluaI_get_player_c_player_class_c_dev(lua_State* tolua_S)
{
  player_class* self = (player_class*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->c_dev);
 return 1;
}

/* set function: c_dev of class  player_class */
static int toluaI_set_player_c_player_class_c_dev(lua_State* tolua_S)
{
  player_class* self = (player_class*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->c_dev = ((s16b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: c_sav of class  player_class */
static int toluaI_get_player_c_player_class_c_sav(lua_State* tolua_S)
{
  player_class* self = (player_class*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->c_sav);
 return 1;
}

/* set function: c_sav of class  player_class */
static int toluaI_set_player_c_player_class_c_sav(lua_State* tolua_S)
{
  player_class* self = (player_class*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->c_sav = ((s16b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: c_stl of class  player_class */
static int toluaI_get_player_c_player_class_c_stl(lua_State* tolua_S)
{
  player_class* self = (player_class*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->c_stl);
 return 1;
}

/* set function: c_stl of class  player_class */
static int toluaI_set_player_c_player_class_c_stl(lua_State* tolua_S)
{
  player_class* self = (player_class*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->c_stl = ((s16b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: c_srh of class  player_class */
static int toluaI_get_player_c_player_class_c_srh(lua_State* tolua_S)
{
  player_class* self = (player_class*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->c_srh);
 return 1;
}

/* set function: c_srh of class  player_class */
static int toluaI_set_player_c_player_class_c_srh(lua_State* tolua_S)
{
  player_class* self = (player_class*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->c_srh = ((s16b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: c_fos of class  player_class */
static int toluaI_get_player_c_player_class_c_fos(lua_State* tolua_S)
{
  player_class* self = (player_class*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->c_fos);
 return 1;
}

/* set function: c_fos of class  player_class */
static int toluaI_set_player_c_player_class_c_fos(lua_State* tolua_S)
{
  player_class* self = (player_class*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->c_fos = ((s16b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: c_thn of class  player_class */
static int toluaI_get_player_c_player_class_c_thn(lua_State* tolua_S)
{
  player_class* self = (player_class*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->c_thn);
 return 1;
}

/* set function: c_thn of class  player_class */
static int toluaI_set_player_c_player_class_c_thn(lua_State* tolua_S)
{
  player_class* self = (player_class*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->c_thn = ((s16b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: c_thb of class  player_class */
static int toluaI_get_player_c_player_class_c_thb(lua_State* tolua_S)
{
  player_class* self = (player_class*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->c_thb);
 return 1;
}

/* set function: c_thb of class  player_class */
static int toluaI_set_player_c_player_class_c_thb(lua_State* tolua_S)
{
  player_class* self = (player_class*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->c_thb = ((s16b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: x_dis of class  player_class */
static int toluaI_get_player_c_player_class_x_dis(lua_State* tolua_S)
{
  player_class* self = (player_class*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->x_dis);
 return 1;
}

/* set function: x_dis of class  player_class */
static int toluaI_set_player_c_player_class_x_dis(lua_State* tolua_S)
{
  player_class* self = (player_class*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->x_dis = ((s16b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: x_dev of class  player_class */
static int toluaI_get_player_c_player_class_x_dev(lua_State* tolua_S)
{
  player_class* self = (player_class*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->x_dev);
 return 1;
}

/* set function: x_dev of class  player_class */
static int toluaI_set_player_c_player_class_x_dev(lua_State* tolua_S)
{
  player_class* self = (player_class*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->x_dev = ((s16b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: x_sav of class  player_class */
static int toluaI_get_player_c_player_class_x_sav(lua_State* tolua_S)
{
  player_class* self = (player_class*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->x_sav);
 return 1;
}

/* set function: x_sav of class  player_class */
static int toluaI_set_player_c_player_class_x_sav(lua_State* tolua_S)
{
  player_class* self = (player_class*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->x_sav = ((s16b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: x_stl of class  player_class */
static int toluaI_get_player_c_player_class_x_stl(lua_State* tolua_S)
{
  player_class* self = (player_class*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->x_stl);
 return 1;
}

/* set function: x_stl of class  player_class */
static int toluaI_set_player_c_player_class_x_stl(lua_State* tolua_S)
{
  player_class* self = (player_class*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->x_stl = ((s16b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: x_srh of class  player_class */
static int toluaI_get_player_c_player_class_x_srh(lua_State* tolua_S)
{
  player_class* self = (player_class*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->x_srh);
 return 1;
}

/* set function: x_srh of class  player_class */
static int toluaI_set_player_c_player_class_x_srh(lua_State* tolua_S)
{
  player_class* self = (player_class*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->x_srh = ((s16b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: x_fos of class  player_class */
static int toluaI_get_player_c_player_class_x_fos(lua_State* tolua_S)
{
  player_class* self = (player_class*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->x_fos);
 return 1;
}

/* set function: x_fos of class  player_class */
static int toluaI_set_player_c_player_class_x_fos(lua_State* tolua_S)
{
  player_class* self = (player_class*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->x_fos = ((s16b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: x_thn of class  player_class */
static int toluaI_get_player_c_player_class_x_thn(lua_State* tolua_S)
{
  player_class* self = (player_class*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->x_thn);
 return 1;
}

/* set function: x_thn of class  player_class */
static int toluaI_set_player_c_player_class_x_thn(lua_State* tolua_S)
{
  player_class* self = (player_class*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->x_thn = ((s16b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: x_thb of class  player_class */
static int toluaI_get_player_c_player_class_x_thb(lua_State* tolua_S)
{
  player_class* self = (player_class*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->x_thb);
 return 1;
}

/* set function: x_thb of class  player_class */
static int toluaI_set_player_c_player_class_x_thb(lua_State* tolua_S)
{
  player_class* self = (player_class*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->x_thb = ((s16b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: c_mhp of class  player_class */
static int toluaI_get_player_c_player_class_c_mhp(lua_State* tolua_S)
{
  player_class* self = (player_class*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->c_mhp);
 return 1;
}

/* set function: c_mhp of class  player_class */
static int toluaI_set_player_c_player_class_c_mhp(lua_State* tolua_S)
{
  player_class* self = (player_class*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->c_mhp = ((s16b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: c_exp of class  player_class */
static int toluaI_get_player_c_player_class_c_exp(lua_State* tolua_S)
{
  player_class* self = (player_class*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->c_exp);
 return 1;
}

/* set function: c_exp of class  player_class */
static int toluaI_set_player_c_player_class_c_exp(lua_State* tolua_S)
{
  player_class* self = (player_class*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->c_exp = ((s16b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: powers of class  player_class */
static int toluaI_get_player_c_player_class_powers(lua_State* tolua_S)
{
 int toluaI_index;
  player_class* self;
 lua_pushstring(tolua_S,".self");
 lua_rawget(tolua_S,1);
 self = (player_class*)  lua_touserdata(tolua_S,-1);
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 tolua_error(tolua_S,"invalid type in array indexing.");
 toluaI_index = (int)tolua_getnumber(tolua_S,2,0)-1;
 if (toluaI_index<0 || toluaI_index>=4)
 tolua_error(tolua_S,"array indexing out of range.");
 tolua_pushnumber(tolua_S,(long)self->powers[toluaI_index]);
 return 1;
}

/* set function: powers of class  player_class */
static int toluaI_set_player_c_player_class_powers(lua_State* tolua_S)
{
 int toluaI_index;
  player_class* self;
 lua_pushstring(tolua_S,".self");
 lua_rawget(tolua_S,1);
 self = (player_class*)  lua_touserdata(tolua_S,-1);
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 tolua_error(tolua_S,"invalid type in array indexing.");
 toluaI_index = (int)tolua_getnumber(tolua_S,2,0)-1;
 if (toluaI_index<0 || toluaI_index>=4)
 tolua_error(tolua_S,"array indexing out of range.");
  self->powers[toluaI_index] = ((s16b)  tolua_getnumber(tolua_S,3,0));
 return 0;
}

/* get function: spell_book of class  player_class */
static int toluaI_get_player_c_player_class_spell_book(lua_State* tolua_S)
{
  player_class* self = (player_class*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->spell_book);
 return 1;
}

/* set function: spell_book of class  player_class */
static int toluaI_set_player_c_player_class_spell_book(lua_State* tolua_S)
{
  player_class* self = (player_class*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->spell_book = ((s16b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: spell_stat of class  player_class */
static int toluaI_get_player_c_player_class_spell_stat(lua_State* tolua_S)
{
  player_class* self = (player_class*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->spell_stat);
 return 1;
}

/* set function: spell_stat of class  player_class */
static int toluaI_set_player_c_player_class_spell_stat(lua_State* tolua_S)
{
  player_class* self = (player_class*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->spell_stat = ((s16b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: spell_lev of class  player_class */
static int toluaI_get_player_c_player_class_spell_lev(lua_State* tolua_S)
{
  player_class* self = (player_class*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->spell_lev);
 return 1;
}

/* set function: spell_lev of class  player_class */
static int toluaI_set_player_c_player_class_spell_lev(lua_State* tolua_S)
{
  player_class* self = (player_class*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->spell_lev = ((s16b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: spell_fail of class  player_class */
static int toluaI_get_player_c_player_class_spell_fail(lua_State* tolua_S)
{
  player_class* self = (player_class*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->spell_fail);
 return 1;
}

/* set function: spell_fail of class  player_class */
static int toluaI_set_player_c_player_class_spell_fail(lua_State* tolua_S)
{
  player_class* self = (player_class*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->spell_fail = ((s16b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: spell_mana of class  player_class */
static int toluaI_get_player_c_player_class_spell_mana(lua_State* tolua_S)
{
  player_class* self = (player_class*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->spell_mana);
 return 1;
}

/* set function: spell_mana of class  player_class */
static int toluaI_set_player_c_player_class_spell_mana(lua_State* tolua_S)
{
  player_class* self = (player_class*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->spell_mana = ((s16b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: spell_first of class  player_class */
static int toluaI_get_player_c_player_class_spell_first(lua_State* tolua_S)
{
  player_class* self = (player_class*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->spell_first);
 return 1;
}

/* set function: spell_first of class  player_class */
static int toluaI_set_player_c_player_class_spell_first(lua_State* tolua_S)
{
  player_class* self = (player_class*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->spell_first = ((s16b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: spell_weight of class  player_class */
static int toluaI_get_player_c_player_class_spell_weight(lua_State* tolua_S)
{
  player_class* self = (player_class*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->spell_weight);
 return 1;
}

/* set function: spell_weight of class  player_class */
static int toluaI_set_player_c_player_class_spell_weight(lua_State* tolua_S)
{
  player_class* self = (player_class*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->spell_weight = ((s16b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: max_spell_level of class  player_class */
static int toluaI_get_player_c_player_class_max_spell_level(lua_State* tolua_S)
{
  player_class* self = (player_class*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->max_spell_level);
 return 1;
}

/* set function: max_spell_level of class  player_class */
static int toluaI_set_player_c_player_class_max_spell_level(lua_State* tolua_S)
{
  player_class* self = (player_class*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->max_spell_level = ((byte)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: magic_max_spell of class  player_class */
static int toluaI_get_player_c_player_class_magic_max_spell(lua_State* tolua_S)
{
  player_class* self = (player_class*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->magic_max_spell);
 return 1;
}

/* set function: magic_max_spell of class  player_class */
static int toluaI_set_player_c_player_class_magic_max_spell(lua_State* tolua_S)
{
  player_class* self = (player_class*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->magic_max_spell = ((byte)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: flags1 of class  player_class */
static int toluaI_get_player_c_player_class_flags1(lua_State* tolua_S)
{
  player_class* self = (player_class*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->flags1);
 return 1;
}

/* set function: flags1 of class  player_class */
static int toluaI_set_player_c_player_class_flags1(lua_State* tolua_S)
{
  player_class* self = (player_class*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->flags1 = ((s32b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: mana of class  player_class */
static int toluaI_get_player_c_player_class_mana(lua_State* tolua_S)
{
  player_class* self = (player_class*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->mana);
 return 1;
}

/* set function: mana of class  player_class */
static int toluaI_set_player_c_player_class_mana(lua_State* tolua_S)
{
  player_class* self = (player_class*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->mana = ((s16b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: blow_num of class  player_class */
static int toluaI_get_player_c_player_class_blow_num(lua_State* tolua_S)
{
  player_class* self = (player_class*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->blow_num);
 return 1;
}

/* set function: blow_num of class  player_class */
static int toluaI_set_player_c_player_class_blow_num(lua_State* tolua_S)
{
  player_class* self = (player_class*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->blow_num = ((s16b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: blow_wgt of class  player_class */
static int toluaI_get_player_c_player_class_blow_wgt(lua_State* tolua_S)
{
  player_class* self = (player_class*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->blow_wgt);
 return 1;
}

/* set function: blow_wgt of class  player_class */
static int toluaI_set_player_c_player_class_blow_wgt(lua_State* tolua_S)
{
  player_class* self = (player_class*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->blow_wgt = ((s16b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: blow_mul of class  player_class */
static int toluaI_get_player_c_player_class_blow_mul(lua_State* tolua_S)
{
  player_class* self = (player_class*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->blow_mul);
 return 1;
}

/* set function: blow_mul of class  player_class */
static int toluaI_set_player_c_player_class_blow_mul(lua_State* tolua_S)
{
  player_class* self = (player_class*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->blow_mul = ((s16b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: extra_blows of class  player_class */
static int toluaI_get_player_c_player_class_extra_blows(lua_State* tolua_S)
{
  player_class* self = (player_class*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->extra_blows);
 return 1;
}

/* set function: extra_blows of class  player_class */
static int toluaI_set_player_c_player_class_extra_blows(lua_State* tolua_S)
{
  player_class* self = (player_class*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->extra_blows = ((s16b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: max_companion of class  player_class */
static int toluaI_get_player_c_player_class_max_companion(lua_State* tolua_S)
{
  player_class* self = (player_class*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->max_companion);
 return 1;
}

/* set function: max_companion of class  player_class */
static int toluaI_set_player_c_player_class_max_companion(lua_State* tolua_S)
{
  player_class* self = (player_class*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->max_companion = ((s16b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: max_companion_plus of class  player_class */
static int toluaI_get_player_c_player_class_max_companion_plus(lua_State* tolua_S)
{
  player_class* self = (player_class*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->max_companion_plus);
 return 1;
}

/* set function: max_companion_plus of class  player_class */
static int toluaI_set_player_c_player_class_max_companion_plus(lua_State* tolua_S)
{
  player_class* self = (player_class*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->max_companion_plus = ((s16b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: Mrealm_choices of class  player_class */
static int toluaI_get_player_c_player_class_Mrealm_choices(lua_State* tolua_S)
{
 int toluaI_index;
  player_class* self;
 lua_pushstring(tolua_S,".self");
 lua_rawget(tolua_S,1);
 self = (player_class*)  lua_touserdata(tolua_S,-1);
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 tolua_error(tolua_S,"invalid type in array indexing.");
 toluaI_index = (int)tolua_getnumber(tolua_S,2,0)-1;
 if (toluaI_index<0 || toluaI_index>=2)
 tolua_error(tolua_S,"array indexing out of range.");
 tolua_pushnumber(tolua_S,(long)self->Mrealm_choices[toluaI_index]);
 return 1;
}

/* set function: Mrealm_choices of class  player_class */
static int toluaI_set_player_c_player_class_Mrealm_choices(lua_State* tolua_S)
{
 int toluaI_index;
  player_class* self;
 lua_pushstring(tolua_S,".self");
 lua_rawget(tolua_S,1);
 self = (player_class*)  lua_touserdata(tolua_S,-1);
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 tolua_error(tolua_S,"invalid type in array indexing.");
 toluaI_index = (int)tolua_getnumber(tolua_S,2,0)-1;
 if (toluaI_index<0 || toluaI_index>=2)
 tolua_error(tolua_S,"array indexing out of range.");
  self->Mrealm_choices[toluaI_index] = ((u32b)  tolua_getnumber(tolua_S,3,0));
 return 0;
}

/* get function: mrealm_choices of class  player_class */
static int toluaI_get_player_c_player_class_mrealm_choices(lua_State* tolua_S)
{
 int toluaI_index;
  player_class* self;
 lua_pushstring(tolua_S,".self");
 lua_rawget(tolua_S,1);
 self = (player_class*)  lua_touserdata(tolua_S,-1);
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 tolua_error(tolua_S,"invalid type in array indexing.");
 toluaI_index = (int)tolua_getnumber(tolua_S,2,0)-1;
 if (toluaI_index<0 || toluaI_index>=2)
 tolua_error(tolua_S,"array indexing out of range.");
 tolua_pushnumber(tolua_S,(long)self->mrealm_choices[toluaI_index]);
 return 1;
}

/* set function: mrealm_choices of class  player_class */
static int toluaI_set_player_c_player_class_mrealm_choices(lua_State* tolua_S)
{
 int toluaI_index;
  player_class* self;
 lua_pushstring(tolua_S,".self");
 lua_rawget(tolua_S,1);
 self = (player_class*)  lua_touserdata(tolua_S,-1);
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 tolua_error(tolua_S,"invalid type in array indexing.");
 toluaI_index = (int)tolua_getnumber(tolua_S,2,0)-1;
 if (toluaI_index<0 || toluaI_index>=2)
 tolua_error(tolua_S,"array indexing out of range.");
  self->mrealm_choices[toluaI_index] = ((u32b)  tolua_getnumber(tolua_S,3,0));
 return 0;
}

/* get function: sense_base of class  player_class */
static int toluaI_get_player_c_player_class_sense_base(lua_State* tolua_S)
{
  player_class* self = (player_class*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->sense_base);
 return 1;
}

/* set function: sense_base of class  player_class */
static int toluaI_set_player_c_player_class_sense_base(lua_State* tolua_S)
{
  player_class* self = (player_class*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->sense_base = ((s32b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: sense_pl of class  player_class */
static int toluaI_get_player_c_player_class_sense_pl(lua_State* tolua_S)
{
  player_class* self = (player_class*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->sense_pl);
 return 1;
}

/* set function: sense_pl of class  player_class */
static int toluaI_set_player_c_player_class_sense_pl(lua_State* tolua_S)
{
  player_class* self = (player_class*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->sense_pl = ((s32b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: sense_plus of class  player_class */
static int toluaI_get_player_c_player_class_sense_plus(lua_State* tolua_S)
{
  player_class* self = (player_class*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->sense_plus);
 return 1;
}

/* set function: sense_plus of class  player_class */
static int toluaI_set_player_c_player_class_sense_plus(lua_State* tolua_S)
{
  player_class* self = (player_class*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->sense_plus = ((s32b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: sense_heavy of class  player_class */
static int toluaI_get_player_c_player_class_sense_heavy(lua_State* tolua_S)
{
  player_class* self = (player_class*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->sense_heavy);
 return 1;
}

/* set function: sense_heavy of class  player_class */
static int toluaI_set_player_c_player_class_sense_heavy(lua_State* tolua_S)
{
  player_class* self = (player_class*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->sense_heavy = ((byte)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: sense_heavy_magic of class  player_class */
static int toluaI_get_player_c_player_class_sense_heavy_magic(lua_State* tolua_S)
{
  player_class* self = (player_class*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->sense_heavy_magic);
 return 1;
}

/* set function: sense_heavy_magic of class  player_class */
static int toluaI_set_player_c_player_class_sense_heavy_magic(lua_State* tolua_S)
{
  player_class* self = (player_class*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->sense_heavy_magic = ((byte)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: magic_key of class  player_class */
static int toluaI_get_player_c_player_class_magic_key(lua_State* tolua_S)
{
  player_class* self = (player_class*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->magic_key);
 return 1;
}

/* set function: magic_key of class  player_class */
static int toluaI_set_player_c_player_class_magic_key(lua_State* tolua_S)
{
  player_class* self = (player_class*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->magic_key = ((s32b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: magic_desc of class  player_class */
static int toluaI_get_player_c_player_class_magic_desc(lua_State* tolua_S)
{
  player_class* self = (player_class*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->magic_desc);
 return 1;
}

/* set function: magic_desc of class  player_class */
static int toluaI_set_player_c_player_class_magic_desc(lua_State* tolua_S)
{
  player_class* self = (player_class*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->magic_desc = ((s32b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: magic_disrupt of class  player_class */
static int toluaI_get_player_c_player_class_magic_disrupt(lua_State* tolua_S)
{
  player_class* self = (player_class*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->magic_disrupt);
 return 1;
}

/* set function: magic_disrupt of class  player_class */
static int toluaI_set_player_c_player_class_magic_disrupt(lua_State* tolua_S)
{
  player_class* self = (player_class*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->magic_disrupt = ((byte)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: cp_ptr */
static int toluaI_get_player_c_cp_ptr(lua_State* tolua_S)
{
 tolua_pushusertype(tolua_S,(void*)cp_ptr,tolua_tag(tolua_S,"player_class"));
 return 1;
}

/* set function: cp_ptr */
static int toluaI_set_player_c_cp_ptr(lua_State* tolua_S)
{
 if (!tolua_istype(tolua_S,1,tolua_tag(tolua_S,"player_class"),0))
 TOLUA_ERR_ASSIGN;
  cp_ptr = ((player_class*)  tolua_getusertype(tolua_S,1,0));
 return 0;
}

/* Open function */
int tolua_player_c_open (lua_State* tolua_S)
{
 tolua_open(tolua_S);
 toluaI_reg_types(tolua_S);
 tolua_cclass(tolua_S,"player_class","");
 tolua_tablevar(tolua_S,"player_class","title",toluaI_get_player_c_player_class_title,toluaI_set_player_c_player_class_title);
 tolua_tablevar(tolua_S,"player_class","desc",toluaI_get_player_c_player_class_desc,toluaI_set_player_c_player_class_desc);
 tolua_tablearray(tolua_S,"player_class","titles",toluaI_get_player_c_player_class_titles,toluaI_set_player_c_player_class_titles);
 tolua_tablearray(tolua_S,"player_class","c_adj",toluaI_get_player_c_player_class_c_adj,toluaI_set_player_c_player_class_c_adj);
 tolua_tablevar(tolua_S,"player_class","c_dis",toluaI_get_player_c_player_class_c_dis,toluaI_set_player_c_player_class_c_dis);
 tolua_tablevar(tolua_S,"player_class","c_dev",toluaI_get_player_c_player_class_c_dev,toluaI_set_player_c_player_class_c_dev);
 tolua_tablevar(tolua_S,"player_class","c_sav",toluaI_get_player_c_player_class_c_sav,toluaI_set_player_c_player_class_c_sav);
 tolua_tablevar(tolua_S,"player_class","c_stl",toluaI_get_player_c_player_class_c_stl,toluaI_set_player_c_player_class_c_stl);
 tolua_tablevar(tolua_S,"player_class","c_srh",toluaI_get_player_c_player_class_c_srh,toluaI_set_player_c_player_class_c_srh);
 tolua_tablevar(tolua_S,"player_class","c_fos",toluaI_get_player_c_player_class_c_fos,toluaI_set_player_c_player_class_c_fos);
 tolua_tablevar(tolua_S,"player_class","c_thn",toluaI_get_player_c_player_class_c_thn,toluaI_set_player_c_player_class_c_thn);
 tolua_tablevar(tolua_S,"player_class","c_thb",toluaI_get_player_c_player_class_c_thb,toluaI_set_player_c_player_class_c_thb);
 tolua_tablevar(tolua_S,"player_class","x_dis",toluaI_get_player_c_player_class_x_dis,toluaI_set_player_c_player_class_x_dis);
 tolua_tablevar(tolua_S,"player_class","x_dev",toluaI_get_player_c_player_class_x_dev,toluaI_set_player_c_player_class_x_dev);
 tolua_tablevar(tolua_S,"player_class","x_sav",toluaI_get_player_c_player_class_x_sav,toluaI_set_player_c_player_class_x_sav);
 tolua_tablevar(tolua_S,"player_class","x_stl",toluaI_get_player_c_player_class_x_stl,toluaI_set_player_c_player_class_x_stl);
 tolua_tablevar(tolua_S,"player_class","x_srh",toluaI_get_player_c_player_class_x_srh,toluaI_set_player_c_player_class_x_srh);
 tolua_tablevar(tolua_S,"player_class","x_fos",toluaI_get_player_c_player_class_x_fos,toluaI_set_player_c_player_class_x_fos);
 tolua_tablevar(tolua_S,"player_class","x_thn",toluaI_get_player_c_player_class_x_thn,toluaI_set_player_c_player_class_x_thn);
 tolua_tablevar(tolua_S,"player_class","x_thb",toluaI_get_player_c_player_class_x_thb,toluaI_set_player_c_player_class_x_thb);
 tolua_tablevar(tolua_S,"player_class","c_mhp",toluaI_get_player_c_player_class_c_mhp,toluaI_set_player_c_player_class_c_mhp);
 tolua_tablevar(tolua_S,"player_class","c_exp",toluaI_get_player_c_player_class_c_exp,toluaI_set_player_c_player_class_c_exp);
 tolua_tablearray(tolua_S,"player_class","powers",toluaI_get_player_c_player_class_powers,toluaI_set_player_c_player_class_powers);
 tolua_tablevar(tolua_S,"player_class","spell_book",toluaI_get_player_c_player_class_spell_book,toluaI_set_player_c_player_class_spell_book);
 tolua_tablevar(tolua_S,"player_class","spell_stat",toluaI_get_player_c_player_class_spell_stat,toluaI_set_player_c_player_class_spell_stat);
 tolua_tablevar(tolua_S,"player_class","spell_lev",toluaI_get_player_c_player_class_spell_lev,toluaI_set_player_c_player_class_spell_lev);
 tolua_tablevar(tolua_S,"player_class","spell_fail",toluaI_get_player_c_player_class_spell_fail,toluaI_set_player_c_player_class_spell_fail);
 tolua_tablevar(tolua_S,"player_class","spell_mana",toluaI_get_player_c_player_class_spell_mana,toluaI_set_player_c_player_class_spell_mana);
 tolua_tablevar(tolua_S,"player_class","spell_first",toluaI_get_player_c_player_class_spell_first,toluaI_set_player_c_player_class_spell_first);
 tolua_tablevar(tolua_S,"player_class","spell_weight",toluaI_get_player_c_player_class_spell_weight,toluaI_set_player_c_player_class_spell_weight);
 tolua_tablevar(tolua_S,"player_class","max_spell_level",toluaI_get_player_c_player_class_max_spell_level,toluaI_set_player_c_player_class_max_spell_level);
 tolua_tablevar(tolua_S,"player_class","magic_max_spell",toluaI_get_player_c_player_class_magic_max_spell,toluaI_set_player_c_player_class_magic_max_spell);
 tolua_tablevar(tolua_S,"player_class","flags1",toluaI_get_player_c_player_class_flags1,toluaI_set_player_c_player_class_flags1);
 tolua_tablevar(tolua_S,"player_class","mana",toluaI_get_player_c_player_class_mana,toluaI_set_player_c_player_class_mana);
 tolua_tablevar(tolua_S,"player_class","blow_num",toluaI_get_player_c_player_class_blow_num,toluaI_set_player_c_player_class_blow_num);
 tolua_tablevar(tolua_S,"player_class","blow_wgt",toluaI_get_player_c_player_class_blow_wgt,toluaI_set_player_c_player_class_blow_wgt);
 tolua_tablevar(tolua_S,"player_class","blow_mul",toluaI_get_player_c_player_class_blow_mul,toluaI_set_player_c_player_class_blow_mul);
 tolua_tablevar(tolua_S,"player_class","extra_blows",toluaI_get_player_c_player_class_extra_blows,toluaI_set_player_c_player_class_extra_blows);
 tolua_tablevar(tolua_S,"player_class","max_companion",toluaI_get_player_c_player_class_max_companion,toluaI_set_player_c_player_class_max_companion);
 tolua_tablevar(tolua_S,"player_class","max_companion_plus",toluaI_get_player_c_player_class_max_companion_plus,toluaI_set_player_c_player_class_max_companion_plus);
 tolua_tablearray(tolua_S,"player_class","Mrealm_choices",toluaI_get_player_c_player_class_Mrealm_choices,toluaI_set_player_c_player_class_Mrealm_choices);
 tolua_tablearray(tolua_S,"player_class","mrealm_choices",toluaI_get_player_c_player_class_mrealm_choices,toluaI_set_player_c_player_class_mrealm_choices);
 tolua_tablevar(tolua_S,"player_class","sense_base",toluaI_get_player_c_player_class_sense_base,toluaI_set_player_c_player_class_sense_base);
 tolua_tablevar(tolua_S,"player_class","sense_pl",toluaI_get_player_c_player_class_sense_pl,toluaI_set_player_c_player_class_sense_pl);
 tolua_tablevar(tolua_S,"player_class","sense_plus",toluaI_get_player_c_player_class_sense_plus,toluaI_set_player_c_player_class_sense_plus);
 tolua_tablevar(tolua_S,"player_class","sense_heavy",toluaI_get_player_c_player_class_sense_heavy,toluaI_set_player_c_player_class_sense_heavy);
 tolua_tablevar(tolua_S,"player_class","sense_heavy_magic",toluaI_get_player_c_player_class_sense_heavy_magic,toluaI_set_player_c_player_class_sense_heavy_magic);
 tolua_tablevar(tolua_S,"player_class","magic_key",toluaI_get_player_c_player_class_magic_key,toluaI_set_player_c_player_class_magic_key);
 tolua_tablevar(tolua_S,"player_class","magic_desc",toluaI_get_player_c_player_class_magic_desc,toluaI_set_player_c_player_class_magic_desc);
 tolua_tablevar(tolua_S,"player_class","magic_disrupt",toluaI_get_player_c_player_class_magic_disrupt,toluaI_set_player_c_player_class_magic_disrupt);
 tolua_globalvar(tolua_S,"cp_ptr",toluaI_get_player_c_cp_ptr,toluaI_set_player_c_cp_ptr);
 return 1;
}
/* Close function */
void tolua_player_c_close (lua_State* tolua_S)
{
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"player_class");
 lua_getglobals(tolua_S);
 lua_pushstring(tolua_S,"cp_ptr"); lua_pushnil(tolua_S); lua_rawset(tolua_S,-3);
 lua_pop(tolua_S,1);
}
