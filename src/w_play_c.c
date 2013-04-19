/*
** Lua binding: player_c
** Generated automatically by tolua 4.0a - angband on Fri Jan 30 19:43:25 2004.
*/

#include "lua/tolua.h"

/* Exported function */
int tolua_player_c_open (lua_State* tolua_S);
void tolua_player_c_close (lua_State* tolua_S);

#include "angband.h"
static cptr get_skill_name(int i) { return s_name + s_info[i].name; }
static char *get_class_name() {return spp_ptr->title + c_name;}
static char *get_race_name() {return rp_ptr->title + rp_name;}
static char *get_subrace_name() {return rmp_ptr->title + rmp_name;}

/* function to register type */
static void toluaI_reg_types (lua_State* tolua_S)
{
 tolua_usertype(tolua_S,"player_class");
 tolua_usertype(tolua_S,"skill_type");
 tolua_usertype(tolua_S,"ability_type");
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

/* get function: name of class  skill_type */
static int toluaI_get_player_c_skill_type_name(lua_State* tolua_S)
{
  skill_type* self = (skill_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->name);
 return 1;
}

/* set function: name of class  skill_type */
static int toluaI_set_player_c_skill_type_name(lua_State* tolua_S)
{
  skill_type* self = (skill_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->name = ((u32b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: desc of class  skill_type */
static int toluaI_get_player_c_skill_type_desc(lua_State* tolua_S)
{
  skill_type* self = (skill_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->desc);
 return 1;
}

/* set function: desc of class  skill_type */
static int toluaI_set_player_c_skill_type_desc(lua_State* tolua_S)
{
  skill_type* self = (skill_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->desc = ((u32b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: action_desc of class  skill_type */
static int toluaI_get_player_c_skill_type_action_desc(lua_State* tolua_S)
{
  skill_type* self = (skill_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->action_desc);
 return 1;
}

/* set function: action_desc of class  skill_type */
static int toluaI_set_player_c_skill_type_action_desc(lua_State* tolua_S)
{
  skill_type* self = (skill_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->action_desc = ((u32b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: action_mkey of class  skill_type */
static int toluaI_get_player_c_skill_type_action_mkey(lua_State* tolua_S)
{
  skill_type* self = (skill_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->action_mkey);
 return 1;
}

/* set function: action_mkey of class  skill_type */
static int toluaI_set_player_c_skill_type_action_mkey(lua_State* tolua_S)
{
  skill_type* self = (skill_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->action_mkey = ((s16b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: i_value of class  skill_type */
static int toluaI_get_player_c_skill_type_i_value(lua_State* tolua_S)
{
  skill_type* self = (skill_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->i_value);
 return 1;
}

/* set function: i_value of class  skill_type */
static int toluaI_set_player_c_skill_type_i_value(lua_State* tolua_S)
{
  skill_type* self = (skill_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->i_value = ((u32b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: i_mod of class  skill_type */
static int toluaI_get_player_c_skill_type_i_mod(lua_State* tolua_S)
{
  skill_type* self = (skill_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->i_mod);
 return 1;
}

/* set function: i_mod of class  skill_type */
static int toluaI_set_player_c_skill_type_i_mod(lua_State* tolua_S)
{
  skill_type* self = (skill_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->i_mod = ((u16b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: value of class  skill_type */
static int toluaI_get_player_c_skill_type_value(lua_State* tolua_S)
{
  skill_type* self = (skill_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->value);
 return 1;
}

/* set function: value of class  skill_type */
static int toluaI_set_player_c_skill_type_value(lua_State* tolua_S)
{
  skill_type* self = (skill_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->value = ((u32b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: mod of class  skill_type */
static int toluaI_get_player_c_skill_type_mod(lua_State* tolua_S)
{
  skill_type* self = (skill_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->mod);
 return 1;
}

/* set function: mod of class  skill_type */
static int toluaI_set_player_c_skill_type_mod(lua_State* tolua_S)
{
  skill_type* self = (skill_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->mod = ((u16b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: rate of class  skill_type */
static int toluaI_get_player_c_skill_type_rate(lua_State* tolua_S)
{
  skill_type* self = (skill_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->rate);
 return 1;
}

/* set function: rate of class  skill_type */
static int toluaI_set_player_c_skill_type_rate(lua_State* tolua_S)
{
  skill_type* self = (skill_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->rate = ((s16b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: uses of class  skill_type */
static int toluaI_get_player_c_skill_type_uses(lua_State* tolua_S)
{
  skill_type* self = (skill_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->uses);
 return 1;
}

/* set function: uses of class  skill_type */
static int toluaI_set_player_c_skill_type_uses(lua_State* tolua_S)
{
  skill_type* self = (skill_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->uses = ((u32b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: action of class  skill_type */
static int toluaI_get_player_c_skill_type_action(lua_State* tolua_S)
{
 int toluaI_index;
  skill_type* self;
 lua_pushstring(tolua_S,".self");
 lua_rawget(tolua_S,1);
 self = (skill_type*)  lua_touserdata(tolua_S,-1);
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 tolua_error(tolua_S,"invalid type in array indexing.");
 toluaI_index = (int)tolua_getnumber(tolua_S,2,0)-1;
 if (toluaI_index<0 || toluaI_index>=9999)
 tolua_error(tolua_S,"array indexing out of range.");
 tolua_pushnumber(tolua_S,(long)self->action[toluaI_index]);
 return 1;
}

/* set function: action of class  skill_type */
static int toluaI_set_player_c_skill_type_action(lua_State* tolua_S)
{
 int toluaI_index;
  skill_type* self;
 lua_pushstring(tolua_S,".self");
 lua_rawget(tolua_S,1);
 self = (skill_type*)  lua_touserdata(tolua_S,-1);
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 tolua_error(tolua_S,"invalid type in array indexing.");
 toluaI_index = (int)tolua_getnumber(tolua_S,2,0)-1;
 if (toluaI_index<0 || toluaI_index>=9999)
 tolua_error(tolua_S,"array indexing out of range.");
  self->action[toluaI_index] = ((s16b)  tolua_getnumber(tolua_S,3,0));
 return 0;
}

/* get function: father of class  skill_type */
static int toluaI_get_player_c_skill_type_father(lua_State* tolua_S)
{
  skill_type* self = (skill_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->father);
 return 1;
}

/* set function: father of class  skill_type */
static int toluaI_set_player_c_skill_type_father(lua_State* tolua_S)
{
  skill_type* self = (skill_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->father = ((s16b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: dev of class  skill_type */
static int toluaI_get_player_c_skill_type_dev(lua_State* tolua_S)
{
  skill_type* self = (skill_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->dev);
 return 1;
}

/* set function: dev of class  skill_type */
static int toluaI_set_player_c_skill_type_dev(lua_State* tolua_S)
{
  skill_type* self = (skill_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->dev = ((bool)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: order of class  skill_type */
static int toluaI_get_player_c_skill_type_order(lua_State* tolua_S)
{
  skill_type* self = (skill_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->order);
 return 1;
}

/* set function: order of class  skill_type */
static int toluaI_set_player_c_skill_type_order(lua_State* tolua_S)
{
  skill_type* self = (skill_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->order = ((s16b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: hidden of class  skill_type */
static int toluaI_get_player_c_skill_type_hidden(lua_State* tolua_S)
{
  skill_type* self = (skill_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->hidden);
 return 1;
}

/* set function: hidden of class  skill_type */
static int toluaI_set_player_c_skill_type_hidden(lua_State* tolua_S)
{
  skill_type* self = (skill_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->hidden = ((bool)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* function: get_skill_name */
static int toluaI_player_c_get_skill_name00(lua_State* tolua_S)
{
 if (
 !tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
 !tolua_isnoobj(tolua_S,2)
 )
 goto tolua_lerror;
 else
 {
  int i = ((int)  tolua_getnumber(tolua_S,1,0));
 {
  cptr toluaI_ret = (cptr)  get_skill_name(i);
 tolua_pushstring(tolua_S,(const char*)toluaI_ret);
 }
 }
 return 1;
tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'get_skill_name'.");
 return 0;
}

/* get function: old_max_s_idx */
static int toluaI_get_player_c_old_max_s_idx(lua_State* tolua_S)
{
 tolua_pushnumber(tolua_S,(long)old_max_s_idx);
 return 1;
}

/* set function: old_max_s_idx */
static int toluaI_set_player_c_old_max_s_idx(lua_State* tolua_S)
{
 if (!tolua_istype(tolua_S,1,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  old_max_s_idx = ((u16b)  tolua_getnumber(tolua_S,1,0));
 return 0;
}

/* get function: max_s_idx */
static int toluaI_get_player_c_max_s_idx(lua_State* tolua_S)
{
 tolua_pushnumber(tolua_S,(long)max_s_idx);
 return 1;
}

/* set function: max_s_idx */
static int toluaI_set_player_c_max_s_idx(lua_State* tolua_S)
{
 if (!tolua_istype(tolua_S,1,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  max_s_idx = ((u16b)  tolua_getnumber(tolua_S,1,0));
 return 0;
}

/* get function: s_info */
static int toluaI_get_player_c_s_info(lua_State* tolua_S)
{
 int toluaI_index;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 tolua_error(tolua_S,"invalid type in array indexing.");
 toluaI_index = (int)tolua_getnumber(tolua_S,2,0)-1;
 if (toluaI_index<0 || toluaI_index>=MAX_SKILLS)
 tolua_error(tolua_S,"array indexing out of range.");
 tolua_pushusertype(tolua_S,(void*)&s_info[toluaI_index],tolua_tag(tolua_S,"skill_type"));
 return 1;
}

/* set function: s_info */
static int toluaI_set_player_c_s_info(lua_State* tolua_S)
{
 int toluaI_index;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 tolua_error(tolua_S,"invalid type in array indexing.");
 toluaI_index = (int)tolua_getnumber(tolua_S,2,0)-1;
 if (toluaI_index<0 || toluaI_index>=MAX_SKILLS)
 tolua_error(tolua_S,"array indexing out of range.");
  s_info[toluaI_index] = *((skill_type*)  tolua_getusertype(tolua_S,3,0));
 return 0;
}

/* function: get_skill */
static int toluaI_player_c_get_skill00(lua_State* tolua_S)
{
 if (
 !tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
 !tolua_isnoobj(tolua_S,2)
 )
 goto tolua_lerror;
 else
 {
  int skill = ((int)  tolua_getnumber(tolua_S,1,0));
 {
  s16b toluaI_ret = (s16b)  get_skill(skill);
 tolua_pushnumber(tolua_S,(long)toluaI_ret);
 }
 }
 return 1;
tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'get_skill'.");
 return 0;
}

/* function: get_skill_scale */
static int toluaI_player_c_get_skill_scale00(lua_State* tolua_S)
{
 if (
 !tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
 !tolua_istype(tolua_S,2,LUA_TNUMBER,0) ||
 !tolua_isnoobj(tolua_S,3)
 )
 goto tolua_lerror;
 else
 {
  int skill = ((int)  tolua_getnumber(tolua_S,1,0));
  u32b scale = ((u32b)  tolua_getnumber(tolua_S,2,0));
 {
  s16b toluaI_ret = (s16b)  get_skill_scale(skill,scale);
 tolua_pushnumber(tolua_S,(long)toluaI_ret);
 }
 }
 return 1;
tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'get_skill_scale'.");
 return 0;
}

/* function: do_get_new_skill */
static int toluaI_player_c_do_get_new_skill00(lua_State* tolua_S)
{
 if (
 !tolua_isnoobj(tolua_S,1)
 )
 goto tolua_lerror;
 else
 {
 {
  do_get_new_skill();
 }
 }
 return 0;
tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'do_get_new_skill'.");
 return 0;
}

/* function: get_melee_skills */
static int toluaI_player_c_get_melee_skills00(lua_State* tolua_S)
{
 if (
 !tolua_isnoobj(tolua_S,1)
 )
 goto tolua_lerror;
 else
 {
 {
  s16b toluaI_ret = (s16b)  get_melee_skills();
 tolua_pushnumber(tolua_S,(long)toluaI_ret);
 }
 }
 return 1;
tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'get_melee_skills'.");
 return 0;
}

/* function: find_skill */
static int toluaI_player_c_find_skill00(lua_State* tolua_S)
{
 if (
 !tolua_istype(tolua_S,1,LUA_TSTRING,0) ||
 !tolua_isnoobj(tolua_S,2)
 )
 goto tolua_lerror;
 else
 {
  cptr name = ((cptr)  tolua_getstring(tolua_S,1,0));
 {
  s16b toluaI_ret = (s16b)  find_skill(name);
 tolua_pushnumber(tolua_S,(long)toluaI_ret);
 }
 }
 return 1;
tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'find_skill'.");
 return 0;
}

/* function: find_skill_i */
static int toluaI_player_c_find_skill_i00(lua_State* tolua_S)
{
 if (
 !tolua_istype(tolua_S,1,LUA_TSTRING,0) ||
 !tolua_isnoobj(tolua_S,2)
 )
 goto tolua_lerror;
 else
 {
  cptr name = ((cptr)  tolua_getstring(tolua_S,1,0));
 {
  s16b toluaI_ret = (s16b)  find_skill_i(name);
 tolua_pushnumber(tolua_S,(long)toluaI_ret);
 }
 }
 return 1;
tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'find_skill_i'.");
 return 0;
}

/* function: get_class_name */
static int toluaI_player_c_get_class_name00(lua_State* tolua_S)
{
 if (
 !tolua_isnoobj(tolua_S,1)
 )
 goto tolua_lerror;
 else
 {
 {
  char* toluaI_ret = (char*)  get_class_name();
 tolua_pushstring(tolua_S,(const char*)toluaI_ret);
 }
 }
 return 1;
tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'get_class_name'.");
 return 0;
}

/* function: get_race_name */
static int toluaI_player_c_get_race_name00(lua_State* tolua_S)
{
 if (
 !tolua_isnoobj(tolua_S,1)
 )
 goto tolua_lerror;
 else
 {
 {
  char* toluaI_ret = (char*)  get_race_name();
 tolua_pushstring(tolua_S,(const char*)toluaI_ret);
 }
 }
 return 1;
tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'get_race_name'.");
 return 0;
}

/* function: get_subrace_name */
static int toluaI_player_c_get_subrace_name00(lua_State* tolua_S)
{
 if (
 !tolua_isnoobj(tolua_S,1)
 )
 goto tolua_lerror;
 else
 {
 {
  char* toluaI_ret = (char*)  get_subrace_name();
 tolua_pushstring(tolua_S,(const char*)toluaI_ret);
 }
 }
 return 1;
tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'get_subrace_name'.");
 return 0;
}

/* get function: action_mkey of class  ability_type */
static int toluaI_get_player_c_ability_type_action_mkey(lua_State* tolua_S)
{
  ability_type* self = (ability_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->action_mkey);
 return 1;
}

/* set function: action_mkey of class  ability_type */
static int toluaI_set_player_c_ability_type_action_mkey(lua_State* tolua_S)
{
  ability_type* self = (ability_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->action_mkey = ((s16b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: cost of class  ability_type */
static int toluaI_get_player_c_ability_type_cost(lua_State* tolua_S)
{
  ability_type* self = (ability_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->cost);
 return 1;
}

/* set function: cost of class  ability_type */
static int toluaI_set_player_c_ability_type_cost(lua_State* tolua_S)
{
  ability_type* self = (ability_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->cost = ((s16b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: acquired of class  ability_type */
static int toluaI_get_player_c_ability_type_acquired(lua_State* tolua_S)
{
  ability_type* self = (ability_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->acquired);
 return 1;
}

/* set function: acquired of class  ability_type */
static int toluaI_set_player_c_ability_type_acquired(lua_State* tolua_S)
{
  ability_type* self = (ability_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->acquired = ((bool)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* function: find_ability */
static int toluaI_player_c_find_ability00(lua_State* tolua_S)
{
 if (
 !tolua_istype(tolua_S,1,LUA_TSTRING,0) ||
 !tolua_isnoobj(tolua_S,2)
 )
 goto tolua_lerror;
 else
 {
  cptr name = ((cptr)  tolua_getstring(tolua_S,1,0));
 {
  s16b toluaI_ret = (s16b)  find_ability(name);
 tolua_pushnumber(tolua_S,(long)toluaI_ret);
 }
 }
 return 1;
tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'find_ability'.");
 return 0;
}

/* function: do_cmd_ability */
static int toluaI_player_c_do_cmd_ability00(lua_State* tolua_S)
{
 if (
 !tolua_isnoobj(tolua_S,1)
 )
 goto tolua_lerror;
 else
 {
 {
  do_cmd_ability();
 }
 }
 return 0;
tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'do_cmd_ability'.");
 return 0;
}

/* function: has_ability */
static int toluaI_player_c_has_ability00(lua_State* tolua_S)
{
 if (
 !tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
 !tolua_isnoobj(tolua_S,2)
 )
 goto tolua_lerror;
 else
 {
  int ab = ((int)  tolua_getnumber(tolua_S,1,0));
 {
  bool toluaI_ret = (bool)  has_ability(ab);
 tolua_pushnumber(tolua_S,(long)toluaI_ret);
 }
 }
 return 1;
tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'has_ability'.");
 return 0;
}

/* get function: max_ab_idx */
static int toluaI_get_player_c_max_ab_idx(lua_State* tolua_S)
{
 tolua_pushnumber(tolua_S,(long)max_ab_idx);
 return 1;
}

/* set function: max_ab_idx */
static int toluaI_set_player_c_max_ab_idx(lua_State* tolua_S)
{
 if (!tolua_istype(tolua_S,1,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  max_ab_idx = ((s16b)  tolua_getnumber(tolua_S,1,0));
 return 0;
}

/* get function: ab_info */
static int toluaI_get_player_c_ab_info(lua_State* tolua_S)
{
 int toluaI_index;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 tolua_error(tolua_S,"invalid type in array indexing.");
 toluaI_index = (int)tolua_getnumber(tolua_S,2,0)-1;
 if (toluaI_index<0 || toluaI_index>=max_ab_idx)
 tolua_error(tolua_S,"array indexing out of range.");
 tolua_pushusertype(tolua_S,(void*)&ab_info[toluaI_index],tolua_tag(tolua_S,"ability_type"));
 return 1;
}

/* set function: ab_info */
static int toluaI_set_player_c_ab_info(lua_State* tolua_S)
{
 int toluaI_index;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 tolua_error(tolua_S,"invalid type in array indexing.");
 toluaI_index = (int)tolua_getnumber(tolua_S,2,0)-1;
 if (toluaI_index<0 || toluaI_index>=max_ab_idx)
 tolua_error(tolua_S,"array indexing out of range.");
  ab_info[toluaI_index] = *((ability_type*)  tolua_getusertype(tolua_S,3,0));
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
 tolua_tablevar(tolua_S,"player_class","sense_base",toluaI_get_player_c_player_class_sense_base,toluaI_set_player_c_player_class_sense_base);
 tolua_tablevar(tolua_S,"player_class","sense_pl",toluaI_get_player_c_player_class_sense_pl,toluaI_set_player_c_player_class_sense_pl);
 tolua_tablevar(tolua_S,"player_class","sense_plus",toluaI_get_player_c_player_class_sense_plus,toluaI_set_player_c_player_class_sense_plus);
 tolua_tablevar(tolua_S,"player_class","sense_heavy",toluaI_get_player_c_player_class_sense_heavy,toluaI_set_player_c_player_class_sense_heavy);
 tolua_tablevar(tolua_S,"player_class","sense_heavy_magic",toluaI_get_player_c_player_class_sense_heavy_magic,toluaI_set_player_c_player_class_sense_heavy_magic);
 tolua_globalvar(tolua_S,"cp_ptr",toluaI_get_player_c_cp_ptr,toluaI_set_player_c_cp_ptr);
 tolua_cclass(tolua_S,"skill_type","");
 tolua_tablevar(tolua_S,"skill_type","name",toluaI_get_player_c_skill_type_name,toluaI_set_player_c_skill_type_name);
 tolua_tablevar(tolua_S,"skill_type","desc",toluaI_get_player_c_skill_type_desc,toluaI_set_player_c_skill_type_desc);
 tolua_tablevar(tolua_S,"skill_type","action_desc",toluaI_get_player_c_skill_type_action_desc,toluaI_set_player_c_skill_type_action_desc);
 tolua_tablevar(tolua_S,"skill_type","action_mkey",toluaI_get_player_c_skill_type_action_mkey,toluaI_set_player_c_skill_type_action_mkey);
 tolua_tablevar(tolua_S,"skill_type","i_value",toluaI_get_player_c_skill_type_i_value,toluaI_set_player_c_skill_type_i_value);
 tolua_tablevar(tolua_S,"skill_type","i_mod",toluaI_get_player_c_skill_type_i_mod,toluaI_set_player_c_skill_type_i_mod);
 tolua_tablevar(tolua_S,"skill_type","value",toluaI_get_player_c_skill_type_value,toluaI_set_player_c_skill_type_value);
 tolua_tablevar(tolua_S,"skill_type","mod",toluaI_get_player_c_skill_type_mod,toluaI_set_player_c_skill_type_mod);
 tolua_tablevar(tolua_S,"skill_type","rate",toluaI_get_player_c_skill_type_rate,toluaI_set_player_c_skill_type_rate);
 tolua_tablevar(tolua_S,"skill_type","uses",toluaI_get_player_c_skill_type_uses,toluaI_set_player_c_skill_type_uses);
 tolua_tablearray(tolua_S,"skill_type","action",toluaI_get_player_c_skill_type_action,toluaI_set_player_c_skill_type_action);
 tolua_tablevar(tolua_S,"skill_type","father",toluaI_get_player_c_skill_type_father,toluaI_set_player_c_skill_type_father);
 tolua_tablevar(tolua_S,"skill_type","dev",toluaI_get_player_c_skill_type_dev,toluaI_set_player_c_skill_type_dev);
 tolua_tablevar(tolua_S,"skill_type","order",toluaI_get_player_c_skill_type_order,toluaI_set_player_c_skill_type_order);
 tolua_tablevar(tolua_S,"skill_type","hidden",toluaI_get_player_c_skill_type_hidden,toluaI_set_player_c_skill_type_hidden);
 tolua_constant(tolua_S,NULL,"MAX_SKILLS",MAX_SKILLS);
 tolua_function(tolua_S,NULL,"get_skill_name",toluaI_player_c_get_skill_name00);
 tolua_globalvar(tolua_S,"old_max_s_idx",toluaI_get_player_c_old_max_s_idx,toluaI_set_player_c_old_max_s_idx);
 tolua_globalvar(tolua_S,"max_s_idx",toluaI_get_player_c_max_s_idx,toluaI_set_player_c_max_s_idx);
 tolua_globalarray(tolua_S,"s_info",toluaI_get_player_c_s_info,toluaI_set_player_c_s_info);
 tolua_constant(tolua_S,NULL,"SKILL_CONVEYANCE",SKILL_CONVEYANCE);
 tolua_constant(tolua_S,NULL,"SKILL_MANA",SKILL_MANA);
 tolua_constant(tolua_S,NULL,"SKILL_FIRE",SKILL_FIRE);
 tolua_constant(tolua_S,NULL,"SKILL_AIR",SKILL_AIR);
 tolua_constant(tolua_S,NULL,"SKILL_WATER",SKILL_WATER);
 tolua_constant(tolua_S,NULL,"SKILL_NATURE",SKILL_NATURE);
 tolua_constant(tolua_S,NULL,"SKILL_EARTH",SKILL_EARTH);
 tolua_constant(tolua_S,NULL,"SKILL_SYMBIOTIC",SKILL_SYMBIOTIC);
 tolua_constant(tolua_S,NULL,"SKILL_MUSIC",SKILL_MUSIC);
 tolua_constant(tolua_S,NULL,"SKILL_DIVINATION",SKILL_DIVINATION);
 tolua_constant(tolua_S,NULL,"SKILL_TEMPORAL",SKILL_TEMPORAL);
 tolua_constant(tolua_S,NULL,"SKILL_DRUID",SKILL_DRUID);
 tolua_constant(tolua_S,NULL,"SKILL_DAEMON",SKILL_DAEMON);
 tolua_constant(tolua_S,NULL,"SKILL_META",SKILL_META);
 tolua_constant(tolua_S,NULL,"SKILL_MAGIC",SKILL_MAGIC);
 tolua_constant(tolua_S,NULL,"SKILL_COMBAT",SKILL_COMBAT);
 tolua_constant(tolua_S,NULL,"SKILL_MASTERY",SKILL_MASTERY);
 tolua_constant(tolua_S,NULL,"SKILL_SWORD",SKILL_SWORD);
 tolua_constant(tolua_S,NULL,"SKILL_AXE",SKILL_AXE);
 tolua_constant(tolua_S,NULL,"SKILL_POLEARM",SKILL_POLEARM);
 tolua_constant(tolua_S,NULL,"SKILL_HAFTED",SKILL_HAFTED);
 tolua_constant(tolua_S,NULL,"SKILL_BACKSTAB",SKILL_BACKSTAB);
 tolua_constant(tolua_S,NULL,"SKILL_ARCHERY",SKILL_ARCHERY);
 tolua_constant(tolua_S,NULL,"SKILL_SLING",SKILL_SLING);
 tolua_constant(tolua_S,NULL,"SKILL_BOW",SKILL_BOW);
 tolua_constant(tolua_S,NULL,"SKILL_XBOW",SKILL_XBOW);
 tolua_constant(tolua_S,NULL,"SKILL_BOOMERANG",SKILL_BOOMERANG);
 tolua_constant(tolua_S,NULL,"SKILL_SPIRITUALITY",SKILL_SPIRITUALITY);
 tolua_constant(tolua_S,NULL,"SKILL_MINDCRAFT",SKILL_MINDCRAFT);
 tolua_constant(tolua_S,NULL,"SKILL_MISC",SKILL_MISC);
 tolua_constant(tolua_S,NULL,"SKILL_NECROMANCY",SKILL_NECROMANCY);
 tolua_constant(tolua_S,NULL,"SKILL_MIMICRY",SKILL_MIMICRY);
 tolua_constant(tolua_S,NULL,"SKILL_ANTIMAGIC",SKILL_ANTIMAGIC);
 tolua_constant(tolua_S,NULL,"SKILL_RUNECRAFT",SKILL_RUNECRAFT);
 tolua_constant(tolua_S,NULL,"SKILL_SNEAK",SKILL_SNEAK);
 tolua_constant(tolua_S,NULL,"SKILL_STEALTH",SKILL_STEALTH);
 tolua_constant(tolua_S,NULL,"SKILL_DISARMING",SKILL_DISARMING);
 tolua_constant(tolua_S,NULL,"SKILL_ALCHEMY",SKILL_ALCHEMY);
 tolua_constant(tolua_S,NULL,"SKILL_STEALING",SKILL_STEALING);
 tolua_constant(tolua_S,NULL,"SKILL_SORCERY",SKILL_SORCERY);
 tolua_constant(tolua_S,NULL,"SKILL_HAND",SKILL_HAND);
 tolua_constant(tolua_S,NULL,"SKILL_THAUMATURGY",SKILL_THAUMATURGY);
 tolua_constant(tolua_S,NULL,"SKILL_SUMMON",SKILL_SUMMON);
 tolua_constant(tolua_S,NULL,"SKILL_SPELL",SKILL_SPELL);
 tolua_constant(tolua_S,NULL,"SKILL_DODGE",SKILL_DODGE);
 tolua_constant(tolua_S,NULL,"SKILL_BEAR",SKILL_BEAR);
 tolua_constant(tolua_S,NULL,"SKILL_LORE",SKILL_LORE);
 tolua_constant(tolua_S,NULL,"SKILL_PRESERVATION",SKILL_PRESERVATION);
 tolua_constant(tolua_S,NULL,"SKILL_POSSESSION",SKILL_POSSESSION);
 tolua_constant(tolua_S,NULL,"SKILL_MIND",SKILL_MIND);
 tolua_constant(tolua_S,NULL,"SKILL_CRITS",SKILL_CRITS);
 tolua_constant(tolua_S,NULL,"SKILL_PRAY",SKILL_PRAY);
 tolua_constant(tolua_S,NULL,"SKILL_LEARN",SKILL_LEARN);
 tolua_constant(tolua_S,NULL,"SKILL_UDUN",SKILL_UDUN);
 tolua_constant(tolua_S,NULL,"SKILL_DEVICE",SKILL_DEVICE);
 tolua_constant(tolua_S,NULL,"SKILL_STUN",SKILL_STUN);
 tolua_constant(tolua_S,NULL,"SKILL_BOULDER",SKILL_BOULDER);
 tolua_constant(tolua_S,NULL,"SKILL_GEOMANCY",SKILL_GEOMANCY);
 tolua_constant(tolua_S,NULL,"SKILL_MAX",SKILL_MAX);
 tolua_constant(tolua_S,NULL,"SKILL_STEP",SKILL_STEP);
 tolua_function(tolua_S,NULL,"get_skill",toluaI_player_c_get_skill00);
 tolua_function(tolua_S,NULL,"get_skill_scale",toluaI_player_c_get_skill_scale00);
 tolua_function(tolua_S,NULL,"do_get_new_skill",toluaI_player_c_do_get_new_skill00);
 tolua_function(tolua_S,NULL,"get_melee_skills",toluaI_player_c_get_melee_skills00);
 tolua_function(tolua_S,NULL,"find_skill",toluaI_player_c_find_skill00);
 tolua_function(tolua_S,NULL,"find_skill_i",toluaI_player_c_find_skill_i00);
 tolua_function(tolua_S,NULL,"get_class_name",toluaI_player_c_get_class_name00);
 tolua_function(tolua_S,NULL,"get_race_name",toluaI_player_c_get_race_name00);
 tolua_function(tolua_S,NULL,"get_subrace_name",toluaI_player_c_get_subrace_name00);
 tolua_cclass(tolua_S,"ability_type","");
 tolua_tablevar(tolua_S,"ability_type","action_mkey",toluaI_get_player_c_ability_type_action_mkey,toluaI_set_player_c_ability_type_action_mkey);
 tolua_tablevar(tolua_S,"ability_type","cost",toluaI_get_player_c_ability_type_cost,toluaI_set_player_c_ability_type_cost);
 tolua_tablevar(tolua_S,"ability_type","acquired",toluaI_get_player_c_ability_type_acquired,toluaI_set_player_c_ability_type_acquired);
 tolua_function(tolua_S,NULL,"find_ability",toluaI_player_c_find_ability00);
 tolua_function(tolua_S,NULL,"do_cmd_ability",toluaI_player_c_do_cmd_ability00);
 tolua_function(tolua_S,NULL,"has_ability",toluaI_player_c_has_ability00);
 tolua_globalvar(tolua_S,"max_ab_idx",toluaI_get_player_c_max_ab_idx,toluaI_set_player_c_max_ab_idx);
 tolua_globalarray(tolua_S,"ab_info",toluaI_get_player_c_ab_info,toluaI_set_player_c_ab_info);
 tolua_constant(tolua_S,NULL,"AB_SPREAD_BLOWS",AB_SPREAD_BLOWS);
 tolua_constant(tolua_S,NULL,"AB_TREE_WALK",AB_TREE_WALK);
 tolua_constant(tolua_S,NULL,"AB_PERFECT_CASTING",AB_PERFECT_CASTING);
 tolua_constant(tolua_S,NULL,"AB_MAX_BLOW1",AB_MAX_BLOW1);
 tolua_constant(tolua_S,NULL,"AB_MAX_BLOW2",AB_MAX_BLOW2);
 tolua_constant(tolua_S,NULL,"AB_AMMO_CREATION",AB_AMMO_CREATION);
 tolua_constant(tolua_S,NULL,"AB_DEATH_TOUCH",AB_DEATH_TOUCH);
 tolua_constant(tolua_S,NULL,"AB_CREATE_ART",AB_CREATE_ART);
 tolua_constant(tolua_S,NULL,"AB_FAR_REACHING",AB_FAR_REACHING);
 tolua_constant(tolua_S,NULL,"AB_TRAPPING",AB_TRAPPING);
 tolua_constant(tolua_S,NULL,"AB_UNDEAD_FORM",AB_UNDEAD_FORM);
 return 1;
}
/* Close function */
void tolua_player_c_close (lua_State* tolua_S)
{
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"player_class");
 lua_getglobals(tolua_S);
 lua_pushstring(tolua_S,"cp_ptr"); lua_pushnil(tolua_S); lua_rawset(tolua_S,-3);
 lua_pop(tolua_S,1);
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"skill_type");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"MAX_SKILLS");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"get_skill_name");
 lua_getglobals(tolua_S);
 lua_pushstring(tolua_S,"old_max_s_idx"); lua_pushnil(tolua_S); lua_rawset(tolua_S,-3);
 lua_pop(tolua_S,1);
 lua_getglobals(tolua_S);
 lua_pushstring(tolua_S,"max_s_idx"); lua_pushnil(tolua_S); lua_rawset(tolua_S,-3);
 lua_pop(tolua_S,1);
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"s_info");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SKILL_CONVEYANCE");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SKILL_MANA");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SKILL_FIRE");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SKILL_AIR");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SKILL_WATER");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SKILL_NATURE");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SKILL_EARTH");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SKILL_SYMBIOTIC");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SKILL_MUSIC");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SKILL_DIVINATION");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SKILL_TEMPORAL");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SKILL_DRUID");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SKILL_DAEMON");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SKILL_META");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SKILL_MAGIC");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SKILL_COMBAT");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SKILL_MASTERY");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SKILL_SWORD");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SKILL_AXE");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SKILL_POLEARM");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SKILL_HAFTED");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SKILL_BACKSTAB");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SKILL_ARCHERY");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SKILL_SLING");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SKILL_BOW");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SKILL_XBOW");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SKILL_BOOMERANG");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SKILL_SPIRITUALITY");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SKILL_MINDCRAFT");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SKILL_MISC");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SKILL_NECROMANCY");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SKILL_MIMICRY");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SKILL_ANTIMAGIC");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SKILL_RUNECRAFT");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SKILL_SNEAK");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SKILL_STEALTH");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SKILL_DISARMING");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SKILL_ALCHEMY");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SKILL_STEALING");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SKILL_SORCERY");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SKILL_HAND");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SKILL_THAUMATURGY");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SKILL_SUMMON");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SKILL_SPELL");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SKILL_DODGE");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SKILL_BEAR");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SKILL_LORE");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SKILL_PRESERVATION");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SKILL_POSSESSION");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SKILL_MIND");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SKILL_CRITS");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SKILL_PRAY");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SKILL_LEARN");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SKILL_UDUN");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SKILL_DEVICE");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SKILL_STUN");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SKILL_BOULDER");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SKILL_GEOMANCY");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SKILL_MAX");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SKILL_STEP");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"get_skill");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"get_skill_scale");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"do_get_new_skill");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"get_melee_skills");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"find_skill");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"find_skill_i");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"get_class_name");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"get_race_name");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"get_subrace_name");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"ability_type");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"find_ability");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"do_cmd_ability");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"has_ability");
 lua_getglobals(tolua_S);
 lua_pushstring(tolua_S,"max_ab_idx"); lua_pushnil(tolua_S); lua_rawset(tolua_S,-3);
 lua_pop(tolua_S,1);
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"ab_info");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"AB_SPREAD_BLOWS");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"AB_TREE_WALK");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"AB_PERFECT_CASTING");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"AB_MAX_BLOW1");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"AB_MAX_BLOW2");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"AB_AMMO_CREATION");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"AB_DEATH_TOUCH");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"AB_CREATE_ART");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"AB_FAR_REACHING");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"AB_TRAPPING");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"AB_UNDEAD_FORM");
}
