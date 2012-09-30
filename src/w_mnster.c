/*
** Lua binding: monster
** Generated automatically by tolua 4.0a - angband on Mon Jul 14 20:43:49 2003.
*/

#include "lua/tolua.h"

/* Exported function */
int tolua_monster_open (lua_State* tolua_S);
void tolua_monster_close (lua_State* tolua_S);

#include "angband.h"
static monster_type lua_monster_forge;
static monster_type *lua_get_monster(int m_idx){return (&m_list[m_idx]);}
static char *lua_monster_desc(monster_type *m_ptr, int mode){static char buf[200]; monster_desc(buf, m_ptr, mode); return buf;}
static char *lua_monster_race_desc(int r_idx, int ego){static char buf[200]; monster_race_desc(buf, r_idx, ego); return buf;}

/* function to register type */
static void toluaI_reg_types (lua_State* tolua_S)
{
 tolua_usertype(tolua_S,"monster_type");
 tolua_usertype(tolua_S,"monster_blow");
 tolua_usertype(tolua_S,"obj_theme");
 tolua_usertype(tolua_S,"object_type");
 tolua_usertype(tolua_S,"monster_race");
}

/* error messages */
#define TOLUA_ERR_SELF tolua_error(tolua_S,"invalid 'self'")
#define TOLUA_ERR_ASSIGN tolua_error(tolua_S,"#vinvalid type in variable assignment.")

/* get function: lua_monster_forge */
static int toluaI_get_monster_monster_forge(lua_State* tolua_S)
{
 tolua_pushusertype(tolua_S,(void*)&lua_monster_forge,tolua_tag(tolua_S,"monster_type"));
 return 1;
}

/* set function: lua_monster_forge */
static int toluaI_set_monster_monster_forge(lua_State* tolua_S)
{
 if (!tolua_istype(tolua_S,2,tolua_tag(tolua_S,"monster_type"),0))
 TOLUA_ERR_ASSIGN;
  lua_monster_forge = *((monster_type*)  tolua_getusertype(tolua_S,2,0));
 return 0;
}

/* get function: method of class  monster_blow */
static int toluaI_get_monster_monster_blow_method(lua_State* tolua_S)
{
  monster_blow* self = (monster_blow*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->method);
 return 1;
}

/* set function: method of class  monster_blow */
static int toluaI_set_monster_monster_blow_method(lua_State* tolua_S)
{
  monster_blow* self = (monster_blow*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->method = ((byte)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: effect of class  monster_blow */
static int toluaI_get_monster_monster_blow_effect(lua_State* tolua_S)
{
  monster_blow* self = (monster_blow*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->effect);
 return 1;
}

/* set function: effect of class  monster_blow */
static int toluaI_set_monster_monster_blow_effect(lua_State* tolua_S)
{
  monster_blow* self = (monster_blow*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->effect = ((byte)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: d_dice of class  monster_blow */
static int toluaI_get_monster_monster_blow_d_dice(lua_State* tolua_S)
{
  monster_blow* self = (monster_blow*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->d_dice);
 return 1;
}

/* set function: d_dice of class  monster_blow */
static int toluaI_set_monster_monster_blow_d_dice(lua_State* tolua_S)
{
  monster_blow* self = (monster_blow*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->d_dice = ((byte)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: d_side of class  monster_blow */
static int toluaI_get_monster_monster_blow_d_side(lua_State* tolua_S)
{
  monster_blow* self = (monster_blow*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->d_side);
 return 1;
}

/* set function: d_side of class  monster_blow */
static int toluaI_set_monster_monster_blow_d_side(lua_State* tolua_S)
{
  monster_blow* self = (monster_blow*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->d_side = ((byte)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: name of class  monster_race */
static int toluaI_get_monster_monster_race_name(lua_State* tolua_S)
{
  monster_race* self = (monster_race*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->name);
 return 1;
}

/* set function: name of class  monster_race */
static int toluaI_set_monster_monster_race_name(lua_State* tolua_S)
{
  monster_race* self = (monster_race*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->name = ((u32b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: text of class  monster_race */
static int toluaI_get_monster_monster_race_text(lua_State* tolua_S)
{
  monster_race* self = (monster_race*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->text);
 return 1;
}

/* set function: text of class  monster_race */
static int toluaI_set_monster_monster_race_text(lua_State* tolua_S)
{
  monster_race* self = (monster_race*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->text = ((u32b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: hdice of class  monster_race */
static int toluaI_get_monster_monster_race_hdice(lua_State* tolua_S)
{
  monster_race* self = (monster_race*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->hdice);
 return 1;
}

/* set function: hdice of class  monster_race */
static int toluaI_set_monster_monster_race_hdice(lua_State* tolua_S)
{
  monster_race* self = (monster_race*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->hdice = ((byte)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: hside of class  monster_race */
static int toluaI_get_monster_monster_race_hside(lua_State* tolua_S)
{
  monster_race* self = (monster_race*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->hside);
 return 1;
}

/* set function: hside of class  monster_race */
static int toluaI_set_monster_monster_race_hside(lua_State* tolua_S)
{
  monster_race* self = (monster_race*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->hside = ((byte)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: ac of class  monster_race */
static int toluaI_get_monster_monster_race_ac(lua_State* tolua_S)
{
  monster_race* self = (monster_race*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->ac);
 return 1;
}

/* set function: ac of class  monster_race */
static int toluaI_set_monster_monster_race_ac(lua_State* tolua_S)
{
  monster_race* self = (monster_race*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->ac = ((s16b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: sleep of class  monster_race */
static int toluaI_get_monster_monster_race_sleep(lua_State* tolua_S)
{
  monster_race* self = (monster_race*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->sleep);
 return 1;
}

/* set function: sleep of class  monster_race */
static int toluaI_set_monster_monster_race_sleep(lua_State* tolua_S)
{
  monster_race* self = (monster_race*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->sleep = ((s16b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: aaf of class  monster_race */
static int toluaI_get_monster_monster_race_aaf(lua_State* tolua_S)
{
  monster_race* self = (monster_race*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->aaf);
 return 1;
}

/* set function: aaf of class  monster_race */
static int toluaI_set_monster_monster_race_aaf(lua_State* tolua_S)
{
  monster_race* self = (monster_race*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->aaf = ((byte)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: speed of class  monster_race */
static int toluaI_get_monster_monster_race_speed(lua_State* tolua_S)
{
  monster_race* self = (monster_race*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->speed);
 return 1;
}

/* set function: speed of class  monster_race */
static int toluaI_set_monster_monster_race_speed(lua_State* tolua_S)
{
  monster_race* self = (monster_race*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->speed = ((byte)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: mexp of class  monster_race */
static int toluaI_get_monster_monster_race_mexp(lua_State* tolua_S)
{
  monster_race* self = (monster_race*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->mexp);
 return 1;
}

/* set function: mexp of class  monster_race */
static int toluaI_set_monster_monster_race_mexp(lua_State* tolua_S)
{
  monster_race* self = (monster_race*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->mexp = ((s32b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: weight of class  monster_race */
static int toluaI_get_monster_monster_race_weight(lua_State* tolua_S)
{
  monster_race* self = (monster_race*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->weight);
 return 1;
}

/* set function: weight of class  monster_race */
static int toluaI_set_monster_monster_race_weight(lua_State* tolua_S)
{
  monster_race* self = (monster_race*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->weight = ((s32b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: freq_inate of class  monster_race */
static int toluaI_get_monster_monster_race_freq_inate(lua_State* tolua_S)
{
  monster_race* self = (monster_race*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->freq_inate);
 return 1;
}

/* set function: freq_inate of class  monster_race */
static int toluaI_set_monster_monster_race_freq_inate(lua_State* tolua_S)
{
  monster_race* self = (monster_race*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->freq_inate = ((byte)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: freq_spell of class  monster_race */
static int toluaI_get_monster_monster_race_freq_spell(lua_State* tolua_S)
{
  monster_race* self = (monster_race*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->freq_spell);
 return 1;
}

/* set function: freq_spell of class  monster_race */
static int toluaI_set_monster_monster_race_freq_spell(lua_State* tolua_S)
{
  monster_race* self = (monster_race*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->freq_spell = ((byte)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: flags1 of class  monster_race */
static int toluaI_get_monster_monster_race_flags1(lua_State* tolua_S)
{
  monster_race* self = (monster_race*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->flags1);
 return 1;
}

/* set function: flags1 of class  monster_race */
static int toluaI_set_monster_monster_race_flags1(lua_State* tolua_S)
{
  monster_race* self = (monster_race*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->flags1 = ((u32b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: flags2 of class  monster_race */
static int toluaI_get_monster_monster_race_flags2(lua_State* tolua_S)
{
  monster_race* self = (monster_race*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->flags2);
 return 1;
}

/* set function: flags2 of class  monster_race */
static int toluaI_set_monster_monster_race_flags2(lua_State* tolua_S)
{
  monster_race* self = (monster_race*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->flags2 = ((u32b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: flags3 of class  monster_race */
static int toluaI_get_monster_monster_race_flags3(lua_State* tolua_S)
{
  monster_race* self = (monster_race*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->flags3);
 return 1;
}

/* set function: flags3 of class  monster_race */
static int toluaI_set_monster_monster_race_flags3(lua_State* tolua_S)
{
  monster_race* self = (monster_race*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->flags3 = ((u32b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: flags4 of class  monster_race */
static int toluaI_get_monster_monster_race_flags4(lua_State* tolua_S)
{
  monster_race* self = (monster_race*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->flags4);
 return 1;
}

/* set function: flags4 of class  monster_race */
static int toluaI_set_monster_monster_race_flags4(lua_State* tolua_S)
{
  monster_race* self = (monster_race*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->flags4 = ((u32b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: flags5 of class  monster_race */
static int toluaI_get_monster_monster_race_flags5(lua_State* tolua_S)
{
  monster_race* self = (monster_race*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->flags5);
 return 1;
}

/* set function: flags5 of class  monster_race */
static int toluaI_set_monster_monster_race_flags5(lua_State* tolua_S)
{
  monster_race* self = (monster_race*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->flags5 = ((u32b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: flags6 of class  monster_race */
static int toluaI_get_monster_monster_race_flags6(lua_State* tolua_S)
{
  monster_race* self = (monster_race*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->flags6);
 return 1;
}

/* set function: flags6 of class  monster_race */
static int toluaI_set_monster_monster_race_flags6(lua_State* tolua_S)
{
  monster_race* self = (monster_race*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->flags6 = ((u32b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: flags7 of class  monster_race */
static int toluaI_get_monster_monster_race_flags7(lua_State* tolua_S)
{
  monster_race* self = (monster_race*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->flags7);
 return 1;
}

/* set function: flags7 of class  monster_race */
static int toluaI_set_monster_monster_race_flags7(lua_State* tolua_S)
{
  monster_race* self = (monster_race*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->flags7 = ((u32b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: flags8 of class  monster_race */
static int toluaI_get_monster_monster_race_flags8(lua_State* tolua_S)
{
  monster_race* self = (monster_race*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->flags8);
 return 1;
}

/* set function: flags8 of class  monster_race */
static int toluaI_set_monster_monster_race_flags8(lua_State* tolua_S)
{
  monster_race* self = (monster_race*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->flags8 = ((u32b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: flags9 of class  monster_race */
static int toluaI_get_monster_monster_race_flags9(lua_State* tolua_S)
{
  monster_race* self = (monster_race*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->flags9);
 return 1;
}

/* set function: flags9 of class  monster_race */
static int toluaI_set_monster_monster_race_flags9(lua_State* tolua_S)
{
  monster_race* self = (monster_race*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->flags9 = ((u32b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: blow of class  monster_race */
static int toluaI_get_monster_monster_race_blow(lua_State* tolua_S)
{
 int toluaI_index;
  monster_race* self;
 lua_pushstring(tolua_S,".self");
 lua_rawget(tolua_S,1);
 self = (monster_race*)  lua_touserdata(tolua_S,-1);
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 tolua_error(tolua_S,"invalid type in array indexing.");
 toluaI_index = (int)tolua_getnumber(tolua_S,2,0)-1;
 if (toluaI_index<0 || toluaI_index>=4)
 tolua_error(tolua_S,"array indexing out of range.");
 tolua_pushusertype(tolua_S,(void*)&self->blow[toluaI_index],tolua_tag(tolua_S,"monster_blow"));
 return 1;
}

/* set function: blow of class  monster_race */
static int toluaI_set_monster_monster_race_blow(lua_State* tolua_S)
{
 int toluaI_index;
  monster_race* self;
 lua_pushstring(tolua_S,".self");
 lua_rawget(tolua_S,1);
 self = (monster_race*)  lua_touserdata(tolua_S,-1);
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 tolua_error(tolua_S,"invalid type in array indexing.");
 toluaI_index = (int)tolua_getnumber(tolua_S,2,0)-1;
 if (toluaI_index<0 || toluaI_index>=4)
 tolua_error(tolua_S,"array indexing out of range.");
  self->blow[toluaI_index] = *((monster_blow*)  tolua_getusertype(tolua_S,3,0));
 return 0;
}

/* get function: body_parts of class  monster_race */
static int toluaI_get_monster_monster_race_body_parts(lua_State* tolua_S)
{
 int toluaI_index;
  monster_race* self;
 lua_pushstring(tolua_S,".self");
 lua_rawget(tolua_S,1);
 self = (monster_race*)  lua_touserdata(tolua_S,-1);
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 tolua_error(tolua_S,"invalid type in array indexing.");
 toluaI_index = (int)tolua_getnumber(tolua_S,2,0)-1;
 if (toluaI_index<0 || toluaI_index>=BODY_MAX)
 tolua_error(tolua_S,"array indexing out of range.");
 tolua_pushnumber(tolua_S,(long)self->body_parts[toluaI_index]);
 return 1;
}

/* set function: body_parts of class  monster_race */
static int toluaI_set_monster_monster_race_body_parts(lua_State* tolua_S)
{
 int toluaI_index;
  monster_race* self;
 lua_pushstring(tolua_S,".self");
 lua_rawget(tolua_S,1);
 self = (monster_race*)  lua_touserdata(tolua_S,-1);
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 tolua_error(tolua_S,"invalid type in array indexing.");
 toluaI_index = (int)tolua_getnumber(tolua_S,2,0)-1;
 if (toluaI_index<0 || toluaI_index>=BODY_MAX)
 tolua_error(tolua_S,"array indexing out of range.");
  self->body_parts[toluaI_index] = ((byte)  tolua_getnumber(tolua_S,3,0));
 return 0;
}

/* get function: level of class  monster_race */
static int toluaI_get_monster_monster_race_level(lua_State* tolua_S)
{
  monster_race* self = (monster_race*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->level);
 return 1;
}

/* set function: level of class  monster_race */
static int toluaI_set_monster_monster_race_level(lua_State* tolua_S)
{
  monster_race* self = (monster_race*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->level = ((byte)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: rarity of class  monster_race */
static int toluaI_get_monster_monster_race_rarity(lua_State* tolua_S)
{
  monster_race* self = (monster_race*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->rarity);
 return 1;
}

/* set function: rarity of class  monster_race */
static int toluaI_set_monster_monster_race_rarity(lua_State* tolua_S)
{
  monster_race* self = (monster_race*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->rarity = ((byte)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: d_attr of class  monster_race */
static int toluaI_get_monster_monster_race_d_attr(lua_State* tolua_S)
{
  monster_race* self = (monster_race*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->d_attr);
 return 1;
}

/* set function: d_attr of class  monster_race */
static int toluaI_set_monster_monster_race_d_attr(lua_State* tolua_S)
{
  monster_race* self = (monster_race*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->d_attr = ((byte)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: d_char of class  monster_race */
static int toluaI_get_monster_monster_race_d_char(lua_State* tolua_S)
{
  monster_race* self = (monster_race*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->d_char);
 return 1;
}

/* set function: d_char of class  monster_race */
static int toluaI_set_monster_monster_race_d_char(lua_State* tolua_S)
{
  monster_race* self = (monster_race*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->d_char = ((char)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: x_attr of class  monster_race */
static int toluaI_get_monster_monster_race_x_attr(lua_State* tolua_S)
{
  monster_race* self = (monster_race*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->x_attr);
 return 1;
}

/* set function: x_attr of class  monster_race */
static int toluaI_set_monster_monster_race_x_attr(lua_State* tolua_S)
{
  monster_race* self = (monster_race*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->x_attr = ((byte)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: x_char of class  monster_race */
static int toluaI_get_monster_monster_race_x_char(lua_State* tolua_S)
{
  monster_race* self = (monster_race*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->x_char);
 return 1;
}

/* set function: x_char of class  monster_race */
static int toluaI_set_monster_monster_race_x_char(lua_State* tolua_S)
{
  monster_race* self = (monster_race*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->x_char = ((char)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: max_num of class  monster_race */
static int toluaI_get_monster_monster_race_max_num(lua_State* tolua_S)
{
  monster_race* self = (monster_race*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->max_num);
 return 1;
}

/* set function: max_num of class  monster_race */
static int toluaI_set_monster_monster_race_max_num(lua_State* tolua_S)
{
  monster_race* self = (monster_race*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->max_num = ((s16b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: cur_num of class  monster_race */
static int toluaI_get_monster_monster_race_cur_num(lua_State* tolua_S)
{
  monster_race* self = (monster_race*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->cur_num);
 return 1;
}

/* set function: cur_num of class  monster_race */
static int toluaI_set_monster_monster_race_cur_num(lua_State* tolua_S)
{
  monster_race* self = (monster_race*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->cur_num = ((byte)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: r_sights of class  monster_race */
static int toluaI_get_monster_monster_race_r_sights(lua_State* tolua_S)
{
  monster_race* self = (monster_race*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->r_sights);
 return 1;
}

/* set function: r_sights of class  monster_race */
static int toluaI_set_monster_monster_race_r_sights(lua_State* tolua_S)
{
  monster_race* self = (monster_race*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->r_sights = ((s16b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: r_deaths of class  monster_race */
static int toluaI_get_monster_monster_race_r_deaths(lua_State* tolua_S)
{
  monster_race* self = (monster_race*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->r_deaths);
 return 1;
}

/* set function: r_deaths of class  monster_race */
static int toluaI_set_monster_monster_race_r_deaths(lua_State* tolua_S)
{
  monster_race* self = (monster_race*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->r_deaths = ((s16b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: r_pkills of class  monster_race */
static int toluaI_get_monster_monster_race_r_pkills(lua_State* tolua_S)
{
  monster_race* self = (monster_race*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->r_pkills);
 return 1;
}

/* set function: r_pkills of class  monster_race */
static int toluaI_set_monster_monster_race_r_pkills(lua_State* tolua_S)
{
  monster_race* self = (monster_race*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->r_pkills = ((s16b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: r_tkills of class  monster_race */
static int toluaI_get_monster_monster_race_r_tkills(lua_State* tolua_S)
{
  monster_race* self = (monster_race*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->r_tkills);
 return 1;
}

/* set function: r_tkills of class  monster_race */
static int toluaI_set_monster_monster_race_r_tkills(lua_State* tolua_S)
{
  monster_race* self = (monster_race*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->r_tkills = ((s16b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: r_wake of class  monster_race */
static int toluaI_get_monster_monster_race_r_wake(lua_State* tolua_S)
{
  monster_race* self = (monster_race*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->r_wake);
 return 1;
}

/* set function: r_wake of class  monster_race */
static int toluaI_set_monster_monster_race_r_wake(lua_State* tolua_S)
{
  monster_race* self = (monster_race*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->r_wake = ((byte)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: r_ignore of class  monster_race */
static int toluaI_get_monster_monster_race_r_ignore(lua_State* tolua_S)
{
  monster_race* self = (monster_race*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->r_ignore);
 return 1;
}

/* set function: r_ignore of class  monster_race */
static int toluaI_set_monster_monster_race_r_ignore(lua_State* tolua_S)
{
  monster_race* self = (monster_race*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->r_ignore = ((byte)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: r_xtra1 of class  monster_race */
static int toluaI_get_monster_monster_race_r_xtra1(lua_State* tolua_S)
{
  monster_race* self = (monster_race*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->r_xtra1);
 return 1;
}

/* set function: r_xtra1 of class  monster_race */
static int toluaI_set_monster_monster_race_r_xtra1(lua_State* tolua_S)
{
  monster_race* self = (monster_race*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->r_xtra1 = ((byte)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: r_xtra2 of class  monster_race */
static int toluaI_get_monster_monster_race_r_xtra2(lua_State* tolua_S)
{
  monster_race* self = (monster_race*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->r_xtra2);
 return 1;
}

/* set function: r_xtra2 of class  monster_race */
static int toluaI_set_monster_monster_race_r_xtra2(lua_State* tolua_S)
{
  monster_race* self = (monster_race*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->r_xtra2 = ((byte)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: r_drop_gold of class  monster_race */
static int toluaI_get_monster_monster_race_r_drop_gold(lua_State* tolua_S)
{
  monster_race* self = (monster_race*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->r_drop_gold);
 return 1;
}

/* set function: r_drop_gold of class  monster_race */
static int toluaI_set_monster_monster_race_r_drop_gold(lua_State* tolua_S)
{
  monster_race* self = (monster_race*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->r_drop_gold = ((byte)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: r_drop_item of class  monster_race */
static int toluaI_get_monster_monster_race_r_drop_item(lua_State* tolua_S)
{
  monster_race* self = (monster_race*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->r_drop_item);
 return 1;
}

/* set function: r_drop_item of class  monster_race */
static int toluaI_set_monster_monster_race_r_drop_item(lua_State* tolua_S)
{
  monster_race* self = (monster_race*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->r_drop_item = ((byte)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: r_cast_inate of class  monster_race */
static int toluaI_get_monster_monster_race_r_cast_inate(lua_State* tolua_S)
{
  monster_race* self = (monster_race*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->r_cast_inate);
 return 1;
}

/* set function: r_cast_inate of class  monster_race */
static int toluaI_set_monster_monster_race_r_cast_inate(lua_State* tolua_S)
{
  monster_race* self = (monster_race*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->r_cast_inate = ((byte)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: r_cast_spell of class  monster_race */
static int toluaI_get_monster_monster_race_r_cast_spell(lua_State* tolua_S)
{
  monster_race* self = (monster_race*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->r_cast_spell);
 return 1;
}

/* set function: r_cast_spell of class  monster_race */
static int toluaI_set_monster_monster_race_r_cast_spell(lua_State* tolua_S)
{
  monster_race* self = (monster_race*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->r_cast_spell = ((byte)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: r_blows of class  monster_race */
static int toluaI_get_monster_monster_race_r_blows(lua_State* tolua_S)
{
 int toluaI_index;
  monster_race* self;
 lua_pushstring(tolua_S,".self");
 lua_rawget(tolua_S,1);
 self = (monster_race*)  lua_touserdata(tolua_S,-1);
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 tolua_error(tolua_S,"invalid type in array indexing.");
 toluaI_index = (int)tolua_getnumber(tolua_S,2,0)-1;
 if (toluaI_index<0 || toluaI_index>=4)
 tolua_error(tolua_S,"array indexing out of range.");
 tolua_pushnumber(tolua_S,(long)self->r_blows[toluaI_index]);
 return 1;
}

/* set function: r_blows of class  monster_race */
static int toluaI_set_monster_monster_race_r_blows(lua_State* tolua_S)
{
 int toluaI_index;
  monster_race* self;
 lua_pushstring(tolua_S,".self");
 lua_rawget(tolua_S,1);
 self = (monster_race*)  lua_touserdata(tolua_S,-1);
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 tolua_error(tolua_S,"invalid type in array indexing.");
 toluaI_index = (int)tolua_getnumber(tolua_S,2,0)-1;
 if (toluaI_index<0 || toluaI_index>=4)
 tolua_error(tolua_S,"array indexing out of range.");
  self->r_blows[toluaI_index] = ((byte)  tolua_getnumber(tolua_S,3,0));
 return 0;
}

/* get function: r_flags1 of class  monster_race */
static int toluaI_get_monster_monster_race_r_flags1(lua_State* tolua_S)
{
  monster_race* self = (monster_race*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->r_flags1);
 return 1;
}

/* set function: r_flags1 of class  monster_race */
static int toluaI_set_monster_monster_race_r_flags1(lua_State* tolua_S)
{
  monster_race* self = (monster_race*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->r_flags1 = ((u32b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: r_flags2 of class  monster_race */
static int toluaI_get_monster_monster_race_r_flags2(lua_State* tolua_S)
{
  monster_race* self = (monster_race*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->r_flags2);
 return 1;
}

/* set function: r_flags2 of class  monster_race */
static int toluaI_set_monster_monster_race_r_flags2(lua_State* tolua_S)
{
  monster_race* self = (monster_race*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->r_flags2 = ((u32b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: r_flags3 of class  monster_race */
static int toluaI_get_monster_monster_race_r_flags3(lua_State* tolua_S)
{
  monster_race* self = (monster_race*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->r_flags3);
 return 1;
}

/* set function: r_flags3 of class  monster_race */
static int toluaI_set_monster_monster_race_r_flags3(lua_State* tolua_S)
{
  monster_race* self = (monster_race*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->r_flags3 = ((u32b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: r_flags4 of class  monster_race */
static int toluaI_get_monster_monster_race_r_flags4(lua_State* tolua_S)
{
  monster_race* self = (monster_race*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->r_flags4);
 return 1;
}

/* set function: r_flags4 of class  monster_race */
static int toluaI_set_monster_monster_race_r_flags4(lua_State* tolua_S)
{
  monster_race* self = (monster_race*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->r_flags4 = ((u32b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: r_flags5 of class  monster_race */
static int toluaI_get_monster_monster_race_r_flags5(lua_State* tolua_S)
{
  monster_race* self = (monster_race*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->r_flags5);
 return 1;
}

/* set function: r_flags5 of class  monster_race */
static int toluaI_set_monster_monster_race_r_flags5(lua_State* tolua_S)
{
  monster_race* self = (monster_race*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->r_flags5 = ((u32b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: r_flags6 of class  monster_race */
static int toluaI_get_monster_monster_race_r_flags6(lua_State* tolua_S)
{
  monster_race* self = (monster_race*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->r_flags6);
 return 1;
}

/* set function: r_flags6 of class  monster_race */
static int toluaI_set_monster_monster_race_r_flags6(lua_State* tolua_S)
{
  monster_race* self = (monster_race*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->r_flags6 = ((u32b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: r_flags7 of class  monster_race */
static int toluaI_get_monster_monster_race_r_flags7(lua_State* tolua_S)
{
  monster_race* self = (monster_race*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->r_flags7);
 return 1;
}

/* set function: r_flags7 of class  monster_race */
static int toluaI_set_monster_monster_race_r_flags7(lua_State* tolua_S)
{
  monster_race* self = (monster_race*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->r_flags7 = ((u32b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: r_flags8 of class  monster_race */
static int toluaI_get_monster_monster_race_r_flags8(lua_State* tolua_S)
{
  monster_race* self = (monster_race*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->r_flags8);
 return 1;
}

/* set function: r_flags8 of class  monster_race */
static int toluaI_set_monster_monster_race_r_flags8(lua_State* tolua_S)
{
  monster_race* self = (monster_race*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->r_flags8 = ((u32b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: r_flags9 of class  monster_race */
static int toluaI_get_monster_monster_race_r_flags9(lua_State* tolua_S)
{
  monster_race* self = (monster_race*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->r_flags9);
 return 1;
}

/* set function: r_flags9 of class  monster_race */
static int toluaI_set_monster_monster_race_r_flags9(lua_State* tolua_S)
{
  monster_race* self = (monster_race*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->r_flags9 = ((u32b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: on_saved of class  monster_race */
static int toluaI_get_monster_monster_race_on_saved(lua_State* tolua_S)
{
  monster_race* self = (monster_race*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->on_saved);
 return 1;
}

/* set function: on_saved of class  monster_race */
static int toluaI_set_monster_monster_race_on_saved(lua_State* tolua_S)
{
  monster_race* self = (monster_race*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->on_saved = ((bool)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: total_visible of class  monster_race */
static int toluaI_get_monster_monster_race_total_visible(lua_State* tolua_S)
{
  monster_race* self = (monster_race*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->total_visible);
 return 1;
}

/* set function: total_visible of class  monster_race */
static int toluaI_set_monster_monster_race_total_visible(lua_State* tolua_S)
{
  monster_race* self = (monster_race*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->total_visible = ((byte)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: drops of class  monster_race */
static int toluaI_get_monster_monster_race_drops(lua_State* tolua_S)
{
  monster_race* self = (monster_race*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushusertype(tolua_S,(void*)&self->drops,tolua_tag(tolua_S,"obj_theme"));
 return 1;
}

/* set function: drops of class  monster_race */
static int toluaI_set_monster_monster_race_drops(lua_State* tolua_S)
{
  monster_race* self = (monster_race*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,tolua_tag(tolua_S,"obj_theme"),0))
 TOLUA_ERR_ASSIGN;
  self->drops = *((obj_theme*)  tolua_getusertype(tolua_S,2,0));
 return 0;
}

/* get function: r_idx of class  monster_type */
static int toluaI_get_monster_monster_type_r_idx(lua_State* tolua_S)
{
  monster_type* self = (monster_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->r_idx);
 return 1;
}

/* set function: r_idx of class  monster_type */
static int toluaI_set_monster_monster_type_r_idx(lua_State* tolua_S)
{
  monster_type* self = (monster_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->r_idx = ((s16b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: ego of class  monster_type */
static int toluaI_get_monster_monster_type_ego(lua_State* tolua_S)
{
  monster_type* self = (monster_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->ego);
 return 1;
}

/* set function: ego of class  monster_type */
static int toluaI_set_monster_monster_type_ego(lua_State* tolua_S)
{
  monster_type* self = (monster_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->ego = ((u16b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: fy of class  monster_type */
static int toluaI_get_monster_monster_type_fy(lua_State* tolua_S)
{
  monster_type* self = (monster_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->fy);
 return 1;
}

/* set function: fy of class  monster_type */
static int toluaI_set_monster_monster_type_fy(lua_State* tolua_S)
{
  monster_type* self = (monster_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->fy = ((byte)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: fx of class  monster_type */
static int toluaI_get_monster_monster_type_fx(lua_State* tolua_S)
{
  monster_type* self = (monster_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->fx);
 return 1;
}

/* set function: fx of class  monster_type */
static int toluaI_set_monster_monster_type_fx(lua_State* tolua_S)
{
  monster_type* self = (monster_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->fx = ((byte)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: hp of class  monster_type */
static int toluaI_get_monster_monster_type_hp(lua_State* tolua_S)
{
  monster_type* self = (monster_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->hp);
 return 1;
}

/* set function: hp of class  monster_type */
static int toluaI_set_monster_monster_type_hp(lua_State* tolua_S)
{
  monster_type* self = (monster_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->hp = ((s16b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: maxhp of class  monster_type */
static int toluaI_get_monster_monster_type_maxhp(lua_State* tolua_S)
{
  monster_type* self = (monster_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->maxhp);
 return 1;
}

/* set function: maxhp of class  monster_type */
static int toluaI_set_monster_monster_type_maxhp(lua_State* tolua_S)
{
  monster_type* self = (monster_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->maxhp = ((s16b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: blow of class  monster_type */
static int toluaI_get_monster_monster_type_blow(lua_State* tolua_S)
{
 int toluaI_index;
  monster_type* self;
 lua_pushstring(tolua_S,".self");
 lua_rawget(tolua_S,1);
 self = (monster_type*)  lua_touserdata(tolua_S,-1);
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 tolua_error(tolua_S,"invalid type in array indexing.");
 toluaI_index = (int)tolua_getnumber(tolua_S,2,0)-1;
 if (toluaI_index<0 || toluaI_index>=4)
 tolua_error(tolua_S,"array indexing out of range.");
 tolua_pushusertype(tolua_S,(void*)&self->blow[toluaI_index],tolua_tag(tolua_S,"monster_blow"));
 return 1;
}

/* set function: blow of class  monster_type */
static int toluaI_set_monster_monster_type_blow(lua_State* tolua_S)
{
 int toluaI_index;
  monster_type* self;
 lua_pushstring(tolua_S,".self");
 lua_rawget(tolua_S,1);
 self = (monster_type*)  lua_touserdata(tolua_S,-1);
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 tolua_error(tolua_S,"invalid type in array indexing.");
 toluaI_index = (int)tolua_getnumber(tolua_S,2,0)-1;
 if (toluaI_index<0 || toluaI_index>=4)
 tolua_error(tolua_S,"array indexing out of range.");
  self->blow[toluaI_index] = *((monster_blow*)  tolua_getusertype(tolua_S,3,0));
 return 0;
}

/* get function: speed of class  monster_type */
static int toluaI_get_monster_monster_type_speed(lua_State* tolua_S)
{
  monster_type* self = (monster_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->speed);
 return 1;
}

/* set function: speed of class  monster_type */
static int toluaI_set_monster_monster_type_speed(lua_State* tolua_S)
{
  monster_type* self = (monster_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->speed = ((byte)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: level of class  monster_type */
static int toluaI_get_monster_monster_type_level(lua_State* tolua_S)
{
  monster_type* self = (monster_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->level);
 return 1;
}

/* set function: level of class  monster_type */
static int toluaI_set_monster_monster_type_level(lua_State* tolua_S)
{
  monster_type* self = (monster_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->level = ((byte)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: ac of class  monster_type */
static int toluaI_get_monster_monster_type_ac(lua_State* tolua_S)
{
  monster_type* self = (monster_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->ac);
 return 1;
}

/* set function: ac of class  monster_type */
static int toluaI_set_monster_monster_type_ac(lua_State* tolua_S)
{
  monster_type* self = (monster_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->ac = ((s16b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: exp of class  monster_type */
static int toluaI_get_monster_monster_type_exp(lua_State* tolua_S)
{
  monster_type* self = (monster_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->exp);
 return 1;
}

/* set function: exp of class  monster_type */
static int toluaI_set_monster_monster_type_exp(lua_State* tolua_S)
{
  monster_type* self = (monster_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->exp = ((u32b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: csleep of class  monster_type */
static int toluaI_get_monster_monster_type_csleep(lua_State* tolua_S)
{
  monster_type* self = (monster_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->csleep);
 return 1;
}

/* set function: csleep of class  monster_type */
static int toluaI_set_monster_monster_type_csleep(lua_State* tolua_S)
{
  monster_type* self = (monster_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->csleep = ((s16b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: mspeed of class  monster_type */
static int toluaI_get_monster_monster_type_mspeed(lua_State* tolua_S)
{
  monster_type* self = (monster_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->mspeed);
 return 1;
}

/* set function: mspeed of class  monster_type */
static int toluaI_set_monster_monster_type_mspeed(lua_State* tolua_S)
{
  monster_type* self = (monster_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->mspeed = ((byte)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: energy of class  monster_type */
static int toluaI_get_monster_monster_type_energy(lua_State* tolua_S)
{
  monster_type* self = (monster_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->energy);
 return 1;
}

/* set function: energy of class  monster_type */
static int toluaI_set_monster_monster_type_energy(lua_State* tolua_S)
{
  monster_type* self = (monster_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->energy = ((byte)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: stunned of class  monster_type */
static int toluaI_get_monster_monster_type_stunned(lua_State* tolua_S)
{
  monster_type* self = (monster_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->stunned);
 return 1;
}

/* set function: stunned of class  monster_type */
static int toluaI_set_monster_monster_type_stunned(lua_State* tolua_S)
{
  monster_type* self = (monster_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->stunned = ((byte)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: confused of class  monster_type */
static int toluaI_get_monster_monster_type_confused(lua_State* tolua_S)
{
  monster_type* self = (monster_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->confused);
 return 1;
}

/* set function: confused of class  monster_type */
static int toluaI_set_monster_monster_type_confused(lua_State* tolua_S)
{
  monster_type* self = (monster_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->confused = ((byte)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: monfear of class  monster_type */
static int toluaI_get_monster_monster_type_monfear(lua_State* tolua_S)
{
  monster_type* self = (monster_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->monfear);
 return 1;
}

/* set function: monfear of class  monster_type */
static int toluaI_set_monster_monster_type_monfear(lua_State* tolua_S)
{
  monster_type* self = (monster_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->monfear = ((byte)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: bleeding of class  monster_type */
static int toluaI_get_monster_monster_type_bleeding(lua_State* tolua_S)
{
  monster_type* self = (monster_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->bleeding);
 return 1;
}

/* set function: bleeding of class  monster_type */
static int toluaI_set_monster_monster_type_bleeding(lua_State* tolua_S)
{
  monster_type* self = (monster_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->bleeding = ((s16b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: poisoned of class  monster_type */
static int toluaI_get_monster_monster_type_poisoned(lua_State* tolua_S)
{
  monster_type* self = (monster_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->poisoned);
 return 1;
}

/* set function: poisoned of class  monster_type */
static int toluaI_set_monster_monster_type_poisoned(lua_State* tolua_S)
{
  monster_type* self = (monster_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->poisoned = ((s16b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: cdis of class  monster_type */
static int toluaI_get_monster_monster_type_cdis(lua_State* tolua_S)
{
  monster_type* self = (monster_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->cdis);
 return 1;
}

/* set function: cdis of class  monster_type */
static int toluaI_set_monster_monster_type_cdis(lua_State* tolua_S)
{
  monster_type* self = (monster_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->cdis = ((byte)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: mflag of class  monster_type */
static int toluaI_get_monster_monster_type_mflag(lua_State* tolua_S)
{
  monster_type* self = (monster_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->mflag);
 return 1;
}

/* set function: mflag of class  monster_type */
static int toluaI_set_monster_monster_type_mflag(lua_State* tolua_S)
{
  monster_type* self = (monster_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->mflag = ((s32b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: ml of class  monster_type */
static int toluaI_get_monster_monster_type_ml(lua_State* tolua_S)
{
  monster_type* self = (monster_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->ml);
 return 1;
}

/* set function: ml of class  monster_type */
static int toluaI_set_monster_monster_type_ml(lua_State* tolua_S)
{
  monster_type* self = (monster_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->ml = ((bool)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: hold_o_idx of class  monster_type */
static int toluaI_get_monster_monster_type_hold_o_idx(lua_State* tolua_S)
{
  monster_type* self = (monster_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->hold_o_idx);
 return 1;
}

/* set function: hold_o_idx of class  monster_type */
static int toluaI_set_monster_monster_type_hold_o_idx(lua_State* tolua_S)
{
  monster_type* self = (monster_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->hold_o_idx = ((s16b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: smart of class  monster_type */
static int toluaI_get_monster_monster_type_smart(lua_State* tolua_S)
{
  monster_type* self = (monster_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->smart);
 return 1;
}

/* set function: smart of class  monster_type */
static int toluaI_set_monster_monster_type_smart(lua_State* tolua_S)
{
  monster_type* self = (monster_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->smart = ((u32b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: status of class  monster_type */
static int toluaI_get_monster_monster_type_status(lua_State* tolua_S)
{
  monster_type* self = (monster_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->status);
 return 1;
}

/* set function: status of class  monster_type */
static int toluaI_set_monster_monster_type_status(lua_State* tolua_S)
{
  monster_type* self = (monster_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->status = ((s16b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: target of class  monster_type */
static int toluaI_get_monster_monster_type_target(lua_State* tolua_S)
{
  monster_type* self = (monster_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->target);
 return 1;
}

/* set function: target of class  monster_type */
static int toluaI_set_monster_monster_type_target(lua_State* tolua_S)
{
  monster_type* self = (monster_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->target = ((s16b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: possessor of class  monster_type */
static int toluaI_get_monster_monster_type_possessor(lua_State* tolua_S)
{
  monster_type* self = (monster_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->possessor);
 return 1;
}

/* set function: possessor of class  monster_type */
static int toluaI_set_monster_monster_type_possessor(lua_State* tolua_S)
{
  monster_type* self = (monster_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->possessor = ((s16b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* function: lua_get_monster */
static int toluaI_monster_monster00(lua_State* tolua_S)
{
 if (
 !tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
 !tolua_isnoobj(tolua_S,2)
 )
 goto tolua_lerror;
 else
 {
  int m_idx = ((int)  tolua_getnumber(tolua_S,1,0));
 {
  monster_type* toluaI_ret = (monster_type*)  lua_get_monster(m_idx);
 tolua_pushusertype(tolua_S,(void*)toluaI_ret,tolua_tag(tolua_S,"monster_type"));
 }
 }
 return 1;
tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'monster'.");
 return 0;
}

/* get function: m_list */
static int toluaI_get_monster_m_list(lua_State* tolua_S)
{
 int toluaI_index;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 tolua_error(tolua_S,"invalid type in array indexing.");
 toluaI_index = (int)tolua_getnumber(tolua_S,2,0)-1;
 if (toluaI_index<0 || toluaI_index>=max_m_idx)
 tolua_error(tolua_S,"array indexing out of range.");
 tolua_pushusertype(tolua_S,(void*)&m_list[toluaI_index],tolua_tag(tolua_S,"monster_type"));
 return 1;
}

/* set function: m_list */
static int toluaI_set_monster_m_list(lua_State* tolua_S)
{
 int toluaI_index;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 tolua_error(tolua_S,"invalid type in array indexing.");
 toluaI_index = (int)tolua_getnumber(tolua_S,2,0)-1;
 if (toluaI_index<0 || toluaI_index>=max_m_idx)
 tolua_error(tolua_S,"array indexing out of range.");
  m_list[toluaI_index] = *((monster_type*)  tolua_getusertype(tolua_S,3,0));
 return 0;
}

/* function: race_info_idx */
static int toluaI_monster_race_info_idx00(lua_State* tolua_S)
{
 if (
 !tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
 !tolua_istype(tolua_S,2,LUA_TNUMBER,0) ||
 !tolua_isnoobj(tolua_S,3)
 )
 goto tolua_lerror;
 else
 {
  int r_idx = ((int)  tolua_getnumber(tolua_S,1,0));
  int ego = ((int)  tolua_getnumber(tolua_S,2,0));
 {
  monster_race* toluaI_ret = (monster_race*)  race_info_idx(r_idx,ego);
 tolua_pushusertype(tolua_S,(void*)toluaI_ret,tolua_tag(tolua_S,"monster_race"));
 }
 }
 return 1;
tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'race_info_idx'.");
 return 0;
}

/* function: delete_monster_idx */
static int toluaI_monster_delete_monster_idx00(lua_State* tolua_S)
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
  delete_monster_idx(i);
 }
 }
 return 0;
tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'delete_monster_idx'.");
 return 0;
}

/* function: m_pop */
static int toluaI_monster_m_pop00(lua_State* tolua_S)
{
 if (
 !tolua_isnoobj(tolua_S,1)
 )
 goto tolua_lerror;
 else
 {
 {
  s16b toluaI_ret = (s16b)  m_pop();
 tolua_pushnumber(tolua_S,(long)toluaI_ret);
 }
 }
 return 1;
tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'm_pop'.");
 return 0;
}

/* function: get_mon_num_prep */
static int toluaI_monster_get_mon_num_prep00(lua_State* tolua_S)
{
 if (
 !tolua_isnoobj(tolua_S,1)
 )
 goto tolua_lerror;
 else
 {
 {
  errr toluaI_ret = (errr)  get_mon_num_prep();
 tolua_pushnumber(tolua_S,(long)toluaI_ret);
 }
 }
 return 1;
tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'get_mon_num_prep'.");
 return 0;
}

/* function: get_mon_num */
static int toluaI_monster_get_mon_num00(lua_State* tolua_S)
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
  s16b toluaI_ret = (s16b)  get_mon_num(level);
 tolua_pushnumber(tolua_S,(long)toluaI_ret);
 }
 }
 return 1;
tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'get_mon_num'.");
 return 0;
}

/* function: lua_monster_desc */
static int toluaI_monster_monster_desc00(lua_State* tolua_S)
{
 if (
 !tolua_istype(tolua_S,1,tolua_tag(tolua_S,"monster_type"),0) ||
 !tolua_istype(tolua_S,2,LUA_TNUMBER,0) ||
 !tolua_isnoobj(tolua_S,3)
 )
 goto tolua_lerror;
 else
 {
  monster_type* m_ptr = ((monster_type*)  tolua_getusertype(tolua_S,1,0));
  int mode = ((int)  tolua_getnumber(tolua_S,2,0));
 {
  char* toluaI_ret = (char*)  lua_monster_desc(m_ptr,mode);
 tolua_pushstring(tolua_S,(const char*)toluaI_ret);
 }
 }
 return 1;
tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'monster_desc'.");
 return 0;
}

/* function: lua_monster_race_desc */
static int toluaI_monster_monster_race_desc00(lua_State* tolua_S)
{
 if (
 !tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
 !tolua_istype(tolua_S,2,LUA_TNUMBER,0) ||
 !tolua_isnoobj(tolua_S,3)
 )
 goto tolua_lerror;
 else
 {
  int r_idx = ((int)  tolua_getnumber(tolua_S,1,0));
  int ego = ((int)  tolua_getnumber(tolua_S,2,0));
 {
  char* toluaI_ret = (char*)  lua_monster_race_desc(r_idx,ego);
 tolua_pushstring(tolua_S,(const char*)toluaI_ret);
 }
 }
 return 1;
tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'monster_race_desc'.");
 return 0;
}

/* function: monster_race_desc */
static int toluaI_monster_monster_race_desc01(lua_State* tolua_S)
{
 if (
 !tolua_istype(tolua_S,1,LUA_TSTRING,0) ||
 !tolua_istype(tolua_S,2,LUA_TNUMBER,0) ||
 !tolua_istype(tolua_S,3,LUA_TNUMBER,0) ||
 !tolua_isnoobj(tolua_S,4)
 )
 goto tolua_lerror;
 else
 {
  char* desc = ((char*)  tolua_getstring(tolua_S,1,0));
  int r_idx = ((int)  tolua_getnumber(tolua_S,2,0));
  int ego = ((int)  tolua_getnumber(tolua_S,3,0));
 {
  monster_race_desc(desc,r_idx,ego);
 }
 }
 return 0;
tolua_lerror:
 return toluaI_monster_monster_race_desc00(tolua_S);
}

/* function: monster_carry */
static int toluaI_monster_monster_carry00(lua_State* tolua_S)
{
 if (
 !tolua_istype(tolua_S,1,tolua_tag(tolua_S,"monster_type"),0) ||
 !tolua_istype(tolua_S,2,LUA_TNUMBER,0) ||
 !tolua_istype(tolua_S,3,tolua_tag(tolua_S,"object_type"),0) ||
 !tolua_isnoobj(tolua_S,4)
 )
 goto tolua_lerror;
 else
 {
  monster_type* m_ptr = ((monster_type*)  tolua_getusertype(tolua_S,1,0));
  int m_idx = ((int)  tolua_getnumber(tolua_S,2,0));
  object_type* q_ptr = ((object_type*)  tolua_getusertype(tolua_S,3,0));
 {
  monster_carry(m_ptr,m_idx,q_ptr);
 }
 }
 return 0;
tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'monster_carry'.");
 return 0;
}

/* function: place_monster_aux */
static int toluaI_monster_place_monster_aux00(lua_State* tolua_S)
{
 if (
 !tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
 !tolua_istype(tolua_S,2,LUA_TNUMBER,0) ||
 !tolua_istype(tolua_S,3,LUA_TNUMBER,0) ||
 !tolua_istype(tolua_S,4,LUA_TNUMBER,0) ||
 !tolua_istype(tolua_S,5,LUA_TNUMBER,0) ||
 !tolua_istype(tolua_S,6,LUA_TNUMBER,0) ||
 !tolua_isnoobj(tolua_S,7)
 )
 goto tolua_lerror;
 else
 {
  int y = ((int)  tolua_getnumber(tolua_S,1,0));
  int x = ((int)  tolua_getnumber(tolua_S,2,0));
  int r_idx = ((int)  tolua_getnumber(tolua_S,3,0));
  bool slp = ((bool)  tolua_getnumber(tolua_S,4,0));
  bool grp = ((bool)  tolua_getnumber(tolua_S,5,0));
  int status = ((int)  tolua_getnumber(tolua_S,6,0));
 {
  bool toluaI_ret = (bool)  place_monster_aux(y,x,r_idx,slp,grp,status);
 tolua_pushnumber(tolua_S,(long)toluaI_ret);
 }
 }
 return 1;
tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'place_monster_aux'.");
 return 0;
}

/* function: place_monster */
static int toluaI_monster_place_monster00(lua_State* tolua_S)
{
 if (
 !tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
 !tolua_istype(tolua_S,2,LUA_TNUMBER,0) ||
 !tolua_istype(tolua_S,3,LUA_TNUMBER,0) ||
 !tolua_istype(tolua_S,4,LUA_TNUMBER,0) ||
 !tolua_isnoobj(tolua_S,5)
 )
 goto tolua_lerror;
 else
 {
  int y = ((int)  tolua_getnumber(tolua_S,1,0));
  int x = ((int)  tolua_getnumber(tolua_S,2,0));
  bool slp = ((bool)  tolua_getnumber(tolua_S,3,0));
  bool grp = ((bool)  tolua_getnumber(tolua_S,4,0));
 {
  bool toluaI_ret = (bool)  place_monster(y,x,slp,grp);
 tolua_pushnumber(tolua_S,(long)toluaI_ret);
 }
 }
 return 1;
tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'place_monster'.");
 return 0;
}

/* function: place_monster_one */
static int toluaI_monster_place_monster_one00(lua_State* tolua_S)
{
 if (
 !tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
 !tolua_istype(tolua_S,2,LUA_TNUMBER,0) ||
 !tolua_istype(tolua_S,3,LUA_TNUMBER,0) ||
 !tolua_istype(tolua_S,4,LUA_TNUMBER,0) ||
 !tolua_istype(tolua_S,5,LUA_TNUMBER,0) ||
 !tolua_istype(tolua_S,6,LUA_TNUMBER,0) ||
 !tolua_isnoobj(tolua_S,7)
 )
 goto tolua_lerror;
 else
 {
  int y = ((int)  tolua_getnumber(tolua_S,1,0));
  int x = ((int)  tolua_getnumber(tolua_S,2,0));
  int r_idx = ((int)  tolua_getnumber(tolua_S,3,0));
  int ego = ((int)  tolua_getnumber(tolua_S,4,0));
  bool slp = ((bool)  tolua_getnumber(tolua_S,5,0));
  int status = ((int)  tolua_getnumber(tolua_S,6,0));
 {
  s16b toluaI_ret = (s16b)  place_monster_one(y,x,r_idx,ego,slp,status);
 tolua_pushnumber(tolua_S,(long)toluaI_ret);
 }
 }
 return 1;
tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'place_monster_one'.");
 return 0;
}

/* function: is_friend */
static int toluaI_monster_is_friend00(lua_State* tolua_S)
{
 if (
 !tolua_istype(tolua_S,1,tolua_tag(tolua_S,"monster_type"),0) ||
 !tolua_isnoobj(tolua_S,2)
 )
 goto tolua_lerror;
 else
 {
  monster_type* m_ptr = ((monster_type*)  tolua_getusertype(tolua_S,1,0));
 {
  int toluaI_ret = (int)  is_friend(m_ptr);
 tolua_pushnumber(tolua_S,(long)toluaI_ret);
 }
 }
 return 1;
tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'is_friend'.");
 return 0;
}

/* function: is_enemy */
static int toluaI_monster_is_enemy00(lua_State* tolua_S)
{
 if (
 !tolua_istype(tolua_S,1,tolua_tag(tolua_S,"monster_type"),0) ||
 !tolua_istype(tolua_S,2,tolua_tag(tolua_S,"monster_type"),0) ||
 !tolua_isnoobj(tolua_S,3)
 )
 goto tolua_lerror;
 else
 {
  monster_type* m_ptr = ((monster_type*)  tolua_getusertype(tolua_S,1,0));
  monster_type* t_ptr = ((monster_type*)  tolua_getusertype(tolua_S,2,0));
 {
  bool toluaI_ret = (bool)  is_enemy(m_ptr,t_ptr);
 tolua_pushnumber(tolua_S,(long)toluaI_ret);
 }
 }
 return 1;
tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'is_enemy'.");
 return 0;
}

/* function: change_side */
static int toluaI_monster_change_side00(lua_State* tolua_S)
{
 if (
 !tolua_istype(tolua_S,1,tolua_tag(tolua_S,"monster_type"),0) ||
 !tolua_isnoobj(tolua_S,2)
 )
 goto tolua_lerror;
 else
 {
  monster_type* m_ptr = ((monster_type*)  tolua_getusertype(tolua_S,1,0));
 {
  bool toluaI_ret = (bool)  change_side(m_ptr);
 tolua_pushnumber(tolua_S,(long)toluaI_ret);
 }
 }
 return 1;
tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'change_side'.");
 return 0;
}

/* function: find_position */
static int toluaI_monster_find_position00(lua_State* tolua_S)
{
 if (
 !tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
 !tolua_istype(tolua_S,2,LUA_TNUMBER,0) ||
 !tolua_istype(tolua_S,3,LUA_TNUMBER,1) ||
 !tolua_istype(tolua_S,4,LUA_TNUMBER,1) ||
 !tolua_isnoobj(tolua_S,5)
 )
 goto tolua_lerror;
 else
 {
  int y = ((int)  tolua_getnumber(tolua_S,1,0));
  int x = ((int)  tolua_getnumber(tolua_S,2,0));
  int yy = ((int)  tolua_getnumber(tolua_S,3,0));
  int xx = ((int)  tolua_getnumber(tolua_S,4,0));
 {
  find_position(y,x,&yy,&xx);
 tolua_pushnumber(tolua_S,(long)yy);
 tolua_pushnumber(tolua_S,(long)xx);
 }
 }
 return 2;
tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'find_position'.");
 return 0;
}

/* get function: summon_specific_level */
static int toluaI_get_monster_summon_specific_level(lua_State* tolua_S)
{
 tolua_pushnumber(tolua_S,(long)summon_specific_level);
 return 1;
}

/* set function: summon_specific_level */
static int toluaI_set_monster_summon_specific_level(lua_State* tolua_S)
{
 if (!tolua_istype(tolua_S,1,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  summon_specific_level = ((int)  tolua_getnumber(tolua_S,1,0));
 return 0;
}

/* get function: summon_kin_type */
static int toluaI_get_monster_summon_kin_type(lua_State* tolua_S)
{
 tolua_pushnumber(tolua_S,(long)summon_kin_type);
 return 1;
}

/* set function: summon_kin_type */
static int toluaI_set_monster_summon_kin_type(lua_State* tolua_S)
{
 if (!tolua_istype(tolua_S,1,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  summon_kin_type = ((char)  tolua_getnumber(tolua_S,1,0));
 return 0;
}

/* function: summon_specific */
static int toluaI_monster_summon_specific00(lua_State* tolua_S)
{
 if (
 !tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
 !tolua_istype(tolua_S,2,LUA_TNUMBER,0) ||
 !tolua_istype(tolua_S,3,LUA_TNUMBER,0) ||
 !tolua_istype(tolua_S,4,LUA_TNUMBER,0) ||
 !tolua_isnoobj(tolua_S,5)
 )
 goto tolua_lerror;
 else
 {
  int y1 = ((int)  tolua_getnumber(tolua_S,1,0));
  int x1 = ((int)  tolua_getnumber(tolua_S,2,0));
  int lev = ((int)  tolua_getnumber(tolua_S,3,0));
  int type = ((int)  tolua_getnumber(tolua_S,4,0));
 {
  bool toluaI_ret = (bool)  summon_specific(y1,x1,lev,type);
 tolua_pushnumber(tolua_S,(long)toluaI_ret);
 }
 }
 return 1;
tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'summon_specific'.");
 return 0;
}

/* function: summon_specific_friendly */
static int toluaI_monster_summon_specific_friendly00(lua_State* tolua_S)
{
 if (
 !tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
 !tolua_istype(tolua_S,2,LUA_TNUMBER,0) ||
 !tolua_istype(tolua_S,3,LUA_TNUMBER,0) ||
 !tolua_istype(tolua_S,4,LUA_TNUMBER,0) ||
 !tolua_istype(tolua_S,5,LUA_TNUMBER,0) ||
 !tolua_isnoobj(tolua_S,6)
 )
 goto tolua_lerror;
 else
 {
  int y1 = ((int)  tolua_getnumber(tolua_S,1,0));
  int x1 = ((int)  tolua_getnumber(tolua_S,2,0));
  int lev = ((int)  tolua_getnumber(tolua_S,3,0));
  int type = ((int)  tolua_getnumber(tolua_S,4,0));
  bool Group_ok = ((bool)  tolua_getnumber(tolua_S,5,0));
 {
  bool toluaI_ret = (bool)  summon_specific_friendly(y1,x1,lev,type,Group_ok);
 tolua_pushnumber(tolua_S,(long)toluaI_ret);
 }
 }
 return 1;
tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'summon_specific_friendly'.");
 return 0;
}

/* function: lua_summon_monster */
static int toluaI_monster_summon_monster_aux00(lua_State* tolua_S)
{
 if (
 !tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
 !tolua_istype(tolua_S,2,LUA_TNUMBER,0) ||
 !tolua_istype(tolua_S,3,LUA_TNUMBER,0) ||
 !tolua_istype(tolua_S,4,LUA_TNUMBER,0) ||
 !tolua_istype(tolua_S,5,LUA_TSTRING,0) ||
 !tolua_isnoobj(tolua_S,6)
 )
 goto tolua_lerror;
 else
 {
  int y = ((int)  tolua_getnumber(tolua_S,1,0));
  int x = ((int)  tolua_getnumber(tolua_S,2,0));
  int lev = ((int)  tolua_getnumber(tolua_S,3,0));
  bool friend = ((bool)  tolua_getnumber(tolua_S,4,0));
  char* fct = ((char*)  tolua_getstring(tolua_S,5,0));
 {
  bool toluaI_ret = (bool)  lua_summon_monster(y,x,lev,friend,fct);
 tolua_pushnumber(tolua_S,(long)toluaI_ret);
 }
 }
 return 1;
tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'summon_monster_aux'.");
 return 0;
}

/* function: can_create_companion */
static int toluaI_monster_can_create_companion00(lua_State* tolua_S)
{
 if (
 !tolua_isnoobj(tolua_S,1)
 )
 goto tolua_lerror;
 else
 {
 {
  bool toluaI_ret = (bool)  can_create_companion();
 tolua_pushnumber(tolua_S,(long)toluaI_ret);
 }
 }
 return 1;
tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'can_create_companion'.");
 return 0;
}

/* function: monster_set_level */
static int toluaI_monster_monster_set_level00(lua_State* tolua_S)
{
 if (
 !tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
 !tolua_istype(tolua_S,2,LUA_TNUMBER,0) ||
 !tolua_isnoobj(tolua_S,3)
 )
 goto tolua_lerror;
 else
 {
  int m_idx = ((int)  tolua_getnumber(tolua_S,1,0));
  int level = ((int)  tolua_getnumber(tolua_S,2,0));
 {
  monster_set_level(m_idx,level);
 }
 }
 return 0;
tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'monster_set_level'.");
 return 0;
}

/* function: do_control_reconnect */
static int toluaI_monster_do_control_reconnect00(lua_State* tolua_S)
{
 if (
 !tolua_isnoobj(tolua_S,1)
 )
 goto tolua_lerror;
 else
 {
 {
  bool toluaI_ret = (bool)  do_control_reconnect();
 tolua_pushnumber(tolua_S,(long)toluaI_ret);
 }
 }
 return 1;
tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'do_control_reconnect'.");
 return 0;
}

/* get function: m_max */
static int toluaI_get_monster_m_max(lua_State* tolua_S)
{
 tolua_pushnumber(tolua_S,(long)m_max);
 return 1;
}

/* set function: m_max */
static int toluaI_set_monster_m_max(lua_State* tolua_S)
{
 if (!tolua_istype(tolua_S,1,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  m_max = ((s16b)  tolua_getnumber(tolua_S,1,0));
 return 0;
}

/* Open function */
int tolua_monster_open (lua_State* tolua_S)
{
 tolua_open(tolua_S);
 toluaI_reg_types(tolua_S);
 tolua_globalvar(tolua_S,"monster_forge",toluaI_get_monster_monster_forge,toluaI_set_monster_monster_forge);
 tolua_constant(tolua_S,NULL,"MSTATUS_ENEMY",MSTATUS_ENEMY);
 tolua_constant(tolua_S,NULL,"MSTATUS_NEUTRAL_M",MSTATUS_NEUTRAL_M);
 tolua_constant(tolua_S,NULL,"MSTATUS_NEUTRAL",MSTATUS_NEUTRAL);
 tolua_constant(tolua_S,NULL,"MSTATUS_NEUTRAL_P",MSTATUS_NEUTRAL_P);
 tolua_constant(tolua_S,NULL,"MSTATUS_FRIEND",MSTATUS_FRIEND);
 tolua_constant(tolua_S,NULL,"MSTATUS_PET",MSTATUS_PET);
 tolua_constant(tolua_S,NULL,"MSTATUS_COMPANION",MSTATUS_COMPANION);
 tolua_constant(tolua_S,NULL,"RF1_UNIQUE",RF1_UNIQUE);
 tolua_constant(tolua_S,NULL,"RF1_QUESTOR",RF1_QUESTOR);
 tolua_constant(tolua_S,NULL,"RF1_MALE",RF1_MALE);
 tolua_constant(tolua_S,NULL,"RF1_FEMALE",RF1_FEMALE);
 tolua_constant(tolua_S,NULL,"RF1_CHAR_CLEAR",RF1_CHAR_CLEAR);
 tolua_constant(tolua_S,NULL,"RF1_CHAR_MULTI",RF1_CHAR_MULTI);
 tolua_constant(tolua_S,NULL,"RF1_ATTR_CLEAR",RF1_ATTR_CLEAR);
 tolua_constant(tolua_S,NULL,"RF1_ATTR_MULTI",RF1_ATTR_MULTI);
 tolua_constant(tolua_S,NULL,"RF1_FORCE_DEPTH",RF1_FORCE_DEPTH);
 tolua_constant(tolua_S,NULL,"RF1_FORCE_MAXHP",RF1_FORCE_MAXHP);
 tolua_constant(tolua_S,NULL,"RF1_FORCE_SLEEP",RF1_FORCE_SLEEP);
 tolua_constant(tolua_S,NULL,"RF1_FORCE_EXTRA",RF1_FORCE_EXTRA);
 tolua_constant(tolua_S,NULL,"RF1_FRIEND",RF1_FRIEND);
 tolua_constant(tolua_S,NULL,"RF1_FRIENDS",RF1_FRIENDS);
 tolua_constant(tolua_S,NULL,"RF1_ESCORT",RF1_ESCORT);
 tolua_constant(tolua_S,NULL,"RF1_ESCORTS",RF1_ESCORTS);
 tolua_constant(tolua_S,NULL,"RF1_NEVER_BLOW",RF1_NEVER_BLOW);
 tolua_constant(tolua_S,NULL,"RF1_NEVER_MOVE",RF1_NEVER_MOVE);
 tolua_constant(tolua_S,NULL,"RF1_RAND_25",RF1_RAND_25);
 tolua_constant(tolua_S,NULL,"RF1_RAND_50",RF1_RAND_50);
 tolua_constant(tolua_S,NULL,"RF1_ONLY_GOLD",RF1_ONLY_GOLD);
 tolua_constant(tolua_S,NULL,"RF1_ONLY_ITEM",RF1_ONLY_ITEM);
 tolua_constant(tolua_S,NULL,"RF1_DROP_60",RF1_DROP_60);
 tolua_constant(tolua_S,NULL,"RF1_DROP_90",RF1_DROP_90);
 tolua_constant(tolua_S,NULL,"RF1_DROP_1D2",RF1_DROP_1D2);
 tolua_constant(tolua_S,NULL,"RF1_DROP_2D2",RF1_DROP_2D2);
 tolua_constant(tolua_S,NULL,"RF1_DROP_3D2",RF1_DROP_3D2);
 tolua_constant(tolua_S,NULL,"RF1_DROP_4D2",RF1_DROP_4D2);
 tolua_constant(tolua_S,NULL,"RF1_DROP_GOOD",RF1_DROP_GOOD);
 tolua_constant(tolua_S,NULL,"RF1_DROP_GREAT",RF1_DROP_GREAT);
 tolua_constant(tolua_S,NULL,"RF1_DROP_USEFUL",RF1_DROP_USEFUL);
 tolua_constant(tolua_S,NULL,"RF1_DROP_CHOSEN",RF1_DROP_CHOSEN);
 tolua_constant(tolua_S,NULL,"RF2_STUPID",RF2_STUPID);
 tolua_constant(tolua_S,NULL,"RF2_SMART",RF2_SMART);
 tolua_constant(tolua_S,NULL,"RF2_CAN_SPEAK",RF2_CAN_SPEAK);
 tolua_constant(tolua_S,NULL,"RF2_REFLECTING",RF2_REFLECTING);
 tolua_constant(tolua_S,NULL,"RF2_INVISIBLE",RF2_INVISIBLE);
 tolua_constant(tolua_S,NULL,"RF2_COLD_BLOOD",RF2_COLD_BLOOD);
 tolua_constant(tolua_S,NULL,"RF2_EMPTY_MIND",RF2_EMPTY_MIND);
 tolua_constant(tolua_S,NULL,"RF2_WEIRD_MIND",RF2_WEIRD_MIND);
 tolua_constant(tolua_S,NULL,"RF2_DEATH_ORB",RF2_DEATH_ORB);
 tolua_constant(tolua_S,NULL,"RF2_REGENERATE",RF2_REGENERATE);
 tolua_constant(tolua_S,NULL,"RF2_SHAPECHANGER",RF2_SHAPECHANGER);
 tolua_constant(tolua_S,NULL,"RF2_ATTR_ANY",RF2_ATTR_ANY);
 tolua_constant(tolua_S,NULL,"RF2_POWERFUL",RF2_POWERFUL);
 tolua_constant(tolua_S,NULL,"RF2_ELDRITCH_HORROR",RF2_ELDRITCH_HORROR);
 tolua_constant(tolua_S,NULL,"RF2_AURA_FIRE",RF2_AURA_FIRE);
 tolua_constant(tolua_S,NULL,"RF2_AURA_ELEC",RF2_AURA_ELEC);
 tolua_constant(tolua_S,NULL,"RF2_OPEN_DOOR",RF2_OPEN_DOOR);
 tolua_constant(tolua_S,NULL,"RF2_BASH_DOOR",RF2_BASH_DOOR);
 tolua_constant(tolua_S,NULL,"RF2_PASS_WALL",RF2_PASS_WALL);
 tolua_constant(tolua_S,NULL,"RF2_KILL_WALL",RF2_KILL_WALL);
 tolua_constant(tolua_S,NULL,"RF2_MOVE_BODY",RF2_MOVE_BODY);
 tolua_constant(tolua_S,NULL,"RF2_KILL_BODY",RF2_KILL_BODY);
 tolua_constant(tolua_S,NULL,"RF2_TAKE_ITEM",RF2_TAKE_ITEM);
 tolua_constant(tolua_S,NULL,"RF2_KILL_ITEM",RF2_KILL_ITEM);
 tolua_constant(tolua_S,NULL,"RF2_BRAIN_1",RF2_BRAIN_1);
 tolua_constant(tolua_S,NULL,"RF2_BRAIN_2",RF2_BRAIN_2);
 tolua_constant(tolua_S,NULL,"RF2_BRAIN_3",RF2_BRAIN_3);
 tolua_constant(tolua_S,NULL,"RF2_BRAIN_4",RF2_BRAIN_4);
 tolua_constant(tolua_S,NULL,"RF2_BRAIN_5",RF2_BRAIN_5);
 tolua_constant(tolua_S,NULL,"RF2_BRAIN_6",RF2_BRAIN_6);
 tolua_constant(tolua_S,NULL,"RF2_BRAIN_7",RF2_BRAIN_7);
 tolua_constant(tolua_S,NULL,"RF2_BRAIN_8",RF2_BRAIN_8);
 tolua_constant(tolua_S,NULL,"RF3_ORC",RF3_ORC);
 tolua_constant(tolua_S,NULL,"RF3_TROLL",RF3_TROLL);
 tolua_constant(tolua_S,NULL,"RF3_GIANT",RF3_GIANT);
 tolua_constant(tolua_S,NULL,"RF3_DRAGON",RF3_DRAGON);
 tolua_constant(tolua_S,NULL,"RF3_DEMON",RF3_DEMON);
 tolua_constant(tolua_S,NULL,"RF3_UNDEAD",RF3_UNDEAD);
 tolua_constant(tolua_S,NULL,"RF3_EVIL",RF3_EVIL);
 tolua_constant(tolua_S,NULL,"RF3_ANIMAL",RF3_ANIMAL);
 tolua_constant(tolua_S,NULL,"RF3_THUNDERLORD",RF3_THUNDERLORD);
 tolua_constant(tolua_S,NULL,"RF3_GOOD",RF3_GOOD);
 tolua_constant(tolua_S,NULL,"RF3_AURA_COLD",RF3_AURA_COLD);
 tolua_constant(tolua_S,NULL,"RF3_NONLIVING",RF3_NONLIVING);
 tolua_constant(tolua_S,NULL,"RF3_HURT_LITE",RF3_HURT_LITE);
 tolua_constant(tolua_S,NULL,"RF3_HURT_ROCK",RF3_HURT_ROCK);
 tolua_constant(tolua_S,NULL,"RF3_SUSCEP_FIRE",RF3_SUSCEP_FIRE);
 tolua_constant(tolua_S,NULL,"RF3_SUSCEP_COLD",RF3_SUSCEP_COLD);
 tolua_constant(tolua_S,NULL,"RF3_IM_ACID",RF3_IM_ACID);
 tolua_constant(tolua_S,NULL,"RF3_IM_ELEC",RF3_IM_ELEC);
 tolua_constant(tolua_S,NULL,"RF3_IM_FIRE",RF3_IM_FIRE);
 tolua_constant(tolua_S,NULL,"RF3_IM_COLD",RF3_IM_COLD);
 tolua_constant(tolua_S,NULL,"RF3_IM_POIS",RF3_IM_POIS);
 tolua_constant(tolua_S,NULL,"RF3_RES_TELE",RF3_RES_TELE);
 tolua_constant(tolua_S,NULL,"RF3_RES_NETH",RF3_RES_NETH);
 tolua_constant(tolua_S,NULL,"RF3_RES_WATE",RF3_RES_WATE);
 tolua_constant(tolua_S,NULL,"RF3_RES_PLAS",RF3_RES_PLAS);
 tolua_constant(tolua_S,NULL,"RF3_RES_NEXU",RF3_RES_NEXU);
 tolua_constant(tolua_S,NULL,"RF3_RES_DISE",RF3_RES_DISE);
 tolua_constant(tolua_S,NULL,"RF3_UNIQUE_4",RF3_UNIQUE_4);
 tolua_constant(tolua_S,NULL,"RF3_NO_FEAR",RF3_NO_FEAR);
 tolua_constant(tolua_S,NULL,"RF3_NO_STUN",RF3_NO_STUN);
 tolua_constant(tolua_S,NULL,"RF3_NO_CONF",RF3_NO_CONF);
 tolua_constant(tolua_S,NULL,"RF3_NO_SLEEP",RF3_NO_SLEEP);
 tolua_constant(tolua_S,NULL,"RF4_SHRIEK",RF4_SHRIEK);
 tolua_constant(tolua_S,NULL,"RF4_MULTIPLY",RF4_MULTIPLY);
 tolua_constant(tolua_S,NULL,"RF4_S_ANIMAL",RF4_S_ANIMAL);
 tolua_constant(tolua_S,NULL,"RF4_ROCKET",RF4_ROCKET);
 tolua_constant(tolua_S,NULL,"RF4_ARROW_1",RF4_ARROW_1);
 tolua_constant(tolua_S,NULL,"RF4_ARROW_2",RF4_ARROW_2);
 tolua_constant(tolua_S,NULL,"RF4_ARROW_3",RF4_ARROW_3);
 tolua_constant(tolua_S,NULL,"RF4_ARROW_4",RF4_ARROW_4);
 tolua_constant(tolua_S,NULL,"RF4_BR_ACID",RF4_BR_ACID);
 tolua_constant(tolua_S,NULL,"RF4_BR_ELEC",RF4_BR_ELEC);
 tolua_constant(tolua_S,NULL,"RF4_BR_FIRE",RF4_BR_FIRE);
 tolua_constant(tolua_S,NULL,"RF4_BR_COLD",RF4_BR_COLD);
 tolua_constant(tolua_S,NULL,"RF4_BR_POIS",RF4_BR_POIS);
 tolua_constant(tolua_S,NULL,"RF4_BR_NETH",RF4_BR_NETH);
 tolua_constant(tolua_S,NULL,"RF4_BR_LITE",RF4_BR_LITE);
 tolua_constant(tolua_S,NULL,"RF4_BR_DARK",RF4_BR_DARK);
 tolua_constant(tolua_S,NULL,"RF4_BR_CONF",RF4_BR_CONF);
 tolua_constant(tolua_S,NULL,"RF4_BR_SOUN",RF4_BR_SOUN);
 tolua_constant(tolua_S,NULL,"RF4_BR_CHAO",RF4_BR_CHAO);
 tolua_constant(tolua_S,NULL,"RF4_BR_DISE",RF4_BR_DISE);
 tolua_constant(tolua_S,NULL,"RF4_BR_NEXU",RF4_BR_NEXU);
 tolua_constant(tolua_S,NULL,"RF4_BR_TIME",RF4_BR_TIME);
 tolua_constant(tolua_S,NULL,"RF4_BR_INER",RF4_BR_INER);
 tolua_constant(tolua_S,NULL,"RF4_BR_GRAV",RF4_BR_GRAV);
 tolua_constant(tolua_S,NULL,"RF4_BR_SHAR",RF4_BR_SHAR);
 tolua_constant(tolua_S,NULL,"RF4_BR_PLAS",RF4_BR_PLAS);
 tolua_constant(tolua_S,NULL,"RF4_BR_WALL",RF4_BR_WALL);
 tolua_constant(tolua_S,NULL,"RF4_BR_MANA",RF4_BR_MANA);
 tolua_constant(tolua_S,NULL,"RF4_BA_NUKE",RF4_BA_NUKE);
 tolua_constant(tolua_S,NULL,"RF4_BR_NUKE",RF4_BR_NUKE);
 tolua_constant(tolua_S,NULL,"RF4_BA_CHAO",RF4_BA_CHAO);
 tolua_constant(tolua_S,NULL,"RF4_BR_DISI",RF4_BR_DISI);
 tolua_constant(tolua_S,NULL,"RF5_BA_ACID",RF5_BA_ACID);
 tolua_constant(tolua_S,NULL,"RF5_BA_ELEC",RF5_BA_ELEC);
 tolua_constant(tolua_S,NULL,"RF5_BA_FIRE",RF5_BA_FIRE);
 tolua_constant(tolua_S,NULL,"RF5_BA_COLD",RF5_BA_COLD);
 tolua_constant(tolua_S,NULL,"RF5_BA_POIS",RF5_BA_POIS);
 tolua_constant(tolua_S,NULL,"RF5_BA_NETH",RF5_BA_NETH);
 tolua_constant(tolua_S,NULL,"RF5_BA_WATE",RF5_BA_WATE);
 tolua_constant(tolua_S,NULL,"RF5_BA_MANA",RF5_BA_MANA);
 tolua_constant(tolua_S,NULL,"RF5_BA_DARK",RF5_BA_DARK);
 tolua_constant(tolua_S,NULL,"RF5_DRAIN_MANA",RF5_DRAIN_MANA);
 tolua_constant(tolua_S,NULL,"RF5_MIND_BLAST",RF5_MIND_BLAST);
 tolua_constant(tolua_S,NULL,"RF5_BRAIN_SMASH",RF5_BRAIN_SMASH);
 tolua_constant(tolua_S,NULL,"RF5_CAUSE_1",RF5_CAUSE_1);
 tolua_constant(tolua_S,NULL,"RF5_CAUSE_2",RF5_CAUSE_2);
 tolua_constant(tolua_S,NULL,"RF5_CAUSE_3",RF5_CAUSE_3);
 tolua_constant(tolua_S,NULL,"RF5_CAUSE_4",RF5_CAUSE_4);
 tolua_constant(tolua_S,NULL,"RF5_BO_ACID",RF5_BO_ACID);
 tolua_constant(tolua_S,NULL,"RF5_BO_ELEC",RF5_BO_ELEC);
 tolua_constant(tolua_S,NULL,"RF5_BO_FIRE",RF5_BO_FIRE);
 tolua_constant(tolua_S,NULL,"RF5_BO_COLD",RF5_BO_COLD);
 tolua_constant(tolua_S,NULL,"RF5_BO_POIS",RF5_BO_POIS);
 tolua_constant(tolua_S,NULL,"RF5_BO_NETH",RF5_BO_NETH);
 tolua_constant(tolua_S,NULL,"RF5_BO_WATE",RF5_BO_WATE);
 tolua_constant(tolua_S,NULL,"RF5_BO_MANA",RF5_BO_MANA);
 tolua_constant(tolua_S,NULL,"RF5_BO_PLAS",RF5_BO_PLAS);
 tolua_constant(tolua_S,NULL,"RF5_BO_ICEE",RF5_BO_ICEE);
 tolua_constant(tolua_S,NULL,"RF5_MISSILE",RF5_MISSILE);
 tolua_constant(tolua_S,NULL,"RF5_SCARE",RF5_SCARE);
 tolua_constant(tolua_S,NULL,"RF5_BLIND",RF5_BLIND);
 tolua_constant(tolua_S,NULL,"RF5_CONF",RF5_CONF);
 tolua_constant(tolua_S,NULL,"RF5_SLOW",RF5_SLOW);
 tolua_constant(tolua_S,NULL,"RF5_HOLD",RF5_HOLD);
 tolua_constant(tolua_S,NULL,"RF6_HASTE",RF6_HASTE);
 tolua_constant(tolua_S,NULL,"RF6_HAND_DOOM",RF6_HAND_DOOM);
 tolua_constant(tolua_S,NULL,"RF6_HEAL",RF6_HEAL);
 tolua_constant(tolua_S,NULL,"RF6_S_ANIMALS",RF6_S_ANIMALS);
 tolua_constant(tolua_S,NULL,"RF6_BLINK",RF6_BLINK);
 tolua_constant(tolua_S,NULL,"RF6_TPORT",RF6_TPORT);
 tolua_constant(tolua_S,NULL,"RF6_TELE_TO",RF6_TELE_TO);
 tolua_constant(tolua_S,NULL,"RF6_TELE_AWAY",RF6_TELE_AWAY);
 tolua_constant(tolua_S,NULL,"RF6_TELE_LEVEL",RF6_TELE_LEVEL);
 tolua_constant(tolua_S,NULL,"RF6_DARKNESS",RF6_DARKNESS);
 tolua_constant(tolua_S,NULL,"RF6_TRAPS",RF6_TRAPS);
 tolua_constant(tolua_S,NULL,"RF6_FORGET",RF6_FORGET);
 tolua_constant(tolua_S,NULL,"RF6_RAISE_DEAD",RF6_RAISE_DEAD);
 tolua_constant(tolua_S,NULL,"RF6_S_BUG",RF6_S_BUG);
 tolua_constant(tolua_S,NULL,"RF6_S_RNG",RF6_S_RNG);
 tolua_constant(tolua_S,NULL,"RF6_S_THUNDERLORD",RF6_S_THUNDERLORD);
 tolua_constant(tolua_S,NULL,"RF6_S_KIN",RF6_S_KIN);
 tolua_constant(tolua_S,NULL,"RF6_S_HI_DEMON",RF6_S_HI_DEMON);
 tolua_constant(tolua_S,NULL,"RF6_S_MONSTER",RF6_S_MONSTER);
 tolua_constant(tolua_S,NULL,"RF6_S_MONSTERS",RF6_S_MONSTERS);
 tolua_constant(tolua_S,NULL,"RF6_S_ANT",RF6_S_ANT);
 tolua_constant(tolua_S,NULL,"RF6_S_SPIDER",RF6_S_SPIDER);
 tolua_constant(tolua_S,NULL,"RF6_S_HOUND",RF6_S_HOUND);
 tolua_constant(tolua_S,NULL,"RF6_S_HYDRA",RF6_S_HYDRA);
 tolua_constant(tolua_S,NULL,"RF6_S_ANGEL",RF6_S_ANGEL);
 tolua_constant(tolua_S,NULL,"RF6_S_DEMON",RF6_S_DEMON);
 tolua_constant(tolua_S,NULL,"RF6_S_UNDEAD",RF6_S_UNDEAD);
 tolua_constant(tolua_S,NULL,"RF6_S_DRAGON",RF6_S_DRAGON);
 tolua_constant(tolua_S,NULL,"RF6_S_HI_UNDEAD",RF6_S_HI_UNDEAD);
 tolua_constant(tolua_S,NULL,"RF6_S_HI_DRAGON",RF6_S_HI_DRAGON);
 tolua_constant(tolua_S,NULL,"RF6_S_WRAITH",RF6_S_WRAITH);
 tolua_constant(tolua_S,NULL,"RF6_S_UNIQUE",RF6_S_UNIQUE);
 tolua_constant(tolua_S,NULL,"RF7_AQUATIC",RF7_AQUATIC);
 tolua_constant(tolua_S,NULL,"RF7_CAN_SWIM",RF7_CAN_SWIM);
 tolua_constant(tolua_S,NULL,"RF7_CAN_FLY",RF7_CAN_FLY);
 tolua_constant(tolua_S,NULL,"RF7_FRIENDLY",RF7_FRIENDLY);
 tolua_constant(tolua_S,NULL,"RF7_PET",RF7_PET);
 tolua_constant(tolua_S,NULL,"RF7_MORTAL",RF7_MORTAL);
 tolua_constant(tolua_S,NULL,"RF7_SPIDER",RF7_SPIDER);
 tolua_constant(tolua_S,NULL,"RF7_NAZGUL",RF7_NAZGUL);
 tolua_constant(tolua_S,NULL,"RF7_DG_CURSE",RF7_DG_CURSE);
 tolua_constant(tolua_S,NULL,"RF7_POSSESSOR",RF7_POSSESSOR);
 tolua_constant(tolua_S,NULL,"RF7_NO_DEATH",RF7_NO_DEATH);
 tolua_constant(tolua_S,NULL,"RF7_NO_TARGET",RF7_NO_TARGET);
 tolua_constant(tolua_S,NULL,"RF7_AI_ANNOY",RF7_AI_ANNOY);
 tolua_constant(tolua_S,NULL,"RF7_AI_SPECIAL",RF7_AI_SPECIAL);
 tolua_constant(tolua_S,NULL,"RF7_NO_THEFT",RF7_NO_THEFT);
 tolua_constant(tolua_S,NULL,"RF7_SPIRIT",RF7_SPIRIT);
 tolua_constant(tolua_S,NULL,"RF7_IM_MELEE",RF7_IM_MELEE);
 tolua_constant(tolua_S,NULL,"RF8_DUNGEON",RF8_DUNGEON);
 tolua_constant(tolua_S,NULL,"RF8_WILD_TOWN",RF8_WILD_TOWN);
 tolua_constant(tolua_S,NULL,"RF8_XXX8X02",RF8_XXX8X02);
 tolua_constant(tolua_S,NULL,"RF8_WILD_SHORE",RF8_WILD_SHORE);
 tolua_constant(tolua_S,NULL,"RF8_WILD_OCEAN",RF8_WILD_OCEAN);
 tolua_constant(tolua_S,NULL,"RF8_WILD_WASTE",RF8_WILD_WASTE);
 tolua_constant(tolua_S,NULL,"RF8_WILD_WOOD",RF8_WILD_WOOD);
 tolua_constant(tolua_S,NULL,"RF8_WILD_VOLCANO",RF8_WILD_VOLCANO);
 tolua_constant(tolua_S,NULL,"RF8_XXX8X08",RF8_XXX8X08);
 tolua_constant(tolua_S,NULL,"RF8_WILD_MOUNTAIN",RF8_WILD_MOUNTAIN);
 tolua_constant(tolua_S,NULL,"RF8_WILD_GRASS",RF8_WILD_GRASS);
 tolua_constant(tolua_S,NULL,"RF8_CTHANGBAND",RF8_CTHANGBAND);
 tolua_constant(tolua_S,NULL,"RF8_ZANGBAND",RF8_ZANGBAND);
 tolua_constant(tolua_S,NULL,"RF8_JOKEANGBAND",RF8_JOKEANGBAND);
 tolua_constant(tolua_S,NULL,"RF8_ANGBAND",RF8_ANGBAND);
 tolua_constant(tolua_S,NULL,"RF8_WILD_TOO",RF8_WILD_TOO);
 tolua_constant(tolua_S,NULL,"RF9_DROP_CORPSE",RF9_DROP_CORPSE);
 tolua_constant(tolua_S,NULL,"RF9_DROP_SKELETON",RF9_DROP_SKELETON);
 tolua_constant(tolua_S,NULL,"RF9_HAS_LITE",RF9_HAS_LITE);
 tolua_constant(tolua_S,NULL,"RF9_MIMIC",RF9_MIMIC);
 tolua_constant(tolua_S,NULL,"RF9_HAS_EGG",RF9_HAS_EGG);
 tolua_constant(tolua_S,NULL,"RF9_IMPRESED",RF9_IMPRESED);
 tolua_constant(tolua_S,NULL,"RF9_SUSCEP_ACID",RF9_SUSCEP_ACID);
 tolua_constant(tolua_S,NULL,"RF9_SUSCEP_ELEC",RF9_SUSCEP_ELEC);
 tolua_constant(tolua_S,NULL,"RF9_SUSCEP_POIS",RF9_SUSCEP_POIS);
 tolua_constant(tolua_S,NULL,"RF9_KILL_TREES",RF9_KILL_TREES);
 tolua_constant(tolua_S,NULL,"RF9_WYRM_PROTECT",RF9_WYRM_PROTECT);
 tolua_constant(tolua_S,NULL,"RF9_DOPPLEGANGER",RF9_DOPPLEGANGER);
 tolua_constant(tolua_S,NULL,"RF9_ONLY_DEPTH",RF9_ONLY_DEPTH);
 tolua_constant(tolua_S,NULL,"RF9_SPECIAL_GENE",RF9_SPECIAL_GENE);
 tolua_constant(tolua_S,NULL,"RF9_NEVER_GENE",RF9_NEVER_GENE);
 tolua_constant(tolua_S,NULL,"MFLAG_VIEW",MFLAG_VIEW);
 tolua_constant(tolua_S,NULL,"MFLAG_QUEST",MFLAG_QUEST);
 tolua_constant(tolua_S,NULL,"MFLAG_PARTIAL",MFLAG_PARTIAL);
 tolua_constant(tolua_S,NULL,"MFLAG_CONTROL",MFLAG_CONTROL);
 tolua_constant(tolua_S,NULL,"MFLAG_BORN",MFLAG_BORN);
 tolua_constant(tolua_S,NULL,"MFLAG_NICE",MFLAG_NICE);
 tolua_constant(tolua_S,NULL,"MFLAG_SHOW",MFLAG_SHOW);
 tolua_constant(tolua_S,NULL,"MFLAG_MARK",MFLAG_MARK);
 tolua_constant(tolua_S,NULL,"MFLAG_NO_DROP",MFLAG_NO_DROP);
 tolua_constant(tolua_S,NULL,"MFLAG_QUEST2",MFLAG_QUEST2);
 tolua_cclass(tolua_S,"monster_blow","");
 tolua_tablevar(tolua_S,"monster_blow","method",toluaI_get_monster_monster_blow_method,toluaI_set_monster_monster_blow_method);
 tolua_tablevar(tolua_S,"monster_blow","effect",toluaI_get_monster_monster_blow_effect,toluaI_set_monster_monster_blow_effect);
 tolua_tablevar(tolua_S,"monster_blow","d_dice",toluaI_get_monster_monster_blow_d_dice,toluaI_set_monster_monster_blow_d_dice);
 tolua_tablevar(tolua_S,"monster_blow","d_side",toluaI_get_monster_monster_blow_d_side,toluaI_set_monster_monster_blow_d_side);
 tolua_cclass(tolua_S,"monster_race","");
 tolua_tablevar(tolua_S,"monster_race","name",toluaI_get_monster_monster_race_name,toluaI_set_monster_monster_race_name);
 tolua_tablevar(tolua_S,"monster_race","text",toluaI_get_monster_monster_race_text,toluaI_set_monster_monster_race_text);
 tolua_tablevar(tolua_S,"monster_race","hdice",toluaI_get_monster_monster_race_hdice,toluaI_set_monster_monster_race_hdice);
 tolua_tablevar(tolua_S,"monster_race","hside",toluaI_get_monster_monster_race_hside,toluaI_set_monster_monster_race_hside);
 tolua_tablevar(tolua_S,"monster_race","ac",toluaI_get_monster_monster_race_ac,toluaI_set_monster_monster_race_ac);
 tolua_tablevar(tolua_S,"monster_race","sleep",toluaI_get_monster_monster_race_sleep,toluaI_set_monster_monster_race_sleep);
 tolua_tablevar(tolua_S,"monster_race","aaf",toluaI_get_monster_monster_race_aaf,toluaI_set_monster_monster_race_aaf);
 tolua_tablevar(tolua_S,"monster_race","speed",toluaI_get_monster_monster_race_speed,toluaI_set_monster_monster_race_speed);
 tolua_tablevar(tolua_S,"monster_race","mexp",toluaI_get_monster_monster_race_mexp,toluaI_set_monster_monster_race_mexp);
 tolua_tablevar(tolua_S,"monster_race","weight",toluaI_get_monster_monster_race_weight,toluaI_set_monster_monster_race_weight);
 tolua_tablevar(tolua_S,"monster_race","freq_inate",toluaI_get_monster_monster_race_freq_inate,toluaI_set_monster_monster_race_freq_inate);
 tolua_tablevar(tolua_S,"monster_race","freq_spell",toluaI_get_monster_monster_race_freq_spell,toluaI_set_monster_monster_race_freq_spell);
 tolua_tablevar(tolua_S,"monster_race","flags1",toluaI_get_monster_monster_race_flags1,toluaI_set_monster_monster_race_flags1);
 tolua_tablevar(tolua_S,"monster_race","flags2",toluaI_get_monster_monster_race_flags2,toluaI_set_monster_monster_race_flags2);
 tolua_tablevar(tolua_S,"monster_race","flags3",toluaI_get_monster_monster_race_flags3,toluaI_set_monster_monster_race_flags3);
 tolua_tablevar(tolua_S,"monster_race","flags4",toluaI_get_monster_monster_race_flags4,toluaI_set_monster_monster_race_flags4);
 tolua_tablevar(tolua_S,"monster_race","flags5",toluaI_get_monster_monster_race_flags5,toluaI_set_monster_monster_race_flags5);
 tolua_tablevar(tolua_S,"monster_race","flags6",toluaI_get_monster_monster_race_flags6,toluaI_set_monster_monster_race_flags6);
 tolua_tablevar(tolua_S,"monster_race","flags7",toluaI_get_monster_monster_race_flags7,toluaI_set_monster_monster_race_flags7);
 tolua_tablevar(tolua_S,"monster_race","flags8",toluaI_get_monster_monster_race_flags8,toluaI_set_monster_monster_race_flags8);
 tolua_tablevar(tolua_S,"monster_race","flags9",toluaI_get_monster_monster_race_flags9,toluaI_set_monster_monster_race_flags9);
 tolua_tablearray(tolua_S,"monster_race","blow",toluaI_get_monster_monster_race_blow,toluaI_set_monster_monster_race_blow);
 tolua_tablearray(tolua_S,"monster_race","body_parts",toluaI_get_monster_monster_race_body_parts,toluaI_set_monster_monster_race_body_parts);
 tolua_tablevar(tolua_S,"monster_race","level",toluaI_get_monster_monster_race_level,toluaI_set_monster_monster_race_level);
 tolua_tablevar(tolua_S,"monster_race","rarity",toluaI_get_monster_monster_race_rarity,toluaI_set_monster_monster_race_rarity);
 tolua_tablevar(tolua_S,"monster_race","d_attr",toluaI_get_monster_monster_race_d_attr,toluaI_set_monster_monster_race_d_attr);
 tolua_tablevar(tolua_S,"monster_race","d_char",toluaI_get_monster_monster_race_d_char,toluaI_set_monster_monster_race_d_char);
 tolua_tablevar(tolua_S,"monster_race","x_attr",toluaI_get_monster_monster_race_x_attr,toluaI_set_monster_monster_race_x_attr);
 tolua_tablevar(tolua_S,"monster_race","x_char",toluaI_get_monster_monster_race_x_char,toluaI_set_monster_monster_race_x_char);
 tolua_tablevar(tolua_S,"monster_race","max_num",toluaI_get_monster_monster_race_max_num,toluaI_set_monster_monster_race_max_num);
 tolua_tablevar(tolua_S,"monster_race","cur_num",toluaI_get_monster_monster_race_cur_num,toluaI_set_monster_monster_race_cur_num);
 tolua_tablevar(tolua_S,"monster_race","r_sights",toluaI_get_monster_monster_race_r_sights,toluaI_set_monster_monster_race_r_sights);
 tolua_tablevar(tolua_S,"monster_race","r_deaths",toluaI_get_monster_monster_race_r_deaths,toluaI_set_monster_monster_race_r_deaths);
 tolua_tablevar(tolua_S,"monster_race","r_pkills",toluaI_get_monster_monster_race_r_pkills,toluaI_set_monster_monster_race_r_pkills);
 tolua_tablevar(tolua_S,"monster_race","r_tkills",toluaI_get_monster_monster_race_r_tkills,toluaI_set_monster_monster_race_r_tkills);
 tolua_tablevar(tolua_S,"monster_race","r_wake",toluaI_get_monster_monster_race_r_wake,toluaI_set_monster_monster_race_r_wake);
 tolua_tablevar(tolua_S,"monster_race","r_ignore",toluaI_get_monster_monster_race_r_ignore,toluaI_set_monster_monster_race_r_ignore);
 tolua_tablevar(tolua_S,"monster_race","r_xtra1",toluaI_get_monster_monster_race_r_xtra1,toluaI_set_monster_monster_race_r_xtra1);
 tolua_tablevar(tolua_S,"monster_race","r_xtra2",toluaI_get_monster_monster_race_r_xtra2,toluaI_set_monster_monster_race_r_xtra2);
 tolua_tablevar(tolua_S,"monster_race","r_drop_gold",toluaI_get_monster_monster_race_r_drop_gold,toluaI_set_monster_monster_race_r_drop_gold);
 tolua_tablevar(tolua_S,"monster_race","r_drop_item",toluaI_get_monster_monster_race_r_drop_item,toluaI_set_monster_monster_race_r_drop_item);
 tolua_tablevar(tolua_S,"monster_race","r_cast_inate",toluaI_get_monster_monster_race_r_cast_inate,toluaI_set_monster_monster_race_r_cast_inate);
 tolua_tablevar(tolua_S,"monster_race","r_cast_spell",toluaI_get_monster_monster_race_r_cast_spell,toluaI_set_monster_monster_race_r_cast_spell);
 tolua_tablearray(tolua_S,"monster_race","r_blows",toluaI_get_monster_monster_race_r_blows,toluaI_set_monster_monster_race_r_blows);
 tolua_tablevar(tolua_S,"monster_race","r_flags1",toluaI_get_monster_monster_race_r_flags1,toluaI_set_monster_monster_race_r_flags1);
 tolua_tablevar(tolua_S,"monster_race","r_flags2",toluaI_get_monster_monster_race_r_flags2,toluaI_set_monster_monster_race_r_flags2);
 tolua_tablevar(tolua_S,"monster_race","r_flags3",toluaI_get_monster_monster_race_r_flags3,toluaI_set_monster_monster_race_r_flags3);
 tolua_tablevar(tolua_S,"monster_race","r_flags4",toluaI_get_monster_monster_race_r_flags4,toluaI_set_monster_monster_race_r_flags4);
 tolua_tablevar(tolua_S,"monster_race","r_flags5",toluaI_get_monster_monster_race_r_flags5,toluaI_set_monster_monster_race_r_flags5);
 tolua_tablevar(tolua_S,"monster_race","r_flags6",toluaI_get_monster_monster_race_r_flags6,toluaI_set_monster_monster_race_r_flags6);
 tolua_tablevar(tolua_S,"monster_race","r_flags7",toluaI_get_monster_monster_race_r_flags7,toluaI_set_monster_monster_race_r_flags7);
 tolua_tablevar(tolua_S,"monster_race","r_flags8",toluaI_get_monster_monster_race_r_flags8,toluaI_set_monster_monster_race_r_flags8);
 tolua_tablevar(tolua_S,"monster_race","r_flags9",toluaI_get_monster_monster_race_r_flags9,toluaI_set_monster_monster_race_r_flags9);
 tolua_tablevar(tolua_S,"monster_race","on_saved",toluaI_get_monster_monster_race_on_saved,toluaI_set_monster_monster_race_on_saved);
 tolua_tablevar(tolua_S,"monster_race","total_visible",toluaI_get_monster_monster_race_total_visible,toluaI_set_monster_monster_race_total_visible);
 tolua_tablevar(tolua_S,"monster_race","drops",toluaI_get_monster_monster_race_drops,toluaI_set_monster_monster_race_drops);
 tolua_cclass(tolua_S,"monster_type","");
 tolua_tablevar(tolua_S,"monster_type","r_idx",toluaI_get_monster_monster_type_r_idx,toluaI_set_monster_monster_type_r_idx);
 tolua_tablevar(tolua_S,"monster_type","ego",toluaI_get_monster_monster_type_ego,toluaI_set_monster_monster_type_ego);
 tolua_tablevar(tolua_S,"monster_type","fy",toluaI_get_monster_monster_type_fy,toluaI_set_monster_monster_type_fy);
 tolua_tablevar(tolua_S,"monster_type","fx",toluaI_get_monster_monster_type_fx,toluaI_set_monster_monster_type_fx);
 tolua_tablevar(tolua_S,"monster_type","hp",toluaI_get_monster_monster_type_hp,toluaI_set_monster_monster_type_hp);
 tolua_tablevar(tolua_S,"monster_type","maxhp",toluaI_get_monster_monster_type_maxhp,toluaI_set_monster_monster_type_maxhp);
 tolua_tablearray(tolua_S,"monster_type","blow",toluaI_get_monster_monster_type_blow,toluaI_set_monster_monster_type_blow);
 tolua_tablevar(tolua_S,"monster_type","speed",toluaI_get_monster_monster_type_speed,toluaI_set_monster_monster_type_speed);
 tolua_tablevar(tolua_S,"monster_type","level",toluaI_get_monster_monster_type_level,toluaI_set_monster_monster_type_level);
 tolua_tablevar(tolua_S,"monster_type","ac",toluaI_get_monster_monster_type_ac,toluaI_set_monster_monster_type_ac);
 tolua_tablevar(tolua_S,"monster_type","exp",toluaI_get_monster_monster_type_exp,toluaI_set_monster_monster_type_exp);
 tolua_tablevar(tolua_S,"monster_type","csleep",toluaI_get_monster_monster_type_csleep,toluaI_set_monster_monster_type_csleep);
 tolua_tablevar(tolua_S,"monster_type","mspeed",toluaI_get_monster_monster_type_mspeed,toluaI_set_monster_monster_type_mspeed);
 tolua_tablevar(tolua_S,"monster_type","energy",toluaI_get_monster_monster_type_energy,toluaI_set_monster_monster_type_energy);
 tolua_tablevar(tolua_S,"monster_type","stunned",toluaI_get_monster_monster_type_stunned,toluaI_set_monster_monster_type_stunned);
 tolua_tablevar(tolua_S,"monster_type","confused",toluaI_get_monster_monster_type_confused,toluaI_set_monster_monster_type_confused);
 tolua_tablevar(tolua_S,"monster_type","monfear",toluaI_get_monster_monster_type_monfear,toluaI_set_monster_monster_type_monfear);
 tolua_tablevar(tolua_S,"monster_type","bleeding",toluaI_get_monster_monster_type_bleeding,toluaI_set_monster_monster_type_bleeding);
 tolua_tablevar(tolua_S,"monster_type","poisoned",toluaI_get_monster_monster_type_poisoned,toluaI_set_monster_monster_type_poisoned);
 tolua_tablevar(tolua_S,"monster_type","cdis",toluaI_get_monster_monster_type_cdis,toluaI_set_monster_monster_type_cdis);
 tolua_tablevar(tolua_S,"monster_type","mflag",toluaI_get_monster_monster_type_mflag,toluaI_set_monster_monster_type_mflag);
 tolua_tablevar(tolua_S,"monster_type","ml",toluaI_get_monster_monster_type_ml,toluaI_set_monster_monster_type_ml);
 tolua_tablevar(tolua_S,"monster_type","hold_o_idx",toluaI_get_monster_monster_type_hold_o_idx,toluaI_set_monster_monster_type_hold_o_idx);
 tolua_tablevar(tolua_S,"monster_type","smart",toluaI_get_monster_monster_type_smart,toluaI_set_monster_monster_type_smart);
 tolua_tablevar(tolua_S,"monster_type","status",toluaI_get_monster_monster_type_status,toluaI_set_monster_monster_type_status);
 tolua_tablevar(tolua_S,"monster_type","target",toluaI_get_monster_monster_type_target,toluaI_set_monster_monster_type_target);
 tolua_tablevar(tolua_S,"monster_type","possessor",toluaI_get_monster_monster_type_possessor,toluaI_set_monster_monster_type_possessor);
 tolua_function(tolua_S,NULL,"monster",toluaI_monster_monster00);
 tolua_globalarray(tolua_S,"m_list",toluaI_get_monster_m_list,toluaI_set_monster_m_list);
 tolua_function(tolua_S,NULL,"race_info_idx",toluaI_monster_race_info_idx00);
 tolua_function(tolua_S,NULL,"delete_monster_idx",toluaI_monster_delete_monster_idx00);
 tolua_function(tolua_S,NULL,"m_pop",toluaI_monster_m_pop00);
 tolua_function(tolua_S,NULL,"get_mon_num_prep",toluaI_monster_get_mon_num_prep00);
 tolua_function(tolua_S,NULL,"get_mon_num",toluaI_monster_get_mon_num00);
 tolua_function(tolua_S,NULL,"monster_desc",toluaI_monster_monster_desc00);
 tolua_function(tolua_S,NULL,"monster_race_desc",toluaI_monster_monster_race_desc00);
 tolua_function(tolua_S,NULL,"monster_race_desc",toluaI_monster_monster_race_desc01);
 tolua_function(tolua_S,NULL,"monster_carry",toluaI_monster_monster_carry00);
 tolua_function(tolua_S,NULL,"place_monster_aux",toluaI_monster_place_monster_aux00);
 tolua_function(tolua_S,NULL,"place_monster",toluaI_monster_place_monster00);
 tolua_function(tolua_S,NULL,"place_monster_one",toluaI_monster_place_monster_one00);
 tolua_function(tolua_S,NULL,"is_friend",toluaI_monster_is_friend00);
 tolua_function(tolua_S,NULL,"is_enemy",toluaI_monster_is_enemy00);
 tolua_function(tolua_S,NULL,"change_side",toluaI_monster_change_side00);
 tolua_function(tolua_S,NULL,"find_position",toluaI_monster_find_position00);
 tolua_globalvar(tolua_S,"summon_specific_level",toluaI_get_monster_summon_specific_level,toluaI_set_monster_summon_specific_level);
 tolua_globalvar(tolua_S,"summon_kin_type",toluaI_get_monster_summon_kin_type,toluaI_set_monster_summon_kin_type);
 tolua_function(tolua_S,NULL,"summon_specific",toluaI_monster_summon_specific00);
 tolua_function(tolua_S,NULL,"summon_specific_friendly",toluaI_monster_summon_specific_friendly00);
 tolua_function(tolua_S,NULL,"summon_monster_aux",toluaI_monster_summon_monster_aux00);
 tolua_function(tolua_S,NULL,"can_create_companion",toluaI_monster_can_create_companion00);
 tolua_function(tolua_S,NULL,"monster_set_level",toluaI_monster_monster_set_level00);
 tolua_constant(tolua_S,NULL,"SUMMON_ANT",SUMMON_ANT);
 tolua_constant(tolua_S,NULL,"SUMMON_SPIDER",SUMMON_SPIDER);
 tolua_constant(tolua_S,NULL,"SUMMON_HOUND",SUMMON_HOUND);
 tolua_constant(tolua_S,NULL,"SUMMON_HYDRA",SUMMON_HYDRA);
 tolua_constant(tolua_S,NULL,"SUMMON_ANGEL",SUMMON_ANGEL);
 tolua_constant(tolua_S,NULL,"SUMMON_DEMON",SUMMON_DEMON);
 tolua_constant(tolua_S,NULL,"SUMMON_UNDEAD",SUMMON_UNDEAD);
 tolua_constant(tolua_S,NULL,"SUMMON_DRAGON",SUMMON_DRAGON);
 tolua_constant(tolua_S,NULL,"SUMMON_HI_UNDEAD",SUMMON_HI_UNDEAD);
 tolua_constant(tolua_S,NULL,"SUMMON_HI_DRAGON",SUMMON_HI_DRAGON);
 tolua_constant(tolua_S,NULL,"SUMMON_WRAITH",SUMMON_WRAITH);
 tolua_constant(tolua_S,NULL,"SUMMON_UNIQUE",SUMMON_UNIQUE);
 tolua_constant(tolua_S,NULL,"SUMMON_BIZARRE1",SUMMON_BIZARRE1);
 tolua_constant(tolua_S,NULL,"SUMMON_BIZARRE2",SUMMON_BIZARRE2);
 tolua_constant(tolua_S,NULL,"SUMMON_BIZARRE3",SUMMON_BIZARRE3);
 tolua_constant(tolua_S,NULL,"SUMMON_BIZARRE4",SUMMON_BIZARRE4);
 tolua_constant(tolua_S,NULL,"SUMMON_BIZARRE5",SUMMON_BIZARRE5);
 tolua_constant(tolua_S,NULL,"SUMMON_BIZARRE6",SUMMON_BIZARRE6);
 tolua_constant(tolua_S,NULL,"SUMMON_HI_DEMON",SUMMON_HI_DEMON);
 tolua_constant(tolua_S,NULL,"SUMMON_KIN",SUMMON_KIN);
 tolua_constant(tolua_S,NULL,"SUMMON_DAWN",SUMMON_DAWN);
 tolua_constant(tolua_S,NULL,"SUMMON_ANIMAL",SUMMON_ANIMAL);
 tolua_constant(tolua_S,NULL,"SUMMON_ANIMAL_RANGER",SUMMON_ANIMAL_RANGER);
 tolua_constant(tolua_S,NULL,"SUMMON_HI_UNDEAD_NO_UNIQUES",SUMMON_HI_UNDEAD_NO_UNIQUES);
 tolua_constant(tolua_S,NULL,"SUMMON_HI_DRAGON_NO_UNIQUES",SUMMON_HI_DRAGON_NO_UNIQUES);
 tolua_constant(tolua_S,NULL,"SUMMON_NO_UNIQUES",SUMMON_NO_UNIQUES);
 tolua_constant(tolua_S,NULL,"SUMMON_PHANTOM",SUMMON_PHANTOM);
 tolua_constant(tolua_S,NULL,"SUMMON_ELEMENTAL",SUMMON_ELEMENTAL);
 tolua_constant(tolua_S,NULL,"SUMMON_THUNDERLORD",SUMMON_THUNDERLORD);
 tolua_constant(tolua_S,NULL,"SUMMON_BLUE_HORROR",SUMMON_BLUE_HORROR);
 tolua_constant(tolua_S,NULL,"SUMMON_BUG",SUMMON_BUG);
 tolua_constant(tolua_S,NULL,"SUMMON_RNG",SUMMON_RNG);
 tolua_constant(tolua_S,NULL,"SUMMON_MINE",SUMMON_MINE);
 tolua_constant(tolua_S,NULL,"SUMMON_HUMAN",SUMMON_HUMAN);
 tolua_constant(tolua_S,NULL,"SUMMON_SHADOWS",SUMMON_SHADOWS);
 tolua_constant(tolua_S,NULL,"SUMMON_GHOST",SUMMON_GHOST);
 tolua_constant(tolua_S,NULL,"SUMMON_QUYLTHULG",SUMMON_QUYLTHULG);
 tolua_constant(tolua_S,NULL,"SUMMON_LUA",SUMMON_LUA);
 tolua_function(tolua_S,NULL,"do_control_reconnect",toluaI_monster_do_control_reconnect00);
 tolua_globalvar(tolua_S,"m_max",toluaI_get_monster_m_max,toluaI_set_monster_m_max);
 return 1;
}
/* Close function */
void tolua_monster_close (lua_State* tolua_S)
{
 lua_getglobals(tolua_S);
 lua_pushstring(tolua_S,"monster_forge"); lua_pushnil(tolua_S); lua_rawset(tolua_S,-3);
 lua_pop(tolua_S,1);
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"MSTATUS_ENEMY");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"MSTATUS_NEUTRAL_M");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"MSTATUS_NEUTRAL");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"MSTATUS_NEUTRAL_P");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"MSTATUS_FRIEND");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"MSTATUS_PET");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"MSTATUS_COMPANION");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF1_UNIQUE");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF1_QUESTOR");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF1_MALE");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF1_FEMALE");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF1_CHAR_CLEAR");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF1_CHAR_MULTI");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF1_ATTR_CLEAR");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF1_ATTR_MULTI");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF1_FORCE_DEPTH");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF1_FORCE_MAXHP");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF1_FORCE_SLEEP");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF1_FORCE_EXTRA");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF1_FRIEND");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF1_FRIENDS");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF1_ESCORT");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF1_ESCORTS");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF1_NEVER_BLOW");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF1_NEVER_MOVE");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF1_RAND_25");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF1_RAND_50");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF1_ONLY_GOLD");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF1_ONLY_ITEM");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF1_DROP_60");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF1_DROP_90");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF1_DROP_1D2");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF1_DROP_2D2");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF1_DROP_3D2");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF1_DROP_4D2");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF1_DROP_GOOD");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF1_DROP_GREAT");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF1_DROP_USEFUL");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF1_DROP_CHOSEN");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF2_STUPID");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF2_SMART");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF2_CAN_SPEAK");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF2_REFLECTING");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF2_INVISIBLE");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF2_COLD_BLOOD");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF2_EMPTY_MIND");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF2_WEIRD_MIND");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF2_DEATH_ORB");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF2_REGENERATE");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF2_SHAPECHANGER");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF2_ATTR_ANY");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF2_POWERFUL");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF2_ELDRITCH_HORROR");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF2_AURA_FIRE");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF2_AURA_ELEC");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF2_OPEN_DOOR");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF2_BASH_DOOR");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF2_PASS_WALL");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF2_KILL_WALL");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF2_MOVE_BODY");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF2_KILL_BODY");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF2_TAKE_ITEM");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF2_KILL_ITEM");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF2_BRAIN_1");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF2_BRAIN_2");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF2_BRAIN_3");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF2_BRAIN_4");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF2_BRAIN_5");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF2_BRAIN_6");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF2_BRAIN_7");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF2_BRAIN_8");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF3_ORC");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF3_TROLL");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF3_GIANT");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF3_DRAGON");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF3_DEMON");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF3_UNDEAD");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF3_EVIL");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF3_ANIMAL");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF3_THUNDERLORD");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF3_GOOD");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF3_AURA_COLD");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF3_NONLIVING");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF3_HURT_LITE");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF3_HURT_ROCK");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF3_SUSCEP_FIRE");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF3_SUSCEP_COLD");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF3_IM_ACID");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF3_IM_ELEC");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF3_IM_FIRE");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF3_IM_COLD");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF3_IM_POIS");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF3_RES_TELE");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF3_RES_NETH");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF3_RES_WATE");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF3_RES_PLAS");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF3_RES_NEXU");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF3_RES_DISE");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF3_UNIQUE_4");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF3_NO_FEAR");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF3_NO_STUN");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF3_NO_CONF");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF3_NO_SLEEP");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF4_SHRIEK");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF4_MULTIPLY");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF4_S_ANIMAL");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF4_ROCKET");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF4_ARROW_1");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF4_ARROW_2");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF4_ARROW_3");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF4_ARROW_4");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF4_BR_ACID");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF4_BR_ELEC");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF4_BR_FIRE");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF4_BR_COLD");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF4_BR_POIS");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF4_BR_NETH");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF4_BR_LITE");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF4_BR_DARK");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF4_BR_CONF");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF4_BR_SOUN");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF4_BR_CHAO");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF4_BR_DISE");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF4_BR_NEXU");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF4_BR_TIME");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF4_BR_INER");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF4_BR_GRAV");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF4_BR_SHAR");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF4_BR_PLAS");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF4_BR_WALL");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF4_BR_MANA");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF4_BA_NUKE");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF4_BR_NUKE");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF4_BA_CHAO");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF4_BR_DISI");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF5_BA_ACID");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF5_BA_ELEC");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF5_BA_FIRE");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF5_BA_COLD");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF5_BA_POIS");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF5_BA_NETH");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF5_BA_WATE");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF5_BA_MANA");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF5_BA_DARK");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF5_DRAIN_MANA");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF5_MIND_BLAST");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF5_BRAIN_SMASH");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF5_CAUSE_1");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF5_CAUSE_2");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF5_CAUSE_3");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF5_CAUSE_4");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF5_BO_ACID");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF5_BO_ELEC");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF5_BO_FIRE");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF5_BO_COLD");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF5_BO_POIS");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF5_BO_NETH");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF5_BO_WATE");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF5_BO_MANA");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF5_BO_PLAS");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF5_BO_ICEE");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF5_MISSILE");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF5_SCARE");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF5_BLIND");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF5_CONF");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF5_SLOW");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF5_HOLD");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF6_HASTE");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF6_HAND_DOOM");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF6_HEAL");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF6_S_ANIMALS");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF6_BLINK");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF6_TPORT");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF6_TELE_TO");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF6_TELE_AWAY");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF6_TELE_LEVEL");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF6_DARKNESS");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF6_TRAPS");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF6_FORGET");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF6_RAISE_DEAD");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF6_S_BUG");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF6_S_RNG");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF6_S_THUNDERLORD");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF6_S_KIN");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF6_S_HI_DEMON");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF6_S_MONSTER");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF6_S_MONSTERS");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF6_S_ANT");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF6_S_SPIDER");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF6_S_HOUND");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF6_S_HYDRA");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF6_S_ANGEL");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF6_S_DEMON");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF6_S_UNDEAD");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF6_S_DRAGON");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF6_S_HI_UNDEAD");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF6_S_HI_DRAGON");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF6_S_WRAITH");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF6_S_UNIQUE");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF7_AQUATIC");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF7_CAN_SWIM");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF7_CAN_FLY");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF7_FRIENDLY");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF7_PET");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF7_MORTAL");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF7_SPIDER");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF7_NAZGUL");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF7_DG_CURSE");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF7_POSSESSOR");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF7_NO_DEATH");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF7_NO_TARGET");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF7_AI_ANNOY");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF7_AI_SPECIAL");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF7_NO_THEFT");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF7_SPIRIT");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF7_IM_MELEE");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF8_DUNGEON");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF8_WILD_TOWN");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF8_XXX8X02");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF8_WILD_SHORE");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF8_WILD_OCEAN");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF8_WILD_WASTE");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF8_WILD_WOOD");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF8_WILD_VOLCANO");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF8_XXX8X08");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF8_WILD_MOUNTAIN");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF8_WILD_GRASS");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF8_CTHANGBAND");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF8_ZANGBAND");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF8_JOKEANGBAND");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF8_ANGBAND");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF8_WILD_TOO");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF9_DROP_CORPSE");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF9_DROP_SKELETON");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF9_HAS_LITE");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF9_MIMIC");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF9_HAS_EGG");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF9_IMPRESED");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF9_SUSCEP_ACID");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF9_SUSCEP_ELEC");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF9_SUSCEP_POIS");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF9_KILL_TREES");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF9_WYRM_PROTECT");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF9_DOPPLEGANGER");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF9_ONLY_DEPTH");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF9_SPECIAL_GENE");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF9_NEVER_GENE");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"MFLAG_VIEW");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"MFLAG_QUEST");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"MFLAG_PARTIAL");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"MFLAG_CONTROL");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"MFLAG_BORN");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"MFLAG_NICE");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"MFLAG_SHOW");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"MFLAG_MARK");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"MFLAG_NO_DROP");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"MFLAG_QUEST2");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"monster_blow");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"monster_race");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"monster_type");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"monster");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"m_list");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"race_info_idx");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"delete_monster_idx");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"m_pop");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"get_mon_num_prep");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"get_mon_num");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"monster_desc");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"monster_race_desc");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"monster_race_desc");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"monster_carry");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"place_monster_aux");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"place_monster");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"place_monster_one");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"is_friend");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"is_enemy");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"change_side");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"find_position");
 lua_getglobals(tolua_S);
 lua_pushstring(tolua_S,"summon_specific_level"); lua_pushnil(tolua_S); lua_rawset(tolua_S,-3);
 lua_pop(tolua_S,1);
 lua_getglobals(tolua_S);
 lua_pushstring(tolua_S,"summon_kin_type"); lua_pushnil(tolua_S); lua_rawset(tolua_S,-3);
 lua_pop(tolua_S,1);
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"summon_specific");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"summon_specific_friendly");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"summon_monster_aux");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"can_create_companion");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"monster_set_level");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SUMMON_ANT");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SUMMON_SPIDER");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SUMMON_HOUND");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SUMMON_HYDRA");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SUMMON_ANGEL");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SUMMON_DEMON");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SUMMON_UNDEAD");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SUMMON_DRAGON");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SUMMON_HI_UNDEAD");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SUMMON_HI_DRAGON");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SUMMON_WRAITH");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SUMMON_UNIQUE");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SUMMON_BIZARRE1");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SUMMON_BIZARRE2");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SUMMON_BIZARRE3");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SUMMON_BIZARRE4");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SUMMON_BIZARRE5");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SUMMON_BIZARRE6");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SUMMON_HI_DEMON");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SUMMON_KIN");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SUMMON_DAWN");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SUMMON_ANIMAL");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SUMMON_ANIMAL_RANGER");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SUMMON_HI_UNDEAD_NO_UNIQUES");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SUMMON_HI_DRAGON_NO_UNIQUES");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SUMMON_NO_UNIQUES");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SUMMON_PHANTOM");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SUMMON_ELEMENTAL");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SUMMON_THUNDERLORD");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SUMMON_BLUE_HORROR");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SUMMON_BUG");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SUMMON_RNG");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SUMMON_MINE");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SUMMON_HUMAN");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SUMMON_SHADOWS");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SUMMON_GHOST");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SUMMON_QUYLTHULG");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SUMMON_LUA");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"do_control_reconnect");
 lua_getglobals(tolua_S);
 lua_pushstring(tolua_S,"m_max"); lua_pushnil(tolua_S); lua_rawset(tolua_S,-3);
 lua_pop(tolua_S,1);
}
