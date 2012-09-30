/*
** Lua binding: monst
** Generated automatically by tolua 4.0a - angband.
*/

#include "lua/tolua.h"

/* Exported function */
int  tolua_monst_open (lua_State* tolua_S);
void tolua_monst_close (lua_State* tolua_S);

#include "angband.h"

/* function to register type */
static void toluaI_reg_types (lua_State* tolua_S)
{
(void) tolua_S;	/* Hack - prevent compiler warnings */
 tolua_usertype(tolua_S,"monster_type");
 tolua_usertype(tolua_S,"monster_blow");
 tolua_usertype(tolua_S,"obj_theme");
 tolua_usertype(tolua_S,"monster_race");
}

/* error messages */
#define TOLUA_ERR_SELF tolua_error(tolua_S,"invalid 'self'")
#define TOLUA_ERR_ASSIGN tolua_error(tolua_S,"#vinvalid type in variable assignment.")

/* get function: method of class  monster_blow */
static int toluaI_get_monst_monster_blow_method(lua_State* tolua_S)
{
  monster_blow* self = (monster_blow*)  tolua_getusertype(tolua_S,1,0);
  if (!self) TOLUA_ERR_SELF;
  tolua_pushnumber(tolua_S,(long)self->method);
 return 1;
}

/* set function: method of class  monster_blow */
static int toluaI_set_monst_monster_blow_method(lua_State* tolua_S)
{
  monster_blow* self = (monster_blow*)  tolua_getusertype(tolua_S,1,0);
  if (!self) TOLUA_ERR_SELF;
  if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
   TOLUA_ERR_ASSIGN;
  self->method = ((byte)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: effect of class  monster_blow */
static int toluaI_get_monst_monster_blow_effect(lua_State* tolua_S)
{
  monster_blow* self = (monster_blow*)  tolua_getusertype(tolua_S,1,0);
  if (!self) TOLUA_ERR_SELF;
  tolua_pushnumber(tolua_S,(long)self->effect);
 return 1;
}

/* set function: effect of class  monster_blow */
static int toluaI_set_monst_monster_blow_effect(lua_State* tolua_S)
{
  monster_blow* self = (monster_blow*)  tolua_getusertype(tolua_S,1,0);
  if (!self) TOLUA_ERR_SELF;
  if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
   TOLUA_ERR_ASSIGN;
  self->effect = ((byte)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: d_dice of class  monster_blow */
static int toluaI_get_monst_monster_blow_d_dice(lua_State* tolua_S)
{
  monster_blow* self = (monster_blow*)  tolua_getusertype(tolua_S,1,0);
  if (!self) TOLUA_ERR_SELF;
  tolua_pushnumber(tolua_S,(long)self->d_dice);
 return 1;
}

/* set function: d_dice of class  monster_blow */
static int toluaI_set_monst_monster_blow_d_dice(lua_State* tolua_S)
{
  monster_blow* self = (monster_blow*)  tolua_getusertype(tolua_S,1,0);
  if (!self) TOLUA_ERR_SELF;
  if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
   TOLUA_ERR_ASSIGN;
  self->d_dice = ((byte)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: d_side of class  monster_blow */
static int toluaI_get_monst_monster_blow_d_side(lua_State* tolua_S)
{
  monster_blow* self = (monster_blow*)  tolua_getusertype(tolua_S,1,0);
  if (!self) TOLUA_ERR_SELF;
  tolua_pushnumber(tolua_S,(long)self->d_side);
 return 1;
}

/* set function: d_side of class  monster_blow */
static int toluaI_set_monst_monster_blow_d_side(lua_State* tolua_S)
{
  monster_blow* self = (monster_blow*)  tolua_getusertype(tolua_S,1,0);
  if (!self) TOLUA_ERR_SELF;
  if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
   TOLUA_ERR_ASSIGN;
  self->d_side = ((byte)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: name of class  monster_race */
static int toluaI_get_monst_monster_race_name(lua_State* tolua_S)
{
  monster_race* self = (monster_race*)  tolua_getusertype(tolua_S,1,0);
  if (!self) TOLUA_ERR_SELF;
  tolua_pushnumber(tolua_S,(long)self->name);
 return 1;
}

/* set function: name of class  monster_race */
static int toluaI_set_monst_monster_race_name(lua_State* tolua_S)
{
  monster_race* self = (monster_race*)  tolua_getusertype(tolua_S,1,0);
  if (!self) TOLUA_ERR_SELF;
  if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
   TOLUA_ERR_ASSIGN;
  self->name = ((u32b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: text of class  monster_race */
static int toluaI_get_monst_monster_race_text(lua_State* tolua_S)
{
  monster_race* self = (monster_race*)  tolua_getusertype(tolua_S,1,0);
  if (!self) TOLUA_ERR_SELF;
  tolua_pushnumber(tolua_S,(long)self->text);
 return 1;
}

/* set function: text of class  monster_race */
static int toluaI_set_monst_monster_race_text(lua_State* tolua_S)
{
  monster_race* self = (monster_race*)  tolua_getusertype(tolua_S,1,0);
  if (!self) TOLUA_ERR_SELF;
  if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
   TOLUA_ERR_ASSIGN;
  self->text = ((u32b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: hdice of class  monster_race */
static int toluaI_get_monst_monster_race_hdice(lua_State* tolua_S)
{
  monster_race* self = (monster_race*)  tolua_getusertype(tolua_S,1,0);
  if (!self) TOLUA_ERR_SELF;
  tolua_pushnumber(tolua_S,(long)self->hdice);
 return 1;
}

/* set function: hdice of class  monster_race */
static int toluaI_set_monst_monster_race_hdice(lua_State* tolua_S)
{
  monster_race* self = (monster_race*)  tolua_getusertype(tolua_S,1,0);
  if (!self) TOLUA_ERR_SELF;
  if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
   TOLUA_ERR_ASSIGN;
  self->hdice = ((byte)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: hside of class  monster_race */
static int toluaI_get_monst_monster_race_hside(lua_State* tolua_S)
{
  monster_race* self = (monster_race*)  tolua_getusertype(tolua_S,1,0);
  if (!self) TOLUA_ERR_SELF;
  tolua_pushnumber(tolua_S,(long)self->hside);
 return 1;
}

/* set function: hside of class  monster_race */
static int toluaI_set_monst_monster_race_hside(lua_State* tolua_S)
{
  monster_race* self = (monster_race*)  tolua_getusertype(tolua_S,1,0);
  if (!self) TOLUA_ERR_SELF;
  if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
   TOLUA_ERR_ASSIGN;
  self->hside = ((byte)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: ac of class  monster_race */
static int toluaI_get_monst_monster_race_ac(lua_State* tolua_S)
{
  monster_race* self = (monster_race*)  tolua_getusertype(tolua_S,1,0);
  if (!self) TOLUA_ERR_SELF;
  tolua_pushnumber(tolua_S,(long)self->ac);
 return 1;
}

/* set function: ac of class  monster_race */
static int toluaI_set_monst_monster_race_ac(lua_State* tolua_S)
{
  monster_race* self = (monster_race*)  tolua_getusertype(tolua_S,1,0);
  if (!self) TOLUA_ERR_SELF;
  if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
   TOLUA_ERR_ASSIGN;
  self->ac = ((s16b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: sleep of class  monster_race */
static int toluaI_get_monst_monster_race_sleep(lua_State* tolua_S)
{
  monster_race* self = (monster_race*)  tolua_getusertype(tolua_S,1,0);
  if (!self) TOLUA_ERR_SELF;
  tolua_pushnumber(tolua_S,(long)self->sleep);
 return 1;
}

/* set function: sleep of class  monster_race */
static int toluaI_set_monst_monster_race_sleep(lua_State* tolua_S)
{
  monster_race* self = (monster_race*)  tolua_getusertype(tolua_S,1,0);
  if (!self) TOLUA_ERR_SELF;
  if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
   TOLUA_ERR_ASSIGN;
  self->sleep = ((s16b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: aaf of class  monster_race */
static int toluaI_get_monst_monster_race_aaf(lua_State* tolua_S)
{
  monster_race* self = (monster_race*)  tolua_getusertype(tolua_S,1,0);
  if (!self) TOLUA_ERR_SELF;
  tolua_pushnumber(tolua_S,(long)self->aaf);
 return 1;
}

/* set function: aaf of class  monster_race */
static int toluaI_set_monst_monster_race_aaf(lua_State* tolua_S)
{
  monster_race* self = (monster_race*)  tolua_getusertype(tolua_S,1,0);
  if (!self) TOLUA_ERR_SELF;
  if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
   TOLUA_ERR_ASSIGN;
  self->aaf = ((byte)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: speed of class  monster_race */
static int toluaI_get_monst_monster_race_speed(lua_State* tolua_S)
{
  monster_race* self = (monster_race*)  tolua_getusertype(tolua_S,1,0);
  if (!self) TOLUA_ERR_SELF;
  tolua_pushnumber(tolua_S,(long)self->speed);
 return 1;
}

/* set function: speed of class  monster_race */
static int toluaI_set_monst_monster_race_speed(lua_State* tolua_S)
{
  monster_race* self = (monster_race*)  tolua_getusertype(tolua_S,1,0);
  if (!self) TOLUA_ERR_SELF;
  if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
   TOLUA_ERR_ASSIGN;
  self->speed = ((byte)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: mexp of class  monster_race */
static int toluaI_get_monst_monster_race_mexp(lua_State* tolua_S)
{
  monster_race* self = (monster_race*)  tolua_getusertype(tolua_S,1,0);
  if (!self) TOLUA_ERR_SELF;
  tolua_pushnumber(tolua_S,(long)self->mexp);
 return 1;
}

/* set function: mexp of class  monster_race */
static int toluaI_set_monst_monster_race_mexp(lua_State* tolua_S)
{
  monster_race* self = (monster_race*)  tolua_getusertype(tolua_S,1,0);
  if (!self) TOLUA_ERR_SELF;
  if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
   TOLUA_ERR_ASSIGN;
  self->mexp = ((s32b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: extra of class  monster_race */
static int toluaI_get_monst_monster_race_extra(lua_State* tolua_S)
{
  monster_race* self = (monster_race*)  tolua_getusertype(tolua_S,1,0);
  if (!self) TOLUA_ERR_SELF;
  tolua_pushnumber(tolua_S,(long)self->extra);
 return 1;
}

/* set function: extra of class  monster_race */
static int toluaI_set_monst_monster_race_extra(lua_State* tolua_S)
{
  monster_race* self = (monster_race*)  tolua_getusertype(tolua_S,1,0);
  if (!self) TOLUA_ERR_SELF;
  if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
   TOLUA_ERR_ASSIGN;
  self->extra = ((s16b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: freq_inate of class  monster_race */
static int toluaI_get_monst_monster_race_freq_inate(lua_State* tolua_S)
{
  monster_race* self = (monster_race*)  tolua_getusertype(tolua_S,1,0);
  if (!self) TOLUA_ERR_SELF;
  tolua_pushnumber(tolua_S,(long)self->freq_inate);
 return 1;
}

/* set function: freq_inate of class  monster_race */
static int toluaI_set_monst_monster_race_freq_inate(lua_State* tolua_S)
{
  monster_race* self = (monster_race*)  tolua_getusertype(tolua_S,1,0);
  if (!self) TOLUA_ERR_SELF;
  if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
   TOLUA_ERR_ASSIGN;
  self->freq_inate = ((byte)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: freq_spell of class  monster_race */
static int toluaI_get_monst_monster_race_freq_spell(lua_State* tolua_S)
{
  monster_race* self = (monster_race*)  tolua_getusertype(tolua_S,1,0);
  if (!self) TOLUA_ERR_SELF;
  tolua_pushnumber(tolua_S,(long)self->freq_spell);
 return 1;
}

/* set function: freq_spell of class  monster_race */
static int toluaI_set_monst_monster_race_freq_spell(lua_State* tolua_S)
{
  monster_race* self = (monster_race*)  tolua_getusertype(tolua_S,1,0);
  if (!self) TOLUA_ERR_SELF;
  if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
   TOLUA_ERR_ASSIGN;
  self->freq_spell = ((byte)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: flags of class  monster_race */
static int toluaI_get_monst_monster_race_flags(lua_State* tolua_S)
{
 int toluaI_index;
  monster_race* self;
 lua_pushstring(tolua_S,".self");
 lua_rawget(tolua_S,1);
 self = (monster_race*)  lua_touserdata(tolua_S,-1);
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
  tolua_error(tolua_S,"invalid type in array indexing.");
 toluaI_index = (int)tolua_getnumber(tolua_S,2,0);
 if (toluaI_index<0 || toluaI_index>=9)
  tolua_error(tolua_S,"array monster_race: flags indexing out of range.");
 tolua_pushnumber(tolua_S,(long)self->flags[toluaI_index]);
 return 1;
}

/* set function: flags of class  monster_race */
static int toluaI_set_monst_monster_race_flags(lua_State* tolua_S)
{
 int toluaI_index;
  monster_race* self;
 lua_pushstring(tolua_S,".self");
 lua_rawget(tolua_S,1);
 self = (monster_race*)  lua_touserdata(tolua_S,-1);
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
  tolua_error(tolua_S,"invalid type in array indexing.");
 toluaI_index = (int)tolua_getnumber(tolua_S,2,0);
 if (toluaI_index<0 || toluaI_index>=9)
  tolua_error(tolua_S,"array monster_race: flags indexing out of range.");
  self->flags[toluaI_index] = ((u32b)  tolua_getnumber(tolua_S,3,0));
 return 0;
}

/* get function: blow of class  monster_race */
static int toluaI_get_monst_monster_race_blow(lua_State* tolua_S)
{
 int toluaI_index;
  monster_race* self;
 lua_pushstring(tolua_S,".self");
 lua_rawget(tolua_S,1);
 self = (monster_race*)  lua_touserdata(tolua_S,-1);
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
  tolua_error(tolua_S,"invalid type in array indexing.");
 toluaI_index = (int)tolua_getnumber(tolua_S,2,0);
 if (toluaI_index<0 || toluaI_index>=4)
  tolua_error(tolua_S,"array monster_race: blow indexing out of range.");
 tolua_pushusertype(tolua_S,(void*)&self->blow[toluaI_index],tolua_tag(tolua_S,"monster_blow"));
 return 1;
}

/* set function: blow of class  monster_race */
static int toluaI_set_monst_monster_race_blow(lua_State* tolua_S)
{
 int toluaI_index;
  monster_race* self;
 lua_pushstring(tolua_S,".self");
 lua_rawget(tolua_S,1);
 self = (monster_race*)  lua_touserdata(tolua_S,-1);
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
  tolua_error(tolua_S,"invalid type in array indexing.");
 toluaI_index = (int)tolua_getnumber(tolua_S,2,0);
 if (toluaI_index<0 || toluaI_index>=4)
  tolua_error(tolua_S,"array monster_race: blow indexing out of range.");
  self->blow[toluaI_index] = *((monster_blow*)  tolua_getusertype(tolua_S,3,0));
 return 0;
}

/* get function: level of class  monster_race */
static int toluaI_get_monst_monster_race_level(lua_State* tolua_S)
{
  monster_race* self = (monster_race*)  tolua_getusertype(tolua_S,1,0);
  if (!self) TOLUA_ERR_SELF;
  tolua_pushnumber(tolua_S,(long)self->level);
 return 1;
}

/* set function: level of class  monster_race */
static int toluaI_set_monst_monster_race_level(lua_State* tolua_S)
{
  monster_race* self = (monster_race*)  tolua_getusertype(tolua_S,1,0);
  if (!self) TOLUA_ERR_SELF;
  if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
   TOLUA_ERR_ASSIGN;
  self->level = ((byte)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: rarity of class  monster_race */
static int toluaI_get_monst_monster_race_rarity(lua_State* tolua_S)
{
  monster_race* self = (monster_race*)  tolua_getusertype(tolua_S,1,0);
  if (!self) TOLUA_ERR_SELF;
  tolua_pushnumber(tolua_S,(long)self->rarity);
 return 1;
}

/* set function: rarity of class  monster_race */
static int toluaI_set_monst_monster_race_rarity(lua_State* tolua_S)
{
  monster_race* self = (monster_race*)  tolua_getusertype(tolua_S,1,0);
  if (!self) TOLUA_ERR_SELF;
  if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
   TOLUA_ERR_ASSIGN;
  self->rarity = ((byte)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: d_attr of class  monster_race */
static int toluaI_get_monst_monster_race_d_attr(lua_State* tolua_S)
{
  monster_race* self = (monster_race*)  tolua_getusertype(tolua_S,1,0);
  if (!self) TOLUA_ERR_SELF;
  tolua_pushnumber(tolua_S,(long)self->d_attr);
 return 1;
}

/* set function: d_attr of class  monster_race */
static int toluaI_set_monst_monster_race_d_attr(lua_State* tolua_S)
{
  monster_race* self = (monster_race*)  tolua_getusertype(tolua_S,1,0);
  if (!self) TOLUA_ERR_SELF;
  if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
   TOLUA_ERR_ASSIGN;
  self->d_attr = ((byte)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: d_char of class  monster_race */
static int toluaI_get_monst_monster_race_d_char(lua_State* tolua_S)
{
  monster_race* self = (monster_race*)  tolua_getusertype(tolua_S,1,0);
  if (!self) TOLUA_ERR_SELF;
  tolua_pushnumber(tolua_S,(long)self->d_char);
 return 1;
}

/* set function: d_char of class  monster_race */
static int toluaI_set_monst_monster_race_d_char(lua_State* tolua_S)
{
  monster_race* self = (monster_race*)  tolua_getusertype(tolua_S,1,0);
  if (!self) TOLUA_ERR_SELF;
  if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
   TOLUA_ERR_ASSIGN;
  self->d_char = ((char)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: x_attr of class  monster_race */
static int toluaI_get_monst_monster_race_x_attr(lua_State* tolua_S)
{
  monster_race* self = (monster_race*)  tolua_getusertype(tolua_S,1,0);
  if (!self) TOLUA_ERR_SELF;
  tolua_pushnumber(tolua_S,(long)self->x_attr);
 return 1;
}

/* set function: x_attr of class  monster_race */
static int toluaI_set_monst_monster_race_x_attr(lua_State* tolua_S)
{
  monster_race* self = (monster_race*)  tolua_getusertype(tolua_S,1,0);
  if (!self) TOLUA_ERR_SELF;
  if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
   TOLUA_ERR_ASSIGN;
  self->x_attr = ((byte)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: x_char of class  monster_race */
static int toluaI_get_monst_monster_race_x_char(lua_State* tolua_S)
{
  monster_race* self = (monster_race*)  tolua_getusertype(tolua_S,1,0);
  if (!self) TOLUA_ERR_SELF;
  tolua_pushnumber(tolua_S,(long)self->x_char);
 return 1;
}

/* set function: x_char of class  monster_race */
static int toluaI_set_monst_monster_race_x_char(lua_State* tolua_S)
{
  monster_race* self = (monster_race*)  tolua_getusertype(tolua_S,1,0);
  if (!self) TOLUA_ERR_SELF;
  if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
   TOLUA_ERR_ASSIGN;
  self->x_char = ((char)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: max_num of class  monster_race */
static int toluaI_get_monst_monster_race_max_num(lua_State* tolua_S)
{
  monster_race* self = (monster_race*)  tolua_getusertype(tolua_S,1,0);
  if (!self) TOLUA_ERR_SELF;
  tolua_pushnumber(tolua_S,(long)self->max_num);
 return 1;
}

/* set function: max_num of class  monster_race */
static int toluaI_set_monst_monster_race_max_num(lua_State* tolua_S)
{
  monster_race* self = (monster_race*)  tolua_getusertype(tolua_S,1,0);
  if (!self) TOLUA_ERR_SELF;
  if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
   TOLUA_ERR_ASSIGN;
  self->max_num = ((byte)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: cur_num of class  monster_race */
static int toluaI_get_monst_monster_race_cur_num(lua_State* tolua_S)
{
  monster_race* self = (monster_race*)  tolua_getusertype(tolua_S,1,0);
  if (!self) TOLUA_ERR_SELF;
  tolua_pushnumber(tolua_S,(long)self->cur_num);
 return 1;
}

/* set function: cur_num of class  monster_race */
static int toluaI_set_monst_monster_race_cur_num(lua_State* tolua_S)
{
  monster_race* self = (monster_race*)  tolua_getusertype(tolua_S,1,0);
  if (!self) TOLUA_ERR_SELF;
  if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
   TOLUA_ERR_ASSIGN;
  self->cur_num = ((byte)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: r_sights of class  monster_race */
static int toluaI_get_monst_monster_race_r_sights(lua_State* tolua_S)
{
  monster_race* self = (monster_race*)  tolua_getusertype(tolua_S,1,0);
  if (!self) TOLUA_ERR_SELF;
  tolua_pushnumber(tolua_S,(long)self->r_sights);
 return 1;
}

/* set function: r_sights of class  monster_race */
static int toluaI_set_monst_monster_race_r_sights(lua_State* tolua_S)
{
  monster_race* self = (monster_race*)  tolua_getusertype(tolua_S,1,0);
  if (!self) TOLUA_ERR_SELF;
  if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
   TOLUA_ERR_ASSIGN;
  self->r_sights = ((s16b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: r_deaths of class  monster_race */
static int toluaI_get_monst_monster_race_r_deaths(lua_State* tolua_S)
{
  monster_race* self = (monster_race*)  tolua_getusertype(tolua_S,1,0);
  if (!self) TOLUA_ERR_SELF;
  tolua_pushnumber(tolua_S,(long)self->r_deaths);
 return 1;
}

/* set function: r_deaths of class  monster_race */
static int toluaI_set_monst_monster_race_r_deaths(lua_State* tolua_S)
{
  monster_race* self = (monster_race*)  tolua_getusertype(tolua_S,1,0);
  if (!self) TOLUA_ERR_SELF;
  if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
   TOLUA_ERR_ASSIGN;
  self->r_deaths = ((s16b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: r_pkills of class  monster_race */
static int toluaI_get_monst_monster_race_r_pkills(lua_State* tolua_S)
{
  monster_race* self = (monster_race*)  tolua_getusertype(tolua_S,1,0);
  if (!self) TOLUA_ERR_SELF;
  tolua_pushnumber(tolua_S,(long)self->r_pkills);
 return 1;
}

/* set function: r_pkills of class  monster_race */
static int toluaI_set_monst_monster_race_r_pkills(lua_State* tolua_S)
{
  monster_race* self = (monster_race*)  tolua_getusertype(tolua_S,1,0);
  if (!self) TOLUA_ERR_SELF;
  if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
   TOLUA_ERR_ASSIGN;
  self->r_pkills = ((s16b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: r_tkills of class  monster_race */
static int toluaI_get_monst_monster_race_r_tkills(lua_State* tolua_S)
{
  monster_race* self = (monster_race*)  tolua_getusertype(tolua_S,1,0);
  if (!self) TOLUA_ERR_SELF;
  tolua_pushnumber(tolua_S,(long)self->r_tkills);
 return 1;
}

/* set function: r_tkills of class  monster_race */
static int toluaI_set_monst_monster_race_r_tkills(lua_State* tolua_S)
{
  monster_race* self = (monster_race*)  tolua_getusertype(tolua_S,1,0);
  if (!self) TOLUA_ERR_SELF;
  if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
   TOLUA_ERR_ASSIGN;
  self->r_tkills = ((s16b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: r_wake of class  monster_race */
static int toluaI_get_monst_monster_race_r_wake(lua_State* tolua_S)
{
  monster_race* self = (monster_race*)  tolua_getusertype(tolua_S,1,0);
  if (!self) TOLUA_ERR_SELF;
  tolua_pushnumber(tolua_S,(long)self->r_wake);
 return 1;
}

/* set function: r_wake of class  monster_race */
static int toluaI_set_monst_monster_race_r_wake(lua_State* tolua_S)
{
  monster_race* self = (monster_race*)  tolua_getusertype(tolua_S,1,0);
  if (!self) TOLUA_ERR_SELF;
  if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
   TOLUA_ERR_ASSIGN;
  self->r_wake = ((byte)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: r_ignore of class  monster_race */
static int toluaI_get_monst_monster_race_r_ignore(lua_State* tolua_S)
{
  monster_race* self = (monster_race*)  tolua_getusertype(tolua_S,1,0);
  if (!self) TOLUA_ERR_SELF;
  tolua_pushnumber(tolua_S,(long)self->r_ignore);
 return 1;
}

/* set function: r_ignore of class  monster_race */
static int toluaI_set_monst_monster_race_r_ignore(lua_State* tolua_S)
{
  monster_race* self = (monster_race*)  tolua_getusertype(tolua_S,1,0);
  if (!self) TOLUA_ERR_SELF;
  if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
   TOLUA_ERR_ASSIGN;
  self->r_ignore = ((byte)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: r_xtra1 of class  monster_race */
static int toluaI_get_monst_monster_race_r_xtra1(lua_State* tolua_S)
{
  monster_race* self = (monster_race*)  tolua_getusertype(tolua_S,1,0);
  if (!self) TOLUA_ERR_SELF;
  tolua_pushnumber(tolua_S,(long)self->r_xtra1);
 return 1;
}

/* set function: r_xtra1 of class  monster_race */
static int toluaI_set_monst_monster_race_r_xtra1(lua_State* tolua_S)
{
  monster_race* self = (monster_race*)  tolua_getusertype(tolua_S,1,0);
  if (!self) TOLUA_ERR_SELF;
  if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
   TOLUA_ERR_ASSIGN;
  self->r_xtra1 = ((byte)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: r_xtra2 of class  monster_race */
static int toluaI_get_monst_monster_race_r_xtra2(lua_State* tolua_S)
{
  monster_race* self = (monster_race*)  tolua_getusertype(tolua_S,1,0);
  if (!self) TOLUA_ERR_SELF;
  tolua_pushnumber(tolua_S,(long)self->r_xtra2);
 return 1;
}

/* set function: r_xtra2 of class  monster_race */
static int toluaI_set_monst_monster_race_r_xtra2(lua_State* tolua_S)
{
  monster_race* self = (monster_race*)  tolua_getusertype(tolua_S,1,0);
  if (!self) TOLUA_ERR_SELF;
  if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
   TOLUA_ERR_ASSIGN;
  self->r_xtra2 = ((byte)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: r_drop_gold of class  monster_race */
static int toluaI_get_monst_monster_race_r_drop_gold(lua_State* tolua_S)
{
  monster_race* self = (monster_race*)  tolua_getusertype(tolua_S,1,0);
  if (!self) TOLUA_ERR_SELF;
  tolua_pushnumber(tolua_S,(long)self->r_drop_gold);
 return 1;
}

/* set function: r_drop_gold of class  monster_race */
static int toluaI_set_monst_monster_race_r_drop_gold(lua_State* tolua_S)
{
  monster_race* self = (monster_race*)  tolua_getusertype(tolua_S,1,0);
  if (!self) TOLUA_ERR_SELF;
  if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
   TOLUA_ERR_ASSIGN;
  self->r_drop_gold = ((byte)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: r_drop_item of class  monster_race */
static int toluaI_get_monst_monster_race_r_drop_item(lua_State* tolua_S)
{
  monster_race* self = (monster_race*)  tolua_getusertype(tolua_S,1,0);
  if (!self) TOLUA_ERR_SELF;
  tolua_pushnumber(tolua_S,(long)self->r_drop_item);
 return 1;
}

/* set function: r_drop_item of class  monster_race */
static int toluaI_set_monst_monster_race_r_drop_item(lua_State* tolua_S)
{
  monster_race* self = (monster_race*)  tolua_getusertype(tolua_S,1,0);
  if (!self) TOLUA_ERR_SELF;
  if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
   TOLUA_ERR_ASSIGN;
  self->r_drop_item = ((byte)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: r_cast_inate of class  monster_race */
static int toluaI_get_monst_monster_race_r_cast_inate(lua_State* tolua_S)
{
  monster_race* self = (monster_race*)  tolua_getusertype(tolua_S,1,0);
  if (!self) TOLUA_ERR_SELF;
  tolua_pushnumber(tolua_S,(long)self->r_cast_inate);
 return 1;
}

/* set function: r_cast_inate of class  monster_race */
static int toluaI_set_monst_monster_race_r_cast_inate(lua_State* tolua_S)
{
  monster_race* self = (monster_race*)  tolua_getusertype(tolua_S,1,0);
  if (!self) TOLUA_ERR_SELF;
  if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
   TOLUA_ERR_ASSIGN;
  self->r_cast_inate = ((byte)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: r_cast_spell of class  monster_race */
static int toluaI_get_monst_monster_race_r_cast_spell(lua_State* tolua_S)
{
  monster_race* self = (monster_race*)  tolua_getusertype(tolua_S,1,0);
  if (!self) TOLUA_ERR_SELF;
  tolua_pushnumber(tolua_S,(long)self->r_cast_spell);
 return 1;
}

/* set function: r_cast_spell of class  monster_race */
static int toluaI_set_monst_monster_race_r_cast_spell(lua_State* tolua_S)
{
  monster_race* self = (monster_race*)  tolua_getusertype(tolua_S,1,0);
  if (!self) TOLUA_ERR_SELF;
  if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
   TOLUA_ERR_ASSIGN;
  self->r_cast_spell = ((byte)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: r_blows of class  monster_race */
static int toluaI_get_monst_monster_race_r_blows(lua_State* tolua_S)
{
 int toluaI_index;
  monster_race* self;
 lua_pushstring(tolua_S,".self");
 lua_rawget(tolua_S,1);
 self = (monster_race*)  lua_touserdata(tolua_S,-1);
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
  tolua_error(tolua_S,"invalid type in array indexing.");
 toluaI_index = (int)tolua_getnumber(tolua_S,2,0);
 if (toluaI_index<0 || toluaI_index>=4)
  tolua_error(tolua_S,"array monster_race: r_blows indexing out of range.");
 tolua_pushnumber(tolua_S,(long)self->r_blows[toluaI_index]);
 return 1;
}

/* set function: r_blows of class  monster_race */
static int toluaI_set_monst_monster_race_r_blows(lua_State* tolua_S)
{
 int toluaI_index;
  monster_race* self;
 lua_pushstring(tolua_S,".self");
 lua_rawget(tolua_S,1);
 self = (monster_race*)  lua_touserdata(tolua_S,-1);
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
  tolua_error(tolua_S,"invalid type in array indexing.");
 toluaI_index = (int)tolua_getnumber(tolua_S,2,0);
 if (toluaI_index<0 || toluaI_index>=4)
  tolua_error(tolua_S,"array monster_race: r_blows indexing out of range.");
  self->r_blows[toluaI_index] = ((byte)  tolua_getnumber(tolua_S,3,0));
 return 0;
}

/* get function: r_flags of class  monster_race */
static int toluaI_get_monst_monster_race_r_flags(lua_State* tolua_S)
{
 int toluaI_index;
  monster_race* self;
 lua_pushstring(tolua_S,".self");
 lua_rawget(tolua_S,1);
 self = (monster_race*)  lua_touserdata(tolua_S,-1);
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
  tolua_error(tolua_S,"invalid type in array indexing.");
 toluaI_index = (int)tolua_getnumber(tolua_S,2,0);
 if (toluaI_index<0 || toluaI_index>=9)
  tolua_error(tolua_S,"array monster_race: r_flags indexing out of range.");
 tolua_pushnumber(tolua_S,(long)self->r_flags[toluaI_index]);
 return 1;
}

/* set function: r_flags of class  monster_race */
static int toluaI_set_monst_monster_race_r_flags(lua_State* tolua_S)
{
 int toluaI_index;
  monster_race* self;
 lua_pushstring(tolua_S,".self");
 lua_rawget(tolua_S,1);
 self = (monster_race*)  lua_touserdata(tolua_S,-1);
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
  tolua_error(tolua_S,"invalid type in array indexing.");
 toluaI_index = (int)tolua_getnumber(tolua_S,2,0);
 if (toluaI_index<0 || toluaI_index>=9)
  tolua_error(tolua_S,"array monster_race: r_flags indexing out of range.");
  self->r_flags[toluaI_index] = ((u32b)  tolua_getnumber(tolua_S,3,0));
 return 0;
}

/* get function: obj_drop of class  monster_race */
static int toluaI_get_monst_monster_race_obj_drop(lua_State* tolua_S)
{
  monster_race* self = (monster_race*)  tolua_getusertype(tolua_S,1,0);
  if (!self) TOLUA_ERR_SELF;
  tolua_pushusertype(tolua_S,(void*)&self->obj_drop,tolua_tag(tolua_S,"obj_theme"));
 return 1;
}

/* set function: obj_drop of class  monster_race */
static int toluaI_set_monst_monster_race_obj_drop(lua_State* tolua_S)
{
  monster_race* self = (monster_race*)  tolua_getusertype(tolua_S,1,0);
  if (!self) TOLUA_ERR_SELF;
  if (!tolua_istype(tolua_S,2,tolua_tag(tolua_S,"obj_theme"),0))
   TOLUA_ERR_ASSIGN;
  self->obj_drop = *((obj_theme*)  tolua_getusertype(tolua_S,2,0));
 return 0;
}

/* get function: r_see of class  monster_race */
static int toluaI_get_monst_monster_race_r_see(lua_State* tolua_S)
{
  monster_race* self = (monster_race*)  tolua_getusertype(tolua_S,1,0);
  if (!self) TOLUA_ERR_SELF;
  tolua_pushnumber(tolua_S,(long)self->r_see);
 return 1;
}

/* set function: r_see of class  monster_race */
static int toluaI_set_monst_monster_race_r_see(lua_State* tolua_S)
{
  monster_race* self = (monster_race*)  tolua_getusertype(tolua_S,1,0);
  if (!self) TOLUA_ERR_SELF;
  if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
   TOLUA_ERR_ASSIGN;
  self->r_see = ((u16b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: r_idx of class  monster_type */
static int toluaI_get_monst_monster_type_r_idx(lua_State* tolua_S)
{
  monster_type* self = (monster_type*)  tolua_getusertype(tolua_S,1,0);
  if (!self) TOLUA_ERR_SELF;
  tolua_pushnumber(tolua_S,(long)self->r_idx);
 return 1;
}

/* set function: r_idx of class  monster_type */
static int toluaI_set_monst_monster_type_r_idx(lua_State* tolua_S)
{
  monster_type* self = (monster_type*)  tolua_getusertype(tolua_S,1,0);
  if (!self) TOLUA_ERR_SELF;
  if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
   TOLUA_ERR_ASSIGN;
  self->r_idx = ((s16b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: fy of class  monster_type */
static int toluaI_get_monst_monster_type_fy(lua_State* tolua_S)
{
  monster_type* self = (monster_type*)  tolua_getusertype(tolua_S,1,0);
  if (!self) TOLUA_ERR_SELF;
  tolua_pushnumber(tolua_S,(long)self->fy);
 return 1;
}

/* set function: fy of class  monster_type */
static int toluaI_set_monst_monster_type_fy(lua_State* tolua_S)
{
  monster_type* self = (monster_type*)  tolua_getusertype(tolua_S,1,0);
  if (!self) TOLUA_ERR_SELF;
  if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
   TOLUA_ERR_ASSIGN;
  self->fy = ((s16b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: fx of class  monster_type */
static int toluaI_get_monst_monster_type_fx(lua_State* tolua_S)
{
  monster_type* self = (monster_type*)  tolua_getusertype(tolua_S,1,0);
  if (!self) TOLUA_ERR_SELF;
  tolua_pushnumber(tolua_S,(long)self->fx);
 return 1;
}

/* set function: fx of class  monster_type */
static int toluaI_set_monst_monster_type_fx(lua_State* tolua_S)
{
  monster_type* self = (monster_type*)  tolua_getusertype(tolua_S,1,0);
  if (!self) TOLUA_ERR_SELF;
  if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
   TOLUA_ERR_ASSIGN;
  self->fx = ((s16b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: hp of class  monster_type */
static int toluaI_get_monst_monster_type_hp(lua_State* tolua_S)
{
  monster_type* self = (monster_type*)  tolua_getusertype(tolua_S,1,0);
  if (!self) TOLUA_ERR_SELF;
  tolua_pushnumber(tolua_S,(long)self->hp);
 return 1;
}

/* set function: hp of class  monster_type */
static int toluaI_set_monst_monster_type_hp(lua_State* tolua_S)
{
  monster_type* self = (monster_type*)  tolua_getusertype(tolua_S,1,0);
  if (!self) TOLUA_ERR_SELF;
  if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
   TOLUA_ERR_ASSIGN;
  self->hp = ((s16b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: maxhp of class  monster_type */
static int toluaI_get_monst_monster_type_maxhp(lua_State* tolua_S)
{
  monster_type* self = (monster_type*)  tolua_getusertype(tolua_S,1,0);
  if (!self) TOLUA_ERR_SELF;
  tolua_pushnumber(tolua_S,(long)self->maxhp);
 return 1;
}

/* set function: maxhp of class  monster_type */
static int toluaI_set_monst_monster_type_maxhp(lua_State* tolua_S)
{
  monster_type* self = (monster_type*)  tolua_getusertype(tolua_S,1,0);
  if (!self) TOLUA_ERR_SELF;
  if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
   TOLUA_ERR_ASSIGN;
  self->maxhp = ((s16b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: csleep of class  monster_type */
static int toluaI_get_monst_monster_type_csleep(lua_State* tolua_S)
{
  monster_type* self = (monster_type*)  tolua_getusertype(tolua_S,1,0);
  if (!self) TOLUA_ERR_SELF;
  tolua_pushnumber(tolua_S,(long)self->csleep);
 return 1;
}

/* set function: csleep of class  monster_type */
static int toluaI_set_monst_monster_type_csleep(lua_State* tolua_S)
{
  monster_type* self = (monster_type*)  tolua_getusertype(tolua_S,1,0);
  if (!self) TOLUA_ERR_SELF;
  if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
   TOLUA_ERR_ASSIGN;
  self->csleep = ((s16b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: mspeed of class  monster_type */
static int toluaI_get_monst_monster_type_mspeed(lua_State* tolua_S)
{
  monster_type* self = (monster_type*)  tolua_getusertype(tolua_S,1,0);
  if (!self) TOLUA_ERR_SELF;
  tolua_pushnumber(tolua_S,(long)self->mspeed);
 return 1;
}

/* set function: mspeed of class  monster_type */
static int toluaI_set_monst_monster_type_mspeed(lua_State* tolua_S)
{
  monster_type* self = (monster_type*)  tolua_getusertype(tolua_S,1,0);
  if (!self) TOLUA_ERR_SELF;
  if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
   TOLUA_ERR_ASSIGN;
  self->mspeed = ((byte)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: energy of class  monster_type */
static int toluaI_get_monst_monster_type_energy(lua_State* tolua_S)
{
  monster_type* self = (monster_type*)  tolua_getusertype(tolua_S,1,0);
  if (!self) TOLUA_ERR_SELF;
  tolua_pushnumber(tolua_S,(long)self->energy);
 return 1;
}

/* set function: energy of class  monster_type */
static int toluaI_set_monst_monster_type_energy(lua_State* tolua_S)
{
  monster_type* self = (monster_type*)  tolua_getusertype(tolua_S,1,0);
  if (!self) TOLUA_ERR_SELF;
  if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
   TOLUA_ERR_ASSIGN;
  self->energy = ((byte)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: stunned of class  monster_type */
static int toluaI_get_monst_monster_type_stunned(lua_State* tolua_S)
{
  monster_type* self = (monster_type*)  tolua_getusertype(tolua_S,1,0);
  if (!self) TOLUA_ERR_SELF;
  tolua_pushnumber(tolua_S,(long)self->stunned);
 return 1;
}

/* set function: stunned of class  monster_type */
static int toluaI_set_monst_monster_type_stunned(lua_State* tolua_S)
{
  monster_type* self = (monster_type*)  tolua_getusertype(tolua_S,1,0);
  if (!self) TOLUA_ERR_SELF;
  if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
   TOLUA_ERR_ASSIGN;
  self->stunned = ((byte)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: confused of class  monster_type */
static int toluaI_get_monst_monster_type_confused(lua_State* tolua_S)
{
  monster_type* self = (monster_type*)  tolua_getusertype(tolua_S,1,0);
  if (!self) TOLUA_ERR_SELF;
  tolua_pushnumber(tolua_S,(long)self->confused);
 return 1;
}

/* set function: confused of class  monster_type */
static int toluaI_set_monst_monster_type_confused(lua_State* tolua_S)
{
  monster_type* self = (monster_type*)  tolua_getusertype(tolua_S,1,0);
  if (!self) TOLUA_ERR_SELF;
  if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
   TOLUA_ERR_ASSIGN;
  self->confused = ((byte)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: monfear of class  monster_type */
static int toluaI_get_monst_monster_type_monfear(lua_State* tolua_S)
{
  monster_type* self = (monster_type*)  tolua_getusertype(tolua_S,1,0);
  if (!self) TOLUA_ERR_SELF;
  tolua_pushnumber(tolua_S,(long)self->monfear);
 return 1;
}

/* set function: monfear of class  monster_type */
static int toluaI_set_monst_monster_type_monfear(lua_State* tolua_S)
{
  monster_type* self = (monster_type*)  tolua_getusertype(tolua_S,1,0);
  if (!self) TOLUA_ERR_SELF;
  if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
   TOLUA_ERR_ASSIGN;
  self->monfear = ((byte)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: invulner of class  monster_type */
static int toluaI_get_monst_monster_type_invulner(lua_State* tolua_S)
{
  monster_type* self = (monster_type*)  tolua_getusertype(tolua_S,1,0);
  if (!self) TOLUA_ERR_SELF;
  tolua_pushnumber(tolua_S,(long)self->invulner);
 return 1;
}

/* set function: invulner of class  monster_type */
static int toluaI_set_monst_monster_type_invulner(lua_State* tolua_S)
{
  monster_type* self = (monster_type*)  tolua_getusertype(tolua_S,1,0);
  if (!self) TOLUA_ERR_SELF;
  if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
   TOLUA_ERR_ASSIGN;
  self->invulner = ((byte)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: cdis of class  monster_type */
static int toluaI_get_monst_monster_type_cdis(lua_State* tolua_S)
{
  monster_type* self = (monster_type*)  tolua_getusertype(tolua_S,1,0);
  if (!self) TOLUA_ERR_SELF;
  tolua_pushnumber(tolua_S,(long)self->cdis);
 return 1;
}

/* set function: cdis of class  monster_type */
static int toluaI_set_monst_monster_type_cdis(lua_State* tolua_S)
{
  monster_type* self = (monster_type*)  tolua_getusertype(tolua_S,1,0);
  if (!self) TOLUA_ERR_SELF;
  if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
   TOLUA_ERR_ASSIGN;
  self->cdis = ((byte)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: mflag of class  monster_type */
static int toluaI_get_monst_monster_type_mflag(lua_State* tolua_S)
{
  monster_type* self = (monster_type*)  tolua_getusertype(tolua_S,1,0);
  if (!self) TOLUA_ERR_SELF;
  tolua_pushnumber(tolua_S,(long)self->mflag);
 return 1;
}

/* set function: mflag of class  monster_type */
static int toluaI_set_monst_monster_type_mflag(lua_State* tolua_S)
{
  monster_type* self = (monster_type*)  tolua_getusertype(tolua_S,1,0);
  if (!self) TOLUA_ERR_SELF;
  if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
   TOLUA_ERR_ASSIGN;
  self->mflag = ((byte)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: ml of class  monster_type */
static int toluaI_get_monst_monster_type_ml(lua_State* tolua_S)
{
  monster_type* self = (monster_type*)  tolua_getusertype(tolua_S,1,0);
  if (!self) TOLUA_ERR_SELF;
  tolua_pushbool(tolua_S,(int)self->ml);
 return 1;
}

/* set function: ml of class  monster_type */
static int toluaI_set_monst_monster_type_ml(lua_State* tolua_S)
{
  monster_type* self = (monster_type*)  tolua_getusertype(tolua_S,1,0);
  if (!self) TOLUA_ERR_SELF;
  if (!tolua_istype(tolua_S,2,tolua_tag(tolua_S,"bool"),0))
   TOLUA_ERR_ASSIGN;
  self->ml = ((bool)  tolua_getbool(tolua_S,2,0));
 return 0;
}

/* get function: hold_o_idx of class  monster_type */
static int toluaI_get_monst_monster_type_hold_o_idx(lua_State* tolua_S)
{
  monster_type* self = (monster_type*)  tolua_getusertype(tolua_S,1,0);
  if (!self) TOLUA_ERR_SELF;
  tolua_pushnumber(tolua_S,(long)self->hold_o_idx);
 return 1;
}

/* set function: hold_o_idx of class  monster_type */
static int toluaI_set_monst_monster_type_hold_o_idx(lua_State* tolua_S)
{
  monster_type* self = (monster_type*)  tolua_getusertype(tolua_S,1,0);
  if (!self) TOLUA_ERR_SELF;
  if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
   TOLUA_ERR_ASSIGN;
  self->hold_o_idx = ((s16b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: smart of class  monster_type */
static int toluaI_get_monst_monster_type_smart(lua_State* tolua_S)
{
  monster_type* self = (monster_type*)  tolua_getusertype(tolua_S,1,0);
  if (!self) TOLUA_ERR_SELF;
  tolua_pushnumber(tolua_S,(long)self->smart);
 return 1;
}

/* set function: smart of class  monster_type */
static int toluaI_set_monst_monster_type_smart(lua_State* tolua_S)
{
  monster_type* self = (monster_type*)  tolua_getusertype(tolua_S,1,0);
  if (!self) TOLUA_ERR_SELF;
  if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
   TOLUA_ERR_ASSIGN;
  self->smart = ((u32b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: m_max */
static int toluaI_get_monst_m_max(lua_State* tolua_S)
{
  tolua_pushnumber(tolua_S,(long)m_max);
 return 1;
}

/* set function: m_max */
static int toluaI_set_monst_m_max(lua_State* tolua_S)
{
  if (!tolua_istype(tolua_S,1,LUA_TNUMBER,0))
   TOLUA_ERR_ASSIGN;
  m_max = ((s16b)  tolua_getnumber(tolua_S,1,0));
 return 0;
}

/* get function: m_cnt */
static int toluaI_get_monst_m_cnt(lua_State* tolua_S)
{
  tolua_pushnumber(tolua_S,(long)m_cnt);
 return 1;
}

/* set function: m_cnt */
static int toluaI_set_monst_m_cnt(lua_State* tolua_S)
{
  if (!tolua_istype(tolua_S,1,LUA_TNUMBER,0))
   TOLUA_ERR_ASSIGN;
  m_cnt = ((s16b)  tolua_getnumber(tolua_S,1,0));
 return 0;
}

/* function: make_attack_normal */
static int toluaI_monst_make_attack_normal00(lua_State* tolua_S)
{
 if (
     !tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
     !tolua_isnoobj(tolua_S,2)
 )
 {
  tolua_error(tolua_S,"#ferror in function 'make_attack_normal'.");
  return 0;
 }
 else
 {
  int m_idx = ((int)  tolua_getnumber(tolua_S,1,0));
  {
   bool toluaI_ret = (bool)  make_attack_normal(m_idx);
   tolua_pushbool(tolua_S,(int)toluaI_ret);
  }
 }
 return 1;
}

/* function: make_attack_spell */
static int toluaI_monst_make_attack_spell00(lua_State* tolua_S)
{
 if (
     !tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
     !tolua_isnoobj(tolua_S,2)
 )
 {
  tolua_error(tolua_S,"#ferror in function 'make_attack_spell'.");
  return 0;
 }
 else
 {
  int m_idx = ((int)  tolua_getnumber(tolua_S,1,0));
  {
   bool toluaI_ret = (bool)  make_attack_spell(m_idx);
   tolua_pushbool(tolua_S,(int)toluaI_ret);
  }
 }
 return 1;
}

/* function: process_monsters */
static int toluaI_monst_process_monsters00(lua_State* tolua_S)
{
 if (
     !tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
     !tolua_isnoobj(tolua_S,2)
 )
 {
  tolua_error(tolua_S,"#ferror in function 'process_monsters'.");
  return 0;
 }
 else
 {
  int min_energy = ((int)  tolua_getnumber(tolua_S,1,0));
  {
   process_monsters(min_energy);
  }
 }
 return 0;
}

/* function: curse_equipment */
static int toluaI_monst_curse_equipment00(lua_State* tolua_S)
{
 if (
     !tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
     !tolua_istype(tolua_S,2,LUA_TNUMBER,0) ||
     !tolua_isnoobj(tolua_S,3)
 )
 {
  tolua_error(tolua_S,"#ferror in function 'curse_equipment'.");
  return 0;
 }
 else
 {
  int chance = ((int)  tolua_getnumber(tolua_S,1,0));
  int heavy_chance = ((int)  tolua_getnumber(tolua_S,2,0));
  {
   curse_equipment(chance,heavy_chance);
  }
 }
 return 0;
}

/* function: mon_take_hit_mon */
static int toluaI_monst_mon_take_hit_mon00(lua_State* tolua_S)
{
 if (
     !tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
     !tolua_istype(tolua_S,2,LUA_TNUMBER,0) ||
     !tolua_istype(tolua_S,3,tolua_tag(tolua_S,"bool"),0) ||
     !tolua_istype(tolua_S,4,LUA_TSTRING,0) ||
     !tolua_isnoobj(tolua_S,5)
 )
 {
  tolua_error(tolua_S,"#ferror in function 'mon_take_hit_mon'.");
  return 0;
 }
 else
 {
  int m_idx = ((int)  tolua_getnumber(tolua_S,1,0));
  int dam = ((int)  tolua_getnumber(tolua_S,2,0));
  bool fear = ((bool)  tolua_getbool(tolua_S,3,0));
  cptr note = ((cptr)  tolua_getstring(tolua_S,4,0));
  {
   mon_take_hit_mon(m_idx,dam,&fear,note);
   tolua_pushbool(tolua_S,(int)fear);
  }
 }
 return 1;
}

/* function: screen_roff_mon */
static int toluaI_monst_screen_roff_mon00(lua_State* tolua_S)
{
 if (
     !tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
     !tolua_istype(tolua_S,2,LUA_TNUMBER,0) ||
     !tolua_isnoobj(tolua_S,3)
 )
 {
  tolua_error(tolua_S,"#ferror in function 'screen_roff_mon'.");
  return 0;
 }
 else
 {
  int r_idx = ((int)  tolua_getnumber(tolua_S,1,0));
  int remember = ((int)  tolua_getnumber(tolua_S,2,0));
  {
   screen_roff_mon(r_idx,remember);
  }
 }
 return 0;
}

/* function: display_roff_mon */
static int toluaI_monst_display_roff_mon00(lua_State* tolua_S)
{
 if (
     !tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
     !tolua_isnoobj(tolua_S,2)
 )
 {
  tolua_error(tolua_S,"#ferror in function 'display_roff_mon'.");
  return 0;
 }
 else
 {
  int r_idx = ((int)  tolua_getnumber(tolua_S,1,0));
  {
   display_roff_mon(r_idx);
  }
 }
 return 0;
}

/* function: display_visible */
static int toluaI_monst_display_visible00(lua_State* tolua_S)
{
 if (
     !tolua_isnoobj(tolua_S,1)
 )
 {
  tolua_error(tolua_S,"#ferror in function 'display_visible'.");
  return 0;
 }
 else
 {
  {
   display_visible();
  }
 }
 return 0;
}

/* get function: horror_desc */
static int toluaI_get_monst_horror_desc(lua_State* tolua_S)
{
 int toluaI_index;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
  tolua_error(tolua_S,"invalid type in array indexing.");
 toluaI_index = (int)tolua_getnumber(tolua_S,2,0);
 if (toluaI_index<0 || toluaI_index>=MAX_SAN_HORROR)
  tolua_error(tolua_S,"array: horror_desc indexing out of range.");
 tolua_pushstring(tolua_S,(const char*)horror_desc[toluaI_index]);
 return 1;
}

/* set function: horror_desc */
static int toluaI_set_monst_horror_desc(lua_State* tolua_S)
{
 int toluaI_index;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
  tolua_error(tolua_S,"invalid type in array indexing.");
 toluaI_index = (int)tolua_getnumber(tolua_S,2,0);
 if (toluaI_index<0 || toluaI_index>=MAX_SAN_HORROR)
  tolua_error(tolua_S,"array: horror_desc indexing out of range.");
  horror_desc[toluaI_index] = ((cptr)  tolua_getstring(tolua_S,3,0));
 return 0;
}

/* get function: funny_desc */
static int toluaI_get_monst_funny_desc(lua_State* tolua_S)
{
 int toluaI_index;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
  tolua_error(tolua_S,"invalid type in array indexing.");
 toluaI_index = (int)tolua_getnumber(tolua_S,2,0);
 if (toluaI_index<0 || toluaI_index>=MAX_SAN_FUNNY)
  tolua_error(tolua_S,"array: funny_desc indexing out of range.");
 tolua_pushstring(tolua_S,(const char*)funny_desc[toluaI_index]);
 return 1;
}

/* set function: funny_desc */
static int toluaI_set_monst_funny_desc(lua_State* tolua_S)
{
 int toluaI_index;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
  tolua_error(tolua_S,"invalid type in array indexing.");
 toluaI_index = (int)tolua_getnumber(tolua_S,2,0);
 if (toluaI_index<0 || toluaI_index>=MAX_SAN_FUNNY)
  tolua_error(tolua_S,"array: funny_desc indexing out of range.");
  funny_desc[toluaI_index] = ((cptr)  tolua_getstring(tolua_S,3,0));
 return 0;
}

/* get function: funny_comments */
static int toluaI_get_monst_funny_comments(lua_State* tolua_S)
{
 int toluaI_index;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
  tolua_error(tolua_S,"invalid type in array indexing.");
 toluaI_index = (int)tolua_getnumber(tolua_S,2,0);
 if (toluaI_index<0 || toluaI_index>=MAX_SAN_COMMENT)
  tolua_error(tolua_S,"array: funny_comments indexing out of range.");
 tolua_pushstring(tolua_S,(const char*)funny_comments[toluaI_index]);
 return 1;
}

/* set function: funny_comments */
static int toluaI_set_monst_funny_comments(lua_State* tolua_S)
{
 int toluaI_index;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
  tolua_error(tolua_S,"invalid type in array indexing.");
 toluaI_index = (int)tolua_getnumber(tolua_S,2,0);
 if (toluaI_index<0 || toluaI_index>=MAX_SAN_COMMENT)
  tolua_error(tolua_S,"array: funny_comments indexing out of range.");
  funny_comments[toluaI_index] = ((cptr)  tolua_getstring(tolua_S,3,0));
 return 0;
}

/* function: delete_monster_idx */
static int toluaI_monst_delete_monster_idx00(lua_State* tolua_S)
{
 if (
     !tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
     !tolua_isnoobj(tolua_S,2)
 )
 {
  tolua_error(tolua_S,"#ferror in function 'delete_monster_idx'.");
  return 0;
 }
 else
 {
  int i = ((int)  tolua_getnumber(tolua_S,1,0));
  {
   delete_monster_idx(i);
  }
 }
 return 0;
}

/* function: delete_monster */
static int toluaI_monst_delete_monster00(lua_State* tolua_S)
{
 if (
     !tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
     !tolua_istype(tolua_S,2,LUA_TNUMBER,0) ||
     !tolua_isnoobj(tolua_S,3)
 )
 {
  tolua_error(tolua_S,"#ferror in function 'delete_monster'.");
  return 0;
 }
 else
 {
  int x = ((int)  tolua_getnumber(tolua_S,1,0));
  int y = ((int)  tolua_getnumber(tolua_S,2,0));
  {
   delete_monster(x,y);
  }
 }
 return 0;
}

/* function: monster_desc */
static int toluaI_monst_monster_desc00(lua_State* tolua_S)
{
 if (
     !tolua_istype(tolua_S,1,LUA_TSTRING,0) ||
     !tolua_istype(tolua_S,2,tolua_tag(tolua_S,"const monster_type"),0) ||
     !tolua_istype(tolua_S,3,LUA_TNUMBER,0) ||
     !tolua_istype(tolua_S,4,LUA_TNUMBER,0) ||
     !tolua_isnoobj(tolua_S,5)
 )
 {
  tolua_error(tolua_S,"#ferror in function 'monster_desc'.");
  return 0;
 }
 else
 {
  char* desc = ((char*)  tolua_getstring(tolua_S,1,0));
  const monster_type* m_ptr = ((const monster_type*)  tolua_getusertype(tolua_S,2,0));
  int mode = ((int)  tolua_getnumber(tolua_S,3,0));
  int max = ((int)  tolua_getnumber(tolua_S,4,0));
  {
   monster_desc(desc,m_ptr,mode,max);
  }
 }
 return 0;
}

/* function: lore_do_probe */
static int toluaI_monst_lore_do_probe00(lua_State* tolua_S)
{
 if (
     !tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
     !tolua_isnoobj(tolua_S,2)
 )
 {
  tolua_error(tolua_S,"#ferror in function 'lore_do_probe'.");
  return 0;
 }
 else
 {
  int m_idx = ((int)  tolua_getnumber(tolua_S,1,0));
  {
   lore_do_probe(m_idx);
  }
 }
 return 0;
}

/* function: lore_treasure */
static int toluaI_monst_lore_treasure00(lua_State* tolua_S)
{
 if (
     !tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
     !tolua_istype(tolua_S,2,LUA_TNUMBER,0) ||
     !tolua_istype(tolua_S,3,LUA_TNUMBER,0) ||
     !tolua_isnoobj(tolua_S,4)
 )
 {
  tolua_error(tolua_S,"#ferror in function 'lore_treasure'.");
  return 0;
 }
 else
 {
  int m_idx = ((int)  tolua_getnumber(tolua_S,1,0));
  int num_item = ((int)  tolua_getnumber(tolua_S,2,0));
  int num_gold = ((int)  tolua_getnumber(tolua_S,3,0));
  {
   lore_treasure(m_idx,num_item,num_gold);
  }
 }
 return 0;
}

/* function: update_mon_vis */
static int toluaI_monst_update_mon_vis00(lua_State* tolua_S)
{
 if (
     !tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
     !tolua_istype(tolua_S,2,LUA_TNUMBER,0) ||
     !tolua_isnoobj(tolua_S,3)
 )
 {
  tolua_error(tolua_S,"#ferror in function 'update_mon_vis'.");
  return 0;
 }
 else
 {
  u16b r_idx = ((u16b)  tolua_getnumber(tolua_S,1,0));
  int increment = ((int)  tolua_getnumber(tolua_S,2,0));
  {
   update_mon_vis(r_idx,increment);
  }
 }
 return 0;
}

/* function: update_mon */
static int toluaI_monst_update_mon00(lua_State* tolua_S)
{
 if (
     !tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
     !tolua_istype(tolua_S,2,tolua_tag(tolua_S,"bool"),0) ||
     !tolua_isnoobj(tolua_S,3)
 )
 {
  tolua_error(tolua_S,"#ferror in function 'update_mon'.");
  return 0;
 }
 else
 {
  int m_idx = ((int)  tolua_getnumber(tolua_S,1,0));
  bool full = ((bool)  tolua_getbool(tolua_S,2,0));
  {
   update_mon(m_idx,full);
  }
 }
 return 0;
}

/* function: place_monster */
static int toluaI_monst_place_monster00(lua_State* tolua_S)
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
  tolua_error(tolua_S,"#ferror in function 'place_monster'.");
  return 0;
 }
 else
 {
  int x = ((int)  tolua_getnumber(tolua_S,1,0));
  int y = ((int)  tolua_getnumber(tolua_S,2,0));
  bool slp = ((bool)  tolua_getbool(tolua_S,3,0));
  bool grp = ((bool)  tolua_getbool(tolua_S,4,0));
  int deltalevel = ((int)  tolua_getnumber(tolua_S,5,0));
  {
   bool toluaI_ret = (bool)  place_monster(x,y,slp,grp,deltalevel);
   tolua_pushbool(tolua_S,(int)toluaI_ret);
  }
 }
 return 1;
}

/* function: alloc_horde */
static int toluaI_monst_alloc_horde00(lua_State* tolua_S)
{
 if (
     !tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
     !tolua_istype(tolua_S,2,LUA_TNUMBER,0) ||
     !tolua_isnoobj(tolua_S,3)
 )
 {
  tolua_error(tolua_S,"#ferror in function 'alloc_horde'.");
  return 0;
 }
 else
 {
  int x = ((int)  tolua_getnumber(tolua_S,1,0));
  int y = ((int)  tolua_getnumber(tolua_S,2,0));
  {
   bool toluaI_ret = (bool)  alloc_horde(x,y);
   tolua_pushbool(tolua_S,(int)toluaI_ret);
  }
 }
 return 1;
}

/* function: summon_specific */
static int toluaI_monst_summon_specific00(lua_State* tolua_S)
{
 if (
     !tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
     !tolua_istype(tolua_S,2,LUA_TNUMBER,0) ||
     !tolua_istype(tolua_S,3,LUA_TNUMBER,0) ||
     !tolua_istype(tolua_S,4,LUA_TNUMBER,0) ||
     !tolua_istype(tolua_S,5,LUA_TNUMBER,0) ||
     !tolua_istype(tolua_S,6,tolua_tag(tolua_S,"bool"),0) ||
     !tolua_istype(tolua_S,7,tolua_tag(tolua_S,"bool"),0) ||
     !tolua_istype(tolua_S,8,tolua_tag(tolua_S,"bool"),0) ||
     !tolua_isnoobj(tolua_S,9)
 )
 {
  tolua_error(tolua_S,"#ferror in function 'summon_specific'.");
  return 0;
 }
 else
 {
  int who = ((int)  tolua_getnumber(tolua_S,1,0));
  int x1 = ((int)  tolua_getnumber(tolua_S,2,0));
  int y1 = ((int)  tolua_getnumber(tolua_S,3,0));
  int lev = ((int)  tolua_getnumber(tolua_S,4,0));
  int type = ((int)  tolua_getnumber(tolua_S,5,0));
  bool group = ((bool)  tolua_getbool(tolua_S,6,0));
  bool friendly = ((bool)  tolua_getbool(tolua_S,7,0));
  bool pet = ((bool)  tolua_getbool(tolua_S,8,0));
  {
   bool toluaI_ret = (bool)  summon_specific(who,x1,y1,lev,type,group,friendly,pet);
   tolua_pushbool(tolua_S,(int)toluaI_ret);
  }
 }
 return 1;
}

/* function: summon_named_creature */
static int toluaI_monst_summon_named_creature00(lua_State* tolua_S)
{
 if (
     !tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
     !tolua_istype(tolua_S,2,LUA_TNUMBER,0) ||
     !tolua_istype(tolua_S,3,LUA_TNUMBER,0) ||
     !tolua_istype(tolua_S,4,tolua_tag(tolua_S,"bool"),0) ||
     !tolua_istype(tolua_S,5,tolua_tag(tolua_S,"bool"),0) ||
     !tolua_istype(tolua_S,6,tolua_tag(tolua_S,"bool"),0) ||
     !tolua_isnoobj(tolua_S,7)
 )
 {
  tolua_error(tolua_S,"#ferror in function 'summon_named_creature'.");
  return 0;
 }
 else
 {
  int x1 = ((int)  tolua_getnumber(tolua_S,1,0));
  int y1 = ((int)  tolua_getnumber(tolua_S,2,0));
  int r_idx = ((int)  tolua_getnumber(tolua_S,3,0));
  bool slp = ((bool)  tolua_getbool(tolua_S,4,0));
  bool group_ok = ((bool)  tolua_getbool(tolua_S,5,0));
  bool pet = ((bool)  tolua_getbool(tolua_S,6,0));
  {
   monster_type* toluaI_ret = (monster_type*)  summon_named_creature(x1,y1,r_idx,slp,group_ok,pet);
   tolua_pushusertype(tolua_S,(void*)toluaI_ret,tolua_tag(tolua_S,"monster_type"));
  }
 }
 return 1;
}

/* function: multiply_monster */
static int toluaI_monst_multiply_monster00(lua_State* tolua_S)
{
 if (
     !tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
     !tolua_istype(tolua_S,2,tolua_tag(tolua_S,"bool"),0) ||
     !tolua_istype(tolua_S,3,tolua_tag(tolua_S,"bool"),0) ||
     !tolua_istype(tolua_S,4,tolua_tag(tolua_S,"bool"),0) ||
     !tolua_isnoobj(tolua_S,5)
 )
 {
  tolua_error(tolua_S,"#ferror in function 'multiply_monster'.");
  return 0;
 }
 else
 {
  int m_idx = ((int)  tolua_getnumber(tolua_S,1,0));
  bool clone = ((bool)  tolua_getbool(tolua_S,2,0));
  bool friendly = ((bool)  tolua_getbool(tolua_S,3,0));
  bool pet = ((bool)  tolua_getbool(tolua_S,4,0));
  {
   monster_type* toluaI_ret = (monster_type*)  multiply_monster(m_idx,clone,friendly,pet);
   tolua_pushusertype(tolua_S,(void*)toluaI_ret,tolua_tag(tolua_S,"monster_type"));
  }
 }
 return 1;
}

/* function: update_smart_learn */
static int toluaI_monst_update_smart_learn00(lua_State* tolua_S)
{
 if (
     !tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
     !tolua_istype(tolua_S,2,LUA_TNUMBER,0) ||
     !tolua_isnoobj(tolua_S,3)
 )
 {
  tolua_error(tolua_S,"#ferror in function 'update_smart_learn'.");
  return 0;
 }
 else
 {
  int m_idx = ((int)  tolua_getnumber(tolua_S,1,0));
  int what = ((int)  tolua_getnumber(tolua_S,2,0));
  {
   update_smart_learn(m_idx,what);
  }
 }
 return 0;
}

/* function: place_monster_one */
static int toluaI_monst_place_monster_one00(lua_State* tolua_S)
{
 if (
     !tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
     !tolua_istype(tolua_S,2,LUA_TNUMBER,0) ||
     !tolua_istype(tolua_S,3,LUA_TNUMBER,0) ||
     !tolua_istype(tolua_S,4,tolua_tag(tolua_S,"bool"),0) ||
     !tolua_istype(tolua_S,5,tolua_tag(tolua_S,"bool"),0) ||
     !tolua_istype(tolua_S,6,tolua_tag(tolua_S,"bool"),0) ||
     !tolua_isnoobj(tolua_S,7)
 )
 {
  tolua_error(tolua_S,"#ferror in function 'place_monster_one'.");
  return 0;
 }
 else
 {
  int x = ((int)  tolua_getnumber(tolua_S,1,0));
  int y = ((int)  tolua_getnumber(tolua_S,2,0));
  int r_idx = ((int)  tolua_getnumber(tolua_S,3,0));
  bool slp = ((bool)  tolua_getbool(tolua_S,4,0));
  bool friendly = ((bool)  tolua_getbool(tolua_S,5,0));
  bool pet = ((bool)  tolua_getbool(tolua_S,6,0));
  {
   monster_type* toluaI_ret = (monster_type*)  place_monster_one(x,y,r_idx,slp,friendly,pet);
   tolua_pushusertype(tolua_S,(void*)toluaI_ret,tolua_tag(tolua_S,"monster_type"));
  }
 }
 return 1;
}

/* function: set_friendly */
static int toluaI_monst_set_friendly00(lua_State* tolua_S)
{
 if (
     !tolua_istype(tolua_S,1,tolua_tag(tolua_S,"monster_type"),0) ||
     !tolua_isnoobj(tolua_S,2)
 )
 {
  tolua_error(tolua_S,"#ferror in function 'set_friendly'.");
  return 0;
 }
 else
 {
  monster_type* m_ptr = ((monster_type*)  tolua_getusertype(tolua_S,1,0));
  {
   set_friendly(m_ptr);
  }
 }
 return 0;
}

/* function: set_pet */
static int toluaI_monst_set_pet00(lua_State* tolua_S)
{
 if (
     !tolua_istype(tolua_S,1,tolua_tag(tolua_S,"monster_type"),0) ||
     !tolua_isnoobj(tolua_S,2)
 )
 {
  tolua_error(tolua_S,"#ferror in function 'set_pet'.");
  return 0;
 }
 else
 {
  monster_type* m_ptr = ((monster_type*)  tolua_getusertype(tolua_S,1,0));
  {
   set_pet(m_ptr);
  }
 }
 return 0;
}

/* function: set_hostile */
static int toluaI_monst_set_hostile00(lua_State* tolua_S)
{
 if (
     !tolua_istype(tolua_S,1,tolua_tag(tolua_S,"monster_type"),0) ||
     !tolua_isnoobj(tolua_S,2)
 )
 {
  tolua_error(tolua_S,"#ferror in function 'set_hostile'.");
  return 0;
 }
 else
 {
  monster_type* m_ptr = ((monster_type*)  tolua_getusertype(tolua_S,1,0));
  {
   set_hostile(m_ptr);
  }
 }
 return 0;
}

/* function: anger_monster */
static int toluaI_monst_anger_monster00(lua_State* tolua_S)
{
 if (
     !tolua_istype(tolua_S,1,tolua_tag(tolua_S,"monster_type"),0) ||
     !tolua_isnoobj(tolua_S,2)
 )
 {
  tolua_error(tolua_S,"#ferror in function 'anger_monster'.");
  return 0;
 }
 else
 {
  monster_type* m_ptr = ((monster_type*)  tolua_getusertype(tolua_S,1,0));
  {
   anger_monster(m_ptr);
  }
 }
 return 0;
}

/* function: are_enemies */
static int toluaI_monst_are_enemies00(lua_State* tolua_S)
{
 if (
     !tolua_istype(tolua_S,1,tolua_tag(tolua_S,"const monster_type"),0) ||
     !tolua_istype(tolua_S,2,tolua_tag(tolua_S,"const monster_type"),0) ||
     !tolua_isnoobj(tolua_S,3)
 )
 {
  tolua_error(tolua_S,"#ferror in function 'are_enemies'.");
  return 0;
 }
 else
 {
  const monster_type* m_ptr1 = ((const monster_type*)  tolua_getusertype(tolua_S,1,0));
  const monster_type* m_ptr2 = ((const monster_type*)  tolua_getusertype(tolua_S,2,0));
  {
   bool toluaI_ret = (bool)  are_enemies(m_ptr1,m_ptr2);
   tolua_pushbool(tolua_S,(int)toluaI_ret);
  }
 }
 return 1;
}

/* function: monster_living */
static int toluaI_monst_monster_living00(lua_State* tolua_S)
{
 if (
     !tolua_istype(tolua_S,1,tolua_tag(tolua_S,"const monster_race"),0) ||
     !tolua_isnoobj(tolua_S,2)
 )
 {
  tolua_error(tolua_S,"#ferror in function 'monster_living'.");
  return 0;
 }
 else
 {
  const monster_race* r_ptr = ((const monster_race*)  tolua_getusertype(tolua_S,1,0));
  {
   bool toluaI_ret = (bool)  monster_living(r_ptr);
   tolua_pushbool(tolua_S,(int)toluaI_ret);
  }
 }
 return 1;
}

/* function: monster_death */
static int toluaI_monst_monster_death00(lua_State* tolua_S)
{
 if (
     !tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
     !tolua_istype(tolua_S,2,tolua_tag(tolua_S,"bool"),0) ||
     !tolua_isnoobj(tolua_S,3)
 )
 {
  tolua_error(tolua_S,"#ferror in function 'monster_death'.");
  return 0;
 }
 else
 {
  int m_idx = ((int)  tolua_getnumber(tolua_S,1,0));
  bool explode = ((bool)  tolua_getbool(tolua_S,2,0));
  {
   bool toluaI_ret = (bool)  monster_death(m_idx,explode);
   tolua_pushbool(tolua_S,(int)toluaI_ret);
  }
 }
 return 1;
}

/* function: mon_take_hit */
static int toluaI_monst_mon_take_hit00(lua_State* tolua_S)
{
 if (
     !tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
     !tolua_istype(tolua_S,2,LUA_TNUMBER,0) ||
     !tolua_istype(tolua_S,3,tolua_tag(tolua_S,"bool"),0) ||
     !tolua_istype(tolua_S,4,LUA_TSTRING,0) ||
     !tolua_isnoobj(tolua_S,5)
 )
 {
  tolua_error(tolua_S,"#ferror in function 'mon_take_hit'.");
  return 0;
 }
 else
 {
  int m_idx = ((int)  tolua_getnumber(tolua_S,1,0));
  int dam = ((int)  tolua_getnumber(tolua_S,2,0));
  bool fear = ((bool)  tolua_getbool(tolua_S,3,0));
  cptr note = ((cptr)  tolua_getstring(tolua_S,4,0));
  {
   bool toluaI_ret = (bool)  mon_take_hit(m_idx,dam,&fear,note);
   tolua_pushbool(tolua_S,(int)toluaI_ret);
   tolua_pushbool(tolua_S,(int)fear);
  }
 }
 return 2;
}

/* Open function */
int tolua_monst_open (lua_State* tolua_S)
{
 tolua_open(tolua_S);
 toluaI_reg_types(tolua_S);
 tolua_constant(tolua_S,NULL,"RBM_HIT",RBM_HIT);
 tolua_constant(tolua_S,NULL,"RBM_TOUCH",RBM_TOUCH);
 tolua_constant(tolua_S,NULL,"RBM_PUNCH",RBM_PUNCH);
 tolua_constant(tolua_S,NULL,"RBM_KICK",RBM_KICK);
 tolua_constant(tolua_S,NULL,"RBM_CLAW",RBM_CLAW);
 tolua_constant(tolua_S,NULL,"RBM_BITE",RBM_BITE);
 tolua_constant(tolua_S,NULL,"RBM_STING",RBM_STING);
 tolua_constant(tolua_S,NULL,"RBM_XXX1",RBM_XXX1);
 tolua_constant(tolua_S,NULL,"RBM_BUTT",RBM_BUTT);
 tolua_constant(tolua_S,NULL,"RBM_CRUSH",RBM_CRUSH);
 tolua_constant(tolua_S,NULL,"RBM_ENGULF",RBM_ENGULF);
 tolua_constant(tolua_S,NULL,"RBM_CHARGE",RBM_CHARGE);
 tolua_constant(tolua_S,NULL,"RBM_CRAWL",RBM_CRAWL);
 tolua_constant(tolua_S,NULL,"RBM_DROOL",RBM_DROOL);
 tolua_constant(tolua_S,NULL,"RBM_SPIT",RBM_SPIT);
 tolua_constant(tolua_S,NULL,"RBM_EXPLODE",RBM_EXPLODE);
 tolua_constant(tolua_S,NULL,"RBM_GAZE",RBM_GAZE);
 tolua_constant(tolua_S,NULL,"RBM_WAIL",RBM_WAIL);
 tolua_constant(tolua_S,NULL,"RBM_SPORE",RBM_SPORE);
 tolua_constant(tolua_S,NULL,"RBM_XXX4",RBM_XXX4);
 tolua_constant(tolua_S,NULL,"RBM_BEG",RBM_BEG);
 tolua_constant(tolua_S,NULL,"RBM_INSULT",RBM_INSULT);
 tolua_constant(tolua_S,NULL,"RBM_MOAN",RBM_MOAN);
 tolua_constant(tolua_S,NULL,"RBM_SHOW",RBM_SHOW);
 tolua_constant(tolua_S,NULL,"RBE_HURT",RBE_HURT);
 tolua_constant(tolua_S,NULL,"RBE_POISON",RBE_POISON);
 tolua_constant(tolua_S,NULL,"RBE_UN_BONUS",RBE_UN_BONUS);
 tolua_constant(tolua_S,NULL,"RBE_UN_POWER",RBE_UN_POWER);
 tolua_constant(tolua_S,NULL,"RBE_EAT_GOLD",RBE_EAT_GOLD);
 tolua_constant(tolua_S,NULL,"RBE_EAT_ITEM",RBE_EAT_ITEM);
 tolua_constant(tolua_S,NULL,"RBE_EAT_FOOD",RBE_EAT_FOOD);
 tolua_constant(tolua_S,NULL,"RBE_EAT_LITE",RBE_EAT_LITE);
 tolua_constant(tolua_S,NULL,"RBE_ACID",RBE_ACID);
 tolua_constant(tolua_S,NULL,"RBE_ELEC",RBE_ELEC);
 tolua_constant(tolua_S,NULL,"RBE_FIRE",RBE_FIRE);
 tolua_constant(tolua_S,NULL,"RBE_COLD",RBE_COLD);
 tolua_constant(tolua_S,NULL,"RBE_BLIND",RBE_BLIND);
 tolua_constant(tolua_S,NULL,"RBE_CONFUSE",RBE_CONFUSE);
 tolua_constant(tolua_S,NULL,"RBE_TERRIFY",RBE_TERRIFY);
 tolua_constant(tolua_S,NULL,"RBE_PARALYZE",RBE_PARALYZE);
 tolua_constant(tolua_S,NULL,"RBE_LOSE_STR",RBE_LOSE_STR);
 tolua_constant(tolua_S,NULL,"RBE_LOSE_INT",RBE_LOSE_INT);
 tolua_constant(tolua_S,NULL,"RBE_LOSE_WIS",RBE_LOSE_WIS);
 tolua_constant(tolua_S,NULL,"RBE_LOSE_DEX",RBE_LOSE_DEX);
 tolua_constant(tolua_S,NULL,"RBE_LOSE_CON",RBE_LOSE_CON);
 tolua_constant(tolua_S,NULL,"RBE_LOSE_CHR",RBE_LOSE_CHR);
 tolua_constant(tolua_S,NULL,"RBE_LOSE_ALL",RBE_LOSE_ALL);
 tolua_constant(tolua_S,NULL,"RBE_SHATTER",RBE_SHATTER);
 tolua_constant(tolua_S,NULL,"RBE_EXP_10",RBE_EXP_10);
 tolua_constant(tolua_S,NULL,"RBE_EXP_20",RBE_EXP_20);
 tolua_constant(tolua_S,NULL,"RBE_EXP_40",RBE_EXP_40);
 tolua_constant(tolua_S,NULL,"RBE_EXP_80",RBE_EXP_80);
 tolua_constant(tolua_S,NULL,"RBE_DISEASE",RBE_DISEASE);
 tolua_constant(tolua_S,NULL,"RBE_TIME",RBE_TIME);
 tolua_constant(tolua_S,NULL,"RBE_EXP_VAMP",RBE_EXP_VAMP);
 tolua_constant(tolua_S,NULL,"SM_RES_ACID",SM_RES_ACID);
 tolua_constant(tolua_S,NULL,"SM_RES_ELEC",SM_RES_ELEC);
 tolua_constant(tolua_S,NULL,"SM_RES_FIRE",SM_RES_FIRE);
 tolua_constant(tolua_S,NULL,"SM_RES_COLD",SM_RES_COLD);
 tolua_constant(tolua_S,NULL,"SM_RES_POIS",SM_RES_POIS);
 tolua_constant(tolua_S,NULL,"SM_RES_NETH",SM_RES_NETH);
 tolua_constant(tolua_S,NULL,"SM_RES_LITE",SM_RES_LITE);
 tolua_constant(tolua_S,NULL,"SM_RES_DARK",SM_RES_DARK);
 tolua_constant(tolua_S,NULL,"SM_RES_FEAR",SM_RES_FEAR);
 tolua_constant(tolua_S,NULL,"SM_RES_CONF",SM_RES_CONF);
 tolua_constant(tolua_S,NULL,"SM_RES_CHAOS",SM_RES_CHAOS);
 tolua_constant(tolua_S,NULL,"SM_RES_DISEN",SM_RES_DISEN);
 tolua_constant(tolua_S,NULL,"SM_RES_BLIND",SM_RES_BLIND);
 tolua_constant(tolua_S,NULL,"SM_RES_NEXUS",SM_RES_NEXUS);
 tolua_constant(tolua_S,NULL,"SM_RES_SOUND",SM_RES_SOUND);
 tolua_constant(tolua_S,NULL,"SM_RES_SHARD",SM_RES_SHARD);
 tolua_constant(tolua_S,NULL,"SM_OPP_ACID",SM_OPP_ACID);
 tolua_constant(tolua_S,NULL,"SM_OPP_ELEC",SM_OPP_ELEC);
 tolua_constant(tolua_S,NULL,"SM_OPP_FIRE",SM_OPP_FIRE);
 tolua_constant(tolua_S,NULL,"SM_OPP_COLD",SM_OPP_COLD);
 tolua_constant(tolua_S,NULL,"SM_OPP_POIS",SM_OPP_POIS);
 tolua_constant(tolua_S,NULL,"SM_MIMIC",SM_MIMIC);
 tolua_constant(tolua_S,NULL,"SM_CLONED",SM_CLONED);
 tolua_constant(tolua_S,NULL,"SM_PET",SM_PET);
 tolua_constant(tolua_S,NULL,"SM_IMM_ACID",SM_IMM_ACID);
 tolua_constant(tolua_S,NULL,"SM_IMM_ELEC",SM_IMM_ELEC);
 tolua_constant(tolua_S,NULL,"SM_IMM_FIRE",SM_IMM_FIRE);
 tolua_constant(tolua_S,NULL,"SM_IMM_COLD",SM_IMM_COLD);
 tolua_constant(tolua_S,NULL,"SM_FRIENDLY",SM_FRIENDLY);
 tolua_constant(tolua_S,NULL,"SM_IMM_REFLECT",SM_IMM_REFLECT);
 tolua_constant(tolua_S,NULL,"SM_IMM_FREE",SM_IMM_FREE);
 tolua_constant(tolua_S,NULL,"SM_IMM_MANA",SM_IMM_MANA);
 tolua_constant(tolua_S,NULL,"MFLAG_VIEW",MFLAG_VIEW);
 tolua_constant(tolua_S,NULL,"MFLAG_TEMP",MFLAG_TEMP);
 tolua_constant(tolua_S,NULL,"MFLAG_XXX2",MFLAG_XXX2);
 tolua_constant(tolua_S,NULL,"MFLAG_XXX3",MFLAG_XXX3);
 tolua_constant(tolua_S,NULL,"MFLAG_MOVE",MFLAG_MOVE);
 tolua_constant(tolua_S,NULL,"MFLAG_NICE",MFLAG_NICE);
 tolua_constant(tolua_S,NULL,"MFLAG_SHOW",MFLAG_SHOW);
 tolua_constant(tolua_S,NULL,"MFLAG_MARK",MFLAG_MARK);
 tolua_constant(tolua_S,NULL,"RF0_UNIQUE",RF0_UNIQUE);
 tolua_constant(tolua_S,NULL,"RF0_QUESTOR",RF0_QUESTOR);
 tolua_constant(tolua_S,NULL,"RF0_MALE",RF0_MALE);
 tolua_constant(tolua_S,NULL,"RF0_FEMALE",RF0_FEMALE);
 tolua_constant(tolua_S,NULL,"RF0_CHAR_CLEAR",RF0_CHAR_CLEAR);
 tolua_constant(tolua_S,NULL,"RF0_CHAR_MIMIC",RF0_CHAR_MIMIC);
 tolua_constant(tolua_S,NULL,"RF0_ATTR_CLEAR",RF0_ATTR_CLEAR);
 tolua_constant(tolua_S,NULL,"RF0_ATTR_MULTI",RF0_ATTR_MULTI);
 tolua_constant(tolua_S,NULL,"RF0_FORCE_DEPTH",RF0_FORCE_DEPTH);
 tolua_constant(tolua_S,NULL,"RF0_FORCE_MAXHP",RF0_FORCE_MAXHP);
 tolua_constant(tolua_S,NULL,"RF0_FORCE_SLEEP",RF0_FORCE_SLEEP);
 tolua_constant(tolua_S,NULL,"RF0_FORCE_EXTRA",RF0_FORCE_EXTRA);
 tolua_constant(tolua_S,NULL,"RF0_XXX_1",RF0_XXX_1);
 tolua_constant(tolua_S,NULL,"RF0_FRIENDS",RF0_FRIENDS);
 tolua_constant(tolua_S,NULL,"RF0_ESCORT",RF0_ESCORT);
 tolua_constant(tolua_S,NULL,"RF0_ESCORTS",RF0_ESCORTS);
 tolua_constant(tolua_S,NULL,"RF0_NEVER_BLOW",RF0_NEVER_BLOW);
 tolua_constant(tolua_S,NULL,"RF0_NEVER_MOVE",RF0_NEVER_MOVE);
 tolua_constant(tolua_S,NULL,"RF0_RAND_25",RF0_RAND_25);
 tolua_constant(tolua_S,NULL,"RF0_RAND_50",RF0_RAND_50);
 tolua_constant(tolua_S,NULL,"RF0_ONLY_GOLD",RF0_ONLY_GOLD);
 tolua_constant(tolua_S,NULL,"RF0_ONLY_ITEM",RF0_ONLY_ITEM);
 tolua_constant(tolua_S,NULL,"RF0_DROP_60",RF0_DROP_60);
 tolua_constant(tolua_S,NULL,"RF0_DROP_90",RF0_DROP_90);
 tolua_constant(tolua_S,NULL,"RF0_DROP_1D2",RF0_DROP_1D2);
 tolua_constant(tolua_S,NULL,"RF0_DROP_2D2",RF0_DROP_2D2);
 tolua_constant(tolua_S,NULL,"RF0_DROP_3D2",RF0_DROP_3D2);
 tolua_constant(tolua_S,NULL,"RF0_DROP_4D2",RF0_DROP_4D2);
 tolua_constant(tolua_S,NULL,"RF0_DROP_GOOD",RF0_DROP_GOOD);
 tolua_constant(tolua_S,NULL,"RF0_DROP_GREAT",RF0_DROP_GREAT);
 tolua_constant(tolua_S,NULL,"RF0_DROP_USEFUL",RF0_DROP_USEFUL);
 tolua_constant(tolua_S,NULL,"RF0_DROP_CHOSEN",RF0_DROP_CHOSEN);
 tolua_constant(tolua_S,NULL,"RF1_STUPID",RF1_STUPID);
 tolua_constant(tolua_S,NULL,"RF1_SMART",RF1_SMART);
 tolua_constant(tolua_S,NULL,"RF1_CAN_SPEAK",RF1_CAN_SPEAK);
 tolua_constant(tolua_S,NULL,"RF1_REFLECTING",RF1_REFLECTING);
 tolua_constant(tolua_S,NULL,"RF1_INVISIBLE",RF1_INVISIBLE);
 tolua_constant(tolua_S,NULL,"RF1_COLD_BLOOD",RF1_COLD_BLOOD);
 tolua_constant(tolua_S,NULL,"RF1_EMPTY_MIND",RF1_EMPTY_MIND);
 tolua_constant(tolua_S,NULL,"RF1_WEIRD_MIND",RF1_WEIRD_MIND);
 tolua_constant(tolua_S,NULL,"RF1_MULTIPLY",RF1_MULTIPLY);
 tolua_constant(tolua_S,NULL,"RF1_REGENERATE",RF1_REGENERATE);
 tolua_constant(tolua_S,NULL,"RF1_SHAPECHANGER",RF1_SHAPECHANGER);
 tolua_constant(tolua_S,NULL,"RF1_ATTR_ANY",RF1_ATTR_ANY);
 tolua_constant(tolua_S,NULL,"RF1_POWERFUL",RF1_POWERFUL);
 tolua_constant(tolua_S,NULL,"RF1_XXX_1",RF1_XXX_1);
 tolua_constant(tolua_S,NULL,"RF1_AURA_FIRE",RF1_AURA_FIRE);
 tolua_constant(tolua_S,NULL,"RF1_AURA_ELEC",RF1_AURA_ELEC);
 tolua_constant(tolua_S,NULL,"RF1_OPEN_DOOR",RF1_OPEN_DOOR);
 tolua_constant(tolua_S,NULL,"RF1_BASH_DOOR",RF1_BASH_DOOR);
 tolua_constant(tolua_S,NULL,"RF1_PASS_WALL",RF1_PASS_WALL);
 tolua_constant(tolua_S,NULL,"RF1_KILL_WALL",RF1_KILL_WALL);
 tolua_constant(tolua_S,NULL,"RF1_MOVE_BODY",RF1_MOVE_BODY);
 tolua_constant(tolua_S,NULL,"RF1_KILL_BODY",RF1_KILL_BODY);
 tolua_constant(tolua_S,NULL,"RF1_TAKE_ITEM",RF1_TAKE_ITEM);
 tolua_constant(tolua_S,NULL,"RF1_KILL_ITEM",RF1_KILL_ITEM);
 tolua_constant(tolua_S,NULL,"RF1_BRAIN_1",RF1_BRAIN_1);
 tolua_constant(tolua_S,NULL,"RF1_BRAIN_2",RF1_BRAIN_2);
 tolua_constant(tolua_S,NULL,"RF1_BRAIN_3",RF1_BRAIN_3);
 tolua_constant(tolua_S,NULL,"RF1_BRAIN_4",RF1_BRAIN_4);
 tolua_constant(tolua_S,NULL,"RF1_BRAIN_5",RF1_BRAIN_5);
 tolua_constant(tolua_S,NULL,"RF1_BRAIN_6",RF1_BRAIN_6);
 tolua_constant(tolua_S,NULL,"RF1_BRAIN_7",RF1_BRAIN_7);
 tolua_constant(tolua_S,NULL,"RF1_QUANTUM",RF1_QUANTUM);
 tolua_constant(tolua_S,NULL,"RF2_ORC",RF2_ORC);
 tolua_constant(tolua_S,NULL,"RF2_TROLL",RF2_TROLL);
 tolua_constant(tolua_S,NULL,"RF2_GIANT",RF2_GIANT);
 tolua_constant(tolua_S,NULL,"RF2_DRAGON",RF2_DRAGON);
 tolua_constant(tolua_S,NULL,"RF2_DEMON",RF2_DEMON);
 tolua_constant(tolua_S,NULL,"RF2_UNDEAD",RF2_UNDEAD);
 tolua_constant(tolua_S,NULL,"RF2_EVIL",RF2_EVIL);
 tolua_constant(tolua_S,NULL,"RF2_ANIMAL",RF2_ANIMAL);
 tolua_constant(tolua_S,NULL,"RF2_AMBERITE",RF2_AMBERITE);
 tolua_constant(tolua_S,NULL,"RF2_GOOD",RF2_GOOD);
 tolua_constant(tolua_S,NULL,"RF2_AURA_COLD",RF2_AURA_COLD);
 tolua_constant(tolua_S,NULL,"RF2_NONLIVING",RF2_NONLIVING);
 tolua_constant(tolua_S,NULL,"RF2_HURT_LITE",RF2_HURT_LITE);
 tolua_constant(tolua_S,NULL,"RF2_HURT_ROCK",RF2_HURT_ROCK);
 tolua_constant(tolua_S,NULL,"RF2_HURT_FIRE",RF2_HURT_FIRE);
 tolua_constant(tolua_S,NULL,"RF2_HURT_COLD",RF2_HURT_COLD);
 tolua_constant(tolua_S,NULL,"RF2_IM_ACID",RF2_IM_ACID);
 tolua_constant(tolua_S,NULL,"RF2_IM_ELEC",RF2_IM_ELEC);
 tolua_constant(tolua_S,NULL,"RF2_IM_FIRE",RF2_IM_FIRE);
 tolua_constant(tolua_S,NULL,"RF2_IM_COLD",RF2_IM_COLD);
 tolua_constant(tolua_S,NULL,"RF2_IM_POIS",RF2_IM_POIS);
 tolua_constant(tolua_S,NULL,"RF2_RES_TELE",RF2_RES_TELE);
 tolua_constant(tolua_S,NULL,"RF2_RES_NETH",RF2_RES_NETH);
 tolua_constant(tolua_S,NULL,"RF2_RES_WATE",RF2_RES_WATE);
 tolua_constant(tolua_S,NULL,"RF2_RES_PLAS",RF2_RES_PLAS);
 tolua_constant(tolua_S,NULL,"RF2_RES_NEXU",RF2_RES_NEXU);
 tolua_constant(tolua_S,NULL,"RF2_RES_DISE",RF2_RES_DISE);
 tolua_constant(tolua_S,NULL,"RF2_UNIQUE_7",RF2_UNIQUE_7);
 tolua_constant(tolua_S,NULL,"RF2_NO_FEAR",RF2_NO_FEAR);
 tolua_constant(tolua_S,NULL,"RF2_NO_STUN",RF2_NO_STUN);
 tolua_constant(tolua_S,NULL,"RF2_NO_CONF",RF2_NO_CONF);
 tolua_constant(tolua_S,NULL,"RF2_NO_SLEEP",RF2_NO_SLEEP);
 tolua_constant(tolua_S,NULL,"RF3_SHRIEK",RF3_SHRIEK);
 tolua_constant(tolua_S,NULL,"RF3_ELDRITCH_HORROR",RF3_ELDRITCH_HORROR);
 tolua_constant(tolua_S,NULL,"RF3_XXX3",RF3_XXX3);
 tolua_constant(tolua_S,NULL,"RF3_ROCKET",RF3_ROCKET);
 tolua_constant(tolua_S,NULL,"RF3_ARROW_1",RF3_ARROW_1);
 tolua_constant(tolua_S,NULL,"RF3_ARROW_2",RF3_ARROW_2);
 tolua_constant(tolua_S,NULL,"RF3_ARROW_3",RF3_ARROW_3);
 tolua_constant(tolua_S,NULL,"RF3_ARROW_4",RF3_ARROW_4);
 tolua_constant(tolua_S,NULL,"RF3_BR_ACID",RF3_BR_ACID);
 tolua_constant(tolua_S,NULL,"RF3_BR_ELEC",RF3_BR_ELEC);
 tolua_constant(tolua_S,NULL,"RF3_BR_FIRE",RF3_BR_FIRE);
 tolua_constant(tolua_S,NULL,"RF3_BR_COLD",RF3_BR_COLD);
 tolua_constant(tolua_S,NULL,"RF3_BR_POIS",RF3_BR_POIS);
 tolua_constant(tolua_S,NULL,"RF3_BR_NETH",RF3_BR_NETH);
 tolua_constant(tolua_S,NULL,"RF3_BR_LITE",RF3_BR_LITE);
 tolua_constant(tolua_S,NULL,"RF3_BR_DARK",RF3_BR_DARK);
 tolua_constant(tolua_S,NULL,"RF3_BR_CONF",RF3_BR_CONF);
 tolua_constant(tolua_S,NULL,"RF3_BR_SOUN",RF3_BR_SOUN);
 tolua_constant(tolua_S,NULL,"RF3_BR_CHAO",RF3_BR_CHAO);
 tolua_constant(tolua_S,NULL,"RF3_BR_DISE",RF3_BR_DISE);
 tolua_constant(tolua_S,NULL,"RF3_BR_NEXU",RF3_BR_NEXU);
 tolua_constant(tolua_S,NULL,"RF3_BR_TIME",RF3_BR_TIME);
 tolua_constant(tolua_S,NULL,"RF3_BR_INER",RF3_BR_INER);
 tolua_constant(tolua_S,NULL,"RF3_BR_GRAV",RF3_BR_GRAV);
 tolua_constant(tolua_S,NULL,"RF3_BR_SHAR",RF3_BR_SHAR);
 tolua_constant(tolua_S,NULL,"RF3_BR_PLAS",RF3_BR_PLAS);
 tolua_constant(tolua_S,NULL,"RF3_BR_WALL",RF3_BR_WALL);
 tolua_constant(tolua_S,NULL,"RF3_BR_MANA",RF3_BR_MANA);
 tolua_constant(tolua_S,NULL,"RF3_BA_NUKE",RF3_BA_NUKE);
 tolua_constant(tolua_S,NULL,"RF3_BR_NUKE",RF3_BR_NUKE);
 tolua_constant(tolua_S,NULL,"RF3_BA_CHAO",RF3_BA_CHAO);
 tolua_constant(tolua_S,NULL,"RF3_BR_DISI",RF3_BR_DISI);
 tolua_constant(tolua_S,NULL,"RF4_BA_ACID",RF4_BA_ACID);
 tolua_constant(tolua_S,NULL,"RF4_BA_ELEC",RF4_BA_ELEC);
 tolua_constant(tolua_S,NULL,"RF4_BA_FIRE",RF4_BA_FIRE);
 tolua_constant(tolua_S,NULL,"RF4_BA_COLD",RF4_BA_COLD);
 tolua_constant(tolua_S,NULL,"RF4_BA_POIS",RF4_BA_POIS);
 tolua_constant(tolua_S,NULL,"RF4_BA_NETH",RF4_BA_NETH);
 tolua_constant(tolua_S,NULL,"RF4_BA_WATE",RF4_BA_WATE);
 tolua_constant(tolua_S,NULL,"RF4_BA_MANA",RF4_BA_MANA);
 tolua_constant(tolua_S,NULL,"RF4_BA_DARK",RF4_BA_DARK);
 tolua_constant(tolua_S,NULL,"RF4_DRAIN_MANA",RF4_DRAIN_MANA);
 tolua_constant(tolua_S,NULL,"RF4_MIND_BLAST",RF4_MIND_BLAST);
 tolua_constant(tolua_S,NULL,"RF4_BRAIN_SMASH",RF4_BRAIN_SMASH);
 tolua_constant(tolua_S,NULL,"RF4_CAUSE_1",RF4_CAUSE_1);
 tolua_constant(tolua_S,NULL,"RF4_CAUSE_2",RF4_CAUSE_2);
 tolua_constant(tolua_S,NULL,"RF4_CAUSE_3",RF4_CAUSE_3);
 tolua_constant(tolua_S,NULL,"RF4_CAUSE_4",RF4_CAUSE_4);
 tolua_constant(tolua_S,NULL,"RF4_BO_ACID",RF4_BO_ACID);
 tolua_constant(tolua_S,NULL,"RF4_BO_ELEC",RF4_BO_ELEC);
 tolua_constant(tolua_S,NULL,"RF4_BO_FIRE",RF4_BO_FIRE);
 tolua_constant(tolua_S,NULL,"RF4_BO_COLD",RF4_BO_COLD);
 tolua_constant(tolua_S,NULL,"RF4_BO_POIS",RF4_BO_POIS);
 tolua_constant(tolua_S,NULL,"RF4_BO_NETH",RF4_BO_NETH);
 tolua_constant(tolua_S,NULL,"RF4_BO_WATE",RF4_BO_WATE);
 tolua_constant(tolua_S,NULL,"RF4_BO_MANA",RF4_BO_MANA);
 tolua_constant(tolua_S,NULL,"RF4_BO_PLAS",RF4_BO_PLAS);
 tolua_constant(tolua_S,NULL,"RF4_BO_ICEE",RF4_BO_ICEE);
 tolua_constant(tolua_S,NULL,"RF4_MISSILE",RF4_MISSILE);
 tolua_constant(tolua_S,NULL,"RF4_SCARE",RF4_SCARE);
 tolua_constant(tolua_S,NULL,"RF4_BLIND",RF4_BLIND);
 tolua_constant(tolua_S,NULL,"RF4_CONF",RF4_CONF);
 tolua_constant(tolua_S,NULL,"RF4_SLOW",RF4_SLOW);
 tolua_constant(tolua_S,NULL,"RF4_HOLD",RF4_HOLD);
 tolua_constant(tolua_S,NULL,"RF5_HASTE",RF5_HASTE);
 tolua_constant(tolua_S,NULL,"RF5_HAND_DOOM",RF5_HAND_DOOM);
 tolua_constant(tolua_S,NULL,"RF5_HEAL",RF5_HEAL);
 tolua_constant(tolua_S,NULL,"RF5_INVULNER",RF5_INVULNER);
 tolua_constant(tolua_S,NULL,"RF5_BLINK",RF5_BLINK);
 tolua_constant(tolua_S,NULL,"RF5_TPORT",RF5_TPORT);
 tolua_constant(tolua_S,NULL,"RF5_XXX3",RF5_XXX3);
 tolua_constant(tolua_S,NULL,"RF5_XXX4",RF5_XXX4);
 tolua_constant(tolua_S,NULL,"RF5_TELE_TO",RF5_TELE_TO);
 tolua_constant(tolua_S,NULL,"RF5_TELE_AWAY",RF5_TELE_AWAY);
 tolua_constant(tolua_S,NULL,"RF5_TELE_LEVEL",RF5_TELE_LEVEL);
 tolua_constant(tolua_S,NULL,"RF5_XXX5",RF5_XXX5);
 tolua_constant(tolua_S,NULL,"RF5_DARKNESS",RF5_DARKNESS);
 tolua_constant(tolua_S,NULL,"RF5_TRAPS",RF5_TRAPS);
 tolua_constant(tolua_S,NULL,"RF5_FORGET",RF5_FORGET);
 tolua_constant(tolua_S,NULL,"RF5_RAISE_DEAD",RF5_RAISE_DEAD);
 tolua_constant(tolua_S,NULL,"RF5_S_KIN",RF5_S_KIN);
 tolua_constant(tolua_S,NULL,"RF5_S_CYBER",RF5_S_CYBER);
 tolua_constant(tolua_S,NULL,"RF5_S_MONSTER",RF5_S_MONSTER);
 tolua_constant(tolua_S,NULL,"RF5_S_MONSTERS",RF5_S_MONSTERS);
 tolua_constant(tolua_S,NULL,"RF5_S_ANT",RF5_S_ANT);
 tolua_constant(tolua_S,NULL,"RF5_S_SPIDER",RF5_S_SPIDER);
 tolua_constant(tolua_S,NULL,"RF5_S_HOUND",RF5_S_HOUND);
 tolua_constant(tolua_S,NULL,"RF5_S_HYDRA",RF5_S_HYDRA);
 tolua_constant(tolua_S,NULL,"RF5_S_ANGEL",RF5_S_ANGEL);
 tolua_constant(tolua_S,NULL,"RF5_S_DEMON",RF5_S_DEMON);
 tolua_constant(tolua_S,NULL,"RF5_S_UNDEAD",RF5_S_UNDEAD);
 tolua_constant(tolua_S,NULL,"RF5_S_DRAGON",RF5_S_DRAGON);
 tolua_constant(tolua_S,NULL,"RF5_S_HI_UNDEAD",RF5_S_HI_UNDEAD);
 tolua_constant(tolua_S,NULL,"RF5_S_HI_DRAGON",RF5_S_HI_DRAGON);
 tolua_constant(tolua_S,NULL,"RF5_S_AMBERITES",RF5_S_AMBERITES);
 tolua_constant(tolua_S,NULL,"RF5_S_UNIQUE",RF5_S_UNIQUE);
 tolua_constant(tolua_S,NULL,"RF6_AQUATIC",RF6_AQUATIC);
 tolua_constant(tolua_S,NULL,"RF6_CAN_SWIM",RF6_CAN_SWIM);
 tolua_constant(tolua_S,NULL,"RF6_CAN_FLY",RF6_CAN_FLY);
 tolua_constant(tolua_S,NULL,"RF6_FRIENDLY",RF6_FRIENDLY);
 tolua_constant(tolua_S,NULL,"RF6_SILLY",RF6_SILLY);
 tolua_constant(tolua_S,NULL,"RF6_LITE_1",RF6_LITE_1);
 tolua_constant(tolua_S,NULL,"RF6_LITE_2",RF6_LITE_2);
 tolua_constant(tolua_S,NULL,"RF7_DUNGEON",RF7_DUNGEON);
 tolua_constant(tolua_S,NULL,"RF7_WILD",RF7_WILD);
 tolua_constant(tolua_S,NULL,"RF8_DROP_CORPSE",RF8_DROP_CORPSE);
 tolua_constant(tolua_S,NULL,"RF8_DROP_SKELETON",RF8_DROP_SKELETON);
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
 tolua_constant(tolua_S,NULL,"SUMMON_AMBERITES",SUMMON_AMBERITES);
 tolua_constant(tolua_S,NULL,"SUMMON_UNIQUE",SUMMON_UNIQUE);
 tolua_constant(tolua_S,NULL,"SUMMON_BIZARRE1",SUMMON_BIZARRE1);
 tolua_constant(tolua_S,NULL,"SUMMON_BIZARRE2",SUMMON_BIZARRE2);
 tolua_constant(tolua_S,NULL,"SUMMON_BIZARRE3",SUMMON_BIZARRE3);
 tolua_constant(tolua_S,NULL,"SUMMON_BIZARRE4",SUMMON_BIZARRE4);
 tolua_constant(tolua_S,NULL,"SUMMON_BIZARRE5",SUMMON_BIZARRE5);
 tolua_constant(tolua_S,NULL,"SUMMON_BIZARRE6",SUMMON_BIZARRE6);
 tolua_constant(tolua_S,NULL,"SUMMON_CYBER",SUMMON_CYBER);
 tolua_constant(tolua_S,NULL,"SUMMON_KIN",SUMMON_KIN);
 tolua_constant(tolua_S,NULL,"SUMMON_DAWN",SUMMON_DAWN);
 tolua_constant(tolua_S,NULL,"SUMMON_ANIMAL",SUMMON_ANIMAL);
 tolua_constant(tolua_S,NULL,"SUMMON_ANIMAL_RANGER",SUMMON_ANIMAL_RANGER);
 tolua_constant(tolua_S,NULL,"SUMMON_HI_UNDEAD_NO_UNIQUES",SUMMON_HI_UNDEAD_NO_UNIQUES);
 tolua_constant(tolua_S,NULL,"SUMMON_HI_DRAGON_NO_UNIQUES",SUMMON_HI_DRAGON_NO_UNIQUES);
 tolua_constant(tolua_S,NULL,"SUMMON_NO_UNIQUES",SUMMON_NO_UNIQUES);
 tolua_constant(tolua_S,NULL,"SUMMON_PHANTOM",SUMMON_PHANTOM);
 tolua_constant(tolua_S,NULL,"SUMMON_ELEMENTAL",SUMMON_ELEMENTAL);
 tolua_constant(tolua_S,NULL,"SUMMON_BLUE_HORROR",SUMMON_BLUE_HORROR);
 tolua_cclass(tolua_S,"monster_blow","");
 tolua_tablevar(tolua_S,"monster_blow","method",toluaI_get_monst_monster_blow_method,toluaI_set_monst_monster_blow_method);
 tolua_tablevar(tolua_S,"monster_blow","effect",toluaI_get_monst_monster_blow_effect,toluaI_set_monst_monster_blow_effect);
 tolua_tablevar(tolua_S,"monster_blow","d_dice",toluaI_get_monst_monster_blow_d_dice,toluaI_set_monst_monster_blow_d_dice);
 tolua_tablevar(tolua_S,"monster_blow","d_side",toluaI_get_monst_monster_blow_d_side,toluaI_set_monst_monster_blow_d_side);
 tolua_cclass(tolua_S,"monster_race","");
 tolua_tablevar(tolua_S,"monster_race","name",toluaI_get_monst_monster_race_name,toluaI_set_monst_monster_race_name);
 tolua_tablevar(tolua_S,"monster_race","text",toluaI_get_monst_monster_race_text,toluaI_set_monst_monster_race_text);
 tolua_tablevar(tolua_S,"monster_race","hdice",toluaI_get_monst_monster_race_hdice,toluaI_set_monst_monster_race_hdice);
 tolua_tablevar(tolua_S,"monster_race","hside",toluaI_get_monst_monster_race_hside,toluaI_set_monst_monster_race_hside);
 tolua_tablevar(tolua_S,"monster_race","ac",toluaI_get_monst_monster_race_ac,toluaI_set_monst_monster_race_ac);
 tolua_tablevar(tolua_S,"monster_race","sleep",toluaI_get_monst_monster_race_sleep,toluaI_set_monst_monster_race_sleep);
 tolua_tablevar(tolua_S,"monster_race","aaf",toluaI_get_monst_monster_race_aaf,toluaI_set_monst_monster_race_aaf);
 tolua_tablevar(tolua_S,"monster_race","speed",toluaI_get_monst_monster_race_speed,toluaI_set_monst_monster_race_speed);
 tolua_tablevar(tolua_S,"monster_race","mexp",toluaI_get_monst_monster_race_mexp,toluaI_set_monst_monster_race_mexp);
 tolua_tablevar(tolua_S,"monster_race","extra",toluaI_get_monst_monster_race_extra,toluaI_set_monst_monster_race_extra);
 tolua_tablevar(tolua_S,"monster_race","freq_inate",toluaI_get_monst_monster_race_freq_inate,toluaI_set_monst_monster_race_freq_inate);
 tolua_tablevar(tolua_S,"monster_race","freq_spell",toluaI_get_monst_monster_race_freq_spell,toluaI_set_monst_monster_race_freq_spell);
 tolua_tablearray(tolua_S,"monster_race","flags",toluaI_get_monst_monster_race_flags,toluaI_set_monst_monster_race_flags);
 tolua_tablearray(tolua_S,"monster_race","blow",toluaI_get_monst_monster_race_blow,toluaI_set_monst_monster_race_blow);
 tolua_tablevar(tolua_S,"monster_race","level",toluaI_get_monst_monster_race_level,toluaI_set_monst_monster_race_level);
 tolua_tablevar(tolua_S,"monster_race","rarity",toluaI_get_monst_monster_race_rarity,toluaI_set_monst_monster_race_rarity);
 tolua_tablevar(tolua_S,"monster_race","d_attr",toluaI_get_monst_monster_race_d_attr,toluaI_set_monst_monster_race_d_attr);
 tolua_tablevar(tolua_S,"monster_race","d_char",toluaI_get_monst_monster_race_d_char,toluaI_set_monst_monster_race_d_char);
 tolua_tablevar(tolua_S,"monster_race","x_attr",toluaI_get_monst_monster_race_x_attr,toluaI_set_monst_monster_race_x_attr);
 tolua_tablevar(tolua_S,"monster_race","x_char",toluaI_get_monst_monster_race_x_char,toluaI_set_monst_monster_race_x_char);
 tolua_tablevar(tolua_S,"monster_race","max_num",toluaI_get_monst_monster_race_max_num,toluaI_set_monst_monster_race_max_num);
 tolua_tablevar(tolua_S,"monster_race","cur_num",toluaI_get_monst_monster_race_cur_num,toluaI_set_monst_monster_race_cur_num);
 tolua_tablevar(tolua_S,"monster_race","r_sights",toluaI_get_monst_monster_race_r_sights,toluaI_set_monst_monster_race_r_sights);
 tolua_tablevar(tolua_S,"monster_race","r_deaths",toluaI_get_monst_monster_race_r_deaths,toluaI_set_monst_monster_race_r_deaths);
 tolua_tablevar(tolua_S,"monster_race","r_pkills",toluaI_get_monst_monster_race_r_pkills,toluaI_set_monst_monster_race_r_pkills);
 tolua_tablevar(tolua_S,"monster_race","r_tkills",toluaI_get_monst_monster_race_r_tkills,toluaI_set_monst_monster_race_r_tkills);
 tolua_tablevar(tolua_S,"monster_race","r_wake",toluaI_get_monst_monster_race_r_wake,toluaI_set_monst_monster_race_r_wake);
 tolua_tablevar(tolua_S,"monster_race","r_ignore",toluaI_get_monst_monster_race_r_ignore,toluaI_set_monst_monster_race_r_ignore);
 tolua_tablevar(tolua_S,"monster_race","r_xtra1",toluaI_get_monst_monster_race_r_xtra1,toluaI_set_monst_monster_race_r_xtra1);
 tolua_tablevar(tolua_S,"monster_race","r_xtra2",toluaI_get_monst_monster_race_r_xtra2,toluaI_set_monst_monster_race_r_xtra2);
 tolua_tablevar(tolua_S,"monster_race","r_drop_gold",toluaI_get_monst_monster_race_r_drop_gold,toluaI_set_monst_monster_race_r_drop_gold);
 tolua_tablevar(tolua_S,"monster_race","r_drop_item",toluaI_get_monst_monster_race_r_drop_item,toluaI_set_monst_monster_race_r_drop_item);
 tolua_tablevar(tolua_S,"monster_race","r_cast_inate",toluaI_get_monst_monster_race_r_cast_inate,toluaI_set_monst_monster_race_r_cast_inate);
 tolua_tablevar(tolua_S,"monster_race","r_cast_spell",toluaI_get_monst_monster_race_r_cast_spell,toluaI_set_monst_monster_race_r_cast_spell);
 tolua_tablearray(tolua_S,"monster_race","r_blows",toluaI_get_monst_monster_race_r_blows,toluaI_set_monst_monster_race_r_blows);
 tolua_tablearray(tolua_S,"monster_race","r_flags",toluaI_get_monst_monster_race_r_flags,toluaI_set_monst_monster_race_r_flags);
 tolua_tablevar(tolua_S,"monster_race","obj_drop",toluaI_get_monst_monster_race_obj_drop,toluaI_set_monst_monster_race_obj_drop);
 tolua_tablevar(tolua_S,"monster_race","r_see",toluaI_get_monst_monster_race_r_see,toluaI_set_monst_monster_race_r_see);
 tolua_cclass(tolua_S,"monster_type","");
 tolua_tablevar(tolua_S,"monster_type","r_idx",toluaI_get_monst_monster_type_r_idx,toluaI_set_monst_monster_type_r_idx);
 tolua_tablevar(tolua_S,"monster_type","fy",toluaI_get_monst_monster_type_fy,toluaI_set_monst_monster_type_fy);
 tolua_tablevar(tolua_S,"monster_type","fx",toluaI_get_monst_monster_type_fx,toluaI_set_monst_monster_type_fx);
 tolua_tablevar(tolua_S,"monster_type","hp",toluaI_get_monst_monster_type_hp,toluaI_set_monst_monster_type_hp);
 tolua_tablevar(tolua_S,"monster_type","maxhp",toluaI_get_monst_monster_type_maxhp,toluaI_set_monst_monster_type_maxhp);
 tolua_tablevar(tolua_S,"monster_type","csleep",toluaI_get_monst_monster_type_csleep,toluaI_set_monst_monster_type_csleep);
 tolua_tablevar(tolua_S,"monster_type","mspeed",toluaI_get_monst_monster_type_mspeed,toluaI_set_monst_monster_type_mspeed);
 tolua_tablevar(tolua_S,"monster_type","energy",toluaI_get_monst_monster_type_energy,toluaI_set_monst_monster_type_energy);
 tolua_tablevar(tolua_S,"monster_type","stunned",toluaI_get_monst_monster_type_stunned,toluaI_set_monst_monster_type_stunned);
 tolua_tablevar(tolua_S,"monster_type","confused",toluaI_get_monst_monster_type_confused,toluaI_set_monst_monster_type_confused);
 tolua_tablevar(tolua_S,"monster_type","monfear",toluaI_get_monst_monster_type_monfear,toluaI_set_monst_monster_type_monfear);
 tolua_tablevar(tolua_S,"monster_type","invulner",toluaI_get_monst_monster_type_invulner,toluaI_set_monst_monster_type_invulner);
 tolua_tablevar(tolua_S,"monster_type","cdis",toluaI_get_monst_monster_type_cdis,toluaI_set_monst_monster_type_cdis);
 tolua_tablevar(tolua_S,"monster_type","mflag",toluaI_get_monst_monster_type_mflag,toluaI_set_monst_monster_type_mflag);
 tolua_tablevar(tolua_S,"monster_type","ml",toluaI_get_monst_monster_type_ml,toluaI_set_monst_monster_type_ml);
 tolua_tablevar(tolua_S,"monster_type","hold_o_idx",toluaI_get_monst_monster_type_hold_o_idx,toluaI_set_monst_monster_type_hold_o_idx);
 tolua_tablevar(tolua_S,"monster_type","smart",toluaI_get_monst_monster_type_smart,toluaI_set_monst_monster_type_smart);
 tolua_globalvar(tolua_S,"m_max",toluaI_get_monst_m_max,toluaI_set_monst_m_max);
 tolua_globalvar(tolua_S,"m_cnt",toluaI_get_monst_m_cnt,toluaI_set_monst_m_cnt);
 tolua_function(tolua_S,NULL,"make_attack_normal",toluaI_monst_make_attack_normal00);
 tolua_function(tolua_S,NULL,"make_attack_spell",toluaI_monst_make_attack_spell00);
 tolua_function(tolua_S,NULL,"process_monsters",toluaI_monst_process_monsters00);
 tolua_function(tolua_S,NULL,"curse_equipment",toluaI_monst_curse_equipment00);
 tolua_function(tolua_S,NULL,"mon_take_hit_mon",toluaI_monst_mon_take_hit_mon00);
 tolua_function(tolua_S,NULL,"screen_roff_mon",toluaI_monst_screen_roff_mon00);
 tolua_function(tolua_S,NULL,"display_roff_mon",toluaI_monst_display_roff_mon00);
 tolua_function(tolua_S,NULL,"display_visible",toluaI_monst_display_visible00);
 tolua_globalarray(tolua_S,"horror_desc",toluaI_get_monst_horror_desc,toluaI_set_monst_horror_desc);
 tolua_globalarray(tolua_S,"funny_desc",toluaI_get_monst_funny_desc,toluaI_set_monst_funny_desc);
 tolua_globalarray(tolua_S,"funny_comments",toluaI_get_monst_funny_comments,toluaI_set_monst_funny_comments);
 tolua_function(tolua_S,NULL,"delete_monster_idx",toluaI_monst_delete_monster_idx00);
 tolua_function(tolua_S,NULL,"delete_monster",toluaI_monst_delete_monster00);
 tolua_function(tolua_S,NULL,"monster_desc",toluaI_monst_monster_desc00);
 tolua_function(tolua_S,NULL,"lore_do_probe",toluaI_monst_lore_do_probe00);
 tolua_function(tolua_S,NULL,"lore_treasure",toluaI_monst_lore_treasure00);
 tolua_function(tolua_S,NULL,"update_mon_vis",toluaI_monst_update_mon_vis00);
 tolua_function(tolua_S,NULL,"update_mon",toluaI_monst_update_mon00);
 tolua_function(tolua_S,NULL,"place_monster",toluaI_monst_place_monster00);
 tolua_function(tolua_S,NULL,"alloc_horde",toluaI_monst_alloc_horde00);
 tolua_function(tolua_S,NULL,"summon_specific",toluaI_monst_summon_specific00);
 tolua_function(tolua_S,NULL,"summon_named_creature",toluaI_monst_summon_named_creature00);
 tolua_function(tolua_S,NULL,"multiply_monster",toluaI_monst_multiply_monster00);
 tolua_function(tolua_S,NULL,"update_smart_learn",toluaI_monst_update_smart_learn00);
 tolua_function(tolua_S,NULL,"place_monster_one",toluaI_monst_place_monster_one00);
 tolua_function(tolua_S,NULL,"set_friendly",toluaI_monst_set_friendly00);
 tolua_function(tolua_S,NULL,"set_pet",toluaI_monst_set_pet00);
 tolua_function(tolua_S,NULL,"set_hostile",toluaI_monst_set_hostile00);
 tolua_function(tolua_S,NULL,"anger_monster",toluaI_monst_anger_monster00);
 tolua_function(tolua_S,NULL,"are_enemies",toluaI_monst_are_enemies00);
 tolua_function(tolua_S,NULL,"monster_living",toluaI_monst_monster_living00);
 tolua_function(tolua_S,NULL,"monster_death",toluaI_monst_monster_death00);
 tolua_function(tolua_S,NULL,"mon_take_hit",toluaI_monst_mon_take_hit00);
 return 1;
}
/* Close function */
void tolua_monst_close (lua_State* tolua_S)
{
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RBM_HIT");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RBM_TOUCH");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RBM_PUNCH");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RBM_KICK");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RBM_CLAW");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RBM_BITE");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RBM_STING");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RBM_XXX1");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RBM_BUTT");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RBM_CRUSH");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RBM_ENGULF");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RBM_CHARGE");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RBM_CRAWL");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RBM_DROOL");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RBM_SPIT");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RBM_EXPLODE");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RBM_GAZE");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RBM_WAIL");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RBM_SPORE");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RBM_XXX4");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RBM_BEG");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RBM_INSULT");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RBM_MOAN");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RBM_SHOW");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RBE_HURT");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RBE_POISON");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RBE_UN_BONUS");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RBE_UN_POWER");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RBE_EAT_GOLD");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RBE_EAT_ITEM");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RBE_EAT_FOOD");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RBE_EAT_LITE");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RBE_ACID");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RBE_ELEC");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RBE_FIRE");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RBE_COLD");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RBE_BLIND");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RBE_CONFUSE");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RBE_TERRIFY");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RBE_PARALYZE");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RBE_LOSE_STR");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RBE_LOSE_INT");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RBE_LOSE_WIS");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RBE_LOSE_DEX");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RBE_LOSE_CON");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RBE_LOSE_CHR");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RBE_LOSE_ALL");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RBE_SHATTER");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RBE_EXP_10");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RBE_EXP_20");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RBE_EXP_40");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RBE_EXP_80");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RBE_DISEASE");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RBE_TIME");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RBE_EXP_VAMP");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SM_RES_ACID");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SM_RES_ELEC");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SM_RES_FIRE");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SM_RES_COLD");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SM_RES_POIS");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SM_RES_NETH");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SM_RES_LITE");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SM_RES_DARK");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SM_RES_FEAR");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SM_RES_CONF");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SM_RES_CHAOS");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SM_RES_DISEN");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SM_RES_BLIND");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SM_RES_NEXUS");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SM_RES_SOUND");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SM_RES_SHARD");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SM_OPP_ACID");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SM_OPP_ELEC");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SM_OPP_FIRE");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SM_OPP_COLD");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SM_OPP_POIS");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SM_MIMIC");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SM_CLONED");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SM_PET");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SM_IMM_ACID");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SM_IMM_ELEC");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SM_IMM_FIRE");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SM_IMM_COLD");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SM_FRIENDLY");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SM_IMM_REFLECT");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SM_IMM_FREE");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SM_IMM_MANA");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"MFLAG_VIEW");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"MFLAG_TEMP");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"MFLAG_XXX2");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"MFLAG_XXX3");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"MFLAG_MOVE");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"MFLAG_NICE");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"MFLAG_SHOW");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"MFLAG_MARK");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF0_UNIQUE");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF0_QUESTOR");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF0_MALE");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF0_FEMALE");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF0_CHAR_CLEAR");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF0_CHAR_MIMIC");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF0_ATTR_CLEAR");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF0_ATTR_MULTI");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF0_FORCE_DEPTH");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF0_FORCE_MAXHP");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF0_FORCE_SLEEP");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF0_FORCE_EXTRA");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF0_XXX_1");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF0_FRIENDS");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF0_ESCORT");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF0_ESCORTS");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF0_NEVER_BLOW");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF0_NEVER_MOVE");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF0_RAND_25");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF0_RAND_50");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF0_ONLY_GOLD");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF0_ONLY_ITEM");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF0_DROP_60");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF0_DROP_90");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF0_DROP_1D2");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF0_DROP_2D2");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF0_DROP_3D2");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF0_DROP_4D2");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF0_DROP_GOOD");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF0_DROP_GREAT");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF0_DROP_USEFUL");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF0_DROP_CHOSEN");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF1_STUPID");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF1_SMART");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF1_CAN_SPEAK");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF1_REFLECTING");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF1_INVISIBLE");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF1_COLD_BLOOD");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF1_EMPTY_MIND");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF1_WEIRD_MIND");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF1_MULTIPLY");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF1_REGENERATE");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF1_SHAPECHANGER");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF1_ATTR_ANY");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF1_POWERFUL");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF1_XXX_1");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF1_AURA_FIRE");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF1_AURA_ELEC");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF1_OPEN_DOOR");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF1_BASH_DOOR");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF1_PASS_WALL");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF1_KILL_WALL");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF1_MOVE_BODY");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF1_KILL_BODY");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF1_TAKE_ITEM");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF1_KILL_ITEM");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF1_BRAIN_1");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF1_BRAIN_2");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF1_BRAIN_3");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF1_BRAIN_4");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF1_BRAIN_5");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF1_BRAIN_6");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF1_BRAIN_7");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF1_QUANTUM");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF2_ORC");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF2_TROLL");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF2_GIANT");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF2_DRAGON");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF2_DEMON");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF2_UNDEAD");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF2_EVIL");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF2_ANIMAL");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF2_AMBERITE");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF2_GOOD");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF2_AURA_COLD");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF2_NONLIVING");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF2_HURT_LITE");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF2_HURT_ROCK");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF2_HURT_FIRE");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF2_HURT_COLD");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF2_IM_ACID");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF2_IM_ELEC");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF2_IM_FIRE");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF2_IM_COLD");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF2_IM_POIS");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF2_RES_TELE");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF2_RES_NETH");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF2_RES_WATE");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF2_RES_PLAS");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF2_RES_NEXU");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF2_RES_DISE");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF2_UNIQUE_7");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF2_NO_FEAR");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF2_NO_STUN");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF2_NO_CONF");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF2_NO_SLEEP");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF3_SHRIEK");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF3_ELDRITCH_HORROR");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF3_XXX3");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF3_ROCKET");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF3_ARROW_1");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF3_ARROW_2");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF3_ARROW_3");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF3_ARROW_4");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF3_BR_ACID");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF3_BR_ELEC");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF3_BR_FIRE");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF3_BR_COLD");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF3_BR_POIS");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF3_BR_NETH");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF3_BR_LITE");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF3_BR_DARK");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF3_BR_CONF");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF3_BR_SOUN");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF3_BR_CHAO");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF3_BR_DISE");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF3_BR_NEXU");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF3_BR_TIME");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF3_BR_INER");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF3_BR_GRAV");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF3_BR_SHAR");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF3_BR_PLAS");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF3_BR_WALL");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF3_BR_MANA");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF3_BA_NUKE");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF3_BR_NUKE");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF3_BA_CHAO");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF3_BR_DISI");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF4_BA_ACID");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF4_BA_ELEC");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF4_BA_FIRE");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF4_BA_COLD");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF4_BA_POIS");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF4_BA_NETH");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF4_BA_WATE");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF4_BA_MANA");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF4_BA_DARK");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF4_DRAIN_MANA");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF4_MIND_BLAST");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF4_BRAIN_SMASH");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF4_CAUSE_1");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF4_CAUSE_2");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF4_CAUSE_3");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF4_CAUSE_4");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF4_BO_ACID");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF4_BO_ELEC");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF4_BO_FIRE");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF4_BO_COLD");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF4_BO_POIS");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF4_BO_NETH");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF4_BO_WATE");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF4_BO_MANA");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF4_BO_PLAS");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF4_BO_ICEE");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF4_MISSILE");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF4_SCARE");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF4_BLIND");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF4_CONF");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF4_SLOW");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF4_HOLD");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF5_HASTE");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF5_HAND_DOOM");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF5_HEAL");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF5_INVULNER");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF5_BLINK");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF5_TPORT");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF5_XXX3");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF5_XXX4");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF5_TELE_TO");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF5_TELE_AWAY");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF5_TELE_LEVEL");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF5_XXX5");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF5_DARKNESS");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF5_TRAPS");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF5_FORGET");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF5_RAISE_DEAD");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF5_S_KIN");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF5_S_CYBER");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF5_S_MONSTER");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF5_S_MONSTERS");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF5_S_ANT");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF5_S_SPIDER");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF5_S_HOUND");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF5_S_HYDRA");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF5_S_ANGEL");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF5_S_DEMON");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF5_S_UNDEAD");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF5_S_DRAGON");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF5_S_HI_UNDEAD");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF5_S_HI_DRAGON");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF5_S_AMBERITES");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF5_S_UNIQUE");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF6_AQUATIC");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF6_CAN_SWIM");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF6_CAN_FLY");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF6_FRIENDLY");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF6_SILLY");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF6_LITE_1");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF6_LITE_2");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF7_DUNGEON");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF7_WILD");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF8_DROP_CORPSE");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"RF8_DROP_SKELETON");
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
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SUMMON_AMBERITES");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SUMMON_UNIQUE");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SUMMON_BIZARRE1");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SUMMON_BIZARRE2");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SUMMON_BIZARRE3");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SUMMON_BIZARRE4");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SUMMON_BIZARRE5");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SUMMON_BIZARRE6");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SUMMON_CYBER");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SUMMON_KIN");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SUMMON_DAWN");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SUMMON_ANIMAL");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SUMMON_ANIMAL_RANGER");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SUMMON_HI_UNDEAD_NO_UNIQUES");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SUMMON_HI_DRAGON_NO_UNIQUES");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SUMMON_NO_UNIQUES");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SUMMON_PHANTOM");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SUMMON_ELEMENTAL");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SUMMON_BLUE_HORROR");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"monster_blow");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"monster_race");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"monster_type");
 lua_getglobals(tolua_S);
 lua_pushstring(tolua_S,"m_max"); lua_pushnil(tolua_S); lua_rawset(tolua_S,-3);
 lua_pop(tolua_S,1);
 lua_getglobals(tolua_S);
 lua_pushstring(tolua_S,"m_cnt"); lua_pushnil(tolua_S); lua_rawset(tolua_S,-3);
 lua_pop(tolua_S,1);
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"make_attack_normal");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"make_attack_spell");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"process_monsters");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"curse_equipment");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"mon_take_hit_mon");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"screen_roff_mon");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"display_roff_mon");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"display_visible");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"horror_desc");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"funny_desc");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"funny_comments");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"delete_monster_idx");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"delete_monster");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"monster_desc");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"lore_do_probe");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"lore_treasure");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"update_mon_vis");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"update_mon");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"place_monster");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"alloc_horde");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"summon_specific");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"summon_named_creature");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"multiply_monster");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"update_smart_learn");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"place_monster_one");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"set_friendly");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"set_pet");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"set_hostile");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"anger_monster");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"are_enemies");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"monster_living");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"monster_death");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"mon_take_hit");
}
