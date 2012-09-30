/*
** Lua binding: player
** Generated automatically by tolua 4.0a - angband on 01/14/02 00:02:44.
*/

#include "lua/tolua.h"

/* Exported function */
int tolua_player_open (lua_State* tolua_S);
void tolua_player_close (lua_State* tolua_S);

#include "angband.h"

/* function to register type */
static void toluaI_reg_types (lua_State* tolua_S)
{
 tolua_usertype(tolua_S,"player_race_mod");
 tolua_usertype(tolua_S,"player_race");
 tolua_usertype(tolua_S,"player_class");
 tolua_usertype(tolua_S,"object_type");
 tolua_usertype(tolua_S,"deity");
 tolua_usertype(tolua_S,"player_type");
}

/* error messages */
#define TOLUA_ERR_SELF tolua_error(tolua_S,"invalid 'self'")
#define TOLUA_ERR_ASSIGN tolua_error(tolua_S,"#vinvalid type in variable assignment.")

/* get function: name of class  deity */
static int toluaI_get_player_deity_name(lua_State* tolua_S)
{
  deity* self = (deity*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushstring(tolua_S,(const char*)self->name);
 return 1;
}

/* set function: name of class  deity */
static int toluaI_set_player_deity_name(lua_State* tolua_S)
{
  deity* self = (deity*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TSTRING,0))
 TOLUA_ERR_ASSIGN;
  self->name = ((cptr)  tolua_getstring(tolua_S,2,0));
 return 0;
}

/* get function: god_of of class  deity */
static int toluaI_get_player_deity_god_of(lua_State* tolua_S)
{
  deity* self = (deity*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushstring(tolua_S,(const char*)self->god_of);
 return 1;
}

/* set function: god_of of class  deity */
static int toluaI_set_player_deity_god_of(lua_State* tolua_S)
{
  deity* self = (deity*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TSTRING,0))
 TOLUA_ERR_ASSIGN;
  self->god_of = ((cptr)  tolua_getstring(tolua_S,2,0));
 return 0;
}

/* get function: grace_deduction of class  deity */
static int toluaI_get_player_deity_grace_deduction(lua_State* tolua_S)
{
  deity* self = (deity*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->grace_deduction);
 return 1;
}

/* set function: grace_deduction of class  deity */
static int toluaI_set_player_deity_grace_deduction(lua_State* tolua_S)
{
  deity* self = (deity*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->grace_deduction = ((byte)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: rarity of class  deity */
static int toluaI_get_player_deity_rarity(lua_State* tolua_S)
{
  deity* self = (deity*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->rarity);
 return 1;
}

/* set function: rarity of class  deity */
static int toluaI_set_player_deity_rarity(lua_State* tolua_S)
{
  deity* self = (deity*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->rarity = ((byte)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: race1 of class  deity */
static int toluaI_get_player_deity_race1(lua_State* tolua_S)
{
  deity* self = (deity*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->race1);
 return 1;
}

/* set function: race1 of class  deity */
static int toluaI_set_player_deity_race1(lua_State* tolua_S)
{
  deity* self = (deity*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->race1 = ((byte)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: race2 of class  deity */
static int toluaI_get_player_deity_race2(lua_State* tolua_S)
{
  deity* self = (deity*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->race2);
 return 1;
}

/* set function: race2 of class  deity */
static int toluaI_set_player_deity_race2(lua_State* tolua_S)
{
  deity* self = (deity*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->race2 = ((byte)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: desc of class  deity */
static int toluaI_get_player_deity_desc(lua_State* tolua_S)
{
  deity* self = (deity*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushstring(tolua_S,(const char*)self->desc);
 return 1;
}

/* set function: desc of class  deity */
static int toluaI_set_player_deity_desc(lua_State* tolua_S)
{
  deity* self = (deity*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TSTRING,0))
 TOLUA_ERR_ASSIGN;
  self->desc = ((cptr)  tolua_getstring(tolua_S,2,0));
 return 0;
}

/* get function: deity_info */
static int toluaI_get_player_deity_info(lua_State* tolua_S)
{
 int toluaI_index;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 tolua_error(tolua_S,"invalid type in array indexing.");
 toluaI_index = (int)tolua_getnumber(tolua_S,2,0)-1;
 if (toluaI_index<0 || toluaI_index>=MAX_GODS)
 tolua_error(tolua_S,"array indexing out of range.");
 tolua_pushusertype(tolua_S,(void*)&deity_info[toluaI_index],tolua_tag(tolua_S,"deity"));
 return 1;
}

/* set function: deity_info */
static int toluaI_set_player_deity_info(lua_State* tolua_S)
{
 int toluaI_index;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 tolua_error(tolua_S,"invalid type in array indexing.");
 toluaI_index = (int)tolua_getnumber(tolua_S,2,0)-1;
 if (toluaI_index<0 || toluaI_index>=MAX_GODS)
 tolua_error(tolua_S,"array indexing out of range.");
  deity_info[toluaI_index] = *((deity*)  tolua_getusertype(tolua_S,3,0));
 return 0;
}

/* get function: oldpy of class  player_type */
static int toluaI_get_player_player_type_oldpy(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->oldpy);
 return 1;
}

/* set function: oldpy of class  player_type */
static int toluaI_set_player_player_type_oldpy(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->oldpy = ((s16b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: oldpx of class  player_type */
static int toluaI_get_player_player_type_oldpx(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->oldpx);
 return 1;
}

/* set function: oldpx of class  player_type */
static int toluaI_set_player_player_type_oldpx(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->oldpx = ((s16b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: psex of class  player_type */
static int toluaI_get_player_player_type_psex(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->psex);
 return 1;
}

/* set function: psex of class  player_type */
static int toluaI_set_player_player_type_psex(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->psex = ((byte)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: prace of class  player_type */
static int toluaI_get_player_player_type_prace(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->prace);
 return 1;
}

/* set function: prace of class  player_type */
static int toluaI_set_player_player_type_prace(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->prace = ((byte)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: pracem of class  player_type */
static int toluaI_get_player_player_type_pracem(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->pracem);
 return 1;
}

/* set function: pracem of class  player_type */
static int toluaI_set_player_player_type_pracem(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->pracem = ((byte)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: pclass of class  player_type */
static int toluaI_get_player_player_type_pclass(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->pclass);
 return 1;
}

/* set function: pclass of class  player_type */
static int toluaI_set_player_player_type_pclass(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->pclass = ((byte)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: realm1 of class  player_type */
static int toluaI_get_player_player_type_realm1(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->realm1);
 return 1;
}

/* set function: realm1 of class  player_type */
static int toluaI_set_player_player_type_realm1(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->realm1 = ((u16b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: realm2 of class  player_type */
static int toluaI_get_player_player_type_realm2(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->realm2);
 return 1;
}

/* set function: realm2 of class  player_type */
static int toluaI_set_player_player_type_realm2(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->realm2 = ((u16b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: mimic_form of class  player_type */
static int toluaI_get_player_player_type_mimic_form(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->mimic_form);
 return 1;
}

/* set function: mimic_form of class  player_type */
static int toluaI_set_player_player_type_mimic_form(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->mimic_form = ((byte)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: oops of class  player_type */
static int toluaI_get_player_player_type_oops(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->oops);
 return 1;
}

/* set function: oops of class  player_type */
static int toluaI_set_player_player_type_oops(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->oops = ((byte)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: hitdie of class  player_type */
static int toluaI_get_player_player_type_hitdie(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->hitdie);
 return 1;
}

/* set function: hitdie of class  player_type */
static int toluaI_set_player_player_type_hitdie(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->hitdie = ((byte)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: expfact of class  player_type */
static int toluaI_get_player_player_type_expfact(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->expfact);
 return 1;
}

/* set function: expfact of class  player_type */
static int toluaI_set_player_player_type_expfact(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->expfact = ((u16b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: allow_one_death of class  player_type */
static int toluaI_get_player_player_type_allow_one_death(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->allow_one_death);
 return 1;
}

/* set function: allow_one_death of class  player_type */
static int toluaI_set_player_player_type_allow_one_death(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->allow_one_death = ((byte)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: age of class  player_type */
static int toluaI_get_player_player_type_age(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->age);
 return 1;
}

/* set function: age of class  player_type */
static int toluaI_set_player_player_type_age(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->age = ((s16b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: ht of class  player_type */
static int toluaI_get_player_player_type_ht(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->ht);
 return 1;
}

/* set function: ht of class  player_type */
static int toluaI_set_player_player_type_ht(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->ht = ((s16b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: wt of class  player_type */
static int toluaI_get_player_player_type_wt(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->wt);
 return 1;
}

/* set function: wt of class  player_type */
static int toluaI_set_player_player_type_wt(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->wt = ((s16b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: sc of class  player_type */
static int toluaI_get_player_player_type_sc(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->sc);
 return 1;
}

/* set function: sc of class  player_type */
static int toluaI_set_player_player_type_sc(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->sc = ((s16b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: au of class  player_type */
static int toluaI_get_player_player_type_au(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->au);
 return 1;
}

/* set function: au of class  player_type */
static int toluaI_set_player_player_type_au(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->au = ((s32b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: max_exp of class  player_type */
static int toluaI_get_player_player_type_max_exp(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->max_exp);
 return 1;
}

/* set function: max_exp of class  player_type */
static int toluaI_set_player_player_type_max_exp(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->max_exp = ((s32b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: exp of class  player_type */
static int toluaI_get_player_player_type_exp(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->exp);
 return 1;
}

/* set function: exp of class  player_type */
static int toluaI_set_player_player_type_exp(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->exp = ((s32b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: exp_frac of class  player_type */
static int toluaI_get_player_player_type_exp_frac(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->exp_frac);
 return 1;
}

/* set function: exp_frac of class  player_type */
static int toluaI_set_player_player_type_exp_frac(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->exp_frac = ((u16b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: lev of class  player_type */
static int toluaI_get_player_player_type_lev(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->lev);
 return 1;
}

/* set function: lev of class  player_type */
static int toluaI_set_player_player_type_lev(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->lev = ((s16b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: town_num of class  player_type */
static int toluaI_get_player_player_type_town_num(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->town_num);
 return 1;
}

/* set function: town_num of class  player_type */
static int toluaI_set_player_player_type_town_num(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->town_num = ((s16b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: inside_quest of class  player_type */
static int toluaI_get_player_player_type_inside_quest(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->inside_quest);
 return 1;
}

/* set function: inside_quest of class  player_type */
static int toluaI_set_player_player_type_inside_quest(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->inside_quest = ((s16b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: exit_bldg of class  player_type */
static int toluaI_get_player_player_type_exit_bldg(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->exit_bldg);
 return 1;
}

/* set function: exit_bldg of class  player_type */
static int toluaI_set_player_player_type_exit_bldg(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->exit_bldg = ((bool)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: wilderness_x of class  player_type */
static int toluaI_get_player_player_type_wilderness_x(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->wilderness_x);
 return 1;
}

/* set function: wilderness_x of class  player_type */
static int toluaI_set_player_player_type_wilderness_x(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->wilderness_x = ((s32b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: wilderness_y of class  player_type */
static int toluaI_get_player_player_type_wilderness_y(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->wilderness_y);
 return 1;
}

/* set function: wilderness_y of class  player_type */
static int toluaI_set_player_player_type_wilderness_y(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->wilderness_y = ((s32b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: wild_mode of class  player_type */
static int toluaI_get_player_player_type_wild_mode(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->wild_mode);
 return 1;
}

/* set function: wild_mode of class  player_type */
static int toluaI_set_player_player_type_wild_mode(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->wild_mode = ((bool)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: old_wild_mode of class  player_type */
static int toluaI_get_player_player_type_old_wild_mode(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->old_wild_mode);
 return 1;
}

/* set function: old_wild_mode of class  player_type */
static int toluaI_set_player_player_type_old_wild_mode(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->old_wild_mode = ((bool)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: mhp of class  player_type */
static int toluaI_get_player_player_type_mhp(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->mhp);
 return 1;
}

/* set function: mhp of class  player_type */
static int toluaI_set_player_player_type_mhp(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->mhp = ((s16b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: chp of class  player_type */
static int toluaI_get_player_player_type_chp(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->chp);
 return 1;
}

/* set function: chp of class  player_type */
static int toluaI_set_player_player_type_chp(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->chp = ((s16b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: chp_frac of class  player_type */
static int toluaI_get_player_player_type_chp_frac(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->chp_frac);
 return 1;
}

/* set function: chp_frac of class  player_type */
static int toluaI_set_player_player_type_chp_frac(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->chp_frac = ((u16b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: hp_mod of class  player_type */
static int toluaI_get_player_player_type_hp_mod(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->hp_mod);
 return 1;
}

/* set function: hp_mod of class  player_type */
static int toluaI_set_player_player_type_hp_mod(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->hp_mod = ((s16b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: msp of class  player_type */
static int toluaI_get_player_player_type_msp(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->msp);
 return 1;
}

/* set function: msp of class  player_type */
static int toluaI_set_player_player_type_msp(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->msp = ((s16b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: csp of class  player_type */
static int toluaI_get_player_player_type_csp(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->csp);
 return 1;
}

/* set function: csp of class  player_type */
static int toluaI_set_player_player_type_csp(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->csp = ((s16b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: csp_frac of class  player_type */
static int toluaI_get_player_player_type_csp_frac(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->csp_frac);
 return 1;
}

/* set function: csp_frac of class  player_type */
static int toluaI_set_player_player_type_csp_frac(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->csp_frac = ((u16b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: msane of class  player_type */
static int toluaI_get_player_player_type_msane(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->msane);
 return 1;
}

/* set function: msane of class  player_type */
static int toluaI_set_player_player_type_msane(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->msane = ((s16b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: csane of class  player_type */
static int toluaI_get_player_player_type_csane(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->csane);
 return 1;
}

/* set function: csane of class  player_type */
static int toluaI_set_player_player_type_csane(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->csane = ((s16b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: csane_frac of class  player_type */
static int toluaI_get_player_player_type_csane_frac(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->csane_frac);
 return 1;
}

/* set function: csane_frac of class  player_type */
static int toluaI_set_player_player_type_csane_frac(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->csane_frac = ((u16b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: mtp of class  player_type */
static int toluaI_get_player_player_type_mtp(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->mtp);
 return 1;
}

/* set function: mtp of class  player_type */
static int toluaI_set_player_player_type_mtp(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->mtp = ((s16b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: ctp of class  player_type */
static int toluaI_get_player_player_type_ctp(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->ctp);
 return 1;
}

/* set function: ctp of class  player_type */
static int toluaI_set_player_player_type_ctp(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->ctp = ((s16b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: tp_aux1 of class  player_type */
static int toluaI_get_player_player_type_tp_aux1(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->tp_aux1);
 return 1;
}

/* set function: tp_aux1 of class  player_type */
static int toluaI_set_player_player_type_tp_aux1(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->tp_aux1 = ((s16b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: tp_aux2 of class  player_type */
static int toluaI_get_player_player_type_tp_aux2(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->tp_aux2);
 return 1;
}

/* set function: tp_aux2 of class  player_type */
static int toluaI_set_player_player_type_tp_aux2(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->tp_aux2 = ((s16b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: grace of class  player_type */
static int toluaI_get_player_player_type_grace(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->grace);
 return 1;
}

/* set function: grace of class  player_type */
static int toluaI_set_player_player_type_grace(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->grace = ((s32b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: god_favor of class  player_type */
static int toluaI_get_player_player_type_god_favor(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->god_favor);
 return 1;
}

/* set function: god_favor of class  player_type */
static int toluaI_set_player_player_type_god_favor(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->god_favor = ((s32b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: pgod of class  player_type */
static int toluaI_get_player_player_type_pgod(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->pgod);
 return 1;
}

/* set function: pgod of class  player_type */
static int toluaI_set_player_player_type_pgod(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->pgod = ((byte)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: max_plv of class  player_type */
static int toluaI_get_player_player_type_max_plv(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->max_plv);
 return 1;
}

/* set function: max_plv of class  player_type */
static int toluaI_set_player_player_type_max_plv(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->max_plv = ((s16b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: stat_max of class  player_type */
static int toluaI_get_player_player_type_stat_max(lua_State* tolua_S)
{
 int toluaI_index;
  player_type* self;
 lua_pushstring(tolua_S,".self");
 lua_rawget(tolua_S,1);
 self = (player_type*)  lua_touserdata(tolua_S,-1);
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 tolua_error(tolua_S,"invalid type in array indexing.");
 toluaI_index = (int)tolua_getnumber(tolua_S,2,0)-1;
 if (toluaI_index<0 || toluaI_index>=6)
 tolua_error(tolua_S,"array indexing out of range.");
 tolua_pushnumber(tolua_S,(long)self->stat_max[toluaI_index]);
 return 1;
}

/* set function: stat_max of class  player_type */
static int toluaI_set_player_player_type_stat_max(lua_State* tolua_S)
{
 int toluaI_index;
  player_type* self;
 lua_pushstring(tolua_S,".self");
 lua_rawget(tolua_S,1);
 self = (player_type*)  lua_touserdata(tolua_S,-1);
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 tolua_error(tolua_S,"invalid type in array indexing.");
 toluaI_index = (int)tolua_getnumber(tolua_S,2,0)-1;
 if (toluaI_index<0 || toluaI_index>=6)
 tolua_error(tolua_S,"array indexing out of range.");
  self->stat_max[toluaI_index] = ((s16b)  tolua_getnumber(tolua_S,3,0));
 return 0;
}

/* get function: stat_cur of class  player_type */
static int toluaI_get_player_player_type_stat_cur(lua_State* tolua_S)
{
 int toluaI_index;
  player_type* self;
 lua_pushstring(tolua_S,".self");
 lua_rawget(tolua_S,1);
 self = (player_type*)  lua_touserdata(tolua_S,-1);
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 tolua_error(tolua_S,"invalid type in array indexing.");
 toluaI_index = (int)tolua_getnumber(tolua_S,2,0)-1;
 if (toluaI_index<0 || toluaI_index>=6)
 tolua_error(tolua_S,"array indexing out of range.");
 tolua_pushnumber(tolua_S,(long)self->stat_cur[toluaI_index]);
 return 1;
}

/* set function: stat_cur of class  player_type */
static int toluaI_set_player_player_type_stat_cur(lua_State* tolua_S)
{
 int toluaI_index;
  player_type* self;
 lua_pushstring(tolua_S,".self");
 lua_rawget(tolua_S,1);
 self = (player_type*)  lua_touserdata(tolua_S,-1);
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 tolua_error(tolua_S,"invalid type in array indexing.");
 toluaI_index = (int)tolua_getnumber(tolua_S,2,0)-1;
 if (toluaI_index<0 || toluaI_index>=6)
 tolua_error(tolua_S,"array indexing out of range.");
  self->stat_cur[toluaI_index] = ((s16b)  tolua_getnumber(tolua_S,3,0));
 return 0;
}

/* get function: luck_cur of class  player_type */
static int toluaI_get_player_player_type_luck_cur(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->luck_cur);
 return 1;
}

/* set function: luck_cur of class  player_type */
static int toluaI_set_player_player_type_luck_cur(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->luck_cur = ((s16b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: luck_max of class  player_type */
static int toluaI_get_player_player_type_luck_max(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->luck_max);
 return 1;
}

/* set function: luck_max of class  player_type */
static int toluaI_set_player_player_type_luck_max(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->luck_max = ((s16b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: luck_base of class  player_type */
static int toluaI_get_player_player_type_luck_base(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->luck_base);
 return 1;
}

/* set function: luck_base of class  player_type */
static int toluaI_set_player_player_type_luck_base(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->luck_base = ((s16b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: fast of class  player_type */
static int toluaI_get_player_player_type_fast(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->fast);
 return 1;
}

/* set function: fast of class  player_type */
static int toluaI_set_player_player_type_fast(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->fast = ((s16b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: lightspeed of class  player_type */
static int toluaI_get_player_player_type_lightspeed(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->lightspeed);
 return 1;
}

/* set function: lightspeed of class  player_type */
static int toluaI_set_player_player_type_lightspeed(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->lightspeed = ((s16b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: slow of class  player_type */
static int toluaI_get_player_player_type_slow(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->slow);
 return 1;
}

/* set function: slow of class  player_type */
static int toluaI_set_player_player_type_slow(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->slow = ((s16b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: blind of class  player_type */
static int toluaI_get_player_player_type_blind(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->blind);
 return 1;
}

/* set function: blind of class  player_type */
static int toluaI_set_player_player_type_blind(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->blind = ((s16b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: paralyzed of class  player_type */
static int toluaI_get_player_player_type_paralyzed(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->paralyzed);
 return 1;
}

/* set function: paralyzed of class  player_type */
static int toluaI_set_player_player_type_paralyzed(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->paralyzed = ((s16b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: confused of class  player_type */
static int toluaI_get_player_player_type_confused(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->confused);
 return 1;
}

/* set function: confused of class  player_type */
static int toluaI_set_player_player_type_confused(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->confused = ((s16b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: afraid of class  player_type */
static int toluaI_get_player_player_type_afraid(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->afraid);
 return 1;
}

/* set function: afraid of class  player_type */
static int toluaI_set_player_player_type_afraid(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->afraid = ((s16b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: image of class  player_type */
static int toluaI_get_player_player_type_image(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->image);
 return 1;
}

/* set function: image of class  player_type */
static int toluaI_set_player_player_type_image(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->image = ((s16b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: poisoned of class  player_type */
static int toluaI_get_player_player_type_poisoned(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->poisoned);
 return 1;
}

/* set function: poisoned of class  player_type */
static int toluaI_set_player_player_type_poisoned(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->poisoned = ((s16b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: cut of class  player_type */
static int toluaI_get_player_player_type_cut(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->cut);
 return 1;
}

/* set function: cut of class  player_type */
static int toluaI_set_player_player_type_cut(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->cut = ((s16b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: stun of class  player_type */
static int toluaI_get_player_player_type_stun(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->stun);
 return 1;
}

/* set function: stun of class  player_type */
static int toluaI_set_player_player_type_stun(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->stun = ((s16b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: protevil of class  player_type */
static int toluaI_get_player_player_type_protevil(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->protevil);
 return 1;
}

/* set function: protevil of class  player_type */
static int toluaI_set_player_player_type_protevil(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->protevil = ((s16b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: protgood of class  player_type */
static int toluaI_get_player_player_type_protgood(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->protgood);
 return 1;
}

/* set function: protgood of class  player_type */
static int toluaI_set_player_player_type_protgood(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->protgood = ((s16b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: protundead of class  player_type */
static int toluaI_get_player_player_type_protundead(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->protundead);
 return 1;
}

/* set function: protundead of class  player_type */
static int toluaI_set_player_player_type_protundead(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->protundead = ((s16b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: invuln of class  player_type */
static int toluaI_get_player_player_type_invuln(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->invuln);
 return 1;
}

/* set function: invuln of class  player_type */
static int toluaI_set_player_player_type_invuln(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->invuln = ((s16b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: hero of class  player_type */
static int toluaI_get_player_player_type_hero(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->hero);
 return 1;
}

/* set function: hero of class  player_type */
static int toluaI_set_player_player_type_hero(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->hero = ((s16b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: shero of class  player_type */
static int toluaI_get_player_player_type_shero(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->shero);
 return 1;
}

/* set function: shero of class  player_type */
static int toluaI_set_player_player_type_shero(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->shero = ((s16b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: shield of class  player_type */
static int toluaI_get_player_player_type_shield(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->shield);
 return 1;
}

/* set function: shield of class  player_type */
static int toluaI_set_player_player_type_shield(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->shield = ((s16b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: shield_power of class  player_type */
static int toluaI_get_player_player_type_shield_power(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->shield_power);
 return 1;
}

/* set function: shield_power of class  player_type */
static int toluaI_set_player_player_type_shield_power(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->shield_power = ((s16b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: shield_opt of class  player_type */
static int toluaI_get_player_player_type_shield_opt(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->shield_opt);
 return 1;
}

/* set function: shield_opt of class  player_type */
static int toluaI_set_player_player_type_shield_opt(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->shield_opt = ((s16b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: blessed of class  player_type */
static int toluaI_get_player_player_type_blessed(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->blessed);
 return 1;
}

/* set function: blessed of class  player_type */
static int toluaI_set_player_player_type_blessed(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->blessed = ((s16b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: tim_invis of class  player_type */
static int toluaI_get_player_player_type_tim_invis(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->tim_invis);
 return 1;
}

/* set function: tim_invis of class  player_type */
static int toluaI_set_player_player_type_tim_invis(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->tim_invis = ((s16b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: tim_infra of class  player_type */
static int toluaI_get_player_player_type_tim_infra(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->tim_infra);
 return 1;
}

/* set function: tim_infra of class  player_type */
static int toluaI_set_player_player_type_tim_infra(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->tim_infra = ((s16b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: oppose_acid of class  player_type */
static int toluaI_get_player_player_type_oppose_acid(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->oppose_acid);
 return 1;
}

/* set function: oppose_acid of class  player_type */
static int toluaI_set_player_player_type_oppose_acid(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->oppose_acid = ((s16b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: oppose_elec of class  player_type */
static int toluaI_get_player_player_type_oppose_elec(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->oppose_elec);
 return 1;
}

/* set function: oppose_elec of class  player_type */
static int toluaI_set_player_player_type_oppose_elec(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->oppose_elec = ((s16b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: oppose_fire of class  player_type */
static int toluaI_get_player_player_type_oppose_fire(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->oppose_fire);
 return 1;
}

/* set function: oppose_fire of class  player_type */
static int toluaI_set_player_player_type_oppose_fire(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->oppose_fire = ((s16b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: oppose_cold of class  player_type */
static int toluaI_get_player_player_type_oppose_cold(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->oppose_cold);
 return 1;
}

/* set function: oppose_cold of class  player_type */
static int toluaI_set_player_player_type_oppose_cold(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->oppose_cold = ((s16b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: oppose_pois of class  player_type */
static int toluaI_get_player_player_type_oppose_pois(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->oppose_pois);
 return 1;
}

/* set function: oppose_pois of class  player_type */
static int toluaI_set_player_player_type_oppose_pois(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->oppose_pois = ((s16b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: oppose_ld of class  player_type */
static int toluaI_get_player_player_type_oppose_ld(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->oppose_ld);
 return 1;
}

/* set function: oppose_ld of class  player_type */
static int toluaI_set_player_player_type_oppose_ld(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->oppose_ld = ((s16b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: oppose_cc of class  player_type */
static int toluaI_get_player_player_type_oppose_cc(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->oppose_cc);
 return 1;
}

/* set function: oppose_cc of class  player_type */
static int toluaI_set_player_player_type_oppose_cc(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->oppose_cc = ((s16b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: oppose_ss of class  player_type */
static int toluaI_get_player_player_type_oppose_ss(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->oppose_ss);
 return 1;
}

/* set function: oppose_ss of class  player_type */
static int toluaI_set_player_player_type_oppose_ss(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->oppose_ss = ((s16b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: oppose_nex of class  player_type */
static int toluaI_get_player_player_type_oppose_nex(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->oppose_nex);
 return 1;
}

/* set function: oppose_nex of class  player_type */
static int toluaI_set_player_player_type_oppose_nex(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->oppose_nex = ((s16b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: tim_esp of class  player_type */
static int toluaI_get_player_player_type_tim_esp(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->tim_esp);
 return 1;
}

/* set function: tim_esp of class  player_type */
static int toluaI_set_player_player_type_tim_esp(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->tim_esp = ((s16b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: tim_wraith of class  player_type */
static int toluaI_get_player_player_type_tim_wraith(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->tim_wraith);
 return 1;
}

/* set function: tim_wraith of class  player_type */
static int toluaI_set_player_player_type_tim_wraith(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->tim_wraith = ((s16b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: tim_ffall of class  player_type */
static int toluaI_get_player_player_type_tim_ffall(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->tim_ffall);
 return 1;
}

/* set function: tim_ffall of class  player_type */
static int toluaI_set_player_player_type_tim_ffall(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->tim_ffall = ((s16b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: tim_fire_aura of class  player_type */
static int toluaI_get_player_player_type_tim_fire_aura(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->tim_fire_aura);
 return 1;
}

/* set function: tim_fire_aura of class  player_type */
static int toluaI_set_player_player_type_tim_fire_aura(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->tim_fire_aura = ((s16b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: resist_magic of class  player_type */
static int toluaI_get_player_player_type_resist_magic(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->resist_magic);
 return 1;
}

/* set function: resist_magic of class  player_type */
static int toluaI_set_player_player_type_resist_magic(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->resist_magic = ((s16b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: tim_invisible of class  player_type */
static int toluaI_get_player_player_type_tim_invisible(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->tim_invisible);
 return 1;
}

/* set function: tim_invisible of class  player_type */
static int toluaI_set_player_player_type_tim_invisible(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->tim_invisible = ((s16b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: tim_inv_pow of class  player_type */
static int toluaI_get_player_player_type_tim_inv_pow(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->tim_inv_pow);
 return 1;
}

/* set function: tim_inv_pow of class  player_type */
static int toluaI_set_player_player_type_tim_inv_pow(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->tim_inv_pow = ((s16b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: tim_mimic of class  player_type */
static int toluaI_get_player_player_type_tim_mimic(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->tim_mimic);
 return 1;
}

/* set function: tim_mimic of class  player_type */
static int toluaI_set_player_player_type_tim_mimic(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->tim_mimic = ((s16b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: tim_lite of class  player_type */
static int toluaI_get_player_player_type_tim_lite(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->tim_lite);
 return 1;
}

/* set function: tim_lite of class  player_type */
static int toluaI_set_player_player_type_tim_lite(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->tim_lite = ((s16b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: holy of class  player_type */
static int toluaI_get_player_player_type_holy(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->holy);
 return 1;
}

/* set function: holy of class  player_type */
static int toluaI_set_player_player_type_holy(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->holy = ((s16b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: walk_water of class  player_type */
static int toluaI_get_player_player_type_walk_water(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->walk_water);
 return 1;
}

/* set function: walk_water of class  player_type */
static int toluaI_set_player_player_type_walk_water(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->walk_water = ((s16b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: tim_mental_barrier of class  player_type */
static int toluaI_get_player_player_type_tim_mental_barrier(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->tim_mental_barrier);
 return 1;
}

/* set function: tim_mental_barrier of class  player_type */
static int toluaI_set_player_player_type_tim_mental_barrier(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->tim_mental_barrier = ((s16b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: strike of class  player_type */
static int toluaI_get_player_player_type_strike(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->strike);
 return 1;
}

/* set function: strike of class  player_type */
static int toluaI_set_player_player_type_strike(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->strike = ((s16b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: meditation of class  player_type */
static int toluaI_get_player_player_type_meditation(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->meditation);
 return 1;
}

/* set function: meditation of class  player_type */
static int toluaI_set_player_player_type_meditation(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->meditation = ((s16b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: tim_reflect of class  player_type */
static int toluaI_get_player_player_type_tim_reflect(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->tim_reflect);
 return 1;
}

/* set function: tim_reflect of class  player_type */
static int toluaI_set_player_player_type_tim_reflect(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->tim_reflect = ((s16b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: tim_res_time of class  player_type */
static int toluaI_get_player_player_type_tim_res_time(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->tim_res_time);
 return 1;
}

/* set function: tim_res_time of class  player_type */
static int toluaI_set_player_player_type_tim_res_time(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->tim_res_time = ((s16b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: tim_deadly of class  player_type */
static int toluaI_get_player_player_type_tim_deadly(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->tim_deadly);
 return 1;
}

/* set function: tim_deadly of class  player_type */
static int toluaI_set_player_player_type_tim_deadly(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->tim_deadly = ((s16b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: prob_travel of class  player_type */
static int toluaI_get_player_player_type_prob_travel(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->prob_travel);
 return 1;
}

/* set function: prob_travel of class  player_type */
static int toluaI_set_player_player_type_prob_travel(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->prob_travel = ((s16b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: disrupt_shield of class  player_type */
static int toluaI_get_player_player_type_disrupt_shield(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->disrupt_shield);
 return 1;
}

/* set function: disrupt_shield of class  player_type */
static int toluaI_set_player_player_type_disrupt_shield(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->disrupt_shield = ((s16b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: parasite of class  player_type */
static int toluaI_get_player_player_type_parasite(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->parasite);
 return 1;
}

/* set function: parasite of class  player_type */
static int toluaI_set_player_player_type_parasite(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->parasite = ((s16b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: parasite_r_idx of class  player_type */
static int toluaI_get_player_player_type_parasite_r_idx(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->parasite_r_idx);
 return 1;
}

/* set function: parasite_r_idx of class  player_type */
static int toluaI_set_player_player_type_parasite_r_idx(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->parasite_r_idx = ((s16b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: loan of class  player_type */
static int toluaI_get_player_player_type_loan(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->loan);
 return 1;
}

/* set function: loan of class  player_type */
static int toluaI_set_player_player_type_loan(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->loan = ((u32b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: loan_time of class  player_type */
static int toluaI_get_player_player_type_loan_time(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->loan_time);
 return 1;
}

/* set function: loan_time of class  player_type */
static int toluaI_set_player_player_type_loan_time(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->loan_time = ((u32b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: immov_cntr of class  player_type */
static int toluaI_get_player_player_type_immov_cntr(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->immov_cntr);
 return 1;
}

/* set function: immov_cntr of class  player_type */
static int toluaI_set_player_player_type_immov_cntr(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->immov_cntr = ((s16b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: chaos_patron of class  player_type */
static int toluaI_get_player_player_type_chaos_patron(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->chaos_patron);
 return 1;
}

/* set function: chaos_patron of class  player_type */
static int toluaI_set_player_player_type_chaos_patron(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->chaos_patron = ((s16b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: muta1 of class  player_type */
static int toluaI_get_player_player_type_muta1(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->muta1);
 return 1;
}

/* set function: muta1 of class  player_type */
static int toluaI_set_player_player_type_muta1(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->muta1 = ((u32b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: muta2 of class  player_type */
static int toluaI_get_player_player_type_muta2(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->muta2);
 return 1;
}

/* set function: muta2 of class  player_type */
static int toluaI_set_player_player_type_muta2(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->muta2 = ((u32b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: muta3 of class  player_type */
static int toluaI_get_player_player_type_muta3(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->muta3);
 return 1;
}

/* set function: muta3 of class  player_type */
static int toluaI_set_player_player_type_muta3(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->muta3 = ((u32b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: recall_dungeon of class  player_type */
static int toluaI_get_player_player_type_recall_dungeon(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->recall_dungeon);
 return 1;
}

/* set function: recall_dungeon of class  player_type */
static int toluaI_set_player_player_type_recall_dungeon(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->recall_dungeon = ((s16b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: word_recall of class  player_type */
static int toluaI_get_player_player_type_word_recall(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->word_recall);
 return 1;
}

/* set function: word_recall of class  player_type */
static int toluaI_set_player_player_type_word_recall(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->word_recall = ((s16b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: energy of class  player_type */
static int toluaI_get_player_player_type_energy(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->energy);
 return 1;
}

/* set function: energy of class  player_type */
static int toluaI_set_player_player_type_energy(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->energy = ((s32b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: food of class  player_type */
static int toluaI_get_player_player_type_food(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->food);
 return 1;
}

/* set function: food of class  player_type */
static int toluaI_set_player_player_type_food(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->food = ((s16b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: confusing of class  player_type */
static int toluaI_get_player_player_type_confusing(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->confusing);
 return 1;
}

/* set function: confusing of class  player_type */
static int toluaI_set_player_player_type_confusing(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->confusing = ((byte)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: searching of class  player_type */
static int toluaI_get_player_player_type_searching(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->searching);
 return 1;
}

/* set function: searching of class  player_type */
static int toluaI_set_player_player_type_searching(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->searching = ((byte)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: new_spells of class  player_type */
static int toluaI_get_player_player_type_new_spells(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->new_spells);
 return 1;
}

/* set function: new_spells of class  player_type */
static int toluaI_set_player_player_type_new_spells(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->new_spells = ((s16b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: old_spells of class  player_type */
static int toluaI_get_player_player_type_old_spells(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->old_spells);
 return 1;
}

/* set function: old_spells of class  player_type */
static int toluaI_set_player_player_type_old_spells(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->old_spells = ((s16b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: xtra_spells of class  player_type */
static int toluaI_get_player_player_type_xtra_spells(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->xtra_spells);
 return 1;
}

/* set function: xtra_spells of class  player_type */
static int toluaI_set_player_player_type_xtra_spells(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->xtra_spells = ((s16b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: cur_lite of class  player_type */
static int toluaI_get_player_player_type_cur_lite(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->cur_lite);
 return 1;
}

/* set function: cur_lite of class  player_type */
static int toluaI_set_player_player_type_cur_lite(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->cur_lite = ((s16b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: notice of class  player_type */
static int toluaI_get_player_player_type_notice(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->notice);
 return 1;
}

/* set function: notice of class  player_type */
static int toluaI_set_player_player_type_notice(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->notice = ((u32b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: update of class  player_type */
static int toluaI_get_player_player_type_update(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->update);
 return 1;
}

/* set function: update of class  player_type */
static int toluaI_set_player_player_type_update(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->update = ((u32b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: redraw of class  player_type */
static int toluaI_get_player_player_type_redraw(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->redraw);
 return 1;
}

/* set function: redraw of class  player_type */
static int toluaI_set_player_player_type_redraw(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->redraw = ((u32b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: window of class  player_type */
static int toluaI_get_player_player_type_window(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->window);
 return 1;
}

/* set function: window of class  player_type */
static int toluaI_set_player_player_type_window(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->window = ((u32b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: stat_use of class  player_type */
static int toluaI_get_player_player_type_stat_use(lua_State* tolua_S)
{
 int toluaI_index;
  player_type* self;
 lua_pushstring(tolua_S,".self");
 lua_rawget(tolua_S,1);
 self = (player_type*)  lua_touserdata(tolua_S,-1);
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 tolua_error(tolua_S,"invalid type in array indexing.");
 toluaI_index = (int)tolua_getnumber(tolua_S,2,0)-1;
 if (toluaI_index<0 || toluaI_index>=6)
 tolua_error(tolua_S,"array indexing out of range.");
 tolua_pushnumber(tolua_S,(long)self->stat_use[toluaI_index]);
 return 1;
}

/* set function: stat_use of class  player_type */
static int toluaI_set_player_player_type_stat_use(lua_State* tolua_S)
{
 int toluaI_index;
  player_type* self;
 lua_pushstring(tolua_S,".self");
 lua_rawget(tolua_S,1);
 self = (player_type*)  lua_touserdata(tolua_S,-1);
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 tolua_error(tolua_S,"invalid type in array indexing.");
 toluaI_index = (int)tolua_getnumber(tolua_S,2,0)-1;
 if (toluaI_index<0 || toluaI_index>=6)
 tolua_error(tolua_S,"array indexing out of range.");
  self->stat_use[toluaI_index] = ((s16b)  tolua_getnumber(tolua_S,3,0));
 return 0;
}

/* get function: stat_top of class  player_type */
static int toluaI_get_player_player_type_stat_top(lua_State* tolua_S)
{
 int toluaI_index;
  player_type* self;
 lua_pushstring(tolua_S,".self");
 lua_rawget(tolua_S,1);
 self = (player_type*)  lua_touserdata(tolua_S,-1);
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 tolua_error(tolua_S,"invalid type in array indexing.");
 toluaI_index = (int)tolua_getnumber(tolua_S,2,0)-1;
 if (toluaI_index<0 || toluaI_index>=6)
 tolua_error(tolua_S,"array indexing out of range.");
 tolua_pushnumber(tolua_S,(long)self->stat_top[toluaI_index]);
 return 1;
}

/* set function: stat_top of class  player_type */
static int toluaI_set_player_player_type_stat_top(lua_State* tolua_S)
{
 int toluaI_index;
  player_type* self;
 lua_pushstring(tolua_S,".self");
 lua_rawget(tolua_S,1);
 self = (player_type*)  lua_touserdata(tolua_S,-1);
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 tolua_error(tolua_S,"invalid type in array indexing.");
 toluaI_index = (int)tolua_getnumber(tolua_S,2,0)-1;
 if (toluaI_index<0 || toluaI_index>=6)
 tolua_error(tolua_S,"array indexing out of range.");
  self->stat_top[toluaI_index] = ((s16b)  tolua_getnumber(tolua_S,3,0));
 return 0;
}

/* get function: stat_add of class  player_type */
static int toluaI_get_player_player_type_stat_add(lua_State* tolua_S)
{
 int toluaI_index;
  player_type* self;
 lua_pushstring(tolua_S,".self");
 lua_rawget(tolua_S,1);
 self = (player_type*)  lua_touserdata(tolua_S,-1);
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 tolua_error(tolua_S,"invalid type in array indexing.");
 toluaI_index = (int)tolua_getnumber(tolua_S,2,0)-1;
 if (toluaI_index<0 || toluaI_index>=6)
 tolua_error(tolua_S,"array indexing out of range.");
 tolua_pushnumber(tolua_S,(long)self->stat_add[toluaI_index]);
 return 1;
}

/* set function: stat_add of class  player_type */
static int toluaI_set_player_player_type_stat_add(lua_State* tolua_S)
{
 int toluaI_index;
  player_type* self;
 lua_pushstring(tolua_S,".self");
 lua_rawget(tolua_S,1);
 self = (player_type*)  lua_touserdata(tolua_S,-1);
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 tolua_error(tolua_S,"invalid type in array indexing.");
 toluaI_index = (int)tolua_getnumber(tolua_S,2,0)-1;
 if (toluaI_index<0 || toluaI_index>=6)
 tolua_error(tolua_S,"array indexing out of range.");
  self->stat_add[toluaI_index] = ((s16b)  tolua_getnumber(tolua_S,3,0));
 return 0;
}

/* get function: stat_ind of class  player_type */
static int toluaI_get_player_player_type_stat_ind(lua_State* tolua_S)
{
 int toluaI_index;
  player_type* self;
 lua_pushstring(tolua_S,".self");
 lua_rawget(tolua_S,1);
 self = (player_type*)  lua_touserdata(tolua_S,-1);
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 tolua_error(tolua_S,"invalid type in array indexing.");
 toluaI_index = (int)tolua_getnumber(tolua_S,2,0)-1;
 if (toluaI_index<0 || toluaI_index>=6)
 tolua_error(tolua_S,"array indexing out of range.");
 tolua_pushnumber(tolua_S,(long)self->stat_ind[toluaI_index]);
 return 1;
}

/* set function: stat_ind of class  player_type */
static int toluaI_set_player_player_type_stat_ind(lua_State* tolua_S)
{
 int toluaI_index;
  player_type* self;
 lua_pushstring(tolua_S,".self");
 lua_rawget(tolua_S,1);
 self = (player_type*)  lua_touserdata(tolua_S,-1);
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 tolua_error(tolua_S,"invalid type in array indexing.");
 toluaI_index = (int)tolua_getnumber(tolua_S,2,0)-1;
 if (toluaI_index<0 || toluaI_index>=6)
 tolua_error(tolua_S,"array indexing out of range.");
  self->stat_ind[toluaI_index] = ((s16b)  tolua_getnumber(tolua_S,3,0));
 return 0;
}

/* get function: stat_cnt of class  player_type */
static int toluaI_get_player_player_type_stat_cnt(lua_State* tolua_S)
{
 int toluaI_index;
  player_type* self;
 lua_pushstring(tolua_S,".self");
 lua_rawget(tolua_S,1);
 self = (player_type*)  lua_touserdata(tolua_S,-1);
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 tolua_error(tolua_S,"invalid type in array indexing.");
 toluaI_index = (int)tolua_getnumber(tolua_S,2,0)-1;
 if (toluaI_index<0 || toluaI_index>=6)
 tolua_error(tolua_S,"array indexing out of range.");
 tolua_pushnumber(tolua_S,(long)self->stat_cnt[toluaI_index]);
 return 1;
}

/* set function: stat_cnt of class  player_type */
static int toluaI_set_player_player_type_stat_cnt(lua_State* tolua_S)
{
 int toluaI_index;
  player_type* self;
 lua_pushstring(tolua_S,".self");
 lua_rawget(tolua_S,1);
 self = (player_type*)  lua_touserdata(tolua_S,-1);
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 tolua_error(tolua_S,"invalid type in array indexing.");
 toluaI_index = (int)tolua_getnumber(tolua_S,2,0)-1;
 if (toluaI_index<0 || toluaI_index>=6)
 tolua_error(tolua_S,"array indexing out of range.");
  self->stat_cnt[toluaI_index] = ((s16b)  tolua_getnumber(tolua_S,3,0));
 return 0;
}

/* get function: stat_los of class  player_type */
static int toluaI_get_player_player_type_stat_los(lua_State* tolua_S)
{
 int toluaI_index;
  player_type* self;
 lua_pushstring(tolua_S,".self");
 lua_rawget(tolua_S,1);
 self = (player_type*)  lua_touserdata(tolua_S,-1);
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 tolua_error(tolua_S,"invalid type in array indexing.");
 toluaI_index = (int)tolua_getnumber(tolua_S,2,0)-1;
 if (toluaI_index<0 || toluaI_index>=6)
 tolua_error(tolua_S,"array indexing out of range.");
 tolua_pushnumber(tolua_S,(long)self->stat_los[toluaI_index]);
 return 1;
}

/* set function: stat_los of class  player_type */
static int toluaI_set_player_player_type_stat_los(lua_State* tolua_S)
{
 int toluaI_index;
  player_type* self;
 lua_pushstring(tolua_S,".self");
 lua_rawget(tolua_S,1);
 self = (player_type*)  lua_touserdata(tolua_S,-1);
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 tolua_error(tolua_S,"invalid type in array indexing.");
 toluaI_index = (int)tolua_getnumber(tolua_S,2,0)-1;
 if (toluaI_index<0 || toluaI_index>=6)
 tolua_error(tolua_S,"array indexing out of range.");
  self->stat_los[toluaI_index] = ((s16b)  tolua_getnumber(tolua_S,3,0));
 return 0;
}

/* get function: immune_acid of class  player_type */
static int toluaI_get_player_player_type_immune_acid(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->immune_acid);
 return 1;
}

/* set function: immune_acid of class  player_type */
static int toluaI_set_player_player_type_immune_acid(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->immune_acid = ((bool)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: immune_elec of class  player_type */
static int toluaI_get_player_player_type_immune_elec(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->immune_elec);
 return 1;
}

/* set function: immune_elec of class  player_type */
static int toluaI_set_player_player_type_immune_elec(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->immune_elec = ((bool)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: immune_fire of class  player_type */
static int toluaI_get_player_player_type_immune_fire(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->immune_fire);
 return 1;
}

/* set function: immune_fire of class  player_type */
static int toluaI_set_player_player_type_immune_fire(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->immune_fire = ((bool)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: immune_cold of class  player_type */
static int toluaI_get_player_player_type_immune_cold(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->immune_cold);
 return 1;
}

/* set function: immune_cold of class  player_type */
static int toluaI_set_player_player_type_immune_cold(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->immune_cold = ((bool)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: immune_neth of class  player_type */
static int toluaI_get_player_player_type_immune_neth(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->immune_neth);
 return 1;
}

/* set function: immune_neth of class  player_type */
static int toluaI_set_player_player_type_immune_neth(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->immune_neth = ((bool)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: resist_acid of class  player_type */
static int toluaI_get_player_player_type_resist_acid(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->resist_acid);
 return 1;
}

/* set function: resist_acid of class  player_type */
static int toluaI_set_player_player_type_resist_acid(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->resist_acid = ((bool)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: resist_elec of class  player_type */
static int toluaI_get_player_player_type_resist_elec(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->resist_elec);
 return 1;
}

/* set function: resist_elec of class  player_type */
static int toluaI_set_player_player_type_resist_elec(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->resist_elec = ((bool)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: resist_fire of class  player_type */
static int toluaI_get_player_player_type_resist_fire(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->resist_fire);
 return 1;
}

/* set function: resist_fire of class  player_type */
static int toluaI_set_player_player_type_resist_fire(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->resist_fire = ((bool)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: resist_cold of class  player_type */
static int toluaI_get_player_player_type_resist_cold(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->resist_cold);
 return 1;
}

/* set function: resist_cold of class  player_type */
static int toluaI_set_player_player_type_resist_cold(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->resist_cold = ((bool)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: resist_pois of class  player_type */
static int toluaI_get_player_player_type_resist_pois(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->resist_pois);
 return 1;
}

/* set function: resist_pois of class  player_type */
static int toluaI_set_player_player_type_resist_pois(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->resist_pois = ((bool)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: resist_conf of class  player_type */
static int toluaI_get_player_player_type_resist_conf(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->resist_conf);
 return 1;
}

/* set function: resist_conf of class  player_type */
static int toluaI_set_player_player_type_resist_conf(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->resist_conf = ((bool)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: resist_sound of class  player_type */
static int toluaI_get_player_player_type_resist_sound(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->resist_sound);
 return 1;
}

/* set function: resist_sound of class  player_type */
static int toluaI_set_player_player_type_resist_sound(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->resist_sound = ((bool)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: resist_lite of class  player_type */
static int toluaI_get_player_player_type_resist_lite(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->resist_lite);
 return 1;
}

/* set function: resist_lite of class  player_type */
static int toluaI_set_player_player_type_resist_lite(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->resist_lite = ((bool)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: resist_dark of class  player_type */
static int toluaI_get_player_player_type_resist_dark(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->resist_dark);
 return 1;
}

/* set function: resist_dark of class  player_type */
static int toluaI_set_player_player_type_resist_dark(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->resist_dark = ((bool)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: resist_chaos of class  player_type */
static int toluaI_get_player_player_type_resist_chaos(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->resist_chaos);
 return 1;
}

/* set function: resist_chaos of class  player_type */
static int toluaI_set_player_player_type_resist_chaos(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->resist_chaos = ((bool)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: resist_disen of class  player_type */
static int toluaI_get_player_player_type_resist_disen(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->resist_disen);
 return 1;
}

/* set function: resist_disen of class  player_type */
static int toluaI_set_player_player_type_resist_disen(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->resist_disen = ((bool)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: resist_shard of class  player_type */
static int toluaI_get_player_player_type_resist_shard(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->resist_shard);
 return 1;
}

/* set function: resist_shard of class  player_type */
static int toluaI_set_player_player_type_resist_shard(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->resist_shard = ((bool)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: resist_nexus of class  player_type */
static int toluaI_get_player_player_type_resist_nexus(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->resist_nexus);
 return 1;
}

/* set function: resist_nexus of class  player_type */
static int toluaI_set_player_player_type_resist_nexus(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->resist_nexus = ((bool)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: resist_blind of class  player_type */
static int toluaI_get_player_player_type_resist_blind(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->resist_blind);
 return 1;
}

/* set function: resist_blind of class  player_type */
static int toluaI_set_player_player_type_resist_blind(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->resist_blind = ((bool)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: resist_neth of class  player_type */
static int toluaI_get_player_player_type_resist_neth(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->resist_neth);
 return 1;
}

/* set function: resist_neth of class  player_type */
static int toluaI_set_player_player_type_resist_neth(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->resist_neth = ((bool)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: resist_fear of class  player_type */
static int toluaI_get_player_player_type_resist_fear(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->resist_fear);
 return 1;
}

/* set function: resist_fear of class  player_type */
static int toluaI_set_player_player_type_resist_fear(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->resist_fear = ((bool)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: resist_continuum of class  player_type */
static int toluaI_get_player_player_type_resist_continuum(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->resist_continuum);
 return 1;
}

/* set function: resist_continuum of class  player_type */
static int toluaI_set_player_player_type_resist_continuum(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->resist_continuum = ((bool)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: sensible_fire of class  player_type */
static int toluaI_get_player_player_type_sensible_fire(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->sensible_fire);
 return 1;
}

/* set function: sensible_fire of class  player_type */
static int toluaI_set_player_player_type_sensible_fire(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->sensible_fire = ((bool)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: reflect of class  player_type */
static int toluaI_get_player_player_type_reflect(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->reflect);
 return 1;
}

/* set function: reflect of class  player_type */
static int toluaI_set_player_player_type_reflect(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->reflect = ((bool)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: sh_fire of class  player_type */
static int toluaI_get_player_player_type_sh_fire(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->sh_fire);
 return 1;
}

/* set function: sh_fire of class  player_type */
static int toluaI_set_player_player_type_sh_fire(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->sh_fire = ((bool)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: sh_elec of class  player_type */
static int toluaI_get_player_player_type_sh_elec(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->sh_elec);
 return 1;
}

/* set function: sh_elec of class  player_type */
static int toluaI_set_player_player_type_sh_elec(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->sh_elec = ((bool)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: wraith_form of class  player_type */
static int toluaI_get_player_player_type_wraith_form(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->wraith_form);
 return 1;
}

/* set function: wraith_form of class  player_type */
static int toluaI_set_player_player_type_wraith_form(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->wraith_form = ((bool)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: anti_magic of class  player_type */
static int toluaI_get_player_player_type_anti_magic(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->anti_magic);
 return 1;
}

/* set function: anti_magic of class  player_type */
static int toluaI_set_player_player_type_anti_magic(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->anti_magic = ((bool)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: anti_tele of class  player_type */
static int toluaI_get_player_player_type_anti_tele(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->anti_tele);
 return 1;
}

/* set function: anti_tele of class  player_type */
static int toluaI_set_player_player_type_anti_tele(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->anti_tele = ((bool)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: sustain_str of class  player_type */
static int toluaI_get_player_player_type_sustain_str(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->sustain_str);
 return 1;
}

/* set function: sustain_str of class  player_type */
static int toluaI_set_player_player_type_sustain_str(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->sustain_str = ((bool)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: sustain_int of class  player_type */
static int toluaI_get_player_player_type_sustain_int(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->sustain_int);
 return 1;
}

/* set function: sustain_int of class  player_type */
static int toluaI_set_player_player_type_sustain_int(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->sustain_int = ((bool)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: sustain_wis of class  player_type */
static int toluaI_get_player_player_type_sustain_wis(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->sustain_wis);
 return 1;
}

/* set function: sustain_wis of class  player_type */
static int toluaI_set_player_player_type_sustain_wis(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->sustain_wis = ((bool)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: sustain_dex of class  player_type */
static int toluaI_get_player_player_type_sustain_dex(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->sustain_dex);
 return 1;
}

/* set function: sustain_dex of class  player_type */
static int toluaI_set_player_player_type_sustain_dex(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->sustain_dex = ((bool)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: sustain_con of class  player_type */
static int toluaI_get_player_player_type_sustain_con(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->sustain_con);
 return 1;
}

/* set function: sustain_con of class  player_type */
static int toluaI_set_player_player_type_sustain_con(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->sustain_con = ((bool)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: sustain_chr of class  player_type */
static int toluaI_get_player_player_type_sustain_chr(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->sustain_chr);
 return 1;
}

/* set function: sustain_chr of class  player_type */
static int toluaI_set_player_player_type_sustain_chr(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->sustain_chr = ((bool)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: aggravate of class  player_type */
static int toluaI_get_player_player_type_aggravate(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->aggravate);
 return 1;
}

/* set function: aggravate of class  player_type */
static int toluaI_set_player_player_type_aggravate(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->aggravate = ((bool)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: teleport of class  player_type */
static int toluaI_get_player_player_type_teleport(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->teleport);
 return 1;
}

/* set function: teleport of class  player_type */
static int toluaI_set_player_player_type_teleport(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->teleport = ((bool)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: exp_drain of class  player_type */
static int toluaI_get_player_player_type_exp_drain(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->exp_drain);
 return 1;
}

/* set function: exp_drain of class  player_type */
static int toluaI_set_player_player_type_exp_drain(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->exp_drain = ((bool)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: drain_mana of class  player_type */
static int toluaI_get_player_player_type_drain_mana(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->drain_mana);
 return 1;
}

/* set function: drain_mana of class  player_type */
static int toluaI_set_player_player_type_drain_mana(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->drain_mana = ((byte)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: drain_life of class  player_type */
static int toluaI_get_player_player_type_drain_life(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->drain_life);
 return 1;
}

/* set function: drain_life of class  player_type */
static int toluaI_set_player_player_type_drain_life(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->drain_life = ((byte)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: climb of class  player_type */
static int toluaI_get_player_player_type_climb(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->climb);
 return 1;
}

/* set function: climb of class  player_type */
static int toluaI_set_player_player_type_climb(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->climb = ((bool)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: fly of class  player_type */
static int toluaI_get_player_player_type_fly(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->fly);
 return 1;
}

/* set function: fly of class  player_type */
static int toluaI_set_player_player_type_fly(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->fly = ((bool)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: ffall of class  player_type */
static int toluaI_get_player_player_type_ffall(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->ffall);
 return 1;
}

/* set function: ffall of class  player_type */
static int toluaI_set_player_player_type_ffall(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->ffall = ((bool)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: lite of class  player_type */
static int toluaI_get_player_player_type_lite(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->lite);
 return 1;
}

/* set function: lite of class  player_type */
static int toluaI_set_player_player_type_lite(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->lite = ((bool)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: free_act of class  player_type */
static int toluaI_get_player_player_type_free_act(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->free_act);
 return 1;
}

/* set function: free_act of class  player_type */
static int toluaI_set_player_player_type_free_act(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->free_act = ((bool)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: see_inv of class  player_type */
static int toluaI_get_player_player_type_see_inv(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->see_inv);
 return 1;
}

/* set function: see_inv of class  player_type */
static int toluaI_set_player_player_type_see_inv(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->see_inv = ((bool)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: regenerate of class  player_type */
static int toluaI_get_player_player_type_regenerate(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->regenerate);
 return 1;
}

/* set function: regenerate of class  player_type */
static int toluaI_set_player_player_type_regenerate(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->regenerate = ((bool)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: hold_life of class  player_type */
static int toluaI_get_player_player_type_hold_life(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->hold_life);
 return 1;
}

/* set function: hold_life of class  player_type */
static int toluaI_set_player_player_type_hold_life(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->hold_life = ((bool)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: telepathy of class  player_type */
static int toluaI_get_player_player_type_telepathy(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->telepathy);
 return 1;
}

/* set function: telepathy of class  player_type */
static int toluaI_set_player_player_type_telepathy(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->telepathy = ((u32b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: slow_digest of class  player_type */
static int toluaI_get_player_player_type_slow_digest(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->slow_digest);
 return 1;
}

/* set function: slow_digest of class  player_type */
static int toluaI_set_player_player_type_slow_digest(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->slow_digest = ((bool)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: bless_blade of class  player_type */
static int toluaI_get_player_player_type_bless_blade(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->bless_blade);
 return 1;
}

/* set function: bless_blade of class  player_type */
static int toluaI_set_player_player_type_bless_blade(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->bless_blade = ((bool)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: xtra_might of class  player_type */
static int toluaI_get_player_player_type_xtra_might(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->xtra_might);
 return 1;
}

/* set function: xtra_might of class  player_type */
static int toluaI_set_player_player_type_xtra_might(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->xtra_might = ((byte)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: impact of class  player_type */
static int toluaI_get_player_player_type_impact(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->impact);
 return 1;
}

/* set function: impact of class  player_type */
static int toluaI_set_player_player_type_impact(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->impact = ((bool)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: auto_id of class  player_type */
static int toluaI_get_player_player_type_auto_id(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->auto_id);
 return 1;
}

/* set function: auto_id of class  player_type */
static int toluaI_set_player_player_type_auto_id(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->auto_id = ((bool)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: dis_to_h of class  player_type */
static int toluaI_get_player_player_type_dis_to_h(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->dis_to_h);
 return 1;
}

/* set function: dis_to_h of class  player_type */
static int toluaI_set_player_player_type_dis_to_h(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->dis_to_h = ((s16b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: dis_to_d of class  player_type */
static int toluaI_get_player_player_type_dis_to_d(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->dis_to_d);
 return 1;
}

/* set function: dis_to_d of class  player_type */
static int toluaI_set_player_player_type_dis_to_d(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->dis_to_d = ((s16b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: dis_to_a of class  player_type */
static int toluaI_get_player_player_type_dis_to_a(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->dis_to_a);
 return 1;
}

/* set function: dis_to_a of class  player_type */
static int toluaI_set_player_player_type_dis_to_a(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->dis_to_a = ((s16b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: dis_ac of class  player_type */
static int toluaI_get_player_player_type_dis_ac(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->dis_ac);
 return 1;
}

/* set function: dis_ac of class  player_type */
static int toluaI_set_player_player_type_dis_ac(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->dis_ac = ((s16b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: to_m of class  player_type */
static int toluaI_get_player_player_type_to_m(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->to_m);
 return 1;
}

/* set function: to_m of class  player_type */
static int toluaI_set_player_player_type_to_m(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->to_m = ((s16b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: to_s of class  player_type */
static int toluaI_get_player_player_type_to_s(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->to_s);
 return 1;
}

/* set function: to_s of class  player_type */
static int toluaI_set_player_player_type_to_s(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->to_s = ((s16b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: to_h of class  player_type */
static int toluaI_get_player_player_type_to_h(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->to_h);
 return 1;
}

/* set function: to_h of class  player_type */
static int toluaI_set_player_player_type_to_h(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->to_h = ((s16b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: to_d of class  player_type */
static int toluaI_get_player_player_type_to_d(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->to_d);
 return 1;
}

/* set function: to_d of class  player_type */
static int toluaI_set_player_player_type_to_d(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->to_d = ((s16b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: to_a of class  player_type */
static int toluaI_get_player_player_type_to_a(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->to_a);
 return 1;
}

/* set function: to_a of class  player_type */
static int toluaI_set_player_player_type_to_a(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->to_a = ((s16b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: ac of class  player_type */
static int toluaI_get_player_player_type_ac(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->ac);
 return 1;
}

/* set function: ac of class  player_type */
static int toluaI_set_player_player_type_ac(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->ac = ((s16b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: antimagic of class  player_type */
static int toluaI_get_player_player_type_antimagic(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->antimagic);
 return 1;
}

/* set function: antimagic of class  player_type */
static int toluaI_set_player_player_type_antimagic(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->antimagic = ((byte)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: antimagic_dis of class  player_type */
static int toluaI_get_player_player_type_antimagic_dis(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->antimagic_dis);
 return 1;
}

/* set function: antimagic_dis of class  player_type */
static int toluaI_set_player_player_type_antimagic_dis(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->antimagic_dis = ((byte)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: see_infra of class  player_type */
static int toluaI_get_player_player_type_see_infra(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->see_infra);
 return 1;
}

/* set function: see_infra of class  player_type */
static int toluaI_set_player_player_type_see_infra(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->see_infra = ((s16b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: skill_dis of class  player_type */
static int toluaI_get_player_player_type_skill_dis(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->skill_dis);
 return 1;
}

/* set function: skill_dis of class  player_type */
static int toluaI_set_player_player_type_skill_dis(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->skill_dis = ((s16b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: skill_dev of class  player_type */
static int toluaI_get_player_player_type_skill_dev(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->skill_dev);
 return 1;
}

/* set function: skill_dev of class  player_type */
static int toluaI_set_player_player_type_skill_dev(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->skill_dev = ((s16b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: skill_sav of class  player_type */
static int toluaI_get_player_player_type_skill_sav(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->skill_sav);
 return 1;
}

/* set function: skill_sav of class  player_type */
static int toluaI_set_player_player_type_skill_sav(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->skill_sav = ((s16b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: skill_stl of class  player_type */
static int toluaI_get_player_player_type_skill_stl(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->skill_stl);
 return 1;
}

/* set function: skill_stl of class  player_type */
static int toluaI_set_player_player_type_skill_stl(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->skill_stl = ((s16b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: skill_srh of class  player_type */
static int toluaI_get_player_player_type_skill_srh(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->skill_srh);
 return 1;
}

/* set function: skill_srh of class  player_type */
static int toluaI_set_player_player_type_skill_srh(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->skill_srh = ((s16b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: skill_fos of class  player_type */
static int toluaI_get_player_player_type_skill_fos(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->skill_fos);
 return 1;
}

/* set function: skill_fos of class  player_type */
static int toluaI_set_player_player_type_skill_fos(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->skill_fos = ((s16b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: skill_thn of class  player_type */
static int toluaI_get_player_player_type_skill_thn(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->skill_thn);
 return 1;
}

/* set function: skill_thn of class  player_type */
static int toluaI_set_player_player_type_skill_thn(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->skill_thn = ((s16b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: skill_thb of class  player_type */
static int toluaI_get_player_player_type_skill_thb(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->skill_thb);
 return 1;
}

/* set function: skill_thb of class  player_type */
static int toluaI_set_player_player_type_skill_thb(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->skill_thb = ((s16b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: skill_tht of class  player_type */
static int toluaI_get_player_player_type_skill_tht(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->skill_tht);
 return 1;
}

/* set function: skill_tht of class  player_type */
static int toluaI_set_player_player_type_skill_tht(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->skill_tht = ((s16b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: skill_dig of class  player_type */
static int toluaI_get_player_player_type_skill_dig(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->skill_dig);
 return 1;
}

/* set function: skill_dig of class  player_type */
static int toluaI_set_player_player_type_skill_dig(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->skill_dig = ((s16b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: companion_killed of class  player_type */
static int toluaI_get_player_player_type_companion_killed(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->companion_killed);
 return 1;
}

/* set function: companion_killed of class  player_type */
static int toluaI_set_player_player_type_companion_killed(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->companion_killed = ((s16b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: black_breath of class  player_type */
static int toluaI_get_player_player_type_black_breath(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->black_breath);
 return 1;
}

/* set function: black_breath of class  player_type */
static int toluaI_set_player_player_type_black_breath(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->black_breath = ((bool)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: body_monster of class  player_type */
static int toluaI_get_player_player_type_body_monster(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->body_monster);
 return 1;
}

/* set function: body_monster of class  player_type */
static int toluaI_set_player_player_type_body_monster(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->body_monster = ((u16b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: powers_mod of class  player_type */
static int toluaI_get_player_player_type_powers_mod(lua_State* tolua_S)
{
 int toluaI_index;
  player_type* self;
 lua_pushstring(tolua_S,".self");
 lua_rawget(tolua_S,1);
 self = (player_type*)  lua_touserdata(tolua_S,-1);
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 tolua_error(tolua_S,"invalid type in array indexing.");
 toluaI_index = (int)tolua_getnumber(tolua_S,2,0)-1;
 if (toluaI_index<0 || toluaI_index>=POWER_MAX_INIT)
 tolua_error(tolua_S,"array indexing out of range.");
 tolua_pushnumber(tolua_S,(long)self->powers_mod[toluaI_index]);
 return 1;
}

/* set function: powers_mod of class  player_type */
static int toluaI_set_player_player_type_powers_mod(lua_State* tolua_S)
{
 int toluaI_index;
  player_type* self;
 lua_pushstring(tolua_S,".self");
 lua_rawget(tolua_S,1);
 self = (player_type*)  lua_touserdata(tolua_S,-1);
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 tolua_error(tolua_S,"invalid type in array indexing.");
 toluaI_index = (int)tolua_getnumber(tolua_S,2,0)-1;
 if (toluaI_index<0 || toluaI_index>=POWER_MAX_INIT)
 tolua_error(tolua_S,"array indexing out of range.");
  self->powers_mod[toluaI_index] = ((bool)  tolua_getnumber(tolua_S,3,0));
 return 0;
}

/* get function: powers of class  player_type */
static int toluaI_get_player_player_type_powers(lua_State* tolua_S)
{
 int toluaI_index;
  player_type* self;
 lua_pushstring(tolua_S,".self");
 lua_rawget(tolua_S,1);
 self = (player_type*)  lua_touserdata(tolua_S,-1);
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 tolua_error(tolua_S,"invalid type in array indexing.");
 toluaI_index = (int)tolua_getnumber(tolua_S,2,0)-1;
 if (toluaI_index<0 || toluaI_index>=power_max)
 tolua_error(tolua_S,"array indexing out of range.");
 tolua_pushnumber(tolua_S,(long)self->powers[toluaI_index]);
 return 1;
}

/* set function: powers of class  player_type */
static int toluaI_set_player_player_type_powers(lua_State* tolua_S)
{
 int toluaI_index;
  player_type* self;
 lua_pushstring(tolua_S,".self");
 lua_rawget(tolua_S,1);
 self = (player_type*)  lua_touserdata(tolua_S,-1);
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 tolua_error(tolua_S,"invalid type in array indexing.");
 toluaI_index = (int)tolua_getnumber(tolua_S,2,0)-1;
 if (toluaI_index<0 || toluaI_index>=power_max)
 tolua_error(tolua_S,"array indexing out of range.");
  self->powers[toluaI_index] = ((bool)  tolua_getnumber(tolua_S,3,0));
 return 0;
}

/* get function: leaving of class  player_type */
static int toluaI_get_player_player_type_leaving(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->leaving);
 return 1;
}

/* set function: leaving of class  player_type */
static int toluaI_set_player_player_type_leaving(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->leaving = ((bool)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: title of class  player_race */
static int toluaI_get_player_player_race_title(lua_State* tolua_S)
{
  player_race* self = (player_race*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->title);
 return 1;
}

/* set function: title of class  player_race */
static int toluaI_set_player_player_race_title(lua_State* tolua_S)
{
  player_race* self = (player_race*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->title = ((s32b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: desc of class  player_race */
static int toluaI_get_player_player_race_desc(lua_State* tolua_S)
{
  player_race* self = (player_race*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->desc);
 return 1;
}

/* set function: desc of class  player_race */
static int toluaI_set_player_player_race_desc(lua_State* tolua_S)
{
  player_race* self = (player_race*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->desc = ((s32b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: title of class  player_race_mod */
static int toluaI_get_player_player_race_mod_title(lua_State* tolua_S)
{
  player_race_mod* self = (player_race_mod*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->title);
 return 1;
}

/* set function: title of class  player_race_mod */
static int toluaI_set_player_player_race_mod_title(lua_State* tolua_S)
{
  player_race_mod* self = (player_race_mod*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->title = ((s32b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: desc of class  player_race_mod */
static int toluaI_get_player_player_race_mod_desc(lua_State* tolua_S)
{
  player_race_mod* self = (player_race_mod*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->desc);
 return 1;
}

/* set function: desc of class  player_race_mod */
static int toluaI_set_player_player_race_mod_desc(lua_State* tolua_S)
{
  player_race_mod* self = (player_race_mod*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->desc = ((s32b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: place of class  player_race_mod */
static int toluaI_get_player_player_race_mod_place(lua_State* tolua_S)
{
  player_race_mod* self = (player_race_mod*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->place);
 return 1;
}

/* set function: place of class  player_race_mod */
static int toluaI_set_player_player_race_mod_place(lua_State* tolua_S)
{
  player_race_mod* self = (player_race_mod*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->place = ((bool)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: energy_use */
static int toluaI_get_player_energy_use(lua_State* tolua_S)
{
 tolua_pushnumber(tolua_S,(long)energy_use);
 return 1;
}

/* set function: energy_use */
static int toluaI_set_player_energy_use(lua_State* tolua_S)
{
 if (!tolua_istype(tolua_S,1,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  energy_use = ((s32b)  tolua_getnumber(tolua_S,1,0));
 return 0;
}

/* get function: p_ptr */
static int toluaI_get_player_p_ptr(lua_State* tolua_S)
{
 tolua_pushusertype(tolua_S,(void*)p_ptr,tolua_tag(tolua_S,"player_type"));
 return 1;
}

/* set function: p_ptr */
static int toluaI_set_player_p_ptr(lua_State* tolua_S)
{
 if (!tolua_istype(tolua_S,1,tolua_tag(tolua_S,"player_type"),0))
 TOLUA_ERR_ASSIGN;
  p_ptr = ((player_type*)  tolua_getusertype(tolua_S,1,0));
 return 0;
}

/* get function: max_rp_idx */
static int toluaI_get_player_max_rp_idx(lua_State* tolua_S)
{
 tolua_pushnumber(tolua_S,(long)max_rp_idx);
 return 1;
}

/* set function: max_rp_idx */
static int toluaI_set_player_max_rp_idx(lua_State* tolua_S)
{
 if (!tolua_istype(tolua_S,1,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  max_rp_idx = ((u16b)  tolua_getnumber(tolua_S,1,0));
 return 0;
}

/* get function: race_info */
static int toluaI_get_player_race_info(lua_State* tolua_S)
{
 int toluaI_index;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 tolua_error(tolua_S,"invalid type in array indexing.");
 toluaI_index = (int)tolua_getnumber(tolua_S,2,0)-1;
 if (toluaI_index<0 || toluaI_index>=max_rp_idx)
 tolua_error(tolua_S,"array indexing out of range.");
 tolua_pushusertype(tolua_S,(void*)&race_info[toluaI_index],tolua_tag(tolua_S,"player_race"));
 return 1;
}

/* set function: race_info */
static int toluaI_set_player_race_info(lua_State* tolua_S)
{
 int toluaI_index;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 tolua_error(tolua_S,"invalid type in array indexing.");
 toluaI_index = (int)tolua_getnumber(tolua_S,2,0)-1;
 if (toluaI_index<0 || toluaI_index>=max_rp_idx)
 tolua_error(tolua_S,"array indexing out of range.");
  race_info[toluaI_index] = *((player_race*)  tolua_getusertype(tolua_S,3,0));
 return 0;
}

/* get function: rp_name */
static int toluaI_get_player_rp_name(lua_State* tolua_S)
{
 tolua_pushstring(tolua_S,(const char*)rp_name);
 return 1;
}

/* set function: rp_name */
static int toluaI_set_player_rp_name(lua_State* tolua_S)
{
 if (!tolua_istype(tolua_S,1,LUA_TSTRING,0))
 TOLUA_ERR_ASSIGN;
  rp_name = ((char*)  tolua_getstring(tolua_S,1,0));
 return 0;
}

/* get function: rp_text */
static int toluaI_get_player_rp_text(lua_State* tolua_S)
{
 tolua_pushstring(tolua_S,(const char*)rp_text);
 return 1;
}

/* set function: rp_text */
static int toluaI_set_player_rp_text(lua_State* tolua_S)
{
 if (!tolua_istype(tolua_S,1,LUA_TSTRING,0))
 TOLUA_ERR_ASSIGN;
  rp_text = ((char*)  tolua_getstring(tolua_S,1,0));
 return 0;
}

/* get function: max_rmp_idx */
static int toluaI_get_player_max_rmp_idx(lua_State* tolua_S)
{
 tolua_pushnumber(tolua_S,(long)max_rmp_idx);
 return 1;
}

/* set function: max_rmp_idx */
static int toluaI_set_player_max_rmp_idx(lua_State* tolua_S)
{
 if (!tolua_istype(tolua_S,1,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  max_rmp_idx = ((u16b)  tolua_getnumber(tolua_S,1,0));
 return 0;
}

/* get function: race_mod_info */
static int toluaI_get_player_race_mod_info(lua_State* tolua_S)
{
 int toluaI_index;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 tolua_error(tolua_S,"invalid type in array indexing.");
 toluaI_index = (int)tolua_getnumber(tolua_S,2,0)-1;
 if (toluaI_index<0 || toluaI_index>=max_rmp_idx)
 tolua_error(tolua_S,"array indexing out of range.");
 tolua_pushusertype(tolua_S,(void*)&race_mod_info[toluaI_index],tolua_tag(tolua_S,"player_race_mod"));
 return 1;
}

/* set function: race_mod_info */
static int toluaI_set_player_race_mod_info(lua_State* tolua_S)
{
 int toluaI_index;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 tolua_error(tolua_S,"invalid type in array indexing.");
 toluaI_index = (int)tolua_getnumber(tolua_S,2,0)-1;
 if (toluaI_index<0 || toluaI_index>=max_rmp_idx)
 tolua_error(tolua_S,"array indexing out of range.");
  race_mod_info[toluaI_index] = *((player_race_mod*)  tolua_getusertype(tolua_S,3,0));
 return 0;
}

/* get function: rmp_name */
static int toluaI_get_player_rmp_name(lua_State* tolua_S)
{
 tolua_pushstring(tolua_S,(const char*)rmp_name);
 return 1;
}

/* set function: rmp_name */
static int toluaI_set_player_rmp_name(lua_State* tolua_S)
{
 if (!tolua_istype(tolua_S,1,LUA_TSTRING,0))
 TOLUA_ERR_ASSIGN;
  rmp_name = ((char*)  tolua_getstring(tolua_S,1,0));
 return 0;
}

/* get function: rmp_text */
static int toluaI_get_player_rmp_text(lua_State* tolua_S)
{
 tolua_pushstring(tolua_S,(const char*)rmp_text);
 return 1;
}

/* set function: rmp_text */
static int toluaI_set_player_rmp_text(lua_State* tolua_S)
{
 if (!tolua_istype(tolua_S,1,LUA_TSTRING,0))
 TOLUA_ERR_ASSIGN;
  rmp_text = ((char*)  tolua_getstring(tolua_S,1,0));
 return 0;
}

/* get function: class_info */
static int toluaI_get_player_class_info(lua_State* tolua_S)
{
 int toluaI_index;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 tolua_error(tolua_S,"invalid type in array indexing.");
 toluaI_index = (int)tolua_getnumber(tolua_S,2,0)-1;
 if (toluaI_index<0 || toluaI_index>=max_c_idx)
 tolua_error(tolua_S,"array indexing out of range.");
 tolua_pushusertype(tolua_S,(void*)&class_info[toluaI_index],tolua_tag(tolua_S,"player_class"));
 return 1;
}

/* set function: class_info */
static int toluaI_set_player_class_info(lua_State* tolua_S)
{
 int toluaI_index;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 tolua_error(tolua_S,"invalid type in array indexing.");
 toluaI_index = (int)tolua_getnumber(tolua_S,2,0)-1;
 if (toluaI_index<0 || toluaI_index>=max_c_idx)
 tolua_error(tolua_S,"array indexing out of range.");
  class_info[toluaI_index] = *((player_class*)  tolua_getusertype(tolua_S,3,0));
 return 0;
}

/* get function: c_name */
static int toluaI_get_player_c_name(lua_State* tolua_S)
{
 tolua_pushstring(tolua_S,(const char*)c_name);
 return 1;
}

/* set function: c_name */
static int toluaI_set_player_c_name(lua_State* tolua_S)
{
 if (!tolua_istype(tolua_S,1,LUA_TSTRING,0))
 TOLUA_ERR_ASSIGN;
  c_name = ((char*)  tolua_getstring(tolua_S,1,0));
 return 0;
}

/* get function: c_text */
static int toluaI_get_player_c_text(lua_State* tolua_S)
{
 tolua_pushstring(tolua_S,(const char*)c_text);
 return 1;
}

/* set function: c_text */
static int toluaI_set_player_c_text(lua_State* tolua_S)
{
 if (!tolua_istype(tolua_S,1,LUA_TSTRING,0))
 TOLUA_ERR_ASSIGN;
  c_text = ((char*)  tolua_getstring(tolua_S,1,0));
 return 0;
}

/* get function: py */
static int toluaI_get_player_py(lua_State* tolua_S)
{
 tolua_pushnumber(tolua_S,(long)py);
 return 1;
}

/* set function: py */
static int toluaI_set_player_py(lua_State* tolua_S)
{
 if (!tolua_istype(tolua_S,1,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  py = ((s16b)  tolua_getnumber(tolua_S,1,0));
 return 0;
}

/* get function: px */
static int toluaI_get_player_px(lua_State* tolua_S)
{
 tolua_pushnumber(tolua_S,(long)px);
 return 1;
}

/* set function: px */
static int toluaI_set_player_px(lua_State* tolua_S)
{
 if (!tolua_istype(tolua_S,1,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  px = ((s16b)  tolua_getnumber(tolua_S,1,0));
 return 0;
}

/* function: set_parasite */
static int toluaI_player_set_parasite00(lua_State* tolua_S)
{
 if (
 !tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
 !tolua_istype(tolua_S,2,LUA_TNUMBER,0) ||
 !tolua_isnoobj(tolua_S,3)
 )
 goto tolua_lerror;
 else
 {
  int v = ((int)  tolua_getnumber(tolua_S,1,0));
  int r = ((int)  tolua_getnumber(tolua_S,2,0));
 {
  bool toluaI_ret = (bool)  set_parasite(v,r);
 tolua_pushnumber(tolua_S,(long)toluaI_ret);
 }
 }
 return 1;
tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'set_parasite'.");
 return 0;
}

/* function: set_disrupt_shield */
static int toluaI_player_set_disrupt_shield00(lua_State* tolua_S)
{
 if (
 !tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
 !tolua_isnoobj(tolua_S,2)
 )
 goto tolua_lerror;
 else
 {
  int v = ((int)  tolua_getnumber(tolua_S,1,0));
 {
  bool toluaI_ret = (bool)  set_disrupt_shield(v);
 tolua_pushnumber(tolua_S,(long)toluaI_ret);
 }
 }
 return 1;
tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'set_disrupt_shield'.");
 return 0;
}

/* function: set_prob_travel */
static int toluaI_player_set_prob_travel00(lua_State* tolua_S)
{
 if (
 !tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
 !tolua_isnoobj(tolua_S,2)
 )
 goto tolua_lerror;
 else
 {
  int v = ((int)  tolua_getnumber(tolua_S,1,0));
 {
  bool toluaI_ret = (bool)  set_prob_travel(v);
 tolua_pushnumber(tolua_S,(long)toluaI_ret);
 }
 }
 return 1;
tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'set_prob_travel'.");
 return 0;
}

/* function: set_tim_deadly */
static int toluaI_player_set_tim_deadly00(lua_State* tolua_S)
{
 if (
 !tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
 !tolua_isnoobj(tolua_S,2)
 )
 goto tolua_lerror;
 else
 {
  int v = ((int)  tolua_getnumber(tolua_S,1,0));
 {
  bool toluaI_ret = (bool)  set_tim_deadly(v);
 tolua_pushnumber(tolua_S,(long)toluaI_ret);
 }
 }
 return 1;
tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'set_tim_deadly'.");
 return 0;
}

/* function: set_tim_res_time */
static int toluaI_player_set_tim_res_time00(lua_State* tolua_S)
{
 if (
 !tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
 !tolua_isnoobj(tolua_S,2)
 )
 goto tolua_lerror;
 else
 {
  int v = ((int)  tolua_getnumber(tolua_S,1,0));
 {
  bool toluaI_ret = (bool)  set_tim_res_time(v);
 tolua_pushnumber(tolua_S,(long)toluaI_ret);
 }
 }
 return 1;
tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'set_tim_res_time'.");
 return 0;
}

/* function: set_tim_reflect */
static int toluaI_player_set_tim_reflect00(lua_State* tolua_S)
{
 if (
 !tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
 !tolua_isnoobj(tolua_S,2)
 )
 goto tolua_lerror;
 else
 {
  int v = ((int)  tolua_getnumber(tolua_S,1,0));
 {
  bool toluaI_ret = (bool)  set_tim_reflect(v);
 tolua_pushnumber(tolua_S,(long)toluaI_ret);
 }
 }
 return 1;
tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'set_tim_reflect'.");
 return 0;
}

/* function: set_meditation */
static int toluaI_player_set_meditation00(lua_State* tolua_S)
{
 if (
 !tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
 !tolua_isnoobj(tolua_S,2)
 )
 goto tolua_lerror;
 else
 {
  int v = ((int)  tolua_getnumber(tolua_S,1,0));
 {
  bool toluaI_ret = (bool)  set_meditation(v);
 tolua_pushnumber(tolua_S,(long)toluaI_ret);
 }
 }
 return 1;
tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'set_meditation'.");
 return 0;
}

/* function: set_strike */
static int toluaI_player_set_strike00(lua_State* tolua_S)
{
 if (
 !tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
 !tolua_isnoobj(tolua_S,2)
 )
 goto tolua_lerror;
 else
 {
  int v = ((int)  tolua_getnumber(tolua_S,1,0));
 {
  bool toluaI_ret = (bool)  set_strike(v);
 tolua_pushnumber(tolua_S,(long)toluaI_ret);
 }
 }
 return 1;
tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'set_strike'.");
 return 0;
}

/* function: set_walk_water */
static int toluaI_player_set_walk_water00(lua_State* tolua_S)
{
 if (
 !tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
 !tolua_isnoobj(tolua_S,2)
 )
 goto tolua_lerror;
 else
 {
  int v = ((int)  tolua_getnumber(tolua_S,1,0));
 {
  bool toluaI_ret = (bool)  set_walk_water(v);
 tolua_pushnumber(tolua_S,(long)toluaI_ret);
 }
 }
 return 1;
tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'set_walk_water'.");
 return 0;
}

/* function: set_tim_ffall */
static int toluaI_player_set_tim_ffall00(lua_State* tolua_S)
{
 if (
 !tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
 !tolua_isnoobj(tolua_S,2)
 )
 goto tolua_lerror;
 else
 {
  int v = ((int)  tolua_getnumber(tolua_S,1,0));
 {
  bool toluaI_ret = (bool)  set_tim_ffall(v);
 tolua_pushnumber(tolua_S,(long)toluaI_ret);
 }
 }
 return 1;
tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'set_tim_ffall'.");
 return 0;
}

/* function: set_tim_fire_aura */
static int toluaI_player_set_tim_fire_aura00(lua_State* tolua_S)
{
 if (
 !tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
 !tolua_isnoobj(tolua_S,2)
 )
 goto tolua_lerror;
 else
 {
  int v = ((int)  tolua_getnumber(tolua_S,1,0));
 {
  bool toluaI_ret = (bool)  set_tim_fire_aura(v);
 tolua_pushnumber(tolua_S,(long)toluaI_ret);
 }
 }
 return 1;
tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'set_tim_fire_aura'.");
 return 0;
}

/* function: set_holy */
static int toluaI_player_set_holy00(lua_State* tolua_S)
{
 if (
 !tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
 !tolua_isnoobj(tolua_S,2)
 )
 goto tolua_lerror;
 else
 {
  int v = ((int)  tolua_getnumber(tolua_S,1,0));
 {
  bool toluaI_ret = (bool)  set_holy(v);
 tolua_pushnumber(tolua_S,(long)toluaI_ret);
 }
 }
 return 1;
tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'set_holy'.");
 return 0;
}

/* function: set_grace */
static int toluaI_player_set_grace00(lua_State* tolua_S)
{
 if (
 !tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
 !tolua_isnoobj(tolua_S,2)
 )
 goto tolua_lerror;
 else
 {
  s32b v = ((s32b)  tolua_getnumber(tolua_S,1,0));
 {
  set_grace(v);
 }
 }
 return 0;
tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'set_grace'.");
 return 0;
}

/* function: set_mimic */
static int toluaI_player_set_mimic00(lua_State* tolua_S)
{
 if (
 !tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
 !tolua_istype(tolua_S,2,LUA_TNUMBER,0) ||
 !tolua_isnoobj(tolua_S,3)
 )
 goto tolua_lerror;
 else
 {
  int v = ((int)  tolua_getnumber(tolua_S,1,0));
  int p = ((int)  tolua_getnumber(tolua_S,2,0));
 {
  bool toluaI_ret = (bool)  set_mimic(v,p);
 tolua_pushnumber(tolua_S,(long)toluaI_ret);
 }
 }
 return 1;
tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'set_mimic'.");
 return 0;
}

/* function: set_no_breeders */
static int toluaI_player_set_no_breeders00(lua_State* tolua_S)
{
 if (
 !tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
 !tolua_isnoobj(tolua_S,2)
 )
 goto tolua_lerror;
 else
 {
  int v = ((int)  tolua_getnumber(tolua_S,1,0));
 {
  bool toluaI_ret = (bool)  set_no_breeders(v);
 tolua_pushnumber(tolua_S,(long)toluaI_ret);
 }
 }
 return 1;
tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'set_no_breeders'.");
 return 0;
}

/* function: set_invis */
static int toluaI_player_set_invis00(lua_State* tolua_S)
{
 if (
 !tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
 !tolua_istype(tolua_S,2,LUA_TNUMBER,0) ||
 !tolua_isnoobj(tolua_S,3)
 )
 goto tolua_lerror;
 else
 {
  int v = ((int)  tolua_getnumber(tolua_S,1,0));
  int p = ((int)  tolua_getnumber(tolua_S,2,0));
 {
  bool toluaI_ret = (bool)  set_invis(v,p);
 tolua_pushnumber(tolua_S,(long)toluaI_ret);
 }
 }
 return 1;
tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'set_invis'.");
 return 0;
}

/* function: set_lite */
static int toluaI_player_set_lite00(lua_State* tolua_S)
{
 if (
 !tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
 !tolua_isnoobj(tolua_S,2)
 )
 goto tolua_lerror;
 else
 {
  int v = ((int)  tolua_getnumber(tolua_S,1,0));
 {
  bool toluaI_ret = (bool)  set_lite(v);
 tolua_pushnumber(tolua_S,(long)toluaI_ret);
 }
 }
 return 1;
tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'set_lite'.");
 return 0;
}

/* function: set_blind */
static int toluaI_player_set_blind00(lua_State* tolua_S)
{
 if (
 !tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
 !tolua_isnoobj(tolua_S,2)
 )
 goto tolua_lerror;
 else
 {
  int v = ((int)  tolua_getnumber(tolua_S,1,0));
 {
  bool toluaI_ret = (bool)  set_blind(v);
 tolua_pushnumber(tolua_S,(long)toluaI_ret);
 }
 }
 return 1;
tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'set_blind'.");
 return 0;
}

/* function: set_confused */
static int toluaI_player_set_confused00(lua_State* tolua_S)
{
 if (
 !tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
 !tolua_isnoobj(tolua_S,2)
 )
 goto tolua_lerror;
 else
 {
  int v = ((int)  tolua_getnumber(tolua_S,1,0));
 {
  bool toluaI_ret = (bool)  set_confused(v);
 tolua_pushnumber(tolua_S,(long)toluaI_ret);
 }
 }
 return 1;
tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'set_confused'.");
 return 0;
}

/* function: set_poisoned */
static int toluaI_player_set_poisoned00(lua_State* tolua_S)
{
 if (
 !tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
 !tolua_isnoobj(tolua_S,2)
 )
 goto tolua_lerror;
 else
 {
  int v = ((int)  tolua_getnumber(tolua_S,1,0));
 {
  bool toluaI_ret = (bool)  set_poisoned(v);
 tolua_pushnumber(tolua_S,(long)toluaI_ret);
 }
 }
 return 1;
tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'set_poisoned'.");
 return 0;
}

/* function: set_afraid */
static int toluaI_player_set_afraid00(lua_State* tolua_S)
{
 if (
 !tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
 !tolua_isnoobj(tolua_S,2)
 )
 goto tolua_lerror;
 else
 {
  int v = ((int)  tolua_getnumber(tolua_S,1,0));
 {
  bool toluaI_ret = (bool)  set_afraid(v);
 tolua_pushnumber(tolua_S,(long)toluaI_ret);
 }
 }
 return 1;
tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'set_afraid'.");
 return 0;
}

/* function: set_paralyzed */
static int toluaI_player_set_paralyzed00(lua_State* tolua_S)
{
 if (
 !tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
 !tolua_isnoobj(tolua_S,2)
 )
 goto tolua_lerror;
 else
 {
  int v = ((int)  tolua_getnumber(tolua_S,1,0));
 {
  bool toluaI_ret = (bool)  set_paralyzed(v);
 tolua_pushnumber(tolua_S,(long)toluaI_ret);
 }
 }
 return 1;
tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'set_paralyzed'.");
 return 0;
}

/* function: set_image */
static int toluaI_player_set_image00(lua_State* tolua_S)
{
 if (
 !tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
 !tolua_isnoobj(tolua_S,2)
 )
 goto tolua_lerror;
 else
 {
  int v = ((int)  tolua_getnumber(tolua_S,1,0));
 {
  bool toluaI_ret = (bool)  set_image(v);
 tolua_pushnumber(tolua_S,(long)toluaI_ret);
 }
 }
 return 1;
tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'set_image'.");
 return 0;
}

/* function: set_fast */
static int toluaI_player_set_fast00(lua_State* tolua_S)
{
 if (
 !tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
 !tolua_isnoobj(tolua_S,2)
 )
 goto tolua_lerror;
 else
 {
  int v = ((int)  tolua_getnumber(tolua_S,1,0));
 {
  bool toluaI_ret = (bool)  set_fast(v);
 tolua_pushnumber(tolua_S,(long)toluaI_ret);
 }
 }
 return 1;
tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'set_fast'.");
 return 0;
}

/* function: set_light_speed */
static int toluaI_player_set_light_speed00(lua_State* tolua_S)
{
 if (
 !tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
 !tolua_isnoobj(tolua_S,2)
 )
 goto tolua_lerror;
 else
 {
  int v = ((int)  tolua_getnumber(tolua_S,1,0));
 {
  bool toluaI_ret = (bool)  set_light_speed(v);
 tolua_pushnumber(tolua_S,(long)toluaI_ret);
 }
 }
 return 1;
tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'set_light_speed'.");
 return 0;
}

/* function: set_slow */
static int toluaI_player_set_slow00(lua_State* tolua_S)
{
 if (
 !tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
 !tolua_isnoobj(tolua_S,2)
 )
 goto tolua_lerror;
 else
 {
  int v = ((int)  tolua_getnumber(tolua_S,1,0));
 {
  bool toluaI_ret = (bool)  set_slow(v);
 tolua_pushnumber(tolua_S,(long)toluaI_ret);
 }
 }
 return 1;
tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'set_slow'.");
 return 0;
}

/* function: set_shield */
static int toluaI_player_set_shield00(lua_State* tolua_S)
{
 if (
 !tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
 !tolua_istype(tolua_S,2,LUA_TNUMBER,0) ||
 !tolua_istype(tolua_S,3,LUA_TNUMBER,0) ||
 !tolua_isnoobj(tolua_S,4)
 )
 goto tolua_lerror;
 else
 {
  int v = ((int)  tolua_getnumber(tolua_S,1,0));
  int p = ((int)  tolua_getnumber(tolua_S,2,0));
  s16b o = ((s16b)  tolua_getnumber(tolua_S,3,0));
 {
  bool toluaI_ret = (bool)  set_shield(v,p,o);
 tolua_pushnumber(tolua_S,(long)toluaI_ret);
 }
 }
 return 1;
tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'set_shield'.");
 return 0;
}

/* function: set_blessed */
static int toluaI_player_set_blessed00(lua_State* tolua_S)
{
 if (
 !tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
 !tolua_isnoobj(tolua_S,2)
 )
 goto tolua_lerror;
 else
 {
  int v = ((int)  tolua_getnumber(tolua_S,1,0));
 {
  bool toluaI_ret = (bool)  set_blessed(v);
 tolua_pushnumber(tolua_S,(long)toluaI_ret);
 }
 }
 return 1;
tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'set_blessed'.");
 return 0;
}

/* function: set_hero */
static int toluaI_player_set_hero00(lua_State* tolua_S)
{
 if (
 !tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
 !tolua_isnoobj(tolua_S,2)
 )
 goto tolua_lerror;
 else
 {
  int v = ((int)  tolua_getnumber(tolua_S,1,0));
 {
  bool toluaI_ret = (bool)  set_hero(v);
 tolua_pushnumber(tolua_S,(long)toluaI_ret);
 }
 }
 return 1;
tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'set_hero'.");
 return 0;
}

/* function: set_shero */
static int toluaI_player_set_shero00(lua_State* tolua_S)
{
 if (
 !tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
 !tolua_isnoobj(tolua_S,2)
 )
 goto tolua_lerror;
 else
 {
  int v = ((int)  tolua_getnumber(tolua_S,1,0));
 {
  bool toluaI_ret = (bool)  set_shero(v);
 tolua_pushnumber(tolua_S,(long)toluaI_ret);
 }
 }
 return 1;
tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'set_shero'.");
 return 0;
}

/* function: set_protevil */
static int toluaI_player_set_protevil00(lua_State* tolua_S)
{
 if (
 !tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
 !tolua_isnoobj(tolua_S,2)
 )
 goto tolua_lerror;
 else
 {
  int v = ((int)  tolua_getnumber(tolua_S,1,0));
 {
  bool toluaI_ret = (bool)  set_protevil(v);
 tolua_pushnumber(tolua_S,(long)toluaI_ret);
 }
 }
 return 1;
tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'set_protevil'.");
 return 0;
}

/* function: set_protgood */
static int toluaI_player_set_protgood00(lua_State* tolua_S)
{
 if (
 !tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
 !tolua_isnoobj(tolua_S,2)
 )
 goto tolua_lerror;
 else
 {
  int v = ((int)  tolua_getnumber(tolua_S,1,0));
 {
  bool toluaI_ret = (bool)  set_protgood(v);
 tolua_pushnumber(tolua_S,(long)toluaI_ret);
 }
 }
 return 1;
tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'set_protgood'.");
 return 0;
}

/* function: set_protundead */
static int toluaI_player_set_protundead00(lua_State* tolua_S)
{
 if (
 !tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
 !tolua_isnoobj(tolua_S,2)
 )
 goto tolua_lerror;
 else
 {
  int v = ((int)  tolua_getnumber(tolua_S,1,0));
 {
  bool toluaI_ret = (bool)  set_protundead(v);
 tolua_pushnumber(tolua_S,(long)toluaI_ret);
 }
 }
 return 1;
tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'set_protundead'.");
 return 0;
}

/* function: set_invuln */
static int toluaI_player_set_invuln00(lua_State* tolua_S)
{
 if (
 !tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
 !tolua_isnoobj(tolua_S,2)
 )
 goto tolua_lerror;
 else
 {
  int v = ((int)  tolua_getnumber(tolua_S,1,0));
 {
  bool toluaI_ret = (bool)  set_invuln(v);
 tolua_pushnumber(tolua_S,(long)toluaI_ret);
 }
 }
 return 1;
tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'set_invuln'.");
 return 0;
}

/* function: set_tim_invis */
static int toluaI_player_set_tim_invis00(lua_State* tolua_S)
{
 if (
 !tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
 !tolua_isnoobj(tolua_S,2)
 )
 goto tolua_lerror;
 else
 {
  int v = ((int)  tolua_getnumber(tolua_S,1,0));
 {
  bool toluaI_ret = (bool)  set_tim_invis(v);
 tolua_pushnumber(tolua_S,(long)toluaI_ret);
 }
 }
 return 1;
tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'set_tim_invis'.");
 return 0;
}

/* function: set_tim_infra */
static int toluaI_player_set_tim_infra00(lua_State* tolua_S)
{
 if (
 !tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
 !tolua_isnoobj(tolua_S,2)
 )
 goto tolua_lerror;
 else
 {
  int v = ((int)  tolua_getnumber(tolua_S,1,0));
 {
  bool toluaI_ret = (bool)  set_tim_infra(v);
 tolua_pushnumber(tolua_S,(long)toluaI_ret);
 }
 }
 return 1;
tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'set_tim_infra'.");
 return 0;
}

/* function: set_mental_barrier */
static int toluaI_player_set_mental_barrier00(lua_State* tolua_S)
{
 if (
 !tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
 !tolua_isnoobj(tolua_S,2)
 )
 goto tolua_lerror;
 else
 {
  int v = ((int)  tolua_getnumber(tolua_S,1,0));
 {
  bool toluaI_ret = (bool)  set_mental_barrier(v);
 tolua_pushnumber(tolua_S,(long)toluaI_ret);
 }
 }
 return 1;
tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'set_mental_barrier'.");
 return 0;
}

/* function: set_oppose_acid */
static int toluaI_player_set_oppose_acid00(lua_State* tolua_S)
{
 if (
 !tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
 !tolua_isnoobj(tolua_S,2)
 )
 goto tolua_lerror;
 else
 {
  int v = ((int)  tolua_getnumber(tolua_S,1,0));
 {
  bool toluaI_ret = (bool)  set_oppose_acid(v);
 tolua_pushnumber(tolua_S,(long)toluaI_ret);
 }
 }
 return 1;
tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'set_oppose_acid'.");
 return 0;
}

/* function: set_oppose_elec */
static int toluaI_player_set_oppose_elec00(lua_State* tolua_S)
{
 if (
 !tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
 !tolua_isnoobj(tolua_S,2)
 )
 goto tolua_lerror;
 else
 {
  int v = ((int)  tolua_getnumber(tolua_S,1,0));
 {
  bool toluaI_ret = (bool)  set_oppose_elec(v);
 tolua_pushnumber(tolua_S,(long)toluaI_ret);
 }
 }
 return 1;
tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'set_oppose_elec'.");
 return 0;
}

/* function: set_oppose_fire */
static int toluaI_player_set_oppose_fire00(lua_State* tolua_S)
{
 if (
 !tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
 !tolua_isnoobj(tolua_S,2)
 )
 goto tolua_lerror;
 else
 {
  int v = ((int)  tolua_getnumber(tolua_S,1,0));
 {
  bool toluaI_ret = (bool)  set_oppose_fire(v);
 tolua_pushnumber(tolua_S,(long)toluaI_ret);
 }
 }
 return 1;
tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'set_oppose_fire'.");
 return 0;
}

/* function: set_oppose_cold */
static int toluaI_player_set_oppose_cold00(lua_State* tolua_S)
{
 if (
 !tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
 !tolua_isnoobj(tolua_S,2)
 )
 goto tolua_lerror;
 else
 {
  int v = ((int)  tolua_getnumber(tolua_S,1,0));
 {
  bool toluaI_ret = (bool)  set_oppose_cold(v);
 tolua_pushnumber(tolua_S,(long)toluaI_ret);
 }
 }
 return 1;
tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'set_oppose_cold'.");
 return 0;
}

/* function: set_oppose_pois */
static int toluaI_player_set_oppose_pois00(lua_State* tolua_S)
{
 if (
 !tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
 !tolua_isnoobj(tolua_S,2)
 )
 goto tolua_lerror;
 else
 {
  int v = ((int)  tolua_getnumber(tolua_S,1,0));
 {
  bool toluaI_ret = (bool)  set_oppose_pois(v);
 tolua_pushnumber(tolua_S,(long)toluaI_ret);
 }
 }
 return 1;
tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'set_oppose_pois'.");
 return 0;
}

/* function: set_oppose_ld */
static int toluaI_player_set_oppose_ld00(lua_State* tolua_S)
{
 if (
 !tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
 !tolua_isnoobj(tolua_S,2)
 )
 goto tolua_lerror;
 else
 {
  int v = ((int)  tolua_getnumber(tolua_S,1,0));
 {
  bool toluaI_ret = (bool)  set_oppose_ld(v);
 tolua_pushnumber(tolua_S,(long)toluaI_ret);
 }
 }
 return 1;
tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'set_oppose_ld'.");
 return 0;
}

/* function: set_oppose_cc */
static int toluaI_player_set_oppose_cc00(lua_State* tolua_S)
{
 if (
 !tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
 !tolua_isnoobj(tolua_S,2)
 )
 goto tolua_lerror;
 else
 {
  int v = ((int)  tolua_getnumber(tolua_S,1,0));
 {
  bool toluaI_ret = (bool)  set_oppose_cc(v);
 tolua_pushnumber(tolua_S,(long)toluaI_ret);
 }
 }
 return 1;
tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'set_oppose_cc'.");
 return 0;
}

/* function: set_oppose_ss */
static int toluaI_player_set_oppose_ss00(lua_State* tolua_S)
{
 if (
 !tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
 !tolua_isnoobj(tolua_S,2)
 )
 goto tolua_lerror;
 else
 {
  int v = ((int)  tolua_getnumber(tolua_S,1,0));
 {
  bool toluaI_ret = (bool)  set_oppose_ss(v);
 tolua_pushnumber(tolua_S,(long)toluaI_ret);
 }
 }
 return 1;
tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'set_oppose_ss'.");
 return 0;
}

/* function: set_oppose_nex */
static int toluaI_player_set_oppose_nex00(lua_State* tolua_S)
{
 if (
 !tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
 !tolua_isnoobj(tolua_S,2)
 )
 goto tolua_lerror;
 else
 {
  int v = ((int)  tolua_getnumber(tolua_S,1,0));
 {
  bool toluaI_ret = (bool)  set_oppose_nex(v);
 tolua_pushnumber(tolua_S,(long)toluaI_ret);
 }
 }
 return 1;
tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'set_oppose_nex'.");
 return 0;
}

/* function: set_stun */
static int toluaI_player_set_stun00(lua_State* tolua_S)
{
 if (
 !tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
 !tolua_isnoobj(tolua_S,2)
 )
 goto tolua_lerror;
 else
 {
  int v = ((int)  tolua_getnumber(tolua_S,1,0));
 {
  bool toluaI_ret = (bool)  set_stun(v);
 tolua_pushnumber(tolua_S,(long)toluaI_ret);
 }
 }
 return 1;
tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'set_stun'.");
 return 0;
}

/* function: set_cut */
static int toluaI_player_set_cut00(lua_State* tolua_S)
{
 if (
 !tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
 !tolua_isnoobj(tolua_S,2)
 )
 goto tolua_lerror;
 else
 {
  int v = ((int)  tolua_getnumber(tolua_S,1,0));
 {
  bool toluaI_ret = (bool)  set_cut(v);
 tolua_pushnumber(tolua_S,(long)toluaI_ret);
 }
 }
 return 1;
tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'set_cut'.");
 return 0;
}

/* function: set_food */
static int toluaI_player_set_food00(lua_State* tolua_S)
{
 if (
 !tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
 !tolua_isnoobj(tolua_S,2)
 )
 goto tolua_lerror;
 else
 {
  int v = ((int)  tolua_getnumber(tolua_S,1,0));
 {
  bool toluaI_ret = (bool)  set_food(v);
 tolua_pushnumber(tolua_S,(long)toluaI_ret);
 }
 }
 return 1;
tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'set_food'.");
 return 0;
}

/* function: check_experience */
static int toluaI_player_check_experience00(lua_State* tolua_S)
{
 if (
 !tolua_isnoobj(tolua_S,1)
 )
 goto tolua_lerror;
 else
 {
 {
  check_experience();
 }
 }
 return 0;
tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'check_experience'.");
 return 0;
}

/* function: check_experience_obj */
static int toluaI_player_check_experience_obj00(lua_State* tolua_S)
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
  check_experience_obj(o_ptr);
 }
 }
 return 0;
tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'check_experience_obj'.");
 return 0;
}

/* function: gain_exp */
static int toluaI_player_gain_exp00(lua_State* tolua_S)
{
 if (
 !tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
 !tolua_isnoobj(tolua_S,2)
 )
 goto tolua_lerror;
 else
 {
  s32b amount = ((s32b)  tolua_getnumber(tolua_S,1,0));
 {
  gain_exp(amount);
 }
 }
 return 0;
tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'gain_exp'.");
 return 0;
}

/* function: lose_exp */
static int toluaI_player_lose_exp00(lua_State* tolua_S)
{
 if (
 !tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
 !tolua_isnoobj(tolua_S,2)
 )
 goto tolua_lerror;
 else
 {
  s32b amount = ((s32b)  tolua_getnumber(tolua_S,1,0));
 {
  lose_exp(amount);
 }
 }
 return 0;
tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'lose_exp'.");
 return 0;
}

/* Open function */
int tolua_player_open (lua_State* tolua_S)
{
 tolua_open(tolua_S);
 toluaI_reg_types(tolua_S);
 tolua_constant(tolua_S,NULL,"A_STR",A_STR);
 tolua_constant(tolua_S,NULL,"A_INT",A_INT);
 tolua_constant(tolua_S,NULL,"A_WIS",A_WIS);
 tolua_constant(tolua_S,NULL,"A_DEX",A_DEX);
 tolua_constant(tolua_S,NULL,"A_CON",A_CON);
 tolua_constant(tolua_S,NULL,"A_CHR",A_CHR);
 tolua_constant(tolua_S,NULL,"SEX_FEMALE",SEX_FEMALE);
 tolua_constant(tolua_S,NULL,"SEX_MALE",SEX_MALE);
 tolua_constant(tolua_S,NULL,"SEX_NEUTER",SEX_NEUTER);
 tolua_constant(tolua_S,NULL,"MAX_SEXES",MAX_SEXES);
 tolua_constant(tolua_S,NULL,"PN_COMBINE",PN_COMBINE);
 tolua_constant(tolua_S,NULL,"PN_REORDER",PN_REORDER);
 tolua_constant(tolua_S,NULL,"PU_BONUS",PU_BONUS);
 tolua_constant(tolua_S,NULL,"PU_TORCH",PU_TORCH);
 tolua_constant(tolua_S,NULL,"PU_BODY",PU_BODY);
 tolua_constant(tolua_S,NULL,"PU_SANITY",PU_SANITY);
 tolua_constant(tolua_S,NULL,"PU_HP",PU_HP);
 tolua_constant(tolua_S,NULL,"PU_MANA",PU_MANA);
 tolua_constant(tolua_S,NULL,"PU_SPELLS",PU_SPELLS);
 tolua_constant(tolua_S,NULL,"PU_POWERS",PU_POWERS);
 tolua_constant(tolua_S,NULL,"PU_UN_VIEW",PU_UN_VIEW);
 tolua_constant(tolua_S,NULL,"PU_UN_LITE",PU_UN_LITE);
 tolua_constant(tolua_S,NULL,"PU_VIEW",PU_VIEW);
 tolua_constant(tolua_S,NULL,"PU_LITE",PU_LITE);
 tolua_constant(tolua_S,NULL,"PU_MONSTERS",PU_MONSTERS);
 tolua_constant(tolua_S,NULL,"PU_DISTANCE",PU_DISTANCE);
 tolua_constant(tolua_S,NULL,"PU_FLOW",PU_FLOW);
 tolua_constant(tolua_S,NULL,"PR_MISC",PR_MISC);
 tolua_constant(tolua_S,NULL,"PR_TITLE",PR_TITLE);
 tolua_constant(tolua_S,NULL,"PR_LEV",PR_LEV);
 tolua_constant(tolua_S,NULL,"PR_EXP",PR_EXP);
 tolua_constant(tolua_S,NULL,"PR_STATS",PR_STATS);
 tolua_constant(tolua_S,NULL,"PR_ARMOR",PR_ARMOR);
 tolua_constant(tolua_S,NULL,"PR_HP",PR_HP);
 tolua_constant(tolua_S,NULL,"PR_MANA",PR_MANA);
 tolua_constant(tolua_S,NULL,"PR_GOLD",PR_GOLD);
 tolua_constant(tolua_S,NULL,"PR_DEPTH",PR_DEPTH);
 tolua_constant(tolua_S,NULL,"PR_HEALTH",PR_HEALTH);
 tolua_constant(tolua_S,NULL,"PR_CUT",PR_CUT);
 tolua_constant(tolua_S,NULL,"PR_STUN",PR_STUN);
 tolua_constant(tolua_S,NULL,"PR_HUNGER",PR_HUNGER);
 tolua_constant(tolua_S,NULL,"PR_TANK",PR_TANK);
 tolua_constant(tolua_S,NULL,"PR_BLIND",PR_BLIND);
 tolua_constant(tolua_S,NULL,"PR_CONFUSED",PR_CONFUSED);
 tolua_constant(tolua_S,NULL,"PR_AFRAID",PR_AFRAID);
 tolua_constant(tolua_S,NULL,"PR_POISONED",PR_POISONED);
 tolua_constant(tolua_S,NULL,"PR_STATE",PR_STATE);
 tolua_constant(tolua_S,NULL,"PR_SPEED",PR_SPEED);
 tolua_constant(tolua_S,NULL,"PR_STUDY",PR_STUDY);
 tolua_constant(tolua_S,NULL,"PR_SANITY",PR_SANITY);
 tolua_constant(tolua_S,NULL,"PR_EXTRA",PR_EXTRA);
 tolua_constant(tolua_S,NULL,"PR_BASIC",PR_BASIC);
 tolua_constant(tolua_S,NULL,"PR_MAP",PR_MAP);
 tolua_constant(tolua_S,NULL,"PR_WIPE",PR_WIPE);
 tolua_constant(tolua_S,NULL,"PR_MH",PR_MH);
 tolua_constant(tolua_S,NULL,"PW_INVEN",PW_INVEN);
 tolua_constant(tolua_S,NULL,"PW_EQUIP",PW_EQUIP);
 tolua_constant(tolua_S,NULL,"PW_SPELL",PW_SPELL);
 tolua_constant(tolua_S,NULL,"PW_PLAYER",PW_PLAYER);
 tolua_constant(tolua_S,NULL,"PW_M_LIST",PW_M_LIST);
 tolua_constant(tolua_S,NULL,"PW_MESSAGE",PW_MESSAGE);
 tolua_constant(tolua_S,NULL,"PW_OVERHEAD",PW_OVERHEAD);
 tolua_constant(tolua_S,NULL,"PW_MONSTER",PW_MONSTER);
 tolua_constant(tolua_S,NULL,"PW_OBJECT",PW_OBJECT);
 tolua_constant(tolua_S,NULL,"PW_SNAPSHOT",PW_SNAPSHOT);
 tolua_constant(tolua_S,NULL,"PW_BORG_1",PW_BORG_1);
 tolua_constant(tolua_S,NULL,"PW_BORG_2",PW_BORG_2);
 tolua_cclass(tolua_S,"deity","");
 tolua_tablevar(tolua_S,"deity","name",toluaI_get_player_deity_name,toluaI_set_player_deity_name);
 tolua_tablevar(tolua_S,"deity","god_of",toluaI_get_player_deity_god_of,toluaI_set_player_deity_god_of);
 tolua_tablevar(tolua_S,"deity","grace_deduction",toluaI_get_player_deity_grace_deduction,toluaI_set_player_deity_grace_deduction);
 tolua_tablevar(tolua_S,"deity","rarity",toluaI_get_player_deity_rarity,toluaI_set_player_deity_rarity);
 tolua_tablevar(tolua_S,"deity","race1",toluaI_get_player_deity_race1,toluaI_set_player_deity_race1);
 tolua_tablevar(tolua_S,"deity","race2",toluaI_get_player_deity_race2,toluaI_set_player_deity_race2);
 tolua_tablevar(tolua_S,"deity","desc",toluaI_get_player_deity_desc,toluaI_set_player_deity_desc);
 tolua_globalarray(tolua_S,"deity_info",toluaI_get_player_deity_info,toluaI_set_player_deity_info);
 tolua_cclass(tolua_S,"player_type","");
 tolua_tablevar(tolua_S,"player_type","oldpy",toluaI_get_player_player_type_oldpy,toluaI_set_player_player_type_oldpy);
 tolua_tablevar(tolua_S,"player_type","oldpx",toluaI_get_player_player_type_oldpx,toluaI_set_player_player_type_oldpx);
 tolua_tablevar(tolua_S,"player_type","psex",toluaI_get_player_player_type_psex,toluaI_set_player_player_type_psex);
 tolua_tablevar(tolua_S,"player_type","prace",toluaI_get_player_player_type_prace,toluaI_set_player_player_type_prace);
 tolua_tablevar(tolua_S,"player_type","pracem",toluaI_get_player_player_type_pracem,toluaI_set_player_player_type_pracem);
 tolua_tablevar(tolua_S,"player_type","pclass",toluaI_get_player_player_type_pclass,toluaI_set_player_player_type_pclass);
 tolua_tablevar(tolua_S,"player_type","realm1",toluaI_get_player_player_type_realm1,toluaI_set_player_player_type_realm1);
 tolua_tablevar(tolua_S,"player_type","realm2",toluaI_get_player_player_type_realm2,toluaI_set_player_player_type_realm2);
 tolua_tablevar(tolua_S,"player_type","mimic_form",toluaI_get_player_player_type_mimic_form,toluaI_set_player_player_type_mimic_form);
 tolua_tablevar(tolua_S,"player_type","oops",toluaI_get_player_player_type_oops,toluaI_set_player_player_type_oops);
 tolua_tablevar(tolua_S,"player_type","hitdie",toluaI_get_player_player_type_hitdie,toluaI_set_player_player_type_hitdie);
 tolua_tablevar(tolua_S,"player_type","expfact",toluaI_get_player_player_type_expfact,toluaI_set_player_player_type_expfact);
 tolua_tablevar(tolua_S,"player_type","allow_one_death",toluaI_get_player_player_type_allow_one_death,toluaI_set_player_player_type_allow_one_death);
 tolua_tablevar(tolua_S,"player_type","age",toluaI_get_player_player_type_age,toluaI_set_player_player_type_age);
 tolua_tablevar(tolua_S,"player_type","ht",toluaI_get_player_player_type_ht,toluaI_set_player_player_type_ht);
 tolua_tablevar(tolua_S,"player_type","wt",toluaI_get_player_player_type_wt,toluaI_set_player_player_type_wt);
 tolua_tablevar(tolua_S,"player_type","sc",toluaI_get_player_player_type_sc,toluaI_set_player_player_type_sc);
 tolua_tablevar(tolua_S,"player_type","au",toluaI_get_player_player_type_au,toluaI_set_player_player_type_au);
 tolua_tablevar(tolua_S,"player_type","max_exp",toluaI_get_player_player_type_max_exp,toluaI_set_player_player_type_max_exp);
 tolua_tablevar(tolua_S,"player_type","exp",toluaI_get_player_player_type_exp,toluaI_set_player_player_type_exp);
 tolua_tablevar(tolua_S,"player_type","exp_frac",toluaI_get_player_player_type_exp_frac,toluaI_set_player_player_type_exp_frac);
 tolua_tablevar(tolua_S,"player_type","lev",toluaI_get_player_player_type_lev,toluaI_set_player_player_type_lev);
 tolua_tablevar(tolua_S,"player_type","town_num",toluaI_get_player_player_type_town_num,toluaI_set_player_player_type_town_num);
 tolua_tablevar(tolua_S,"player_type","inside_quest",toluaI_get_player_player_type_inside_quest,toluaI_set_player_player_type_inside_quest);
 tolua_tablevar(tolua_S,"player_type","exit_bldg",toluaI_get_player_player_type_exit_bldg,toluaI_set_player_player_type_exit_bldg);
 tolua_tablevar(tolua_S,"player_type","wilderness_x",toluaI_get_player_player_type_wilderness_x,toluaI_set_player_player_type_wilderness_x);
 tolua_tablevar(tolua_S,"player_type","wilderness_y",toluaI_get_player_player_type_wilderness_y,toluaI_set_player_player_type_wilderness_y);
 tolua_tablevar(tolua_S,"player_type","wild_mode",toluaI_get_player_player_type_wild_mode,toluaI_set_player_player_type_wild_mode);
 tolua_tablevar(tolua_S,"player_type","old_wild_mode",toluaI_get_player_player_type_old_wild_mode,toluaI_set_player_player_type_old_wild_mode);
 tolua_tablevar(tolua_S,"player_type","mhp",toluaI_get_player_player_type_mhp,toluaI_set_player_player_type_mhp);
 tolua_tablevar(tolua_S,"player_type","chp",toluaI_get_player_player_type_chp,toluaI_set_player_player_type_chp);
 tolua_tablevar(tolua_S,"player_type","chp_frac",toluaI_get_player_player_type_chp_frac,toluaI_set_player_player_type_chp_frac);
 tolua_tablevar(tolua_S,"player_type","hp_mod",toluaI_get_player_player_type_hp_mod,toluaI_set_player_player_type_hp_mod);
 tolua_tablevar(tolua_S,"player_type","msp",toluaI_get_player_player_type_msp,toluaI_set_player_player_type_msp);
 tolua_tablevar(tolua_S,"player_type","csp",toluaI_get_player_player_type_csp,toluaI_set_player_player_type_csp);
 tolua_tablevar(tolua_S,"player_type","csp_frac",toluaI_get_player_player_type_csp_frac,toluaI_set_player_player_type_csp_frac);
 tolua_tablevar(tolua_S,"player_type","msane",toluaI_get_player_player_type_msane,toluaI_set_player_player_type_msane);
 tolua_tablevar(tolua_S,"player_type","csane",toluaI_get_player_player_type_csane,toluaI_set_player_player_type_csane);
 tolua_tablevar(tolua_S,"player_type","csane_frac",toluaI_get_player_player_type_csane_frac,toluaI_set_player_player_type_csane_frac);
 tolua_tablevar(tolua_S,"player_type","mtp",toluaI_get_player_player_type_mtp,toluaI_set_player_player_type_mtp);
 tolua_tablevar(tolua_S,"player_type","ctp",toluaI_get_player_player_type_ctp,toluaI_set_player_player_type_ctp);
 tolua_tablevar(tolua_S,"player_type","tp_aux1",toluaI_get_player_player_type_tp_aux1,toluaI_set_player_player_type_tp_aux1);
 tolua_tablevar(tolua_S,"player_type","tp_aux2",toluaI_get_player_player_type_tp_aux2,toluaI_set_player_player_type_tp_aux2);
 tolua_tablevar(tolua_S,"player_type","grace",toluaI_get_player_player_type_grace,toluaI_set_player_player_type_grace);
 tolua_tablevar(tolua_S,"player_type","god_favor",toluaI_get_player_player_type_god_favor,toluaI_set_player_player_type_god_favor);
 tolua_tablevar(tolua_S,"player_type","pgod",toluaI_get_player_player_type_pgod,toluaI_set_player_player_type_pgod);
 tolua_tablevar(tolua_S,"player_type","max_plv",toluaI_get_player_player_type_max_plv,toluaI_set_player_player_type_max_plv);
 tolua_tablearray(tolua_S,"player_type","stat_max",toluaI_get_player_player_type_stat_max,toluaI_set_player_player_type_stat_max);
 tolua_tablearray(tolua_S,"player_type","stat_cur",toluaI_get_player_player_type_stat_cur,toluaI_set_player_player_type_stat_cur);
 tolua_tablevar(tolua_S,"player_type","luck_cur",toluaI_get_player_player_type_luck_cur,toluaI_set_player_player_type_luck_cur);
 tolua_tablevar(tolua_S,"player_type","luck_max",toluaI_get_player_player_type_luck_max,toluaI_set_player_player_type_luck_max);
 tolua_tablevar(tolua_S,"player_type","luck_base",toluaI_get_player_player_type_luck_base,toluaI_set_player_player_type_luck_base);
 tolua_tablevar(tolua_S,"player_type","fast",toluaI_get_player_player_type_fast,toluaI_set_player_player_type_fast);
 tolua_tablevar(tolua_S,"player_type","lightspeed",toluaI_get_player_player_type_lightspeed,toluaI_set_player_player_type_lightspeed);
 tolua_tablevar(tolua_S,"player_type","slow",toluaI_get_player_player_type_slow,toluaI_set_player_player_type_slow);
 tolua_tablevar(tolua_S,"player_type","blind",toluaI_get_player_player_type_blind,toluaI_set_player_player_type_blind);
 tolua_tablevar(tolua_S,"player_type","paralyzed",toluaI_get_player_player_type_paralyzed,toluaI_set_player_player_type_paralyzed);
 tolua_tablevar(tolua_S,"player_type","confused",toluaI_get_player_player_type_confused,toluaI_set_player_player_type_confused);
 tolua_tablevar(tolua_S,"player_type","afraid",toluaI_get_player_player_type_afraid,toluaI_set_player_player_type_afraid);
 tolua_tablevar(tolua_S,"player_type","image",toluaI_get_player_player_type_image,toluaI_set_player_player_type_image);
 tolua_tablevar(tolua_S,"player_type","poisoned",toluaI_get_player_player_type_poisoned,toluaI_set_player_player_type_poisoned);
 tolua_tablevar(tolua_S,"player_type","cut",toluaI_get_player_player_type_cut,toluaI_set_player_player_type_cut);
 tolua_tablevar(tolua_S,"player_type","stun",toluaI_get_player_player_type_stun,toluaI_set_player_player_type_stun);
 tolua_tablevar(tolua_S,"player_type","protevil",toluaI_get_player_player_type_protevil,toluaI_set_player_player_type_protevil);
 tolua_tablevar(tolua_S,"player_type","protgood",toluaI_get_player_player_type_protgood,toluaI_set_player_player_type_protgood);
 tolua_tablevar(tolua_S,"player_type","protundead",toluaI_get_player_player_type_protundead,toluaI_set_player_player_type_protundead);
 tolua_tablevar(tolua_S,"player_type","invuln",toluaI_get_player_player_type_invuln,toluaI_set_player_player_type_invuln);
 tolua_tablevar(tolua_S,"player_type","hero",toluaI_get_player_player_type_hero,toluaI_set_player_player_type_hero);
 tolua_tablevar(tolua_S,"player_type","shero",toluaI_get_player_player_type_shero,toluaI_set_player_player_type_shero);
 tolua_tablevar(tolua_S,"player_type","shield",toluaI_get_player_player_type_shield,toluaI_set_player_player_type_shield);
 tolua_tablevar(tolua_S,"player_type","shield_power",toluaI_get_player_player_type_shield_power,toluaI_set_player_player_type_shield_power);
 tolua_tablevar(tolua_S,"player_type","shield_opt",toluaI_get_player_player_type_shield_opt,toluaI_set_player_player_type_shield_opt);
 tolua_tablevar(tolua_S,"player_type","blessed",toluaI_get_player_player_type_blessed,toluaI_set_player_player_type_blessed);
 tolua_tablevar(tolua_S,"player_type","tim_invis",toluaI_get_player_player_type_tim_invis,toluaI_set_player_player_type_tim_invis);
 tolua_tablevar(tolua_S,"player_type","tim_infra",toluaI_get_player_player_type_tim_infra,toluaI_set_player_player_type_tim_infra);
 tolua_tablevar(tolua_S,"player_type","oppose_acid",toluaI_get_player_player_type_oppose_acid,toluaI_set_player_player_type_oppose_acid);
 tolua_tablevar(tolua_S,"player_type","oppose_elec",toluaI_get_player_player_type_oppose_elec,toluaI_set_player_player_type_oppose_elec);
 tolua_tablevar(tolua_S,"player_type","oppose_fire",toluaI_get_player_player_type_oppose_fire,toluaI_set_player_player_type_oppose_fire);
 tolua_tablevar(tolua_S,"player_type","oppose_cold",toluaI_get_player_player_type_oppose_cold,toluaI_set_player_player_type_oppose_cold);
 tolua_tablevar(tolua_S,"player_type","oppose_pois",toluaI_get_player_player_type_oppose_pois,toluaI_set_player_player_type_oppose_pois);
 tolua_tablevar(tolua_S,"player_type","oppose_ld",toluaI_get_player_player_type_oppose_ld,toluaI_set_player_player_type_oppose_ld);
 tolua_tablevar(tolua_S,"player_type","oppose_cc",toluaI_get_player_player_type_oppose_cc,toluaI_set_player_player_type_oppose_cc);
 tolua_tablevar(tolua_S,"player_type","oppose_ss",toluaI_get_player_player_type_oppose_ss,toluaI_set_player_player_type_oppose_ss);
 tolua_tablevar(tolua_S,"player_type","oppose_nex",toluaI_get_player_player_type_oppose_nex,toluaI_set_player_player_type_oppose_nex);
 tolua_tablevar(tolua_S,"player_type","tim_esp",toluaI_get_player_player_type_tim_esp,toluaI_set_player_player_type_tim_esp);
 tolua_tablevar(tolua_S,"player_type","tim_wraith",toluaI_get_player_player_type_tim_wraith,toluaI_set_player_player_type_tim_wraith);
 tolua_tablevar(tolua_S,"player_type","tim_ffall",toluaI_get_player_player_type_tim_ffall,toluaI_set_player_player_type_tim_ffall);
 tolua_tablevar(tolua_S,"player_type","tim_fire_aura",toluaI_get_player_player_type_tim_fire_aura,toluaI_set_player_player_type_tim_fire_aura);
 tolua_tablevar(tolua_S,"player_type","resist_magic",toluaI_get_player_player_type_resist_magic,toluaI_set_player_player_type_resist_magic);
 tolua_tablevar(tolua_S,"player_type","tim_invisible",toluaI_get_player_player_type_tim_invisible,toluaI_set_player_player_type_tim_invisible);
 tolua_tablevar(tolua_S,"player_type","tim_inv_pow",toluaI_get_player_player_type_tim_inv_pow,toluaI_set_player_player_type_tim_inv_pow);
 tolua_tablevar(tolua_S,"player_type","tim_mimic",toluaI_get_player_player_type_tim_mimic,toluaI_set_player_player_type_tim_mimic);
 tolua_tablevar(tolua_S,"player_type","tim_lite",toluaI_get_player_player_type_tim_lite,toluaI_set_player_player_type_tim_lite);
 tolua_tablevar(tolua_S,"player_type","holy",toluaI_get_player_player_type_holy,toluaI_set_player_player_type_holy);
 tolua_tablevar(tolua_S,"player_type","walk_water",toluaI_get_player_player_type_walk_water,toluaI_set_player_player_type_walk_water);
 tolua_tablevar(tolua_S,"player_type","tim_mental_barrier",toluaI_get_player_player_type_tim_mental_barrier,toluaI_set_player_player_type_tim_mental_barrier);
 tolua_tablevar(tolua_S,"player_type","strike",toluaI_get_player_player_type_strike,toluaI_set_player_player_type_strike);
 tolua_tablevar(tolua_S,"player_type","meditation",toluaI_get_player_player_type_meditation,toluaI_set_player_player_type_meditation);
 tolua_tablevar(tolua_S,"player_type","tim_reflect",toluaI_get_player_player_type_tim_reflect,toluaI_set_player_player_type_tim_reflect);
 tolua_tablevar(tolua_S,"player_type","tim_res_time",toluaI_get_player_player_type_tim_res_time,toluaI_set_player_player_type_tim_res_time);
 tolua_tablevar(tolua_S,"player_type","tim_deadly",toluaI_get_player_player_type_tim_deadly,toluaI_set_player_player_type_tim_deadly);
 tolua_tablevar(tolua_S,"player_type","prob_travel",toluaI_get_player_player_type_prob_travel,toluaI_set_player_player_type_prob_travel);
 tolua_tablevar(tolua_S,"player_type","disrupt_shield",toluaI_get_player_player_type_disrupt_shield,toluaI_set_player_player_type_disrupt_shield);
 tolua_tablevar(tolua_S,"player_type","parasite",toluaI_get_player_player_type_parasite,toluaI_set_player_player_type_parasite);
 tolua_tablevar(tolua_S,"player_type","parasite_r_idx",toluaI_get_player_player_type_parasite_r_idx,toluaI_set_player_player_type_parasite_r_idx);
 tolua_tablevar(tolua_S,"player_type","loan",toluaI_get_player_player_type_loan,toluaI_set_player_player_type_loan);
 tolua_tablevar(tolua_S,"player_type","loan_time",toluaI_get_player_player_type_loan_time,toluaI_set_player_player_type_loan_time);
 tolua_tablevar(tolua_S,"player_type","immov_cntr",toluaI_get_player_player_type_immov_cntr,toluaI_set_player_player_type_immov_cntr);
 tolua_tablevar(tolua_S,"player_type","chaos_patron",toluaI_get_player_player_type_chaos_patron,toluaI_set_player_player_type_chaos_patron);
 tolua_tablevar(tolua_S,"player_type","muta1",toluaI_get_player_player_type_muta1,toluaI_set_player_player_type_muta1);
 tolua_tablevar(tolua_S,"player_type","muta2",toluaI_get_player_player_type_muta2,toluaI_set_player_player_type_muta2);
 tolua_tablevar(tolua_S,"player_type","muta3",toluaI_get_player_player_type_muta3,toluaI_set_player_player_type_muta3);
 tolua_tablevar(tolua_S,"player_type","recall_dungeon",toluaI_get_player_player_type_recall_dungeon,toluaI_set_player_player_type_recall_dungeon);
 tolua_tablevar(tolua_S,"player_type","word_recall",toluaI_get_player_player_type_word_recall,toluaI_set_player_player_type_word_recall);
 tolua_tablevar(tolua_S,"player_type","energy",toluaI_get_player_player_type_energy,toluaI_set_player_player_type_energy);
 tolua_tablevar(tolua_S,"player_type","food",toluaI_get_player_player_type_food,toluaI_set_player_player_type_food);
 tolua_tablevar(tolua_S,"player_type","confusing",toluaI_get_player_player_type_confusing,toluaI_set_player_player_type_confusing);
 tolua_tablevar(tolua_S,"player_type","searching",toluaI_get_player_player_type_searching,toluaI_set_player_player_type_searching);
 tolua_tablevar(tolua_S,"player_type","new_spells",toluaI_get_player_player_type_new_spells,toluaI_set_player_player_type_new_spells);
 tolua_tablevar(tolua_S,"player_type","old_spells",toluaI_get_player_player_type_old_spells,toluaI_set_player_player_type_old_spells);
 tolua_tablevar(tolua_S,"player_type","xtra_spells",toluaI_get_player_player_type_xtra_spells,toluaI_set_player_player_type_xtra_spells);
 tolua_tablevar(tolua_S,"player_type","cur_lite",toluaI_get_player_player_type_cur_lite,toluaI_set_player_player_type_cur_lite);
 tolua_tablevar(tolua_S,"player_type","notice",toluaI_get_player_player_type_notice,toluaI_set_player_player_type_notice);
 tolua_tablevar(tolua_S,"player_type","update",toluaI_get_player_player_type_update,toluaI_set_player_player_type_update);
 tolua_tablevar(tolua_S,"player_type","redraw",toluaI_get_player_player_type_redraw,toluaI_set_player_player_type_redraw);
 tolua_tablevar(tolua_S,"player_type","window",toluaI_get_player_player_type_window,toluaI_set_player_player_type_window);
 tolua_tablearray(tolua_S,"player_type","stat_use",toluaI_get_player_player_type_stat_use,toluaI_set_player_player_type_stat_use);
 tolua_tablearray(tolua_S,"player_type","stat_top",toluaI_get_player_player_type_stat_top,toluaI_set_player_player_type_stat_top);
 tolua_tablearray(tolua_S,"player_type","stat_add",toluaI_get_player_player_type_stat_add,toluaI_set_player_player_type_stat_add);
 tolua_tablearray(tolua_S,"player_type","stat_ind",toluaI_get_player_player_type_stat_ind,toluaI_set_player_player_type_stat_ind);
 tolua_tablearray(tolua_S,"player_type","stat_cnt",toluaI_get_player_player_type_stat_cnt,toluaI_set_player_player_type_stat_cnt);
 tolua_tablearray(tolua_S,"player_type","stat_los",toluaI_get_player_player_type_stat_los,toluaI_set_player_player_type_stat_los);
 tolua_tablevar(tolua_S,"player_type","immune_acid",toluaI_get_player_player_type_immune_acid,toluaI_set_player_player_type_immune_acid);
 tolua_tablevar(tolua_S,"player_type","immune_elec",toluaI_get_player_player_type_immune_elec,toluaI_set_player_player_type_immune_elec);
 tolua_tablevar(tolua_S,"player_type","immune_fire",toluaI_get_player_player_type_immune_fire,toluaI_set_player_player_type_immune_fire);
 tolua_tablevar(tolua_S,"player_type","immune_cold",toluaI_get_player_player_type_immune_cold,toluaI_set_player_player_type_immune_cold);
 tolua_tablevar(tolua_S,"player_type","immune_neth",toluaI_get_player_player_type_immune_neth,toluaI_set_player_player_type_immune_neth);
 tolua_tablevar(tolua_S,"player_type","resist_acid",toluaI_get_player_player_type_resist_acid,toluaI_set_player_player_type_resist_acid);
 tolua_tablevar(tolua_S,"player_type","resist_elec",toluaI_get_player_player_type_resist_elec,toluaI_set_player_player_type_resist_elec);
 tolua_tablevar(tolua_S,"player_type","resist_fire",toluaI_get_player_player_type_resist_fire,toluaI_set_player_player_type_resist_fire);
 tolua_tablevar(tolua_S,"player_type","resist_cold",toluaI_get_player_player_type_resist_cold,toluaI_set_player_player_type_resist_cold);
 tolua_tablevar(tolua_S,"player_type","resist_pois",toluaI_get_player_player_type_resist_pois,toluaI_set_player_player_type_resist_pois);
 tolua_tablevar(tolua_S,"player_type","resist_conf",toluaI_get_player_player_type_resist_conf,toluaI_set_player_player_type_resist_conf);
 tolua_tablevar(tolua_S,"player_type","resist_sound",toluaI_get_player_player_type_resist_sound,toluaI_set_player_player_type_resist_sound);
 tolua_tablevar(tolua_S,"player_type","resist_lite",toluaI_get_player_player_type_resist_lite,toluaI_set_player_player_type_resist_lite);
 tolua_tablevar(tolua_S,"player_type","resist_dark",toluaI_get_player_player_type_resist_dark,toluaI_set_player_player_type_resist_dark);
 tolua_tablevar(tolua_S,"player_type","resist_chaos",toluaI_get_player_player_type_resist_chaos,toluaI_set_player_player_type_resist_chaos);
 tolua_tablevar(tolua_S,"player_type","resist_disen",toluaI_get_player_player_type_resist_disen,toluaI_set_player_player_type_resist_disen);
 tolua_tablevar(tolua_S,"player_type","resist_shard",toluaI_get_player_player_type_resist_shard,toluaI_set_player_player_type_resist_shard);
 tolua_tablevar(tolua_S,"player_type","resist_nexus",toluaI_get_player_player_type_resist_nexus,toluaI_set_player_player_type_resist_nexus);
 tolua_tablevar(tolua_S,"player_type","resist_blind",toluaI_get_player_player_type_resist_blind,toluaI_set_player_player_type_resist_blind);
 tolua_tablevar(tolua_S,"player_type","resist_neth",toluaI_get_player_player_type_resist_neth,toluaI_set_player_player_type_resist_neth);
 tolua_tablevar(tolua_S,"player_type","resist_fear",toluaI_get_player_player_type_resist_fear,toluaI_set_player_player_type_resist_fear);
 tolua_tablevar(tolua_S,"player_type","resist_continuum",toluaI_get_player_player_type_resist_continuum,toluaI_set_player_player_type_resist_continuum);
 tolua_tablevar(tolua_S,"player_type","sensible_fire",toluaI_get_player_player_type_sensible_fire,toluaI_set_player_player_type_sensible_fire);
 tolua_tablevar(tolua_S,"player_type","reflect",toluaI_get_player_player_type_reflect,toluaI_set_player_player_type_reflect);
 tolua_tablevar(tolua_S,"player_type","sh_fire",toluaI_get_player_player_type_sh_fire,toluaI_set_player_player_type_sh_fire);
 tolua_tablevar(tolua_S,"player_type","sh_elec",toluaI_get_player_player_type_sh_elec,toluaI_set_player_player_type_sh_elec);
 tolua_tablevar(tolua_S,"player_type","wraith_form",toluaI_get_player_player_type_wraith_form,toluaI_set_player_player_type_wraith_form);
 tolua_tablevar(tolua_S,"player_type","anti_magic",toluaI_get_player_player_type_anti_magic,toluaI_set_player_player_type_anti_magic);
 tolua_tablevar(tolua_S,"player_type","anti_tele",toluaI_get_player_player_type_anti_tele,toluaI_set_player_player_type_anti_tele);
 tolua_tablevar(tolua_S,"player_type","sustain_str",toluaI_get_player_player_type_sustain_str,toluaI_set_player_player_type_sustain_str);
 tolua_tablevar(tolua_S,"player_type","sustain_int",toluaI_get_player_player_type_sustain_int,toluaI_set_player_player_type_sustain_int);
 tolua_tablevar(tolua_S,"player_type","sustain_wis",toluaI_get_player_player_type_sustain_wis,toluaI_set_player_player_type_sustain_wis);
 tolua_tablevar(tolua_S,"player_type","sustain_dex",toluaI_get_player_player_type_sustain_dex,toluaI_set_player_player_type_sustain_dex);
 tolua_tablevar(tolua_S,"player_type","sustain_con",toluaI_get_player_player_type_sustain_con,toluaI_set_player_player_type_sustain_con);
 tolua_tablevar(tolua_S,"player_type","sustain_chr",toluaI_get_player_player_type_sustain_chr,toluaI_set_player_player_type_sustain_chr);
 tolua_tablevar(tolua_S,"player_type","aggravate",toluaI_get_player_player_type_aggravate,toluaI_set_player_player_type_aggravate);
 tolua_tablevar(tolua_S,"player_type","teleport",toluaI_get_player_player_type_teleport,toluaI_set_player_player_type_teleport);
 tolua_tablevar(tolua_S,"player_type","exp_drain",toluaI_get_player_player_type_exp_drain,toluaI_set_player_player_type_exp_drain);
 tolua_tablevar(tolua_S,"player_type","drain_mana",toluaI_get_player_player_type_drain_mana,toluaI_set_player_player_type_drain_mana);
 tolua_tablevar(tolua_S,"player_type","drain_life",toluaI_get_player_player_type_drain_life,toluaI_set_player_player_type_drain_life);
 tolua_tablevar(tolua_S,"player_type","climb",toluaI_get_player_player_type_climb,toluaI_set_player_player_type_climb);
 tolua_tablevar(tolua_S,"player_type","fly",toluaI_get_player_player_type_fly,toluaI_set_player_player_type_fly);
 tolua_tablevar(tolua_S,"player_type","ffall",toluaI_get_player_player_type_ffall,toluaI_set_player_player_type_ffall);
 tolua_tablevar(tolua_S,"player_type","lite",toluaI_get_player_player_type_lite,toluaI_set_player_player_type_lite);
 tolua_tablevar(tolua_S,"player_type","free_act",toluaI_get_player_player_type_free_act,toluaI_set_player_player_type_free_act);
 tolua_tablevar(tolua_S,"player_type","see_inv",toluaI_get_player_player_type_see_inv,toluaI_set_player_player_type_see_inv);
 tolua_tablevar(tolua_S,"player_type","regenerate",toluaI_get_player_player_type_regenerate,toluaI_set_player_player_type_regenerate);
 tolua_tablevar(tolua_S,"player_type","hold_life",toluaI_get_player_player_type_hold_life,toluaI_set_player_player_type_hold_life);
 tolua_tablevar(tolua_S,"player_type","telepathy",toluaI_get_player_player_type_telepathy,toluaI_set_player_player_type_telepathy);
 tolua_tablevar(tolua_S,"player_type","slow_digest",toluaI_get_player_player_type_slow_digest,toluaI_set_player_player_type_slow_digest);
 tolua_tablevar(tolua_S,"player_type","bless_blade",toluaI_get_player_player_type_bless_blade,toluaI_set_player_player_type_bless_blade);
 tolua_tablevar(tolua_S,"player_type","xtra_might",toluaI_get_player_player_type_xtra_might,toluaI_set_player_player_type_xtra_might);
 tolua_tablevar(tolua_S,"player_type","impact",toluaI_get_player_player_type_impact,toluaI_set_player_player_type_impact);
 tolua_tablevar(tolua_S,"player_type","auto_id",toluaI_get_player_player_type_auto_id,toluaI_set_player_player_type_auto_id);
 tolua_tablevar(tolua_S,"player_type","dis_to_h",toluaI_get_player_player_type_dis_to_h,toluaI_set_player_player_type_dis_to_h);
 tolua_tablevar(tolua_S,"player_type","dis_to_d",toluaI_get_player_player_type_dis_to_d,toluaI_set_player_player_type_dis_to_d);
 tolua_tablevar(tolua_S,"player_type","dis_to_a",toluaI_get_player_player_type_dis_to_a,toluaI_set_player_player_type_dis_to_a);
 tolua_tablevar(tolua_S,"player_type","dis_ac",toluaI_get_player_player_type_dis_ac,toluaI_set_player_player_type_dis_ac);
 tolua_tablevar(tolua_S,"player_type","to_m",toluaI_get_player_player_type_to_m,toluaI_set_player_player_type_to_m);
 tolua_tablevar(tolua_S,"player_type","to_s",toluaI_get_player_player_type_to_s,toluaI_set_player_player_type_to_s);
 tolua_tablevar(tolua_S,"player_type","to_h",toluaI_get_player_player_type_to_h,toluaI_set_player_player_type_to_h);
 tolua_tablevar(tolua_S,"player_type","to_d",toluaI_get_player_player_type_to_d,toluaI_set_player_player_type_to_d);
 tolua_tablevar(tolua_S,"player_type","to_a",toluaI_get_player_player_type_to_a,toluaI_set_player_player_type_to_a);
 tolua_tablevar(tolua_S,"player_type","ac",toluaI_get_player_player_type_ac,toluaI_set_player_player_type_ac);
 tolua_tablevar(tolua_S,"player_type","antimagic",toluaI_get_player_player_type_antimagic,toluaI_set_player_player_type_antimagic);
 tolua_tablevar(tolua_S,"player_type","antimagic_dis",toluaI_get_player_player_type_antimagic_dis,toluaI_set_player_player_type_antimagic_dis);
 tolua_tablevar(tolua_S,"player_type","see_infra",toluaI_get_player_player_type_see_infra,toluaI_set_player_player_type_see_infra);
 tolua_tablevar(tolua_S,"player_type","skill_dis",toluaI_get_player_player_type_skill_dis,toluaI_set_player_player_type_skill_dis);
 tolua_tablevar(tolua_S,"player_type","skill_dev",toluaI_get_player_player_type_skill_dev,toluaI_set_player_player_type_skill_dev);
 tolua_tablevar(tolua_S,"player_type","skill_sav",toluaI_get_player_player_type_skill_sav,toluaI_set_player_player_type_skill_sav);
 tolua_tablevar(tolua_S,"player_type","skill_stl",toluaI_get_player_player_type_skill_stl,toluaI_set_player_player_type_skill_stl);
 tolua_tablevar(tolua_S,"player_type","skill_srh",toluaI_get_player_player_type_skill_srh,toluaI_set_player_player_type_skill_srh);
 tolua_tablevar(tolua_S,"player_type","skill_fos",toluaI_get_player_player_type_skill_fos,toluaI_set_player_player_type_skill_fos);
 tolua_tablevar(tolua_S,"player_type","skill_thn",toluaI_get_player_player_type_skill_thn,toluaI_set_player_player_type_skill_thn);
 tolua_tablevar(tolua_S,"player_type","skill_thb",toluaI_get_player_player_type_skill_thb,toluaI_set_player_player_type_skill_thb);
 tolua_tablevar(tolua_S,"player_type","skill_tht",toluaI_get_player_player_type_skill_tht,toluaI_set_player_player_type_skill_tht);
 tolua_tablevar(tolua_S,"player_type","skill_dig",toluaI_get_player_player_type_skill_dig,toluaI_set_player_player_type_skill_dig);
 tolua_tablevar(tolua_S,"player_type","companion_killed",toluaI_get_player_player_type_companion_killed,toluaI_set_player_player_type_companion_killed);
 tolua_tablevar(tolua_S,"player_type","black_breath",toluaI_get_player_player_type_black_breath,toluaI_set_player_player_type_black_breath);
 tolua_tablevar(tolua_S,"player_type","body_monster",toluaI_get_player_player_type_body_monster,toluaI_set_player_player_type_body_monster);
 tolua_tablearray(tolua_S,"player_type","powers_mod",toluaI_get_player_player_type_powers_mod,toluaI_set_player_player_type_powers_mod);
 tolua_tablearray(tolua_S,"player_type","powers",toluaI_get_player_player_type_powers,toluaI_set_player_player_type_powers);
 tolua_tablevar(tolua_S,"player_type","leaving",toluaI_get_player_player_type_leaving,toluaI_set_player_player_type_leaving);
 tolua_cclass(tolua_S,"player_race","");
 tolua_tablevar(tolua_S,"player_race","title",toluaI_get_player_player_race_title,toluaI_set_player_player_race_title);
 tolua_tablevar(tolua_S,"player_race","desc",toluaI_get_player_player_race_desc,toluaI_set_player_player_race_desc);
 tolua_cclass(tolua_S,"player_race_mod","");
 tolua_tablevar(tolua_S,"player_race_mod","title",toluaI_get_player_player_race_mod_title,toluaI_set_player_player_race_mod_title);
 tolua_tablevar(tolua_S,"player_race_mod","desc",toluaI_get_player_player_race_mod_desc,toluaI_set_player_player_race_mod_desc);
 tolua_tablevar(tolua_S,"player_race_mod","place",toluaI_get_player_player_race_mod_place,toluaI_set_player_player_race_mod_place);
 tolua_globalvar(tolua_S,"energy_use",toluaI_get_player_energy_use,toluaI_set_player_energy_use);
 tolua_globalvar(tolua_S,"p_ptr",toluaI_get_player_p_ptr,toluaI_set_player_p_ptr);
 tolua_globalvar(tolua_S,"max_rp_idx",toluaI_get_player_max_rp_idx,toluaI_set_player_max_rp_idx);
 tolua_globalarray(tolua_S,"race_info",toluaI_get_player_race_info,toluaI_set_player_race_info);
 tolua_globalvar(tolua_S,"rp_name",toluaI_get_player_rp_name,toluaI_set_player_rp_name);
 tolua_globalvar(tolua_S,"rp_text",toluaI_get_player_rp_text,toluaI_set_player_rp_text);
 tolua_globalvar(tolua_S,"max_rmp_idx",toluaI_get_player_max_rmp_idx,toluaI_set_player_max_rmp_idx);
 tolua_globalarray(tolua_S,"race_mod_info",toluaI_get_player_race_mod_info,toluaI_set_player_race_mod_info);
 tolua_globalvar(tolua_S,"rmp_name",toluaI_get_player_rmp_name,toluaI_set_player_rmp_name);
 tolua_globalvar(tolua_S,"rmp_text",toluaI_get_player_rmp_text,toluaI_set_player_rmp_text);
 tolua_globalarray(tolua_S,"class_info",toluaI_get_player_class_info,toluaI_set_player_class_info);
 tolua_globalvar(tolua_S,"c_name",toluaI_get_player_c_name,toluaI_set_player_c_name);
 tolua_globalvar(tolua_S,"c_text",toluaI_get_player_c_text,toluaI_set_player_c_text);
 tolua_globalvar(tolua_S,"py",toluaI_get_player_py,toluaI_set_player_py);
 tolua_globalvar(tolua_S,"px",toluaI_get_player_px,toluaI_set_player_px);
 tolua_function(tolua_S,NULL,"set_parasite",toluaI_player_set_parasite00);
 tolua_function(tolua_S,NULL,"set_disrupt_shield",toluaI_player_set_disrupt_shield00);
 tolua_function(tolua_S,NULL,"set_prob_travel",toluaI_player_set_prob_travel00);
 tolua_function(tolua_S,NULL,"set_tim_deadly",toluaI_player_set_tim_deadly00);
 tolua_function(tolua_S,NULL,"set_tim_res_time",toluaI_player_set_tim_res_time00);
 tolua_function(tolua_S,NULL,"set_tim_reflect",toluaI_player_set_tim_reflect00);
 tolua_function(tolua_S,NULL,"set_meditation",toluaI_player_set_meditation00);
 tolua_function(tolua_S,NULL,"set_strike",toluaI_player_set_strike00);
 tolua_function(tolua_S,NULL,"set_walk_water",toluaI_player_set_walk_water00);
 tolua_function(tolua_S,NULL,"set_tim_ffall",toluaI_player_set_tim_ffall00);
 tolua_function(tolua_S,NULL,"set_tim_fire_aura",toluaI_player_set_tim_fire_aura00);
 tolua_function(tolua_S,NULL,"set_holy",toluaI_player_set_holy00);
 tolua_function(tolua_S,NULL,"set_grace",toluaI_player_set_grace00);
 tolua_function(tolua_S,NULL,"set_mimic",toluaI_player_set_mimic00);
 tolua_function(tolua_S,NULL,"set_no_breeders",toluaI_player_set_no_breeders00);
 tolua_function(tolua_S,NULL,"set_invis",toluaI_player_set_invis00);
 tolua_function(tolua_S,NULL,"set_lite",toluaI_player_set_lite00);
 tolua_function(tolua_S,NULL,"set_blind",toluaI_player_set_blind00);
 tolua_function(tolua_S,NULL,"set_confused",toluaI_player_set_confused00);
 tolua_function(tolua_S,NULL,"set_poisoned",toluaI_player_set_poisoned00);
 tolua_function(tolua_S,NULL,"set_afraid",toluaI_player_set_afraid00);
 tolua_function(tolua_S,NULL,"set_paralyzed",toluaI_player_set_paralyzed00);
 tolua_function(tolua_S,NULL,"set_image",toluaI_player_set_image00);
 tolua_function(tolua_S,NULL,"set_fast",toluaI_player_set_fast00);
 tolua_function(tolua_S,NULL,"set_light_speed",toluaI_player_set_light_speed00);
 tolua_function(tolua_S,NULL,"set_slow",toluaI_player_set_slow00);
 tolua_function(tolua_S,NULL,"set_shield",toluaI_player_set_shield00);
 tolua_function(tolua_S,NULL,"set_blessed",toluaI_player_set_blessed00);
 tolua_function(tolua_S,NULL,"set_hero",toluaI_player_set_hero00);
 tolua_function(tolua_S,NULL,"set_shero",toluaI_player_set_shero00);
 tolua_function(tolua_S,NULL,"set_protevil",toluaI_player_set_protevil00);
 tolua_function(tolua_S,NULL,"set_protgood",toluaI_player_set_protgood00);
 tolua_function(tolua_S,NULL,"set_protundead",toluaI_player_set_protundead00);
 tolua_function(tolua_S,NULL,"set_invuln",toluaI_player_set_invuln00);
 tolua_function(tolua_S,NULL,"set_tim_invis",toluaI_player_set_tim_invis00);
 tolua_function(tolua_S,NULL,"set_tim_infra",toluaI_player_set_tim_infra00);
 tolua_function(tolua_S,NULL,"set_mental_barrier",toluaI_player_set_mental_barrier00);
 tolua_function(tolua_S,NULL,"set_oppose_acid",toluaI_player_set_oppose_acid00);
 tolua_function(tolua_S,NULL,"set_oppose_elec",toluaI_player_set_oppose_elec00);
 tolua_function(tolua_S,NULL,"set_oppose_fire",toluaI_player_set_oppose_fire00);
 tolua_function(tolua_S,NULL,"set_oppose_cold",toluaI_player_set_oppose_cold00);
 tolua_function(tolua_S,NULL,"set_oppose_pois",toluaI_player_set_oppose_pois00);
 tolua_function(tolua_S,NULL,"set_oppose_ld",toluaI_player_set_oppose_ld00);
 tolua_function(tolua_S,NULL,"set_oppose_cc",toluaI_player_set_oppose_cc00);
 tolua_function(tolua_S,NULL,"set_oppose_ss",toluaI_player_set_oppose_ss00);
 tolua_function(tolua_S,NULL,"set_oppose_nex",toluaI_player_set_oppose_nex00);
 tolua_function(tolua_S,NULL,"set_stun",toluaI_player_set_stun00);
 tolua_function(tolua_S,NULL,"set_cut",toluaI_player_set_cut00);
 tolua_function(tolua_S,NULL,"set_food",toluaI_player_set_food00);
 tolua_function(tolua_S,NULL,"check_experience",toluaI_player_check_experience00);
 tolua_function(tolua_S,NULL,"check_experience_obj",toluaI_player_check_experience_obj00);
 tolua_function(tolua_S,NULL,"gain_exp",toluaI_player_gain_exp00);
 tolua_function(tolua_S,NULL,"lose_exp",toluaI_player_lose_exp00);
 return 1;
}
/* Close function */
void tolua_player_close (lua_State* tolua_S)
{
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"A_STR");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"A_INT");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"A_WIS");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"A_DEX");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"A_CON");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"A_CHR");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SEX_FEMALE");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SEX_MALE");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"SEX_NEUTER");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"MAX_SEXES");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"PN_COMBINE");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"PN_REORDER");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"PU_BONUS");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"PU_TORCH");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"PU_BODY");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"PU_SANITY");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"PU_HP");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"PU_MANA");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"PU_SPELLS");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"PU_POWERS");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"PU_UN_VIEW");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"PU_UN_LITE");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"PU_VIEW");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"PU_LITE");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"PU_MONSTERS");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"PU_DISTANCE");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"PU_FLOW");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"PR_MISC");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"PR_TITLE");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"PR_LEV");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"PR_EXP");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"PR_STATS");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"PR_ARMOR");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"PR_HP");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"PR_MANA");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"PR_GOLD");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"PR_DEPTH");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"PR_HEALTH");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"PR_CUT");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"PR_STUN");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"PR_HUNGER");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"PR_TANK");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"PR_BLIND");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"PR_CONFUSED");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"PR_AFRAID");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"PR_POISONED");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"PR_STATE");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"PR_SPEED");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"PR_STUDY");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"PR_SANITY");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"PR_EXTRA");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"PR_BASIC");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"PR_MAP");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"PR_WIPE");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"PR_MH");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"PW_INVEN");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"PW_EQUIP");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"PW_SPELL");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"PW_PLAYER");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"PW_M_LIST");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"PW_MESSAGE");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"PW_OVERHEAD");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"PW_MONSTER");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"PW_OBJECT");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"PW_SNAPSHOT");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"PW_BORG_1");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"PW_BORG_2");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"deity");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"deity_info");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"player_type");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"player_race");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"player_race_mod");
 lua_getglobals(tolua_S);
 lua_pushstring(tolua_S,"energy_use"); lua_pushnil(tolua_S); lua_rawset(tolua_S,-3);
 lua_pop(tolua_S,1);
 lua_getglobals(tolua_S);
 lua_pushstring(tolua_S,"p_ptr"); lua_pushnil(tolua_S); lua_rawset(tolua_S,-3);
 lua_pop(tolua_S,1);
 lua_getglobals(tolua_S);
 lua_pushstring(tolua_S,"max_rp_idx"); lua_pushnil(tolua_S); lua_rawset(tolua_S,-3);
 lua_pop(tolua_S,1);
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"race_info");
 lua_getglobals(tolua_S);
 lua_pushstring(tolua_S,"rp_name"); lua_pushnil(tolua_S); lua_rawset(tolua_S,-3);
 lua_pop(tolua_S,1);
 lua_getglobals(tolua_S);
 lua_pushstring(tolua_S,"rp_text"); lua_pushnil(tolua_S); lua_rawset(tolua_S,-3);
 lua_pop(tolua_S,1);
 lua_getglobals(tolua_S);
 lua_pushstring(tolua_S,"max_rmp_idx"); lua_pushnil(tolua_S); lua_rawset(tolua_S,-3);
 lua_pop(tolua_S,1);
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"race_mod_info");
 lua_getglobals(tolua_S);
 lua_pushstring(tolua_S,"rmp_name"); lua_pushnil(tolua_S); lua_rawset(tolua_S,-3);
 lua_pop(tolua_S,1);
 lua_getglobals(tolua_S);
 lua_pushstring(tolua_S,"rmp_text"); lua_pushnil(tolua_S); lua_rawset(tolua_S,-3);
 lua_pop(tolua_S,1);
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"class_info");
 lua_getglobals(tolua_S);
 lua_pushstring(tolua_S,"c_name"); lua_pushnil(tolua_S); lua_rawset(tolua_S,-3);
 lua_pop(tolua_S,1);
 lua_getglobals(tolua_S);
 lua_pushstring(tolua_S,"c_text"); lua_pushnil(tolua_S); lua_rawset(tolua_S,-3);
 lua_pop(tolua_S,1);
 lua_getglobals(tolua_S);
 lua_pushstring(tolua_S,"py"); lua_pushnil(tolua_S); lua_rawset(tolua_S,-3);
 lua_pop(tolua_S,1);
 lua_getglobals(tolua_S);
 lua_pushstring(tolua_S,"px"); lua_pushnil(tolua_S); lua_rawset(tolua_S,-3);
 lua_pop(tolua_S,1);
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"set_parasite");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"set_disrupt_shield");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"set_prob_travel");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"set_tim_deadly");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"set_tim_res_time");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"set_tim_reflect");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"set_meditation");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"set_strike");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"set_walk_water");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"set_tim_ffall");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"set_tim_fire_aura");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"set_holy");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"set_grace");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"set_mimic");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"set_no_breeders");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"set_invis");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"set_lite");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"set_blind");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"set_confused");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"set_poisoned");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"set_afraid");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"set_paralyzed");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"set_image");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"set_fast");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"set_light_speed");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"set_slow");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"set_shield");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"set_blessed");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"set_hero");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"set_shero");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"set_protevil");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"set_protgood");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"set_protundead");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"set_invuln");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"set_tim_invis");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"set_tim_infra");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"set_mental_barrier");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"set_oppose_acid");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"set_oppose_elec");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"set_oppose_fire");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"set_oppose_cold");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"set_oppose_pois");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"set_oppose_ld");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"set_oppose_cc");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"set_oppose_ss");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"set_oppose_nex");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"set_stun");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"set_cut");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"set_food");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"check_experience");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"check_experience_obj");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"gain_exp");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"lose_exp");
}
