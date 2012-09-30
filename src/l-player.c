/*
** Lua binding: player
** Generated automatically by tolua 4.0a - angband.
*/

#include "lua/tolua.h"

/* Exported function */
int  tolua_player_open (lua_State* tolua_S);
void tolua_player_close (lua_State* tolua_S);

#include "angband.h"

/* function to register type */
static void toluaI_reg_types (lua_State* tolua_S)
{
(void) tolua_S;	/* Hack - prevent compiler warnings */
 tolua_usertype(tolua_S,"player_timed");
 tolua_usertype(tolua_S,"player_realm");
 tolua_usertype(tolua_S,"player_type");
 tolua_usertype(tolua_S,"player_state");
 tolua_usertype(tolua_S,"player_data");
 tolua_usertype(tolua_S,"player_stat");
 tolua_usertype(tolua_S,"player_spell");
}

/* get function: age of class  player_data */
static int toluaI_get_player_player_data_age(lua_State* tolua_S)
{
  TOLUA_GET_SELF(player_data);
  tolua_pushnumber(tolua_S,(long)self->age);
 return 1;
}

/* set function: age of class  player_data */
static int toluaI_set_player_player_data_age(lua_State* tolua_S)
{
  TOLUA_GET_SELF(player_data);
  if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
   TOLUA_ERR_ASSIGN;
  self->age = ((s16b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: ht of class  player_data */
static int toluaI_get_player_player_data_ht(lua_State* tolua_S)
{
  TOLUA_GET_SELF(player_data);
  tolua_pushnumber(tolua_S,(long)self->ht);
 return 1;
}

/* set function: ht of class  player_data */
static int toluaI_set_player_player_data_ht(lua_State* tolua_S)
{
  TOLUA_GET_SELF(player_data);
  if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
   TOLUA_ERR_ASSIGN;
  self->ht = ((s16b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: wt of class  player_data */
static int toluaI_get_player_player_data_wt(lua_State* tolua_S)
{
  TOLUA_GET_SELF(player_data);
  tolua_pushnumber(tolua_S,(long)self->wt);
 return 1;
}

/* set function: wt of class  player_data */
static int toluaI_set_player_player_data_wt(lua_State* tolua_S)
{
  TOLUA_GET_SELF(player_data);
  if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
   TOLUA_ERR_ASSIGN;
  self->wt = ((s16b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: sc of class  player_data */
static int toluaI_get_player_player_data_sc(lua_State* tolua_S)
{
  TOLUA_GET_SELF(player_data);
  tolua_pushnumber(tolua_S,(long)self->sc);
 return 1;
}

/* set function: sc of class  player_data */
static int toluaI_set_player_player_data_sc(lua_State* tolua_S)
{
  TOLUA_GET_SELF(player_data);
  if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
   TOLUA_ERR_ASSIGN;
  self->sc = ((s16b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: hitdie of class  player_data */
static int toluaI_get_player_player_data_hitdie(lua_State* tolua_S)
{
  TOLUA_GET_SELF(player_data);
  tolua_pushnumber(tolua_S,(long)self->hitdie);
 return 1;
}

/* set function: hitdie of class  player_data */
static int toluaI_set_player_player_data_hitdie(lua_State* tolua_S)
{
  TOLUA_GET_SELF(player_data);
  if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
   TOLUA_ERR_ASSIGN;
  self->hitdie = ((byte)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: psex of class  player_data */
static int toluaI_get_player_player_data_psex(lua_State* tolua_S)
{
  TOLUA_GET_SELF(player_data);
  tolua_pushnumber(tolua_S,(long)self->psex);
 return 1;
}

/* set function: psex of class  player_data */
static int toluaI_set_player_player_data_psex(lua_State* tolua_S)
{
  TOLUA_GET_SELF(player_data);
  if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
   TOLUA_ERR_ASSIGN;
  self->psex = ((byte)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: prace of class  player_data */
static int toluaI_get_player_player_data_prace(lua_State* tolua_S)
{
  TOLUA_GET_SELF(player_data);
  tolua_pushnumber(tolua_S,(long)self->prace);
 return 1;
}

/* set function: prace of class  player_data */
static int toluaI_set_player_player_data_prace(lua_State* tolua_S)
{
  TOLUA_GET_SELF(player_data);
  if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
   TOLUA_ERR_ASSIGN;
  self->prace = ((byte)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: pclass of class  player_data */
static int toluaI_get_player_player_data_pclass(lua_State* tolua_S)
{
  TOLUA_GET_SELF(player_data);
  tolua_pushnumber(tolua_S,(long)self->pclass);
 return 1;
}

/* set function: pclass of class  player_data */
static int toluaI_set_player_player_data_pclass(lua_State* tolua_S)
{
  TOLUA_GET_SELF(player_data);
  if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
   TOLUA_ERR_ASSIGN;
  self->pclass = ((byte)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: learned of class  player_realm */
static int toluaI_get_player_player_realm_learned(lua_State* tolua_S)
{
  TOLUA_GET_SELF(player_realm);
  tolua_pushnumber(tolua_S,(long)self->learned);
 return 1;
}

/* set function: learned of class  player_realm */
static int toluaI_set_player_player_realm_learned(lua_State* tolua_S)
{
  TOLUA_GET_SELF(player_realm);
  if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
   TOLUA_ERR_ASSIGN;
  self->learned = ((u32b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: worked of class  player_realm */
static int toluaI_get_player_player_realm_worked(lua_State* tolua_S)
{
  TOLUA_GET_SELF(player_realm);
  tolua_pushnumber(tolua_S,(long)self->worked);
 return 1;
}

/* set function: worked of class  player_realm */
static int toluaI_set_player_player_realm_worked(lua_State* tolua_S)
{
  TOLUA_GET_SELF(player_realm);
  if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
   TOLUA_ERR_ASSIGN;
  self->worked = ((u32b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: forgotten of class  player_realm */
static int toluaI_get_player_player_realm_forgotten(lua_State* tolua_S)
{
  TOLUA_GET_SELF(player_realm);
  tolua_pushnumber(tolua_S,(long)self->forgotten);
 return 1;
}

/* set function: forgotten of class  player_realm */
static int toluaI_set_player_player_realm_forgotten(lua_State* tolua_S)
{
  TOLUA_GET_SELF(player_realm);
  if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
   TOLUA_ERR_ASSIGN;
  self->forgotten = ((u32b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: realm of class  player_realm */
static int toluaI_get_player_player_realm_realm(lua_State* tolua_S)
{
  TOLUA_GET_SELF(player_realm);
  tolua_pushnumber(tolua_S,(long)self->realm);
 return 1;
}

/* set function: realm of class  player_realm */
static int toluaI_set_player_player_realm_realm(lua_State* tolua_S)
{
  TOLUA_GET_SELF(player_realm);
  if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
   TOLUA_ERR_ASSIGN;
  self->realm = ((byte)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: r of class  player_spell */
static int toluaI_get_player_player_spell_r(lua_State* tolua_S)
{
 int toluaI_index;
 TOLUA_ARRAY_SELF(player_spell);
 TOLUA_ARRAY_INDEX("player_spell: r",2);
 tolua_pushusertype(tolua_S,(void*)&self->r[toluaI_index],tolua_tag(tolua_S,"player_realm"));
 return 1;
}

/* set function: r of class  player_spell */
static int toluaI_set_player_player_spell_r(lua_State* tolua_S)
{
 int toluaI_index;
 TOLUA_ARRAY_SELF(player_spell);
 TOLUA_ARRAY_INDEX("player_spell: r",2);
  self->r[toluaI_index] = *((player_realm*)  tolua_getusertype(tolua_S,3,0));
 return 0;
}

/* get function: order of class  player_spell */
static int toluaI_get_player_player_spell_order(lua_State* tolua_S)
{
 int toluaI_index;
 TOLUA_ARRAY_SELF(player_spell);
 TOLUA_ARRAY_INDEX("player_spell: order",PY_MAX_SPELLS);
 tolua_pushnumber(tolua_S,(long)self->order[toluaI_index]);
 return 1;
}

/* set function: order of class  player_spell */
static int toluaI_set_player_player_spell_order(lua_State* tolua_S)
{
 int toluaI_index;
 TOLUA_ARRAY_SELF(player_spell);
 TOLUA_ARRAY_INDEX("player_spell: order",PY_MAX_SPELLS);
  self->order[toluaI_index] = ((byte)  tolua_getnumber(tolua_S,3,0));
 return 0;
}

/* get function: fast of class  player_timed */
static int toluaI_get_player_player_timed_fast(lua_State* tolua_S)
{
  TOLUA_GET_SELF(player_timed);
  tolua_pushnumber(tolua_S,(long)self->fast);
 return 1;
}

/* set function: fast of class  player_timed */
static int toluaI_set_player_player_timed_fast(lua_State* tolua_S)
{
  TOLUA_GET_SELF(player_timed);
  if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
   TOLUA_ERR_ASSIGN;
  self->fast = ((s16b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: slow of class  player_timed */
static int toluaI_get_player_player_timed_slow(lua_State* tolua_S)
{
  TOLUA_GET_SELF(player_timed);
  tolua_pushnumber(tolua_S,(long)self->slow);
 return 1;
}

/* set function: slow of class  player_timed */
static int toluaI_set_player_player_timed_slow(lua_State* tolua_S)
{
  TOLUA_GET_SELF(player_timed);
  if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
   TOLUA_ERR_ASSIGN;
  self->slow = ((s16b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: blind of class  player_timed */
static int toluaI_get_player_player_timed_blind(lua_State* tolua_S)
{
  TOLUA_GET_SELF(player_timed);
  tolua_pushnumber(tolua_S,(long)self->blind);
 return 1;
}

/* set function: blind of class  player_timed */
static int toluaI_set_player_player_timed_blind(lua_State* tolua_S)
{
  TOLUA_GET_SELF(player_timed);
  if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
   TOLUA_ERR_ASSIGN;
  self->blind = ((s16b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: paralyzed of class  player_timed */
static int toluaI_get_player_player_timed_paralyzed(lua_State* tolua_S)
{
  TOLUA_GET_SELF(player_timed);
  tolua_pushnumber(tolua_S,(long)self->paralyzed);
 return 1;
}

/* set function: paralyzed of class  player_timed */
static int toluaI_set_player_player_timed_paralyzed(lua_State* tolua_S)
{
  TOLUA_GET_SELF(player_timed);
  if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
   TOLUA_ERR_ASSIGN;
  self->paralyzed = ((s16b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: confused of class  player_timed */
static int toluaI_get_player_player_timed_confused(lua_State* tolua_S)
{
  TOLUA_GET_SELF(player_timed);
  tolua_pushnumber(tolua_S,(long)self->confused);
 return 1;
}

/* set function: confused of class  player_timed */
static int toluaI_set_player_player_timed_confused(lua_State* tolua_S)
{
  TOLUA_GET_SELF(player_timed);
  if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
   TOLUA_ERR_ASSIGN;
  self->confused = ((s16b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: afraid of class  player_timed */
static int toluaI_get_player_player_timed_afraid(lua_State* tolua_S)
{
  TOLUA_GET_SELF(player_timed);
  tolua_pushnumber(tolua_S,(long)self->afraid);
 return 1;
}

/* set function: afraid of class  player_timed */
static int toluaI_set_player_player_timed_afraid(lua_State* tolua_S)
{
  TOLUA_GET_SELF(player_timed);
  if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
   TOLUA_ERR_ASSIGN;
  self->afraid = ((s16b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: image of class  player_timed */
static int toluaI_get_player_player_timed_image(lua_State* tolua_S)
{
  TOLUA_GET_SELF(player_timed);
  tolua_pushnumber(tolua_S,(long)self->image);
 return 1;
}

/* set function: image of class  player_timed */
static int toluaI_set_player_player_timed_image(lua_State* tolua_S)
{
  TOLUA_GET_SELF(player_timed);
  if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
   TOLUA_ERR_ASSIGN;
  self->image = ((s16b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: poisoned of class  player_timed */
static int toluaI_get_player_player_timed_poisoned(lua_State* tolua_S)
{
  TOLUA_GET_SELF(player_timed);
  tolua_pushnumber(tolua_S,(long)self->poisoned);
 return 1;
}

/* set function: poisoned of class  player_timed */
static int toluaI_set_player_player_timed_poisoned(lua_State* tolua_S)
{
  TOLUA_GET_SELF(player_timed);
  if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
   TOLUA_ERR_ASSIGN;
  self->poisoned = ((s16b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: cut of class  player_timed */
static int toluaI_get_player_player_timed_cut(lua_State* tolua_S)
{
  TOLUA_GET_SELF(player_timed);
  tolua_pushnumber(tolua_S,(long)self->cut);
 return 1;
}

/* set function: cut of class  player_timed */
static int toluaI_set_player_player_timed_cut(lua_State* tolua_S)
{
  TOLUA_GET_SELF(player_timed);
  if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
   TOLUA_ERR_ASSIGN;
  self->cut = ((s16b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: stun of class  player_timed */
static int toluaI_get_player_player_timed_stun(lua_State* tolua_S)
{
  TOLUA_GET_SELF(player_timed);
  tolua_pushnumber(tolua_S,(long)self->stun);
 return 1;
}

/* set function: stun of class  player_timed */
static int toluaI_set_player_player_timed_stun(lua_State* tolua_S)
{
  TOLUA_GET_SELF(player_timed);
  if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
   TOLUA_ERR_ASSIGN;
  self->stun = ((s16b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: protevil of class  player_timed */
static int toluaI_get_player_player_timed_protevil(lua_State* tolua_S)
{
  TOLUA_GET_SELF(player_timed);
  tolua_pushnumber(tolua_S,(long)self->protevil);
 return 1;
}

/* set function: protevil of class  player_timed */
static int toluaI_set_player_player_timed_protevil(lua_State* tolua_S)
{
  TOLUA_GET_SELF(player_timed);
  if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
   TOLUA_ERR_ASSIGN;
  self->protevil = ((s16b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: invuln of class  player_timed */
static int toluaI_get_player_player_timed_invuln(lua_State* tolua_S)
{
  TOLUA_GET_SELF(player_timed);
  tolua_pushnumber(tolua_S,(long)self->invuln);
 return 1;
}

/* set function: invuln of class  player_timed */
static int toluaI_set_player_player_timed_invuln(lua_State* tolua_S)
{
  TOLUA_GET_SELF(player_timed);
  if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
   TOLUA_ERR_ASSIGN;
  self->invuln = ((s16b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: hero of class  player_timed */
static int toluaI_get_player_player_timed_hero(lua_State* tolua_S)
{
  TOLUA_GET_SELF(player_timed);
  tolua_pushnumber(tolua_S,(long)self->hero);
 return 1;
}

/* set function: hero of class  player_timed */
static int toluaI_set_player_player_timed_hero(lua_State* tolua_S)
{
  TOLUA_GET_SELF(player_timed);
  if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
   TOLUA_ERR_ASSIGN;
  self->hero = ((s16b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: shero of class  player_timed */
static int toluaI_get_player_player_timed_shero(lua_State* tolua_S)
{
  TOLUA_GET_SELF(player_timed);
  tolua_pushnumber(tolua_S,(long)self->shero);
 return 1;
}

/* set function: shero of class  player_timed */
static int toluaI_set_player_player_timed_shero(lua_State* tolua_S)
{
  TOLUA_GET_SELF(player_timed);
  if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
   TOLUA_ERR_ASSIGN;
  self->shero = ((s16b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: shield of class  player_timed */
static int toluaI_get_player_player_timed_shield(lua_State* tolua_S)
{
  TOLUA_GET_SELF(player_timed);
  tolua_pushnumber(tolua_S,(long)self->shield);
 return 1;
}

/* set function: shield of class  player_timed */
static int toluaI_set_player_player_timed_shield(lua_State* tolua_S)
{
  TOLUA_GET_SELF(player_timed);
  if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
   TOLUA_ERR_ASSIGN;
  self->shield = ((s16b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: blessed of class  player_timed */
static int toluaI_get_player_player_timed_blessed(lua_State* tolua_S)
{
  TOLUA_GET_SELF(player_timed);
  tolua_pushnumber(tolua_S,(long)self->blessed);
 return 1;
}

/* set function: blessed of class  player_timed */
static int toluaI_set_player_player_timed_blessed(lua_State* tolua_S)
{
  TOLUA_GET_SELF(player_timed);
  if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
   TOLUA_ERR_ASSIGN;
  self->blessed = ((s16b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: invis of class  player_timed */
static int toluaI_get_player_player_timed_invis(lua_State* tolua_S)
{
  TOLUA_GET_SELF(player_timed);
  tolua_pushnumber(tolua_S,(long)self->invis);
 return 1;
}

/* set function: invis of class  player_timed */
static int toluaI_set_player_player_timed_invis(lua_State* tolua_S)
{
  TOLUA_GET_SELF(player_timed);
  if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
   TOLUA_ERR_ASSIGN;
  self->invis = ((s16b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: infra of class  player_timed */
static int toluaI_get_player_player_timed_infra(lua_State* tolua_S)
{
  TOLUA_GET_SELF(player_timed);
  tolua_pushnumber(tolua_S,(long)self->infra);
 return 1;
}

/* set function: infra of class  player_timed */
static int toluaI_set_player_player_timed_infra(lua_State* tolua_S)
{
  TOLUA_GET_SELF(player_timed);
  if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
   TOLUA_ERR_ASSIGN;
  self->infra = ((s16b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: oppose_acid of class  player_timed */
static int toluaI_get_player_player_timed_oppose_acid(lua_State* tolua_S)
{
  TOLUA_GET_SELF(player_timed);
  tolua_pushnumber(tolua_S,(long)self->oppose_acid);
 return 1;
}

/* set function: oppose_acid of class  player_timed */
static int toluaI_set_player_player_timed_oppose_acid(lua_State* tolua_S)
{
  TOLUA_GET_SELF(player_timed);
  if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
   TOLUA_ERR_ASSIGN;
  self->oppose_acid = ((s16b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: oppose_elec of class  player_timed */
static int toluaI_get_player_player_timed_oppose_elec(lua_State* tolua_S)
{
  TOLUA_GET_SELF(player_timed);
  tolua_pushnumber(tolua_S,(long)self->oppose_elec);
 return 1;
}

/* set function: oppose_elec of class  player_timed */
static int toluaI_set_player_player_timed_oppose_elec(lua_State* tolua_S)
{
  TOLUA_GET_SELF(player_timed);
  if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
   TOLUA_ERR_ASSIGN;
  self->oppose_elec = ((s16b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: oppose_fire of class  player_timed */
static int toluaI_get_player_player_timed_oppose_fire(lua_State* tolua_S)
{
  TOLUA_GET_SELF(player_timed);
  tolua_pushnumber(tolua_S,(long)self->oppose_fire);
 return 1;
}

/* set function: oppose_fire of class  player_timed */
static int toluaI_set_player_player_timed_oppose_fire(lua_State* tolua_S)
{
  TOLUA_GET_SELF(player_timed);
  if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
   TOLUA_ERR_ASSIGN;
  self->oppose_fire = ((s16b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: oppose_cold of class  player_timed */
static int toluaI_get_player_player_timed_oppose_cold(lua_State* tolua_S)
{
  TOLUA_GET_SELF(player_timed);
  tolua_pushnumber(tolua_S,(long)self->oppose_cold);
 return 1;
}

/* set function: oppose_cold of class  player_timed */
static int toluaI_set_player_player_timed_oppose_cold(lua_State* tolua_S)
{
  TOLUA_GET_SELF(player_timed);
  if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
   TOLUA_ERR_ASSIGN;
  self->oppose_cold = ((s16b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: oppose_pois of class  player_timed */
static int toluaI_get_player_player_timed_oppose_pois(lua_State* tolua_S)
{
  TOLUA_GET_SELF(player_timed);
  tolua_pushnumber(tolua_S,(long)self->oppose_pois);
 return 1;
}

/* set function: oppose_pois of class  player_timed */
static int toluaI_set_player_player_timed_oppose_pois(lua_State* tolua_S)
{
  TOLUA_GET_SELF(player_timed);
  if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
   TOLUA_ERR_ASSIGN;
  self->oppose_pois = ((s16b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: esp of class  player_timed */
static int toluaI_get_player_player_timed_esp(lua_State* tolua_S)
{
  TOLUA_GET_SELF(player_timed);
  tolua_pushnumber(tolua_S,(long)self->esp);
 return 1;
}

/* set function: esp of class  player_timed */
static int toluaI_set_player_player_timed_esp(lua_State* tolua_S)
{
  TOLUA_GET_SELF(player_timed);
  if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
   TOLUA_ERR_ASSIGN;
  self->esp = ((s16b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: wraith_form of class  player_timed */
static int toluaI_get_player_player_timed_wraith_form(lua_State* tolua_S)
{
  TOLUA_GET_SELF(player_timed);
  tolua_pushnumber(tolua_S,(long)self->wraith_form);
 return 1;
}

/* set function: wraith_form of class  player_timed */
static int toluaI_set_player_player_timed_wraith_form(lua_State* tolua_S)
{
  TOLUA_GET_SELF(player_timed);
  if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
   TOLUA_ERR_ASSIGN;
  self->wraith_form = ((s16b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: resist_magic of class  player_timed */
static int toluaI_get_player_player_timed_resist_magic(lua_State* tolua_S)
{
  TOLUA_GET_SELF(player_timed);
  tolua_pushnumber(tolua_S,(long)self->resist_magic);
 return 1;
}

/* set function: resist_magic of class  player_timed */
static int toluaI_set_player_player_timed_resist_magic(lua_State* tolua_S)
{
  TOLUA_GET_SELF(player_timed);
  if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
   TOLUA_ERR_ASSIGN;
  self->resist_magic = ((s16b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: word_recall of class  player_timed */
static int toluaI_get_player_player_timed_word_recall(lua_State* tolua_S)
{
  TOLUA_GET_SELF(player_timed);
  tolua_pushnumber(tolua_S,(long)self->word_recall);
 return 1;
}

/* set function: word_recall of class  player_timed */
static int toluaI_set_player_player_timed_word_recall(lua_State* tolua_S)
{
  TOLUA_GET_SELF(player_timed);
  if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
   TOLUA_ERR_ASSIGN;
  self->word_recall = ((s16b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: died_from of class  player_state */
static int toluaI_get_player_player_state_died_from(lua_State* tolua_S)
{
 int toluaI_index;
 TOLUA_ARRAY_SELF(player_state);
 TOLUA_ARRAY_INDEX("player_state: died_from",80);
 tolua_pushnumber(tolua_S,(long)self->died_from[toluaI_index]);
 return 1;
}

/* set function: died_from of class  player_state */
static int toluaI_set_player_player_state_died_from(lua_State* tolua_S)
{
 int toluaI_index;
 TOLUA_ARRAY_SELF(player_state);
 TOLUA_ARRAY_INDEX("player_state: died_from",80);
  self->died_from[toluaI_index] = ((char)  tolua_getnumber(tolua_S,3,0));
 return 0;
}

/* get function: resting of class  player_state */
static int toluaI_get_player_player_state_resting(lua_State* tolua_S)
{
  TOLUA_GET_SELF(player_state);
  tolua_pushnumber(tolua_S,(long)self->resting);
 return 1;
}

/* set function: resting of class  player_state */
static int toluaI_set_player_player_state_resting(lua_State* tolua_S)
{
  TOLUA_GET_SELF(player_state);
  if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
   TOLUA_ERR_ASSIGN;
  self->resting = ((s16b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: running of class  player_state */
static int toluaI_get_player_player_state_running(lua_State* tolua_S)
{
  TOLUA_GET_SELF(player_state);
  tolua_pushnumber(tolua_S,(long)self->running);
 return 1;
}

/* set function: running of class  player_state */
static int toluaI_set_player_player_state_running(lua_State* tolua_S)
{
  TOLUA_GET_SELF(player_state);
  if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
   TOLUA_ERR_ASSIGN;
  self->running = ((s16b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: confusing of class  player_state */
static int toluaI_get_player_player_state_confusing(lua_State* tolua_S)
{
  TOLUA_GET_SELF(player_state);
  tolua_pushnumber(tolua_S,(long)self->confusing);
 return 1;
}

/* set function: confusing of class  player_state */
static int toluaI_set_player_player_state_confusing(lua_State* tolua_S)
{
  TOLUA_GET_SELF(player_state);
  if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
   TOLUA_ERR_ASSIGN;
  self->confusing = ((byte)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: searching of class  player_state */
static int toluaI_get_player_player_state_searching(lua_State* tolua_S)
{
  TOLUA_GET_SELF(player_state);
  tolua_pushnumber(tolua_S,(long)self->searching);
 return 1;
}

/* set function: searching of class  player_state */
static int toluaI_set_player_player_state_searching(lua_State* tolua_S)
{
  TOLUA_GET_SELF(player_state);
  if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
   TOLUA_ERR_ASSIGN;
  self->searching = ((byte)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: total_winner of class  player_state */
static int toluaI_get_player_player_state_total_winner(lua_State* tolua_S)
{
  TOLUA_GET_SELF(player_state);
  tolua_pushnumber(tolua_S,(long)self->total_winner);
 return 1;
}

/* set function: total_winner of class  player_state */
static int toluaI_set_player_player_state_total_winner(lua_State* tolua_S)
{
  TOLUA_GET_SELF(player_state);
  if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
   TOLUA_ERR_ASSIGN;
  self->total_winner = ((u16b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: panic_save of class  player_state */
static int toluaI_get_player_player_state_panic_save(lua_State* tolua_S)
{
  TOLUA_GET_SELF(player_state);
  tolua_pushnumber(tolua_S,(long)self->panic_save);
 return 1;
}

/* set function: panic_save of class  player_state */
static int toluaI_set_player_player_state_panic_save(lua_State* tolua_S)
{
  TOLUA_GET_SELF(player_state);
  if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
   TOLUA_ERR_ASSIGN;
  self->panic_save = ((u16b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: noscore of class  player_state */
static int toluaI_get_player_player_state_noscore(lua_State* tolua_S)
{
  TOLUA_GET_SELF(player_state);
  tolua_pushnumber(tolua_S,(long)self->noscore);
 return 1;
}

/* set function: noscore of class  player_state */
static int toluaI_set_player_player_state_noscore(lua_State* tolua_S)
{
  TOLUA_GET_SELF(player_state);
  if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
   TOLUA_ERR_ASSIGN;
  self->noscore = ((u16b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: is_dead of class  player_state */
static int toluaI_get_player_player_state_is_dead(lua_State* tolua_S)
{
  TOLUA_GET_SELF(player_state);
  tolua_pushbool(tolua_S,(int)self->is_dead);
 return 1;
}

/* set function: is_dead of class  player_state */
static int toluaI_set_player_player_state_is_dead(lua_State* tolua_S)
{
  TOLUA_GET_SELF(player_state);
  if (!tolua_istype(tolua_S,2,tolua_tag(tolua_S,"bool"),0))
   TOLUA_ERR_ASSIGN;
  self->is_dead = ((bool)  tolua_getbool(tolua_S,2,0));
 return 0;
}

/* get function: wizard of class  player_state */
static int toluaI_get_player_player_state_wizard(lua_State* tolua_S)
{
  TOLUA_GET_SELF(player_state);
  tolua_pushbool(tolua_S,(int)self->wizard);
 return 1;
}

/* set function: wizard of class  player_state */
static int toluaI_set_player_player_state_wizard(lua_State* tolua_S)
{
  TOLUA_GET_SELF(player_state);
  if (!tolua_istype(tolua_S,2,tolua_tag(tolua_S,"bool"),0))
   TOLUA_ERR_ASSIGN;
  self->wizard = ((bool)  tolua_getbool(tolua_S,2,0));
 return 0;
}

/* get function: playing of class  player_state */
static int toluaI_get_player_player_state_playing(lua_State* tolua_S)
{
  TOLUA_GET_SELF(player_state);
  tolua_pushbool(tolua_S,(int)self->playing);
 return 1;
}

/* set function: playing of class  player_state */
static int toluaI_set_player_player_state_playing(lua_State* tolua_S)
{
  TOLUA_GET_SELF(player_state);
  if (!tolua_istype(tolua_S,2,tolua_tag(tolua_S,"bool"),0))
   TOLUA_ERR_ASSIGN;
  self->playing = ((bool)  tolua_getbool(tolua_S,2,0));
 return 0;
}

/* get function: leaving of class  player_state */
static int toluaI_get_player_player_state_leaving(lua_State* tolua_S)
{
  TOLUA_GET_SELF(player_state);
  tolua_pushbool(tolua_S,(int)self->leaving);
 return 1;
}

/* set function: leaving of class  player_state */
static int toluaI_set_player_player_state_leaving(lua_State* tolua_S)
{
  TOLUA_GET_SELF(player_state);
  if (!tolua_istype(tolua_S,2,tolua_tag(tolua_S,"bool"),0))
   TOLUA_ERR_ASSIGN;
  self->leaving = ((bool)  tolua_getbool(tolua_S,2,0));
 return 0;
}

/* get function: create_up_stair of class  player_state */
static int toluaI_get_player_player_state_create_up_stair(lua_State* tolua_S)
{
  TOLUA_GET_SELF(player_state);
  tolua_pushbool(tolua_S,(int)self->create_up_stair);
 return 1;
}

/* set function: create_up_stair of class  player_state */
static int toluaI_set_player_player_state_create_up_stair(lua_State* tolua_S)
{
  TOLUA_GET_SELF(player_state);
  if (!tolua_istype(tolua_S,2,tolua_tag(tolua_S,"bool"),0))
   TOLUA_ERR_ASSIGN;
  self->create_up_stair = ((bool)  tolua_getbool(tolua_S,2,0));
 return 0;
}

/* get function: create_down_stair of class  player_state */
static int toluaI_get_player_player_state_create_down_stair(lua_State* tolua_S)
{
  TOLUA_GET_SELF(player_state);
  tolua_pushbool(tolua_S,(int)self->create_down_stair);
 return 1;
}

/* set function: create_down_stair of class  player_state */
static int toluaI_set_player_player_state_create_down_stair(lua_State* tolua_S)
{
  TOLUA_GET_SELF(player_state);
  if (!tolua_istype(tolua_S,2,tolua_tag(tolua_S,"bool"),0))
   TOLUA_ERR_ASSIGN;
  self->create_down_stair = ((bool)  tolua_getbool(tolua_S,2,0));
 return 0;
}

/* get function: energy_use of class  player_state */
static int toluaI_get_player_player_state_energy_use(lua_State* tolua_S)
{
  TOLUA_GET_SELF(player_state);
  tolua_pushnumber(tolua_S,(long)self->energy_use);
 return 1;
}

/* set function: energy_use of class  player_state */
static int toluaI_set_player_player_state_energy_use(lua_State* tolua_S)
{
  TOLUA_GET_SELF(player_state);
  if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
   TOLUA_ERR_ASSIGN;
  self->energy_use = ((s16b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: cumber_armor of class  player_state */
static int toluaI_get_player_player_state_cumber_armor(lua_State* tolua_S)
{
  TOLUA_GET_SELF(player_state);
  tolua_pushbool(tolua_S,(int)self->cumber_armor);
 return 1;
}

/* set function: cumber_armor of class  player_state */
static int toluaI_set_player_player_state_cumber_armor(lua_State* tolua_S)
{
  TOLUA_GET_SELF(player_state);
  if (!tolua_istype(tolua_S,2,tolua_tag(tolua_S,"bool"),0))
   TOLUA_ERR_ASSIGN;
  self->cumber_armor = ((bool)  tolua_getbool(tolua_S,2,0));
 return 0;
}

/* get function: cumber_glove of class  player_state */
static int toluaI_get_player_player_state_cumber_glove(lua_State* tolua_S)
{
  TOLUA_GET_SELF(player_state);
  tolua_pushbool(tolua_S,(int)self->cumber_glove);
 return 1;
}

/* set function: cumber_glove of class  player_state */
static int toluaI_set_player_player_state_cumber_glove(lua_State* tolua_S)
{
  TOLUA_GET_SELF(player_state);
  if (!tolua_istype(tolua_S,2,tolua_tag(tolua_S,"bool"),0))
   TOLUA_ERR_ASSIGN;
  self->cumber_glove = ((bool)  tolua_getbool(tolua_S,2,0));
 return 0;
}

/* get function: heavy_wield of class  player_state */
static int toluaI_get_player_player_state_heavy_wield(lua_State* tolua_S)
{
  TOLUA_GET_SELF(player_state);
  tolua_pushbool(tolua_S,(int)self->heavy_wield);
 return 1;
}

/* set function: heavy_wield of class  player_state */
static int toluaI_set_player_player_state_heavy_wield(lua_State* tolua_S)
{
  TOLUA_GET_SELF(player_state);
  if (!tolua_istype(tolua_S,2,tolua_tag(tolua_S,"bool"),0))
   TOLUA_ERR_ASSIGN;
  self->heavy_wield = ((bool)  tolua_getbool(tolua_S,2,0));
 return 0;
}

/* get function: heavy_shoot of class  player_state */
static int toluaI_get_player_player_state_heavy_shoot(lua_State* tolua_S)
{
  TOLUA_GET_SELF(player_state);
  tolua_pushbool(tolua_S,(int)self->heavy_shoot);
 return 1;
}

/* set function: heavy_shoot of class  player_state */
static int toluaI_set_player_player_state_heavy_shoot(lua_State* tolua_S)
{
  TOLUA_GET_SELF(player_state);
  if (!tolua_istype(tolua_S,2,tolua_tag(tolua_S,"bool"),0))
   TOLUA_ERR_ASSIGN;
  self->heavy_shoot = ((bool)  tolua_getbool(tolua_S,2,0));
 return 0;
}

/* get function: icky_wield of class  player_state */
static int toluaI_get_player_player_state_icky_wield(lua_State* tolua_S)
{
  TOLUA_GET_SELF(player_state);
  tolua_pushbool(tolua_S,(int)self->icky_wield);
 return 1;
}

/* set function: icky_wield of class  player_state */
static int toluaI_set_player_player_state_icky_wield(lua_State* tolua_S)
{
  TOLUA_GET_SELF(player_state);
  if (!tolua_istype(tolua_S,2,tolua_tag(tolua_S,"bool"),0))
   TOLUA_ERR_ASSIGN;
  self->icky_wield = ((bool)  tolua_getbool(tolua_S,2,0));
 return 0;
}

/* get function: detected of class  player_state */
static int toluaI_get_player_player_state_detected(lua_State* tolua_S)
{
  TOLUA_GET_SELF(player_state);
  tolua_pushbool(tolua_S,(int)self->detected);
 return 1;
}

/* set function: detected of class  player_state */
static int toluaI_set_player_player_state_detected(lua_State* tolua_S)
{
  TOLUA_GET_SELF(player_state);
  if (!tolua_istype(tolua_S,2,tolua_tag(tolua_S,"bool"),0))
   TOLUA_ERR_ASSIGN;
  self->detected = ((bool)  tolua_getbool(tolua_S,2,0));
 return 0;
}

/* get function: skip_more of class  player_state */
static int toluaI_get_player_player_state_skip_more(lua_State* tolua_S)
{
  TOLUA_GET_SELF(player_state);
  tolua_pushbool(tolua_S,(int)self->skip_more);
 return 1;
}

/* set function: skip_more of class  player_state */
static int toluaI_set_player_player_state_skip_more(lua_State* tolua_S)
{
  TOLUA_GET_SELF(player_state);
  if (!tolua_istype(tolua_S,2,tolua_tag(tolua_S,"bool"),0))
   TOLUA_ERR_ASSIGN;
  self->skip_more = ((bool)  tolua_getbool(tolua_S,2,0));
 return 0;
}

/* get function: mon_fight of class  player_state */
static int toluaI_get_player_player_state_mon_fight(lua_State* tolua_S)
{
  TOLUA_GET_SELF(player_state);
  tolua_pushbool(tolua_S,(int)self->mon_fight);
 return 1;
}

/* set function: mon_fight of class  player_state */
static int toluaI_set_player_player_state_mon_fight(lua_State* tolua_S)
{
  TOLUA_GET_SELF(player_state);
  if (!tolua_istype(tolua_S,2,tolua_tag(tolua_S,"bool"),0))
   TOLUA_ERR_ASSIGN;
  self->mon_fight = ((bool)  tolua_getbool(tolua_S,2,0));
 return 0;
}

/* get function: monk_armour_stat of class  player_state */
static int toluaI_get_player_player_state_monk_armour_stat(lua_State* tolua_S)
{
  TOLUA_GET_SELF(player_state);
  tolua_pushbool(tolua_S,(int)self->monk_armour_stat);
 return 1;
}

/* set function: monk_armour_stat of class  player_state */
static int toluaI_set_player_player_state_monk_armour_stat(lua_State* tolua_S)
{
  TOLUA_GET_SELF(player_state);
  if (!tolua_istype(tolua_S,2,tolua_tag(tolua_S,"bool"),0))
   TOLUA_ERR_ASSIGN;
  self->monk_armour_stat = ((bool)  tolua_getbool(tolua_S,2,0));
 return 0;
}

/* get function: noise_level of class  player_state */
static int toluaI_get_player_player_state_noise_level(lua_State* tolua_S)
{
  TOLUA_GET_SELF(player_state);
  tolua_pushnumber(tolua_S,(long)self->noise_level);
 return 1;
}

/* set function: noise_level of class  player_state */
static int toluaI_set_player_player_state_noise_level(lua_State* tolua_S)
{
  TOLUA_GET_SELF(player_state);
  if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
   TOLUA_ERR_ASSIGN;
  self->noise_level = ((byte)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: store_top of class  player_state */
static int toluaI_get_player_player_state_store_top(lua_State* tolua_S)
{
  TOLUA_GET_SELF(player_state);
  tolua_pushnumber(tolua_S,(long)self->store_top);
 return 1;
}

/* set function: store_top of class  player_state */
static int toluaI_set_player_player_state_store_top(lua_State* tolua_S)
{
  TOLUA_GET_SELF(player_state);
  if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
   TOLUA_ERR_ASSIGN;
  self->store_top = ((u16b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: max of class  player_stat */
static int toluaI_get_player_player_stat_max(lua_State* tolua_S)
{
  TOLUA_GET_SELF(player_stat);
  tolua_pushnumber(tolua_S,(long)self->max);
 return 1;
}

/* set function: max of class  player_stat */
static int toluaI_set_player_player_stat_max(lua_State* tolua_S)
{
  TOLUA_GET_SELF(player_stat);
  if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
   TOLUA_ERR_ASSIGN;
  self->max = ((s16b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: cur of class  player_stat */
static int toluaI_get_player_player_stat_cur(lua_State* tolua_S)
{
  TOLUA_GET_SELF(player_stat);
  tolua_pushnumber(tolua_S,(long)self->cur);
 return 1;
}

/* set function: cur of class  player_stat */
static int toluaI_set_player_player_stat_cur(lua_State* tolua_S)
{
  TOLUA_GET_SELF(player_stat);
  if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
   TOLUA_ERR_ASSIGN;
  self->cur = ((s16b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: use of class  player_stat */
static int toluaI_get_player_player_stat_use(lua_State* tolua_S)
{
  TOLUA_GET_SELF(player_stat);
  tolua_pushnumber(tolua_S,(long)self->use);
 return 1;
}

/* set function: use of class  player_stat */
static int toluaI_set_player_player_stat_use(lua_State* tolua_S)
{
  TOLUA_GET_SELF(player_stat);
  if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
   TOLUA_ERR_ASSIGN;
  self->use = ((s16b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: top of class  player_stat */
static int toluaI_get_player_player_stat_top(lua_State* tolua_S)
{
  TOLUA_GET_SELF(player_stat);
  tolua_pushnumber(tolua_S,(long)self->top);
 return 1;
}

/* set function: top of class  player_stat */
static int toluaI_set_player_player_stat_top(lua_State* tolua_S)
{
  TOLUA_GET_SELF(player_stat);
  if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
   TOLUA_ERR_ASSIGN;
  self->top = ((s16b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: add of class  player_stat */
static int toluaI_get_player_player_stat_add(lua_State* tolua_S)
{
  TOLUA_GET_SELF(player_stat);
  tolua_pushnumber(tolua_S,(long)self->add);
 return 1;
}

/* set function: add of class  player_stat */
static int toluaI_set_player_player_stat_add(lua_State* tolua_S)
{
  TOLUA_GET_SELF(player_stat);
  if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
   TOLUA_ERR_ASSIGN;
  self->add = ((s16b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: ind of class  player_stat */
static int toluaI_get_player_player_stat_ind(lua_State* tolua_S)
{
  TOLUA_GET_SELF(player_stat);
  tolua_pushnumber(tolua_S,(long)self->ind);
 return 1;
}

/* set function: ind of class  player_stat */
static int toluaI_set_player_player_stat_ind(lua_State* tolua_S)
{
  TOLUA_GET_SELF(player_stat);
  if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
   TOLUA_ERR_ASSIGN;
  self->ind = ((s16b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: px of class  player_type */
static int toluaI_get_player_player_type_px(lua_State* tolua_S)
{
  TOLUA_GET_SELF(player_type);
  tolua_pushnumber(tolua_S,(long)self->px);
 return 1;
}

/* set function: px of class  player_type */
static int toluaI_set_player_player_type_px(lua_State* tolua_S)
{
  TOLUA_GET_SELF(player_type);
  if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
   TOLUA_ERR_ASSIGN;
  self->px = ((s16b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: py of class  player_type */
static int toluaI_get_player_player_type_py(lua_State* tolua_S)
{
  TOLUA_GET_SELF(player_type);
  tolua_pushnumber(tolua_S,(long)self->py);
 return 1;
}

/* set function: py of class  player_type */
static int toluaI_set_player_player_type_py(lua_State* tolua_S)
{
  TOLUA_GET_SELF(player_type);
  if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
   TOLUA_ERR_ASSIGN;
  self->py = ((s16b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: rp of class  player_type */
static int toluaI_get_player_player_type_rp(lua_State* tolua_S)
{
  TOLUA_GET_SELF(player_type);
  tolua_pushusertype(tolua_S,(void*)&self->rp,tolua_tag(tolua_S,"player_data"));
 return 1;
}

/* set function: rp of class  player_type */
static int toluaI_set_player_player_type_rp(lua_State* tolua_S)
{
  TOLUA_GET_SELF(player_type);
  if (!tolua_istype(tolua_S,2,tolua_tag(tolua_S,"player_data"),0))
   TOLUA_ERR_ASSIGN;
  self->rp = *((player_data*)  tolua_getusertype(tolua_S,2,0));
 return 0;
}

/* get function: depth of class  player_type */
static int toluaI_get_player_player_type_depth(lua_State* tolua_S)
{
  TOLUA_GET_SELF(player_type);
  tolua_pushnumber(tolua_S,(long)self->depth);
 return 1;
}

/* set function: depth of class  player_type */
static int toluaI_set_player_player_type_depth(lua_State* tolua_S)
{
  TOLUA_GET_SELF(player_type);
  if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
   TOLUA_ERR_ASSIGN;
  self->depth = ((s16b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: max_lev of class  player_type */
static int toluaI_get_player_player_type_max_lev(lua_State* tolua_S)
{
  TOLUA_GET_SELF(player_type);
  tolua_pushnumber(tolua_S,(long)self->max_lev);
 return 1;
}

/* set function: max_lev of class  player_type */
static int toluaI_set_player_player_type_max_lev(lua_State* tolua_S)
{
  TOLUA_GET_SELF(player_type);
  if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
   TOLUA_ERR_ASSIGN;
  self->max_lev = ((s16b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: lev of class  player_type */
static int toluaI_get_player_player_type_lev(lua_State* tolua_S)
{
  TOLUA_GET_SELF(player_type);
  tolua_pushnumber(tolua_S,(long)self->lev);
 return 1;
}

/* set function: lev of class  player_type */
static int toluaI_set_player_player_type_lev(lua_State* tolua_S)
{
  TOLUA_GET_SELF(player_type);
  if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
   TOLUA_ERR_ASSIGN;
  self->lev = ((s16b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: exp_frac of class  player_type */
static int toluaI_get_player_player_type_exp_frac(lua_State* tolua_S)
{
  TOLUA_GET_SELF(player_type);
  tolua_pushnumber(tolua_S,(long)self->exp_frac);
 return 1;
}

/* set function: exp_frac of class  player_type */
static int toluaI_set_player_player_type_exp_frac(lua_State* tolua_S)
{
  TOLUA_GET_SELF(player_type);
  if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
   TOLUA_ERR_ASSIGN;
  self->exp_frac = ((u16b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: max_exp of class  player_type */
static int toluaI_get_player_player_type_max_exp(lua_State* tolua_S)
{
  TOLUA_GET_SELF(player_type);
  tolua_pushnumber(tolua_S,(long)self->max_exp);
 return 1;
}

/* set function: max_exp of class  player_type */
static int toluaI_set_player_player_type_max_exp(lua_State* tolua_S)
{
  TOLUA_GET_SELF(player_type);
  if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
   TOLUA_ERR_ASSIGN;
  self->max_exp = ((s32b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: exp of class  player_type */
static int toluaI_get_player_player_type_exp(lua_State* tolua_S)
{
  TOLUA_GET_SELF(player_type);
  tolua_pushnumber(tolua_S,(long)self->exp);
 return 1;
}

/* set function: exp of class  player_type */
static int toluaI_set_player_player_type_exp(lua_State* tolua_S)
{
  TOLUA_GET_SELF(player_type);
  if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
   TOLUA_ERR_ASSIGN;
  self->exp = ((s32b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: au of class  player_type */
static int toluaI_get_player_player_type_au(lua_State* tolua_S)
{
  TOLUA_GET_SELF(player_type);
  tolua_pushnumber(tolua_S,(long)self->au);
 return 1;
}

/* set function: au of class  player_type */
static int toluaI_set_player_player_type_au(lua_State* tolua_S)
{
  TOLUA_GET_SELF(player_type);
  if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
   TOLUA_ERR_ASSIGN;
  self->au = ((s32b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: place_num of class  player_type */
static int toluaI_get_player_player_type_place_num(lua_State* tolua_S)
{
  TOLUA_GET_SELF(player_type);
  tolua_pushnumber(tolua_S,(long)self->place_num);
 return 1;
}

/* set function: place_num of class  player_type */
static int toluaI_set_player_player_type_place_num(lua_State* tolua_S)
{
  TOLUA_GET_SELF(player_type);
  if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
   TOLUA_ERR_ASSIGN;
  self->place_num = ((s16b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: wilderness_x of class  player_type */
static int toluaI_get_player_player_type_wilderness_x(lua_State* tolua_S)
{
  TOLUA_GET_SELF(player_type);
  tolua_pushnumber(tolua_S,(long)self->wilderness_x);
 return 1;
}

/* set function: wilderness_x of class  player_type */
static int toluaI_set_player_player_type_wilderness_x(lua_State* tolua_S)
{
  TOLUA_GET_SELF(player_type);
  if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
   TOLUA_ERR_ASSIGN;
  self->wilderness_x = ((s32b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: wilderness_y of class  player_type */
static int toluaI_get_player_player_type_wilderness_y(lua_State* tolua_S)
{
  TOLUA_GET_SELF(player_type);
  tolua_pushnumber(tolua_S,(long)self->wilderness_y);
 return 1;
}

/* set function: wilderness_y of class  player_type */
static int toluaI_set_player_player_type_wilderness_y(lua_State* tolua_S)
{
  TOLUA_GET_SELF(player_type);
  if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
   TOLUA_ERR_ASSIGN;
  self->wilderness_y = ((s32b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: mhp of class  player_type */
static int toluaI_get_player_player_type_mhp(lua_State* tolua_S)
{
  TOLUA_GET_SELF(player_type);
  tolua_pushnumber(tolua_S,(long)self->mhp);
 return 1;
}

/* set function: mhp of class  player_type */
static int toluaI_set_player_player_type_mhp(lua_State* tolua_S)
{
  TOLUA_GET_SELF(player_type);
  if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
   TOLUA_ERR_ASSIGN;
  self->mhp = ((s16b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: chp of class  player_type */
static int toluaI_get_player_player_type_chp(lua_State* tolua_S)
{
  TOLUA_GET_SELF(player_type);
  tolua_pushnumber(tolua_S,(long)self->chp);
 return 1;
}

/* set function: chp of class  player_type */
static int toluaI_set_player_player_type_chp(lua_State* tolua_S)
{
  TOLUA_GET_SELF(player_type);
  if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
   TOLUA_ERR_ASSIGN;
  self->chp = ((s16b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: chp_frac of class  player_type */
static int toluaI_get_player_player_type_chp_frac(lua_State* tolua_S)
{
  TOLUA_GET_SELF(player_type);
  tolua_pushnumber(tolua_S,(long)self->chp_frac);
 return 1;
}

/* set function: chp_frac of class  player_type */
static int toluaI_set_player_player_type_chp_frac(lua_State* tolua_S)
{
  TOLUA_GET_SELF(player_type);
  if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
   TOLUA_ERR_ASSIGN;
  self->chp_frac = ((u16b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: msp of class  player_type */
static int toluaI_get_player_player_type_msp(lua_State* tolua_S)
{
  TOLUA_GET_SELF(player_type);
  tolua_pushnumber(tolua_S,(long)self->msp);
 return 1;
}

/* set function: msp of class  player_type */
static int toluaI_set_player_player_type_msp(lua_State* tolua_S)
{
  TOLUA_GET_SELF(player_type);
  if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
   TOLUA_ERR_ASSIGN;
  self->msp = ((s16b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: csp of class  player_type */
static int toluaI_get_player_player_type_csp(lua_State* tolua_S)
{
  TOLUA_GET_SELF(player_type);
  tolua_pushnumber(tolua_S,(long)self->csp);
 return 1;
}

/* set function: csp of class  player_type */
static int toluaI_set_player_player_type_csp(lua_State* tolua_S)
{
  TOLUA_GET_SELF(player_type);
  if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
   TOLUA_ERR_ASSIGN;
  self->csp = ((s16b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: csp_frac of class  player_type */
static int toluaI_get_player_player_type_csp_frac(lua_State* tolua_S)
{
  TOLUA_GET_SELF(player_type);
  tolua_pushnumber(tolua_S,(long)self->csp_frac);
 return 1;
}

/* set function: csp_frac of class  player_type */
static int toluaI_set_player_player_type_csp_frac(lua_State* tolua_S)
{
  TOLUA_GET_SELF(player_type);
  if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
   TOLUA_ERR_ASSIGN;
  self->csp_frac = ((u16b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: spell of class  player_type */
static int toluaI_get_player_player_type_spell(lua_State* tolua_S)
{
  TOLUA_GET_SELF(player_type);
  tolua_pushusertype(tolua_S,(void*)&self->spell,tolua_tag(tolua_S,"player_spell"));
 return 1;
}

/* set function: spell of class  player_type */
static int toluaI_set_player_player_type_spell(lua_State* tolua_S)
{
  TOLUA_GET_SELF(player_type);
  if (!tolua_istype(tolua_S,2,tolua_tag(tolua_S,"player_spell"),0))
   TOLUA_ERR_ASSIGN;
  self->spell = *((player_spell*)  tolua_getusertype(tolua_S,2,0));
 return 0;
}

/* get function: muta1 of class  player_type */
static int toluaI_get_player_player_type_muta1(lua_State* tolua_S)
{
  TOLUA_GET_SELF(player_type);
  tolua_pushnumber(tolua_S,(long)self->muta1);
 return 1;
}

/* set function: muta1 of class  player_type */
static int toluaI_set_player_player_type_muta1(lua_State* tolua_S)
{
  TOLUA_GET_SELF(player_type);
  if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
   TOLUA_ERR_ASSIGN;
  self->muta1 = ((u32b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: muta2 of class  player_type */
static int toluaI_get_player_player_type_muta2(lua_State* tolua_S)
{
  TOLUA_GET_SELF(player_type);
  tolua_pushnumber(tolua_S,(long)self->muta2);
 return 1;
}

/* set function: muta2 of class  player_type */
static int toluaI_set_player_player_type_muta2(lua_State* tolua_S)
{
  TOLUA_GET_SELF(player_type);
  if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
   TOLUA_ERR_ASSIGN;
  self->muta2 = ((u32b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: muta3 of class  player_type */
static int toluaI_get_player_player_type_muta3(lua_State* tolua_S)
{
  TOLUA_GET_SELF(player_type);
  tolua_pushnumber(tolua_S,(long)self->muta3);
 return 1;
}

/* set function: muta3 of class  player_type */
static int toluaI_set_player_player_type_muta3(lua_State* tolua_S)
{
  TOLUA_GET_SELF(player_type);
  if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
   TOLUA_ERR_ASSIGN;
  self->muta3 = ((u32b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: virtues of class  player_type */
static int toluaI_get_player_player_type_virtues(lua_State* tolua_S)
{
 int toluaI_index;
 TOLUA_ARRAY_SELF(player_type);
 TOLUA_ARRAY_INDEX("player_type: virtues",MAX_PLAYER_VIRTUES);
 tolua_pushnumber(tolua_S,(long)self->virtues[toluaI_index]);
 return 1;
}

/* set function: virtues of class  player_type */
static int toluaI_set_player_player_type_virtues(lua_State* tolua_S)
{
 int toluaI_index;
 TOLUA_ARRAY_SELF(player_type);
 TOLUA_ARRAY_INDEX("player_type: virtues",MAX_PLAYER_VIRTUES);
  self->virtues[toluaI_index] = ((s16b)  tolua_getnumber(tolua_S,3,0));
 return 0;
}

/* get function: vir_types of class  player_type */
static int toluaI_get_player_player_type_vir_types(lua_State* tolua_S)
{
 int toluaI_index;
 TOLUA_ARRAY_SELF(player_type);
 TOLUA_ARRAY_INDEX("player_type: vir_types",MAX_PLAYER_VIRTUES);
 tolua_pushnumber(tolua_S,(long)self->vir_types[toluaI_index]);
 return 1;
}

/* set function: vir_types of class  player_type */
static int toluaI_set_player_player_type_vir_types(lua_State* tolua_S)
{
 int toluaI_index;
 TOLUA_ARRAY_SELF(player_type);
 TOLUA_ARRAY_INDEX("player_type: vir_types",MAX_PLAYER_VIRTUES);
  self->vir_types[toluaI_index] = ((s16b)  tolua_getnumber(tolua_S,3,0));
 return 0;
}

/* get function: chaos_patron of class  player_type */
static int toluaI_get_player_player_type_chaos_patron(lua_State* tolua_S)
{
  TOLUA_GET_SELF(player_type);
  tolua_pushnumber(tolua_S,(long)self->chaos_patron);
 return 1;
}

/* set function: chaos_patron of class  player_type */
static int toluaI_set_player_player_type_chaos_patron(lua_State* tolua_S)
{
  TOLUA_GET_SELF(player_type);
  if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
   TOLUA_ERR_ASSIGN;
  self->chaos_patron = ((s16b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: energy of class  player_type */
static int toluaI_get_player_player_type_energy(lua_State* tolua_S)
{
  TOLUA_GET_SELF(player_type);
  tolua_pushnumber(tolua_S,(long)self->energy);
 return 1;
}

/* set function: energy of class  player_type */
static int toluaI_set_player_player_type_energy(lua_State* tolua_S)
{
  TOLUA_GET_SELF(player_type);
  if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
   TOLUA_ERR_ASSIGN;
  self->energy = ((s16b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: food of class  player_type */
static int toluaI_get_player_player_type_food(lua_State* tolua_S)
{
  TOLUA_GET_SELF(player_type);
  tolua_pushnumber(tolua_S,(long)self->food);
 return 1;
}

/* set function: food of class  player_type */
static int toluaI_set_player_player_type_food(lua_State* tolua_S)
{
  TOLUA_GET_SELF(player_type);
  if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
   TOLUA_ERR_ASSIGN;
  self->food = ((s16b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: player_hp of class  player_type */
static int toluaI_get_player_player_type_player_hp(lua_State* tolua_S)
{
 int toluaI_index;
 TOLUA_ARRAY_SELF(player_type);
 TOLUA_ARRAY_INDEX("player_type: player_hp",PY_MAX_LEVEL);
 tolua_pushnumber(tolua_S,(long)self->player_hp[toluaI_index]);
 return 1;
}

/* set function: player_hp of class  player_type */
static int toluaI_set_player_player_type_player_hp(lua_State* tolua_S)
{
 int toluaI_index;
 TOLUA_ARRAY_SELF(player_type);
 TOLUA_ARRAY_INDEX("player_type: player_hp",PY_MAX_LEVEL);
  self->player_hp[toluaI_index] = ((s16b)  tolua_getnumber(tolua_S,3,0));
 return 0;
}

/* get function: expfact of class  player_type */
static int toluaI_get_player_player_type_expfact(lua_State* tolua_S)
{
  TOLUA_GET_SELF(player_type);
  tolua_pushnumber(tolua_S,(long)self->expfact);
 return 1;
}

/* set function: expfact of class  player_type */
static int toluaI_set_player_player_type_expfact(lua_State* tolua_S)
{
  TOLUA_GET_SELF(player_type);
  if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
   TOLUA_ERR_ASSIGN;
  self->expfact = ((u16b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: tim of class  player_type */
static int toluaI_get_player_player_type_tim(lua_State* tolua_S)
{
  TOLUA_GET_SELF(player_type);
  tolua_pushusertype(tolua_S,(void*)&self->tim,tolua_tag(tolua_S,"player_timed"));
 return 1;
}

/* set function: tim of class  player_type */
static int toluaI_set_player_player_type_tim(lua_State* tolua_S)
{
  TOLUA_GET_SELF(player_type);
  if (!tolua_istype(tolua_S,2,tolua_tag(tolua_S,"player_timed"),0))
   TOLUA_ERR_ASSIGN;
  self->tim = *((player_timed*)  tolua_getusertype(tolua_S,2,0));
 return 0;
}

/* get function: state of class  player_type */
static int toluaI_get_player_player_type_state(lua_State* tolua_S)
{
  TOLUA_GET_SELF(player_type);
  tolua_pushusertype(tolua_S,(void*)&self->state,tolua_tag(tolua_S,"player_state"));
 return 1;
}

/* set function: state of class  player_type */
static int toluaI_set_player_player_type_state(lua_State* tolua_S)
{
  TOLUA_GET_SELF(player_type);
  if (!tolua_istype(tolua_S,2,tolua_tag(tolua_S,"player_state"),0))
   TOLUA_ERR_ASSIGN;
  self->state = *((player_state*)  tolua_getusertype(tolua_S,2,0));
 return 0;
}

/* get function: skills of class  player_type */
static int toluaI_get_player_player_type_skills(lua_State* tolua_S)
{
 int toluaI_index;
 TOLUA_ARRAY_SELF(player_type);
 TOLUA_ARRAY_INDEX("player_type: skills",MAX_SKILL);
 tolua_pushnumber(tolua_S,(long)self->skills[toluaI_index]);
 return 1;
}

/* set function: skills of class  player_type */
static int toluaI_set_player_player_type_skills(lua_State* tolua_S)
{
 int toluaI_index;
 TOLUA_ARRAY_SELF(player_type);
 TOLUA_ARRAY_INDEX("player_type: skills",MAX_SKILL);
  self->skills[toluaI_index] = ((s16b)  tolua_getnumber(tolua_S,3,0));
 return 0;
}

/* get function: stat of class  player_type */
static int toluaI_get_player_player_type_stat(lua_State* tolua_S)
{
 int toluaI_index;
 TOLUA_ARRAY_SELF(player_type);
 TOLUA_ARRAY_INDEX("player_type: stat",A_MAX);
 tolua_pushusertype(tolua_S,(void*)&self->stat[toluaI_index],tolua_tag(tolua_S,"player_stat"));
 return 1;
}

/* set function: stat of class  player_type */
static int toluaI_set_player_player_type_stat(lua_State* tolua_S)
{
 int toluaI_index;
 TOLUA_ARRAY_SELF(player_type);
 TOLUA_ARRAY_INDEX("player_type: stat",A_MAX);
  self->stat[toluaI_index] = *((player_stat*)  tolua_getusertype(tolua_S,3,0));
 return 0;
}

/* get function: sp_bonus of class  player_type */
static int toluaI_get_player_player_type_sp_bonus(lua_State* tolua_S)
{
  TOLUA_GET_SELF(player_type);
  tolua_pushnumber(tolua_S,(long)self->sp_bonus);
 return 1;
}

/* set function: sp_bonus of class  player_type */
static int toluaI_set_player_player_type_sp_bonus(lua_State* tolua_S)
{
  TOLUA_GET_SELF(player_type);
  if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
   TOLUA_ERR_ASSIGN;
  self->sp_bonus = ((int)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: align of class  player_type */
static int toluaI_get_player_player_type_align(lua_State* tolua_S)
{
  TOLUA_GET_SELF(player_type);
  tolua_pushnumber(tolua_S,(long)self->align);
 return 1;
}

/* set function: align of class  player_type */
static int toluaI_set_player_player_type_align(lua_State* tolua_S)
{
  TOLUA_GET_SELF(player_type);
  if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
   TOLUA_ERR_ASSIGN;
  self->align = ((s32b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: total_weight of class  player_type */
static int toluaI_get_player_player_type_total_weight(lua_State* tolua_S)
{
  TOLUA_GET_SELF(player_type);
  tolua_pushnumber(tolua_S,(long)self->total_weight);
 return 1;
}

/* set function: total_weight of class  player_type */
static int toluaI_set_player_player_type_total_weight(lua_State* tolua_S)
{
  TOLUA_GET_SELF(player_type);
  if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
   TOLUA_ERR_ASSIGN;
  self->total_weight = ((s16b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: target_set of class  player_type */
static int toluaI_get_player_player_type_target_set(lua_State* tolua_S)
{
  TOLUA_GET_SELF(player_type);
  tolua_pushnumber(tolua_S,(long)self->target_set);
 return 1;
}

/* set function: target_set of class  player_type */
static int toluaI_set_player_player_type_target_set(lua_State* tolua_S)
{
  TOLUA_GET_SELF(player_type);
  if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
   TOLUA_ERR_ASSIGN;
  self->target_set = ((s16b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: target_who of class  player_type */
static int toluaI_get_player_player_type_target_who(lua_State* tolua_S)
{
  TOLUA_GET_SELF(player_type);
  tolua_pushnumber(tolua_S,(long)self->target_who);
 return 1;
}

/* set function: target_who of class  player_type */
static int toluaI_set_player_player_type_target_who(lua_State* tolua_S)
{
  TOLUA_GET_SELF(player_type);
  if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
   TOLUA_ERR_ASSIGN;
  self->target_who = ((s16b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: target_row of class  player_type */
static int toluaI_get_player_player_type_target_row(lua_State* tolua_S)
{
  TOLUA_GET_SELF(player_type);
  tolua_pushnumber(tolua_S,(long)self->target_row);
 return 1;
}

/* set function: target_row of class  player_type */
static int toluaI_set_player_player_type_target_row(lua_State* tolua_S)
{
  TOLUA_GET_SELF(player_type);
  if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
   TOLUA_ERR_ASSIGN;
  self->target_row = ((s16b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: target_col of class  player_type */
static int toluaI_get_player_player_type_target_col(lua_State* tolua_S)
{
  TOLUA_GET_SELF(player_type);
  tolua_pushnumber(tolua_S,(long)self->target_col);
 return 1;
}

/* set function: target_col of class  player_type */
static int toluaI_set_player_player_type_target_col(lua_State* tolua_S)
{
  TOLUA_GET_SELF(player_type);
  if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
   TOLUA_ERR_ASSIGN;
  self->target_col = ((s16b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: health_who of class  player_type */
static int toluaI_get_player_player_type_health_who(lua_State* tolua_S)
{
  TOLUA_GET_SELF(player_type);
  tolua_pushnumber(tolua_S,(long)self->health_who);
 return 1;
}

/* set function: health_who of class  player_type */
static int toluaI_set_player_player_type_health_who(lua_State* tolua_S)
{
  TOLUA_GET_SELF(player_type);
  if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
   TOLUA_ERR_ASSIGN;
  self->health_who = ((s16b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: monster_race_idx of class  player_type */
static int toluaI_get_player_player_type_monster_race_idx(lua_State* tolua_S)
{
  TOLUA_GET_SELF(player_type);
  tolua_pushnumber(tolua_S,(long)self->monster_race_idx);
 return 1;
}

/* set function: monster_race_idx of class  player_type */
static int toluaI_set_player_player_type_monster_race_idx(lua_State* tolua_S)
{
  TOLUA_GET_SELF(player_type);
  if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
   TOLUA_ERR_ASSIGN;
  self->monster_race_idx = ((s16b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: max_seen_r_idx of class  player_type */
static int toluaI_get_player_player_type_max_seen_r_idx(lua_State* tolua_S)
{
  TOLUA_GET_SELF(player_type);
  tolua_pushnumber(tolua_S,(long)self->max_seen_r_idx);
 return 1;
}

/* set function: max_seen_r_idx of class  player_type */
static int toluaI_set_player_player_type_max_seen_r_idx(lua_State* tolua_S)
{
  TOLUA_GET_SELF(player_type);
  if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
   TOLUA_ERR_ASSIGN;
  self->max_seen_r_idx = ((u16b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: object_kind_idx of class  player_type */
static int toluaI_get_player_player_type_object_kind_idx(lua_State* tolua_S)
{
  TOLUA_GET_SELF(player_type);
  tolua_pushnumber(tolua_S,(long)self->object_kind_idx);
 return 1;
}

/* set function: object_kind_idx of class  player_type */
static int toluaI_set_player_player_type_object_kind_idx(lua_State* tolua_S)
{
  TOLUA_GET_SELF(player_type);
  if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
   TOLUA_ERR_ASSIGN;
  self->object_kind_idx = ((s16b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: new_spells of class  player_type */
static int toluaI_get_player_player_type_new_spells(lua_State* tolua_S)
{
  TOLUA_GET_SELF(player_type);
  tolua_pushnumber(tolua_S,(long)self->new_spells);
 return 1;
}

/* set function: new_spells of class  player_type */
static int toluaI_set_player_player_type_new_spells(lua_State* tolua_S)
{
  TOLUA_GET_SELF(player_type);
  if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
   TOLUA_ERR_ASSIGN;
  self->new_spells = ((s16b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: cur_lite of class  player_type */
static int toluaI_get_player_player_type_cur_lite(lua_State* tolua_S)
{
  TOLUA_GET_SELF(player_type);
  tolua_pushnumber(tolua_S,(long)self->cur_lite);
 return 1;
}

/* set function: cur_lite of class  player_type */
static int toluaI_set_player_player_type_cur_lite(lua_State* tolua_S)
{
  TOLUA_GET_SELF(player_type);
  if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
   TOLUA_ERR_ASSIGN;
  self->cur_lite = ((s16b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: notice of class  player_type */
static int toluaI_get_player_player_type_notice(lua_State* tolua_S)
{
  TOLUA_GET_SELF(player_type);
  tolua_pushnumber(tolua_S,(long)self->notice);
 return 1;
}

/* set function: notice of class  player_type */
static int toluaI_set_player_player_type_notice(lua_State* tolua_S)
{
  TOLUA_GET_SELF(player_type);
  if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
   TOLUA_ERR_ASSIGN;
  self->notice = ((u32b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: update of class  player_type */
static int toluaI_get_player_player_type_update(lua_State* tolua_S)
{
  TOLUA_GET_SELF(player_type);
  tolua_pushnumber(tolua_S,(long)self->update);
 return 1;
}

/* set function: update of class  player_type */
static int toluaI_set_player_player_type_update(lua_State* tolua_S)
{
  TOLUA_GET_SELF(player_type);
  if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
   TOLUA_ERR_ASSIGN;
  self->update = ((u32b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: redraw of class  player_type */
static int toluaI_get_player_player_type_redraw(lua_State* tolua_S)
{
  TOLUA_GET_SELF(player_type);
  tolua_pushnumber(tolua_S,(long)self->redraw);
 return 1;
}

/* set function: redraw of class  player_type */
static int toluaI_set_player_player_type_redraw(lua_State* tolua_S)
{
  TOLUA_GET_SELF(player_type);
  if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
   TOLUA_ERR_ASSIGN;
  self->redraw = ((u32b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: window of class  player_type */
static int toluaI_get_player_player_type_window(lua_State* tolua_S)
{
  TOLUA_GET_SELF(player_type);
  tolua_pushnumber(tolua_S,(long)self->window);
 return 1;
}

/* set function: window of class  player_type */
static int toluaI_set_player_player_type_window(lua_State* tolua_S)
{
  TOLUA_GET_SELF(player_type);
  if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
   TOLUA_ERR_ASSIGN;
  self->window = ((u32b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: flags of class  player_type */
static int toluaI_get_player_player_type_flags(lua_State* tolua_S)
{
 int toluaI_index;
 TOLUA_ARRAY_SELF(player_type);
 TOLUA_ARRAY_INDEX("player_type: flags",4);
 tolua_pushnumber(tolua_S,(long)self->flags[toluaI_index]);
 return 1;
}

/* set function: flags of class  player_type */
static int toluaI_set_player_player_type_flags(lua_State* tolua_S)
{
 int toluaI_index;
 TOLUA_ARRAY_SELF(player_type);
 TOLUA_ARRAY_INDEX("player_type: flags",4);
  self->flags[toluaI_index] = ((u32b)  tolua_getnumber(tolua_S,3,0));
 return 0;
}

/* get function: dis_to_h of class  player_type */
static int toluaI_get_player_player_type_dis_to_h(lua_State* tolua_S)
{
  TOLUA_GET_SELF(player_type);
  tolua_pushnumber(tolua_S,(long)self->dis_to_h);
 return 1;
}

/* set function: dis_to_h of class  player_type */
static int toluaI_set_player_player_type_dis_to_h(lua_State* tolua_S)
{
  TOLUA_GET_SELF(player_type);
  if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
   TOLUA_ERR_ASSIGN;
  self->dis_to_h = ((s16b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: dis_to_d of class  player_type */
static int toluaI_get_player_player_type_dis_to_d(lua_State* tolua_S)
{
  TOLUA_GET_SELF(player_type);
  tolua_pushnumber(tolua_S,(long)self->dis_to_d);
 return 1;
}

/* set function: dis_to_d of class  player_type */
static int toluaI_set_player_player_type_dis_to_d(lua_State* tolua_S)
{
  TOLUA_GET_SELF(player_type);
  if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
   TOLUA_ERR_ASSIGN;
  self->dis_to_d = ((s16b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: dis_to_a of class  player_type */
static int toluaI_get_player_player_type_dis_to_a(lua_State* tolua_S)
{
  TOLUA_GET_SELF(player_type);
  tolua_pushnumber(tolua_S,(long)self->dis_to_a);
 return 1;
}

/* set function: dis_to_a of class  player_type */
static int toluaI_set_player_player_type_dis_to_a(lua_State* tolua_S)
{
  TOLUA_GET_SELF(player_type);
  if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
   TOLUA_ERR_ASSIGN;
  self->dis_to_a = ((s16b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: dis_ac of class  player_type */
static int toluaI_get_player_player_type_dis_ac(lua_State* tolua_S)
{
  TOLUA_GET_SELF(player_type);
  tolua_pushnumber(tolua_S,(long)self->dis_ac);
 return 1;
}

/* set function: dis_ac of class  player_type */
static int toluaI_set_player_player_type_dis_ac(lua_State* tolua_S)
{
  TOLUA_GET_SELF(player_type);
  if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
   TOLUA_ERR_ASSIGN;
  self->dis_ac = ((s16b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: to_h of class  player_type */
static int toluaI_get_player_player_type_to_h(lua_State* tolua_S)
{
  TOLUA_GET_SELF(player_type);
  tolua_pushnumber(tolua_S,(long)self->to_h);
 return 1;
}

/* set function: to_h of class  player_type */
static int toluaI_set_player_player_type_to_h(lua_State* tolua_S)
{
  TOLUA_GET_SELF(player_type);
  if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
   TOLUA_ERR_ASSIGN;
  self->to_h = ((s16b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: to_d of class  player_type */
static int toluaI_get_player_player_type_to_d(lua_State* tolua_S)
{
  TOLUA_GET_SELF(player_type);
  tolua_pushnumber(tolua_S,(long)self->to_d);
 return 1;
}

/* set function: to_d of class  player_type */
static int toluaI_set_player_player_type_to_d(lua_State* tolua_S)
{
  TOLUA_GET_SELF(player_type);
  if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
   TOLUA_ERR_ASSIGN;
  self->to_d = ((s16b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: to_a of class  player_type */
static int toluaI_get_player_player_type_to_a(lua_State* tolua_S)
{
  TOLUA_GET_SELF(player_type);
  tolua_pushnumber(tolua_S,(long)self->to_a);
 return 1;
}

/* set function: to_a of class  player_type */
static int toluaI_set_player_player_type_to_a(lua_State* tolua_S)
{
  TOLUA_GET_SELF(player_type);
  if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
   TOLUA_ERR_ASSIGN;
  self->to_a = ((s16b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: ac of class  player_type */
static int toluaI_get_player_player_type_ac(lua_State* tolua_S)
{
  TOLUA_GET_SELF(player_type);
  tolua_pushnumber(tolua_S,(long)self->ac);
 return 1;
}

/* set function: ac of class  player_type */
static int toluaI_set_player_player_type_ac(lua_State* tolua_S)
{
  TOLUA_GET_SELF(player_type);
  if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
   TOLUA_ERR_ASSIGN;
  self->ac = ((s16b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: see_infra of class  player_type */
static int toluaI_get_player_player_type_see_infra(lua_State* tolua_S)
{
  TOLUA_GET_SELF(player_type);
  tolua_pushnumber(tolua_S,(long)self->see_infra);
 return 1;
}

/* set function: see_infra of class  player_type */
static int toluaI_set_player_player_type_see_infra(lua_State* tolua_S)
{
  TOLUA_GET_SELF(player_type);
  if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
   TOLUA_ERR_ASSIGN;
  self->see_infra = ((s16b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: noise of class  player_type */
static int toluaI_get_player_player_type_noise(lua_State* tolua_S)
{
  TOLUA_GET_SELF(player_type);
  tolua_pushnumber(tolua_S,(long)self->noise);
 return 1;
}

/* set function: noise of class  player_type */
static int toluaI_set_player_player_type_noise(lua_State* tolua_S)
{
  TOLUA_GET_SELF(player_type);
  if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
   TOLUA_ERR_ASSIGN;
  self->noise = ((u32b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: num_blow of class  player_type */
static int toluaI_get_player_player_type_num_blow(lua_State* tolua_S)
{
  TOLUA_GET_SELF(player_type);
  tolua_pushnumber(tolua_S,(long)self->num_blow);
 return 1;
}

/* set function: num_blow of class  player_type */
static int toluaI_set_player_player_type_num_blow(lua_State* tolua_S)
{
  TOLUA_GET_SELF(player_type);
  if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
   TOLUA_ERR_ASSIGN;
  self->num_blow = ((s16b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: num_fire of class  player_type */
static int toluaI_get_player_player_type_num_fire(lua_State* tolua_S)
{
  TOLUA_GET_SELF(player_type);
  tolua_pushnumber(tolua_S,(long)self->num_fire);
 return 1;
}

/* set function: num_fire of class  player_type */
static int toluaI_set_player_player_type_num_fire(lua_State* tolua_S)
{
  TOLUA_GET_SELF(player_type);
  if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
   TOLUA_ERR_ASSIGN;
  self->num_fire = ((s16b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: ammo_mult of class  player_type */
static int toluaI_get_player_player_type_ammo_mult(lua_State* tolua_S)
{
  TOLUA_GET_SELF(player_type);
  tolua_pushnumber(tolua_S,(long)self->ammo_mult);
 return 1;
}

/* set function: ammo_mult of class  player_type */
static int toluaI_set_player_player_type_ammo_mult(lua_State* tolua_S)
{
  TOLUA_GET_SELF(player_type);
  if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
   TOLUA_ERR_ASSIGN;
  self->ammo_mult = ((byte)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: ammo_tval of class  player_type */
static int toluaI_get_player_player_type_ammo_tval(lua_State* tolua_S)
{
  TOLUA_GET_SELF(player_type);
  tolua_pushnumber(tolua_S,(long)self->ammo_tval);
 return 1;
}

/* set function: ammo_tval of class  player_type */
static int toluaI_set_player_player_type_ammo_tval(lua_State* tolua_S)
{
  TOLUA_GET_SELF(player_type);
  if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
   TOLUA_ERR_ASSIGN;
  self->ammo_tval = ((byte)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: bow_energy of class  player_type */
static int toluaI_get_player_player_type_bow_energy(lua_State* tolua_S)
{
  TOLUA_GET_SELF(player_type);
  tolua_pushnumber(tolua_S,(long)self->bow_energy);
 return 1;
}

/* set function: bow_energy of class  player_type */
static int toluaI_set_player_player_type_bow_energy(lua_State* tolua_S)
{
  TOLUA_GET_SELF(player_type);
  if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
   TOLUA_ERR_ASSIGN;
  self->bow_energy = ((byte)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: pspeed of class  player_type */
static int toluaI_get_player_player_type_pspeed(lua_State* tolua_S)
{
  TOLUA_GET_SELF(player_type);
  tolua_pushnumber(tolua_S,(long)self->pspeed);
 return 1;
}

/* set function: pspeed of class  player_type */
static int toluaI_set_player_player_type_pspeed(lua_State* tolua_S)
{
  TOLUA_GET_SELF(player_type);
  if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
   TOLUA_ERR_ASSIGN;
  self->pspeed = ((s16b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: pet_follow_distance of class  player_type */
static int toluaI_get_player_player_type_pet_follow_distance(lua_State* tolua_S)
{
  TOLUA_GET_SELF(player_type);
  tolua_pushnumber(tolua_S,(long)self->pet_follow_distance);
 return 1;
}

/* set function: pet_follow_distance of class  player_type */
static int toluaI_set_player_player_type_pet_follow_distance(lua_State* tolua_S)
{
  TOLUA_GET_SELF(player_type);
  if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
   TOLUA_ERR_ASSIGN;
  self->pet_follow_distance = ((s16b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: pet_open_doors of class  player_type */
static int toluaI_get_player_player_type_pet_open_doors(lua_State* tolua_S)
{
  TOLUA_GET_SELF(player_type);
  tolua_pushnumber(tolua_S,(long)self->pet_open_doors);
 return 1;
}

/* set function: pet_open_doors of class  player_type */
static int toluaI_set_player_player_type_pet_open_doors(lua_State* tolua_S)
{
  TOLUA_GET_SELF(player_type);
  if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
   TOLUA_ERR_ASSIGN;
  self->pet_open_doors = ((byte)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: pet_pickup_items of class  player_type */
static int toluaI_get_player_player_type_pet_pickup_items(lua_State* tolua_S)
{
  TOLUA_GET_SELF(player_type);
  tolua_pushnumber(tolua_S,(long)self->pet_pickup_items);
 return 1;
}

/* set function: pet_pickup_items of class  player_type */
static int toluaI_set_player_player_type_pet_pickup_items(lua_State* tolua_S)
{
  TOLUA_GET_SELF(player_type);
  if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
   TOLUA_ERR_ASSIGN;
  self->pet_pickup_items = ((byte)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: options of class  player_type */
static int toluaI_get_player_player_type_options(lua_State* tolua_S)
{
 int toluaI_index;
 TOLUA_ARRAY_SELF(player_type);
 TOLUA_ARRAY_INDEX("player_type: options",OPT_PLAYER);
 tolua_pushbool(tolua_S,(int)self->options[toluaI_index]);
 return 1;
}

/* set function: options of class  player_type */
static int toluaI_set_player_player_type_options(lua_State* tolua_S)
{
 int toluaI_index;
 TOLUA_ARRAY_SELF(player_type);
 TOLUA_ARRAY_INDEX("player_type: options",OPT_PLAYER);
  self->options[toluaI_index] = ((bool)  tolua_getbool(tolua_S,3,0));
 return 0;
}

/* get function: birth of class  player_type */
static int toluaI_get_player_player_type_birth(lua_State* tolua_S)
{
 int toluaI_index;
 TOLUA_ARRAY_SELF(player_type);
 TOLUA_ARRAY_INDEX("player_type: birth",OPT_BIRTH);
 tolua_pushbool(tolua_S,(int)self->birth[toluaI_index]);
 return 1;
}

/* set function: birth of class  player_type */
static int toluaI_set_player_player_type_birth(lua_State* tolua_S)
{
 int toluaI_index;
 TOLUA_ARRAY_SELF(player_type);
 TOLUA_ARRAY_INDEX("player_type: birth",OPT_BIRTH);
  self->birth[toluaI_index] = ((bool)  tolua_getbool(tolua_S,3,0));
 return 0;
}

/* get function: p_ptr */
static int toluaI_get_player_player(lua_State* tolua_S)
{
  tolua_pushusertype(tolua_S,(void*)p_ptr,tolua_tag(tolua_S,"player_type"));
 return 1;
}

/* set function: p_ptr */
static int toluaI_set_player_player(lua_State* tolua_S)
{
  if (!tolua_istype(tolua_S,1,tolua_tag(tolua_S,"player_type"),0))
   TOLUA_ERR_ASSIGN;
  p_ptr = ((player_type*)  tolua_getusertype(tolua_S,1,0));
 return 0;
}

/* get function: ironman_nightmare */
static int toluaI_get_player_ironman_nightmare(lua_State* tolua_S)
{
  tolua_pushbool(tolua_S,(int)ironman_nightmare);
 return 1;
}

/* set function: ironman_nightmare */
static int toluaI_set_player_ironman_nightmare(lua_State* tolua_S)
{
  if (!tolua_istype(tolua_S,1,tolua_tag(tolua_S,"bool"),0))
   TOLUA_ERR_ASSIGN;
  ironman_nightmare = ((bool)  tolua_getbool(tolua_S,1,0));
 return 0;
}

/* get function: autosave_l */
static int toluaI_get_player_autosave_l(lua_State* tolua_S)
{
  tolua_pushnumber(tolua_S,(long)autosave_l);
 return 1;
}

/* set function: autosave_l */
static int toluaI_set_player_autosave_l(lua_State* tolua_S)
{
  if (!tolua_istype(tolua_S,1,LUA_TNUMBER,0))
   TOLUA_ERR_ASSIGN;
  autosave_l = ((byte)  tolua_getnumber(tolua_S,1,0));
 return 0;
}

/* function: inc_blind */
static int toluaI_player_inc_blind00(lua_State* tolua_S)
{
 if (!tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
     !tolua_isnoobj(tolua_S,2))
 {
  TOLUA_ERR_FN(inc_blind);
 } else {
  int v = ((int)  tolua_getnumber(tolua_S,1,0));
  bool toluaI_ret = (bool)  inc_blind(v);
  tolua_pushbool(tolua_S,(int)toluaI_ret);
 }
 return 1;
}

/* function: clear_blind */
static int toluaI_player_clear_blind00(lua_State* tolua_S)
{
 if (!tolua_isnoobj(tolua_S,1))
 {
  TOLUA_ERR_FN(clear_blind);
 } else {
  bool toluaI_ret = (bool)  clear_blind();
  tolua_pushbool(tolua_S,(int)toluaI_ret);
 }
 return 1;
}

/* function: inc_confused */
static int toluaI_player_inc_confused00(lua_State* tolua_S)
{
 if (!tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
     !tolua_isnoobj(tolua_S,2))
 {
  TOLUA_ERR_FN(inc_confused);
 } else {
  int v = ((int)  tolua_getnumber(tolua_S,1,0));
  bool toluaI_ret = (bool)  inc_confused(v);
  tolua_pushbool(tolua_S,(int)toluaI_ret);
 }
 return 1;
}

/* function: clear_confused */
static int toluaI_player_clear_confused00(lua_State* tolua_S)
{
 if (!tolua_isnoobj(tolua_S,1))
 {
  TOLUA_ERR_FN(clear_confused);
 } else {
  bool toluaI_ret = (bool)  clear_confused();
  tolua_pushbool(tolua_S,(int)toluaI_ret);
 }
 return 1;
}

/* function: inc_poisoned */
static int toluaI_player_inc_poisoned00(lua_State* tolua_S)
{
 if (!tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
     !tolua_isnoobj(tolua_S,2))
 {
  TOLUA_ERR_FN(inc_poisoned);
 } else {
  int v = ((int)  tolua_getnumber(tolua_S,1,0));
  bool toluaI_ret = (bool)  inc_poisoned(v);
  tolua_pushbool(tolua_S,(int)toluaI_ret);
 }
 return 1;
}

/* function: clear_poisoned */
static int toluaI_player_clear_poisoned00(lua_State* tolua_S)
{
 if (!tolua_isnoobj(tolua_S,1))
 {
  TOLUA_ERR_FN(clear_poisoned);
 } else {
  bool toluaI_ret = (bool)  clear_poisoned();
  tolua_pushbool(tolua_S,(int)toluaI_ret);
 }
 return 1;
}

/* function: inc_afraid */
static int toluaI_player_inc_afraid00(lua_State* tolua_S)
{
 if (!tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
     !tolua_isnoobj(tolua_S,2))
 {
  TOLUA_ERR_FN(inc_afraid);
 } else {
  int v = ((int)  tolua_getnumber(tolua_S,1,0));
  bool toluaI_ret = (bool)  inc_afraid(v);
  tolua_pushbool(tolua_S,(int)toluaI_ret);
 }
 return 1;
}

/* function: clear_afraid */
static int toluaI_player_clear_afraid00(lua_State* tolua_S)
{
 if (!tolua_isnoobj(tolua_S,1))
 {
  TOLUA_ERR_FN(clear_afraid);
 } else {
  bool toluaI_ret = (bool)  clear_afraid();
  tolua_pushbool(tolua_S,(int)toluaI_ret);
 }
 return 1;
}

/* function: inc_paralyzed */
static int toluaI_player_inc_paralyzed00(lua_State* tolua_S)
{
 if (!tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
     !tolua_isnoobj(tolua_S,2))
 {
  TOLUA_ERR_FN(inc_paralyzed);
 } else {
  int v = ((int)  tolua_getnumber(tolua_S,1,0));
  bool toluaI_ret = (bool)  inc_paralyzed(v);
  tolua_pushbool(tolua_S,(int)toluaI_ret);
 }
 return 1;
}

/* function: clear_paralyzed */
static int toluaI_player_clear_paralyzed00(lua_State* tolua_S)
{
 if (!tolua_isnoobj(tolua_S,1))
 {
  TOLUA_ERR_FN(clear_paralyzed);
 } else {
  bool toluaI_ret = (bool)  clear_paralyzed();
  tolua_pushbool(tolua_S,(int)toluaI_ret);
 }
 return 1;
}

/* function: inc_image */
static int toluaI_player_inc_image00(lua_State* tolua_S)
{
 if (!tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
     !tolua_isnoobj(tolua_S,2))
 {
  TOLUA_ERR_FN(inc_image);
 } else {
  int v = ((int)  tolua_getnumber(tolua_S,1,0));
  bool toluaI_ret = (bool)  inc_image(v);
  tolua_pushbool(tolua_S,(int)toluaI_ret);
 }
 return 1;
}

/* function: clear_image */
static int toluaI_player_clear_image00(lua_State* tolua_S)
{
 if (!tolua_isnoobj(tolua_S,1))
 {
  TOLUA_ERR_FN(clear_image);
 } else {
  bool toluaI_ret = (bool)  clear_image();
  tolua_pushbool(tolua_S,(int)toluaI_ret);
 }
 return 1;
}

/* function: inc_fast */
static int toluaI_player_inc_fast00(lua_State* tolua_S)
{
 if (!tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
     !tolua_isnoobj(tolua_S,2))
 {
  TOLUA_ERR_FN(inc_fast);
 } else {
  int v = ((int)  tolua_getnumber(tolua_S,1,0));
  bool toluaI_ret = (bool)  inc_fast(v);
  tolua_pushbool(tolua_S,(int)toluaI_ret);
 }
 return 1;
}

/* function: clear_fast */
static int toluaI_player_clear_fast00(lua_State* tolua_S)
{
 if (!tolua_isnoobj(tolua_S,1))
 {
  TOLUA_ERR_FN(clear_fast);
 } else {
  bool toluaI_ret = (bool)  clear_fast();
  tolua_pushbool(tolua_S,(int)toluaI_ret);
 }
 return 1;
}

/* function: inc_slow */
static int toluaI_player_inc_slow00(lua_State* tolua_S)
{
 if (!tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
     !tolua_isnoobj(tolua_S,2))
 {
  TOLUA_ERR_FN(inc_slow);
 } else {
  int v = ((int)  tolua_getnumber(tolua_S,1,0));
  bool toluaI_ret = (bool)  inc_slow(v);
  tolua_pushbool(tolua_S,(int)toluaI_ret);
 }
 return 1;
}

/* function: clear_slow */
static int toluaI_player_clear_slow00(lua_State* tolua_S)
{
 if (!tolua_isnoobj(tolua_S,1))
 {
  TOLUA_ERR_FN(clear_slow);
 } else {
  bool toluaI_ret = (bool)  clear_slow();
  tolua_pushbool(tolua_S,(int)toluaI_ret);
 }
 return 1;
}

/* function: inc_shield */
static int toluaI_player_inc_shield00(lua_State* tolua_S)
{
 if (!tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
     !tolua_isnoobj(tolua_S,2))
 {
  TOLUA_ERR_FN(inc_shield);
 } else {
  int v = ((int)  tolua_getnumber(tolua_S,1,0));
  bool toluaI_ret = (bool)  inc_shield(v);
  tolua_pushbool(tolua_S,(int)toluaI_ret);
 }
 return 1;
}

/* function: inc_blessed */
static int toluaI_player_inc_blessed00(lua_State* tolua_S)
{
 if (!tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
     !tolua_isnoobj(tolua_S,2))
 {
  TOLUA_ERR_FN(inc_blessed);
 } else {
  int v = ((int)  tolua_getnumber(tolua_S,1,0));
  bool toluaI_ret = (bool)  inc_blessed(v);
  tolua_pushbool(tolua_S,(int)toluaI_ret);
 }
 return 1;
}

/* function: inc_hero */
static int toluaI_player_inc_hero00(lua_State* tolua_S)
{
 if (!tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
     !tolua_isnoobj(tolua_S,2))
 {
  TOLUA_ERR_FN(inc_hero);
 } else {
  int v = ((int)  tolua_getnumber(tolua_S,1,0));
  bool toluaI_ret = (bool)  inc_hero(v);
  tolua_pushbool(tolua_S,(int)toluaI_ret);
 }
 return 1;
}

/* function: inc_shero */
static int toluaI_player_inc_shero00(lua_State* tolua_S)
{
 if (!tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
     !tolua_isnoobj(tolua_S,2))
 {
  TOLUA_ERR_FN(inc_shero);
 } else {
  int v = ((int)  tolua_getnumber(tolua_S,1,0));
  bool toluaI_ret = (bool)  inc_shero(v);
  tolua_pushbool(tolua_S,(int)toluaI_ret);
 }
 return 1;
}

/* function: inc_protevil */
static int toluaI_player_inc_protevil00(lua_State* tolua_S)
{
 if (!tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
     !tolua_isnoobj(tolua_S,2))
 {
  TOLUA_ERR_FN(inc_protevil);
 } else {
  int v = ((int)  tolua_getnumber(tolua_S,1,0));
  bool toluaI_ret = (bool)  inc_protevil(v);
  tolua_pushbool(tolua_S,(int)toluaI_ret);
 }
 return 1;
}

/* function: inc_wraith_form */
static int toluaI_player_inc_wraith_form00(lua_State* tolua_S)
{
 if (!tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
     !tolua_isnoobj(tolua_S,2))
 {
  TOLUA_ERR_FN(inc_wraith_form);
 } else {
  int v = ((int)  tolua_getnumber(tolua_S,1,0));
  bool toluaI_ret = (bool)  inc_wraith_form(v);
  tolua_pushbool(tolua_S,(int)toluaI_ret);
 }
 return 1;
}

/* function: inc_tim_esp */
static int toluaI_player_inc_tim_esp00(lua_State* tolua_S)
{
 if (!tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
     !tolua_isnoobj(tolua_S,2))
 {
  TOLUA_ERR_FN(inc_tim_esp);
 } else {
  int v = ((int)  tolua_getnumber(tolua_S,1,0));
  bool toluaI_ret = (bool)  inc_tim_esp(v);
  tolua_pushbool(tolua_S,(int)toluaI_ret);
 }
 return 1;
}

/* function: clear_tim_esp */
static int toluaI_player_clear_tim_esp00(lua_State* tolua_S)
{
 if (!tolua_isnoobj(tolua_S,1))
 {
  TOLUA_ERR_FN(clear_tim_esp);
 } else {
  bool toluaI_ret = (bool)  clear_tim_esp();
  tolua_pushbool(tolua_S,(int)toluaI_ret);
 }
 return 1;
}

/* function: inc_invuln */
static int toluaI_player_inc_invuln00(lua_State* tolua_S)
{
 if (!tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
     !tolua_isnoobj(tolua_S,2))
 {
  TOLUA_ERR_FN(inc_invuln);
 } else {
  int v = ((int)  tolua_getnumber(tolua_S,1,0));
  bool toluaI_ret = (bool)  inc_invuln(v);
  tolua_pushbool(tolua_S,(int)toluaI_ret);
 }
 return 1;
}

/* function: inc_tim_invis */
static int toluaI_player_inc_tim_invis00(lua_State* tolua_S)
{
 if (!tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
     !tolua_isnoobj(tolua_S,2))
 {
  TOLUA_ERR_FN(inc_tim_invis);
 } else {
  int v = ((int)  tolua_getnumber(tolua_S,1,0));
  bool toluaI_ret = (bool)  inc_tim_invis(v);
  tolua_pushbool(tolua_S,(int)toluaI_ret);
 }
 return 1;
}

/* function: inc_tim_infra */
static int toluaI_player_inc_tim_infra00(lua_State* tolua_S)
{
 if (!tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
     !tolua_isnoobj(tolua_S,2))
 {
  TOLUA_ERR_FN(inc_tim_infra);
 } else {
  int v = ((int)  tolua_getnumber(tolua_S,1,0));
  bool toluaI_ret = (bool)  inc_tim_infra(v);
  tolua_pushbool(tolua_S,(int)toluaI_ret);
 }
 return 1;
}

/* function: inc_oppose_acid */
static int toluaI_player_inc_oppose_acid00(lua_State* tolua_S)
{
 if (!tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
     !tolua_isnoobj(tolua_S,2))
 {
  TOLUA_ERR_FN(inc_oppose_acid);
 } else {
  int v = ((int)  tolua_getnumber(tolua_S,1,0));
  bool toluaI_ret = (bool)  inc_oppose_acid(v);
  tolua_pushbool(tolua_S,(int)toluaI_ret);
 }
 return 1;
}

/* function: inc_oppose_elec */
static int toluaI_player_inc_oppose_elec00(lua_State* tolua_S)
{
 if (!tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
     !tolua_isnoobj(tolua_S,2))
 {
  TOLUA_ERR_FN(inc_oppose_elec);
 } else {
  int v = ((int)  tolua_getnumber(tolua_S,1,0));
  bool toluaI_ret = (bool)  inc_oppose_elec(v);
  tolua_pushbool(tolua_S,(int)toluaI_ret);
 }
 return 1;
}

/* function: inc_oppose_fire */
static int toluaI_player_inc_oppose_fire00(lua_State* tolua_S)
{
 if (!tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
     !tolua_isnoobj(tolua_S,2))
 {
  TOLUA_ERR_FN(inc_oppose_fire);
 } else {
  int v = ((int)  tolua_getnumber(tolua_S,1,0));
  bool toluaI_ret = (bool)  inc_oppose_fire(v);
  tolua_pushbool(tolua_S,(int)toluaI_ret);
 }
 return 1;
}

/* function: inc_oppose_cold */
static int toluaI_player_inc_oppose_cold00(lua_State* tolua_S)
{
 if (!tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
     !tolua_isnoobj(tolua_S,2))
 {
  TOLUA_ERR_FN(inc_oppose_cold);
 } else {
  int v = ((int)  tolua_getnumber(tolua_S,1,0));
  bool toluaI_ret = (bool)  inc_oppose_cold(v);
  tolua_pushbool(tolua_S,(int)toluaI_ret);
 }
 return 1;
}

/* function: inc_oppose_pois */
static int toluaI_player_inc_oppose_pois00(lua_State* tolua_S)
{
 if (!tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
     !tolua_isnoobj(tolua_S,2))
 {
  TOLUA_ERR_FN(inc_oppose_pois);
 } else {
  int v = ((int)  tolua_getnumber(tolua_S,1,0));
  bool toluaI_ret = (bool)  inc_oppose_pois(v);
  tolua_pushbool(tolua_S,(int)toluaI_ret);
 }
 return 1;
}

/* function: res_acid_lvl */
static int toluaI_player_res_acid_lvl00(lua_State* tolua_S)
{
 if (!tolua_isnoobj(tolua_S,1))
 {
  TOLUA_ERR_FN(res_acid_lvl);
 } else {
  byte toluaI_ret = (byte)  res_acid_lvl();
  tolua_pushnumber(tolua_S,(long)toluaI_ret);
 }
 return 1;
}

/* function: res_elec_lvl */
static int toluaI_player_res_elec_lvl00(lua_State* tolua_S)
{
 if (!tolua_isnoobj(tolua_S,1))
 {
  TOLUA_ERR_FN(res_elec_lvl);
 } else {
  byte toluaI_ret = (byte)  res_elec_lvl();
  tolua_pushnumber(tolua_S,(long)toluaI_ret);
 }
 return 1;
}

/* function: res_fire_lvl */
static int toluaI_player_res_fire_lvl00(lua_State* tolua_S)
{
 if (!tolua_isnoobj(tolua_S,1))
 {
  TOLUA_ERR_FN(res_fire_lvl);
 } else {
  byte toluaI_ret = (byte)  res_fire_lvl();
  tolua_pushnumber(tolua_S,(long)toluaI_ret);
 }
 return 1;
}

/* function: res_cold_lvl */
static int toluaI_player_res_cold_lvl00(lua_State* tolua_S)
{
 if (!tolua_isnoobj(tolua_S,1))
 {
  TOLUA_ERR_FN(res_cold_lvl);
 } else {
  byte toluaI_ret = (byte)  res_cold_lvl();
  tolua_pushnumber(tolua_S,(long)toluaI_ret);
 }
 return 1;
}

/* function: res_pois_lvl */
static int toluaI_player_res_pois_lvl00(lua_State* tolua_S)
{
 if (!tolua_isnoobj(tolua_S,1))
 {
  TOLUA_ERR_FN(res_pois_lvl);
 } else {
  byte toluaI_ret = (byte)  res_pois_lvl();
  tolua_pushnumber(tolua_S,(long)toluaI_ret);
 }
 return 1;
}

/* function: inc_stun */
static int toluaI_player_inc_stun00(lua_State* tolua_S)
{
 if (!tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
     !tolua_isnoobj(tolua_S,2))
 {
  TOLUA_ERR_FN(inc_stun);
 } else {
  int v = ((int)  tolua_getnumber(tolua_S,1,0));
  bool toluaI_ret = (bool)  inc_stun(v);
  tolua_pushbool(tolua_S,(int)toluaI_ret);
 }
 return 1;
}

/* function: clear_stun */
static int toluaI_player_clear_stun00(lua_State* tolua_S)
{
 if (!tolua_isnoobj(tolua_S,1))
 {
  TOLUA_ERR_FN(clear_stun);
 } else {
  bool toluaI_ret = (bool)  clear_stun();
  tolua_pushbool(tolua_S,(int)toluaI_ret);
 }
 return 1;
}

/* function: inc_cut */
static int toluaI_player_inc_cut00(lua_State* tolua_S)
{
 if (!tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
     !tolua_isnoobj(tolua_S,2))
 {
  TOLUA_ERR_FN(inc_cut);
 } else {
  int v = ((int)  tolua_getnumber(tolua_S,1,0));
  bool toluaI_ret = (bool)  inc_cut(v);
  tolua_pushbool(tolua_S,(int)toluaI_ret);
 }
 return 1;
}

/* function: clear_cut */
static int toluaI_player_clear_cut00(lua_State* tolua_S)
{
 if (!tolua_isnoobj(tolua_S,1))
 {
  TOLUA_ERR_FN(clear_cut);
 } else {
  bool toluaI_ret = (bool)  clear_cut();
  tolua_pushbool(tolua_S,(int)toluaI_ret);
 }
 return 1;
}

/* function: set_food */
static int toluaI_player_set_food00(lua_State* tolua_S)
{
 if (!tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
     !tolua_isnoobj(tolua_S,2))
 {
  TOLUA_ERR_FN(set_food);
 } else {
  int v = ((int)  tolua_getnumber(tolua_S,1,0));
  bool toluaI_ret = (bool)  set_food(v);
  tolua_pushbool(tolua_S,(int)toluaI_ret);
 }
 return 1;
}

/* function: gain_exp */
static int toluaI_player_gain_exp00(lua_State* tolua_S)
{
 if (!tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
     !tolua_isnoobj(tolua_S,2))
 {
  TOLUA_ERR_FN(gain_exp);
 } else {
  s32b amount = ((s32b)  tolua_getnumber(tolua_S,1,0));
  gain_exp(amount);
 }
 return 0;
}

/* function: lose_exp */
static int toluaI_player_lose_exp00(lua_State* tolua_S)
{
 if (!tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
     !tolua_isnoobj(tolua_S,2))
 {
  TOLUA_ERR_FN(lose_exp);
 } else {
  s32b amount = ((s32b)  tolua_getnumber(tolua_S,1,0));
  lose_exp(amount);
 }
 return 0;
}

/* function: hp_player */
static int toluaI_player_hp_player00(lua_State* tolua_S)
{
 if (!tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
     !tolua_isnoobj(tolua_S,2))
 {
  TOLUA_ERR_FN(hp_player);
 } else {
  int num = ((int)  tolua_getnumber(tolua_S,1,0));
  bool toluaI_ret = (bool)  hp_player(num);
  tolua_pushbool(tolua_S,(int)toluaI_ret);
 }
 return 1;
}

/* function: do_dec_stat */
static int toluaI_player_do_dec_stat00(lua_State* tolua_S)
{
 if (!tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
     !tolua_isnoobj(tolua_S,2))
 {
  TOLUA_ERR_FN(do_dec_stat);
 } else {
  int stat = ((int)  tolua_getnumber(tolua_S,1,0));
  bool toluaI_ret = (bool)  do_dec_stat(stat);
  tolua_pushbool(tolua_S,(int)toluaI_ret);
 }
 return 1;
}

/* function: do_res_stat */
static int toluaI_player_do_res_stat00(lua_State* tolua_S)
{
 if (!tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
     !tolua_isnoobj(tolua_S,2))
 {
  TOLUA_ERR_FN(do_res_stat);
 } else {
  int stat = ((int)  tolua_getnumber(tolua_S,1,0));
  bool toluaI_ret = (bool)  do_res_stat(stat);
  tolua_pushbool(tolua_S,(int)toluaI_ret);
 }
 return 1;
}

/* function: do_inc_stat */
static int toluaI_player_do_inc_stat00(lua_State* tolua_S)
{
 if (!tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
     !tolua_isnoobj(tolua_S,2))
 {
  TOLUA_ERR_FN(do_inc_stat);
 } else {
  int stat = ((int)  tolua_getnumber(tolua_S,1,0));
  bool toluaI_ret = (bool)  do_inc_stat(stat);
  tolua_pushbool(tolua_S,(int)toluaI_ret);
 }
 return 1;
}

/* function: take_hit */
static int toluaI_player_take_hit00(lua_State* tolua_S)
{
 if (!tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
     !tolua_istype(tolua_S,2,LUA_TSTRING,0) ||
     !tolua_isnoobj(tolua_S,3))
 {
  TOLUA_ERR_FN(take_hit);
 } else {
  int dam = ((int)  tolua_getnumber(tolua_S,1,0));
  cptr kb_str = ((cptr)  tolua_getstring(tolua_S,2,0));
  take_hit(dam,kb_str);
 }
 return 0;
}

/* function: inc_stat */
static int toluaI_player_inc_stat00(lua_State* tolua_S)
{
 if (!tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
     !tolua_isnoobj(tolua_S,2))
 {
  TOLUA_ERR_FN(inc_stat);
 } else {
  int stat = ((int)  tolua_getnumber(tolua_S,1,0));
  bool toluaI_ret = (bool)  inc_stat(stat);
  tolua_pushbool(tolua_S,(int)toluaI_ret);
 }
 return 1;
}

/* function: dec_stat */
static int toluaI_player_dec_stat00(lua_State* tolua_S)
{
 if (!tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
     !tolua_istype(tolua_S,2,LUA_TNUMBER,0) ||
     !tolua_istype(tolua_S,3,LUA_TNUMBER,0) ||
     !tolua_isnoobj(tolua_S,4))
 {
  TOLUA_ERR_FN(dec_stat);
 } else {
  int stat = ((int)  tolua_getnumber(tolua_S,1,0));
  int amount = ((int)  tolua_getnumber(tolua_S,2,0));
  int permanent = ((int)  tolua_getnumber(tolua_S,3,0));
  bool toluaI_ret = (bool)  dec_stat(stat,amount,permanent);
  tolua_pushbool(tolua_S,(int)toluaI_ret);
 }
 return 1;
}

/* function: res_stat */
static int toluaI_player_res_stat00(lua_State* tolua_S)
{
 if (!tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
     !tolua_isnoobj(tolua_S,2))
 {
  TOLUA_ERR_FN(res_stat);
 } else {
  int stat = ((int)  tolua_getnumber(tolua_S,1,0));
  bool toluaI_ret = (bool)  res_stat(stat);
  tolua_pushbool(tolua_S,(int)toluaI_ret);
 }
 return 1;
}

/* function: lose_all_info */
static int toluaI_player_lose_all_info00(lua_State* tolua_S)
{
 if (!tolua_isnoobj(tolua_S,1))
 {
  TOLUA_ERR_FN(lose_all_info);
 } else {
  bool toluaI_ret = (bool)  lose_all_info();
  tolua_pushbool(tolua_S,(int)toluaI_ret);
 }
 return 1;
}

/* function: restore_level */
static int toluaI_player_restore_level00(lua_State* tolua_S)
{
 if (!tolua_isnoobj(tolua_S,1))
 {
  TOLUA_ERR_FN(restore_level);
 } else {
  bool toluaI_ret = (bool)  restore_level();
  tolua_pushbool(tolua_S,(int)toluaI_ret);
 }
 return 1;
}

/* function: player_res */
static int toluaI_player_player_res00(lua_State* tolua_S)
{
 if (!tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
     !tolua_isnoobj(tolua_S,2))
 {
  TOLUA_ERR_FN(player_res);
 } else {
  u32b flag = ((u32b)  tolua_getnumber(tolua_S,1,0));
  bool toluaI_ret = (bool)  player_res(flag);
  tolua_pushbool(tolua_S,(int)toluaI_ret);
 }
 return 1;
}

/* function: have_nightmare */
static int toluaI_player_have_nightmare00(lua_State* tolua_S)
{
 if (!tolua_isnoobj(tolua_S,1))
 {
  TOLUA_ERR_FN(have_nightmare);
 } else {
  have_nightmare();
 }
 return 0;
}

/* function: do_cmd_save_game */
static int toluaI_player_do_cmd_save_game00(lua_State* tolua_S)
{
 if (!tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
     !tolua_isnoobj(tolua_S,2))
 {
  TOLUA_ERR_FN(do_cmd_save_game);
 } else {
  int is_autosave = ((int)  tolua_getnumber(tolua_S,1,0));
  do_cmd_save_game(is_autosave);
 }
 return 0;
}

/* Open function */
int tolua_player_open (lua_State* tolua_S)
{
 tolua_open(tolua_S);
 toluaI_reg_types(tolua_S);
 TOLUA_DEF(CLASS_WARRIOR);
 TOLUA_DEF(CLASS_MAGE);
 TOLUA_DEF(CLASS_PRIEST);
 TOLUA_DEF(CLASS_ROGUE);
 TOLUA_DEF(CLASS_RANGER);
 TOLUA_DEF(CLASS_PALADIN);
 TOLUA_DEF(CLASS_WARRIOR_MAGE);
 TOLUA_DEF(CLASS_CHAOS_WARRIOR);
 TOLUA_DEF(CLASS_MONK);
 TOLUA_DEF(CLASS_MINDCRAFTER);
 TOLUA_DEF(CLASS_HIGH_MAGE);
 TOLUA_DEF(A_STR);
 TOLUA_DEF(A_INT);
 TOLUA_DEF(A_WIS);
 TOLUA_DEF(A_DEX);
 TOLUA_DEF(A_CON);
 TOLUA_DEF(A_CHR);
 TOLUA_DEF(A_MAX);
 TOLUA_DEF(SEX_FEMALE);
 TOLUA_DEF(SEX_MALE);
 TOLUA_DEF(PY_MAX_EXP);
 TOLUA_DEF(PY_MAX_GOLD);
 TOLUA_DEF(PY_MAX_LEVEL);
 TOLUA_DEF(PY_FOOD_MAX);
 TOLUA_DEF(PY_FOOD_FULL);
 TOLUA_DEF(PY_FOOD_ALERT);
 TOLUA_DEF(PY_FOOD_WEAK);
 TOLUA_DEF(PY_FOOD_FAINT);
 TOLUA_DEF(PY_FOOD_STARVE);
 tolua_cclass(tolua_S,"player_data","");
 tolua_tablevar(tolua_S,"player_data","age",toluaI_get_player_player_data_age,toluaI_set_player_player_data_age);
 tolua_tablevar(tolua_S,"player_data","ht",toluaI_get_player_player_data_ht,toluaI_set_player_player_data_ht);
 tolua_tablevar(tolua_S,"player_data","wt",toluaI_get_player_player_data_wt,toluaI_set_player_player_data_wt);
 tolua_tablevar(tolua_S,"player_data","sc",toluaI_get_player_player_data_sc,toluaI_set_player_player_data_sc);
 tolua_tablevar(tolua_S,"player_data","hitdie",toluaI_get_player_player_data_hitdie,toluaI_set_player_player_data_hitdie);
 tolua_tablevar(tolua_S,"player_data","psex",toluaI_get_player_player_data_psex,toluaI_set_player_player_data_psex);
 tolua_tablevar(tolua_S,"player_data","prace",toluaI_get_player_player_data_prace,toluaI_set_player_player_data_prace);
 tolua_tablevar(tolua_S,"player_data","pclass",toluaI_get_player_player_data_pclass,toluaI_set_player_player_data_pclass);
 tolua_cclass(tolua_S,"player_realm","");
 tolua_tablevar(tolua_S,"player_realm","learned",toluaI_get_player_player_realm_learned,toluaI_set_player_player_realm_learned);
 tolua_tablevar(tolua_S,"player_realm","worked",toluaI_get_player_player_realm_worked,toluaI_set_player_player_realm_worked);
 tolua_tablevar(tolua_S,"player_realm","forgotten",toluaI_get_player_player_realm_forgotten,toluaI_set_player_player_realm_forgotten);
 tolua_tablevar(tolua_S,"player_realm","realm",toluaI_get_player_player_realm_realm,toluaI_set_player_player_realm_realm);
 tolua_cclass(tolua_S,"player_spell","");
 tolua_tablearray(tolua_S,"player_spell","r",toluaI_get_player_player_spell_r,toluaI_set_player_player_spell_r);
 tolua_tablearray(tolua_S,"player_spell","order",toluaI_get_player_player_spell_order,toluaI_set_player_player_spell_order);
 tolua_cclass(tolua_S,"player_timed","");
 tolua_tablevar(tolua_S,"player_timed","fast",toluaI_get_player_player_timed_fast,toluaI_set_player_player_timed_fast);
 tolua_tablevar(tolua_S,"player_timed","slow",toluaI_get_player_player_timed_slow,toluaI_set_player_player_timed_slow);
 tolua_tablevar(tolua_S,"player_timed","blind",toluaI_get_player_player_timed_blind,toluaI_set_player_player_timed_blind);
 tolua_tablevar(tolua_S,"player_timed","paralyzed",toluaI_get_player_player_timed_paralyzed,toluaI_set_player_player_timed_paralyzed);
 tolua_tablevar(tolua_S,"player_timed","confused",toluaI_get_player_player_timed_confused,toluaI_set_player_player_timed_confused);
 tolua_tablevar(tolua_S,"player_timed","afraid",toluaI_get_player_player_timed_afraid,toluaI_set_player_player_timed_afraid);
 tolua_tablevar(tolua_S,"player_timed","image",toluaI_get_player_player_timed_image,toluaI_set_player_player_timed_image);
 tolua_tablevar(tolua_S,"player_timed","poisoned",toluaI_get_player_player_timed_poisoned,toluaI_set_player_player_timed_poisoned);
 tolua_tablevar(tolua_S,"player_timed","cut",toluaI_get_player_player_timed_cut,toluaI_set_player_player_timed_cut);
 tolua_tablevar(tolua_S,"player_timed","stun",toluaI_get_player_player_timed_stun,toluaI_set_player_player_timed_stun);
 tolua_tablevar(tolua_S,"player_timed","protevil",toluaI_get_player_player_timed_protevil,toluaI_set_player_player_timed_protevil);
 tolua_tablevar(tolua_S,"player_timed","invuln",toluaI_get_player_player_timed_invuln,toluaI_set_player_player_timed_invuln);
 tolua_tablevar(tolua_S,"player_timed","hero",toluaI_get_player_player_timed_hero,toluaI_set_player_player_timed_hero);
 tolua_tablevar(tolua_S,"player_timed","shero",toluaI_get_player_player_timed_shero,toluaI_set_player_player_timed_shero);
 tolua_tablevar(tolua_S,"player_timed","shield",toluaI_get_player_player_timed_shield,toluaI_set_player_player_timed_shield);
 tolua_tablevar(tolua_S,"player_timed","blessed",toluaI_get_player_player_timed_blessed,toluaI_set_player_player_timed_blessed);
 tolua_tablevar(tolua_S,"player_timed","invis",toluaI_get_player_player_timed_invis,toluaI_set_player_player_timed_invis);
 tolua_tablevar(tolua_S,"player_timed","infra",toluaI_get_player_player_timed_infra,toluaI_set_player_player_timed_infra);
 tolua_tablevar(tolua_S,"player_timed","oppose_acid",toluaI_get_player_player_timed_oppose_acid,toluaI_set_player_player_timed_oppose_acid);
 tolua_tablevar(tolua_S,"player_timed","oppose_elec",toluaI_get_player_player_timed_oppose_elec,toluaI_set_player_player_timed_oppose_elec);
 tolua_tablevar(tolua_S,"player_timed","oppose_fire",toluaI_get_player_player_timed_oppose_fire,toluaI_set_player_player_timed_oppose_fire);
 tolua_tablevar(tolua_S,"player_timed","oppose_cold",toluaI_get_player_player_timed_oppose_cold,toluaI_set_player_player_timed_oppose_cold);
 tolua_tablevar(tolua_S,"player_timed","oppose_pois",toluaI_get_player_player_timed_oppose_pois,toluaI_set_player_player_timed_oppose_pois);
 tolua_tablevar(tolua_S,"player_timed","esp",toluaI_get_player_player_timed_esp,toluaI_set_player_player_timed_esp);
 tolua_tablevar(tolua_S,"player_timed","wraith_form",toluaI_get_player_player_timed_wraith_form,toluaI_set_player_player_timed_wraith_form);
 tolua_tablevar(tolua_S,"player_timed","resist_magic",toluaI_get_player_player_timed_resist_magic,toluaI_set_player_player_timed_resist_magic);
 tolua_tablevar(tolua_S,"player_timed","word_recall",toluaI_get_player_player_timed_word_recall,toluaI_set_player_player_timed_word_recall);
 tolua_cclass(tolua_S,"player_state","");
 tolua_tablearray(tolua_S,"player_state","died_from",toluaI_get_player_player_state_died_from,toluaI_set_player_player_state_died_from);
 tolua_tablevar(tolua_S,"player_state","resting",toluaI_get_player_player_state_resting,toluaI_set_player_player_state_resting);
 tolua_tablevar(tolua_S,"player_state","running",toluaI_get_player_player_state_running,toluaI_set_player_player_state_running);
 tolua_tablevar(tolua_S,"player_state","confusing",toluaI_get_player_player_state_confusing,toluaI_set_player_player_state_confusing);
 tolua_tablevar(tolua_S,"player_state","searching",toluaI_get_player_player_state_searching,toluaI_set_player_player_state_searching);
 tolua_tablevar(tolua_S,"player_state","total_winner",toluaI_get_player_player_state_total_winner,toluaI_set_player_player_state_total_winner);
 tolua_tablevar(tolua_S,"player_state","panic_save",toluaI_get_player_player_state_panic_save,toluaI_set_player_player_state_panic_save);
 tolua_tablevar(tolua_S,"player_state","noscore",toluaI_get_player_player_state_noscore,toluaI_set_player_player_state_noscore);
 tolua_tablevar(tolua_S,"player_state","is_dead",toluaI_get_player_player_state_is_dead,toluaI_set_player_player_state_is_dead);
 tolua_tablevar(tolua_S,"player_state","wizard",toluaI_get_player_player_state_wizard,toluaI_set_player_player_state_wizard);
 tolua_tablevar(tolua_S,"player_state","playing",toluaI_get_player_player_state_playing,toluaI_set_player_player_state_playing);
 tolua_tablevar(tolua_S,"player_state","leaving",toluaI_get_player_player_state_leaving,toluaI_set_player_player_state_leaving);
 tolua_tablevar(tolua_S,"player_state","create_up_stair",toluaI_get_player_player_state_create_up_stair,toluaI_set_player_player_state_create_up_stair);
 tolua_tablevar(tolua_S,"player_state","create_down_stair",toluaI_get_player_player_state_create_down_stair,toluaI_set_player_player_state_create_down_stair);
 tolua_tablevar(tolua_S,"player_state","energy_use",toluaI_get_player_player_state_energy_use,toluaI_set_player_player_state_energy_use);
 tolua_tablevar(tolua_S,"player_state","cumber_armor",toluaI_get_player_player_state_cumber_armor,toluaI_set_player_player_state_cumber_armor);
 tolua_tablevar(tolua_S,"player_state","cumber_glove",toluaI_get_player_player_state_cumber_glove,toluaI_set_player_player_state_cumber_glove);
 tolua_tablevar(tolua_S,"player_state","heavy_wield",toluaI_get_player_player_state_heavy_wield,toluaI_set_player_player_state_heavy_wield);
 tolua_tablevar(tolua_S,"player_state","heavy_shoot",toluaI_get_player_player_state_heavy_shoot,toluaI_set_player_player_state_heavy_shoot);
 tolua_tablevar(tolua_S,"player_state","icky_wield",toluaI_get_player_player_state_icky_wield,toluaI_set_player_player_state_icky_wield);
 tolua_tablevar(tolua_S,"player_state","detected",toluaI_get_player_player_state_detected,toluaI_set_player_player_state_detected);
 tolua_tablevar(tolua_S,"player_state","skip_more",toluaI_get_player_player_state_skip_more,toluaI_set_player_player_state_skip_more);
 tolua_tablevar(tolua_S,"player_state","mon_fight",toluaI_get_player_player_state_mon_fight,toluaI_set_player_player_state_mon_fight);
 tolua_tablevar(tolua_S,"player_state","monk_armour_stat",toluaI_get_player_player_state_monk_armour_stat,toluaI_set_player_player_state_monk_armour_stat);
 tolua_tablevar(tolua_S,"player_state","noise_level",toluaI_get_player_player_state_noise_level,toluaI_set_player_player_state_noise_level);
 tolua_tablevar(tolua_S,"player_state","store_top",toluaI_get_player_player_state_store_top,toluaI_set_player_player_state_store_top);
 TOLUA_DEF(MAX_SKILL);
 TOLUA_DEF(SKILL_DIS);
 TOLUA_DEF(SKILL_DEV);
 TOLUA_DEF(SKILL_SAV);
 TOLUA_DEF(SKILL_STL);
 TOLUA_DEF(SKILL_SNS);
 TOLUA_DEF(SKILL_FOS);
 TOLUA_DEF(SKILL_THN);
 TOLUA_DEF(SKILL_THB);
 TOLUA_DEF(SKILL_THT);
 TOLUA_DEF(SKILL_DIG);
 tolua_cclass(tolua_S,"player_stat","");
 tolua_tablevar(tolua_S,"player_stat","max",toluaI_get_player_player_stat_max,toluaI_set_player_player_stat_max);
 tolua_tablevar(tolua_S,"player_stat","cur",toluaI_get_player_player_stat_cur,toluaI_set_player_player_stat_cur);
 tolua_tablevar(tolua_S,"player_stat","use",toluaI_get_player_player_stat_use,toluaI_set_player_player_stat_use);
 tolua_tablevar(tolua_S,"player_stat","top",toluaI_get_player_player_stat_top,toluaI_set_player_player_stat_top);
 tolua_tablevar(tolua_S,"player_stat","add",toluaI_get_player_player_stat_add,toluaI_set_player_player_stat_add);
 tolua_tablevar(tolua_S,"player_stat","ind",toluaI_get_player_player_stat_ind,toluaI_set_player_player_stat_ind);
 tolua_cclass(tolua_S,"player_type","");
 tolua_tablevar(tolua_S,"player_type","px",toluaI_get_player_player_type_px,toluaI_set_player_player_type_px);
 tolua_tablevar(tolua_S,"player_type","py",toluaI_get_player_player_type_py,toluaI_set_player_player_type_py);
 tolua_tablevar(tolua_S,"player_type","rp",toluaI_get_player_player_type_rp,toluaI_set_player_player_type_rp);
 tolua_tablevar(tolua_S,"player_type","depth",toluaI_get_player_player_type_depth,toluaI_set_player_player_type_depth);
 tolua_tablevar(tolua_S,"player_type","max_lev",toluaI_get_player_player_type_max_lev,toluaI_set_player_player_type_max_lev);
 tolua_tablevar(tolua_S,"player_type","lev",toluaI_get_player_player_type_lev,toluaI_set_player_player_type_lev);
 tolua_tablevar(tolua_S,"player_type","exp_frac",toluaI_get_player_player_type_exp_frac,toluaI_set_player_player_type_exp_frac);
 tolua_tablevar(tolua_S,"player_type","max_exp",toluaI_get_player_player_type_max_exp,toluaI_set_player_player_type_max_exp);
 tolua_tablevar(tolua_S,"player_type","exp",toluaI_get_player_player_type_exp,toluaI_set_player_player_type_exp);
 tolua_tablevar(tolua_S,"player_type","au",toluaI_get_player_player_type_au,toluaI_set_player_player_type_au);
 tolua_tablevar(tolua_S,"player_type","place_num",toluaI_get_player_player_type_place_num,toluaI_set_player_player_type_place_num);
 tolua_tablevar(tolua_S,"player_type","wilderness_x",toluaI_get_player_player_type_wilderness_x,toluaI_set_player_player_type_wilderness_x);
 tolua_tablevar(tolua_S,"player_type","wilderness_y",toluaI_get_player_player_type_wilderness_y,toluaI_set_player_player_type_wilderness_y);
 tolua_tablevar(tolua_S,"player_type","mhp",toluaI_get_player_player_type_mhp,toluaI_set_player_player_type_mhp);
 tolua_tablevar(tolua_S,"player_type","chp",toluaI_get_player_player_type_chp,toluaI_set_player_player_type_chp);
 tolua_tablevar(tolua_S,"player_type","chp_frac",toluaI_get_player_player_type_chp_frac,toluaI_set_player_player_type_chp_frac);
 tolua_tablevar(tolua_S,"player_type","msp",toluaI_get_player_player_type_msp,toluaI_set_player_player_type_msp);
 tolua_tablevar(tolua_S,"player_type","csp",toluaI_get_player_player_type_csp,toluaI_set_player_player_type_csp);
 tolua_tablevar(tolua_S,"player_type","csp_frac",toluaI_get_player_player_type_csp_frac,toluaI_set_player_player_type_csp_frac);
 tolua_tablevar(tolua_S,"player_type","spell",toluaI_get_player_player_type_spell,toluaI_set_player_player_type_spell);
 tolua_tablevar(tolua_S,"player_type","muta1",toluaI_get_player_player_type_muta1,toluaI_set_player_player_type_muta1);
 tolua_tablevar(tolua_S,"player_type","muta2",toluaI_get_player_player_type_muta2,toluaI_set_player_player_type_muta2);
 tolua_tablevar(tolua_S,"player_type","muta3",toluaI_get_player_player_type_muta3,toluaI_set_player_player_type_muta3);
 tolua_tablearray(tolua_S,"player_type","virtues",toluaI_get_player_player_type_virtues,toluaI_set_player_player_type_virtues);
 tolua_tablearray(tolua_S,"player_type","vir_types",toluaI_get_player_player_type_vir_types,toluaI_set_player_player_type_vir_types);
 tolua_tablevar(tolua_S,"player_type","chaos_patron",toluaI_get_player_player_type_chaos_patron,toluaI_set_player_player_type_chaos_patron);
 tolua_tablevar(tolua_S,"player_type","energy",toluaI_get_player_player_type_energy,toluaI_set_player_player_type_energy);
 tolua_tablevar(tolua_S,"player_type","food",toluaI_get_player_player_type_food,toluaI_set_player_player_type_food);
 tolua_tablearray(tolua_S,"player_type","player_hp",toluaI_get_player_player_type_player_hp,toluaI_set_player_player_type_player_hp);
 tolua_tablevar(tolua_S,"player_type","expfact",toluaI_get_player_player_type_expfact,toluaI_set_player_player_type_expfact);
 tolua_tablevar(tolua_S,"player_type","tim",toluaI_get_player_player_type_tim,toluaI_set_player_player_type_tim);
 tolua_tablevar(tolua_S,"player_type","state",toluaI_get_player_player_type_state,toluaI_set_player_player_type_state);
 tolua_tablearray(tolua_S,"player_type","skills",toluaI_get_player_player_type_skills,toluaI_set_player_player_type_skills);
 tolua_tablearray(tolua_S,"player_type","stat",toluaI_get_player_player_type_stat,toluaI_set_player_player_type_stat);
 tolua_tablevar(tolua_S,"player_type","sp_bonus",toluaI_get_player_player_type_sp_bonus,toluaI_set_player_player_type_sp_bonus);
 tolua_tablevar(tolua_S,"player_type","align",toluaI_get_player_player_type_align,toluaI_set_player_player_type_align);
 tolua_tablevar(tolua_S,"player_type","total_weight",toluaI_get_player_player_type_total_weight,toluaI_set_player_player_type_total_weight);
 tolua_tablevar(tolua_S,"player_type","target_set",toluaI_get_player_player_type_target_set,toluaI_set_player_player_type_target_set);
 tolua_tablevar(tolua_S,"player_type","target_who",toluaI_get_player_player_type_target_who,toluaI_set_player_player_type_target_who);
 tolua_tablevar(tolua_S,"player_type","target_row",toluaI_get_player_player_type_target_row,toluaI_set_player_player_type_target_row);
 tolua_tablevar(tolua_S,"player_type","target_col",toluaI_get_player_player_type_target_col,toluaI_set_player_player_type_target_col);
 tolua_tablevar(tolua_S,"player_type","health_who",toluaI_get_player_player_type_health_who,toluaI_set_player_player_type_health_who);
 tolua_tablevar(tolua_S,"player_type","monster_race_idx",toluaI_get_player_player_type_monster_race_idx,toluaI_set_player_player_type_monster_race_idx);
 tolua_tablevar(tolua_S,"player_type","max_seen_r_idx",toluaI_get_player_player_type_max_seen_r_idx,toluaI_set_player_player_type_max_seen_r_idx);
 tolua_tablevar(tolua_S,"player_type","object_kind_idx",toluaI_get_player_player_type_object_kind_idx,toluaI_set_player_player_type_object_kind_idx);
 tolua_tablevar(tolua_S,"player_type","new_spells",toluaI_get_player_player_type_new_spells,toluaI_set_player_player_type_new_spells);
 tolua_tablevar(tolua_S,"player_type","cur_lite",toluaI_get_player_player_type_cur_lite,toluaI_set_player_player_type_cur_lite);
 tolua_tablevar(tolua_S,"player_type","notice",toluaI_get_player_player_type_notice,toluaI_set_player_player_type_notice);
 tolua_tablevar(tolua_S,"player_type","update",toluaI_get_player_player_type_update,toluaI_set_player_player_type_update);
 tolua_tablevar(tolua_S,"player_type","redraw",toluaI_get_player_player_type_redraw,toluaI_set_player_player_type_redraw);
 tolua_tablevar(tolua_S,"player_type","window",toluaI_get_player_player_type_window,toluaI_set_player_player_type_window);
 tolua_tablearray(tolua_S,"player_type","flags",toluaI_get_player_player_type_flags,toluaI_set_player_player_type_flags);
 tolua_tablevar(tolua_S,"player_type","dis_to_h",toluaI_get_player_player_type_dis_to_h,toluaI_set_player_player_type_dis_to_h);
 tolua_tablevar(tolua_S,"player_type","dis_to_d",toluaI_get_player_player_type_dis_to_d,toluaI_set_player_player_type_dis_to_d);
 tolua_tablevar(tolua_S,"player_type","dis_to_a",toluaI_get_player_player_type_dis_to_a,toluaI_set_player_player_type_dis_to_a);
 tolua_tablevar(tolua_S,"player_type","dis_ac",toluaI_get_player_player_type_dis_ac,toluaI_set_player_player_type_dis_ac);
 tolua_tablevar(tolua_S,"player_type","to_h",toluaI_get_player_player_type_to_h,toluaI_set_player_player_type_to_h);
 tolua_tablevar(tolua_S,"player_type","to_d",toluaI_get_player_player_type_to_d,toluaI_set_player_player_type_to_d);
 tolua_tablevar(tolua_S,"player_type","to_a",toluaI_get_player_player_type_to_a,toluaI_set_player_player_type_to_a);
 tolua_tablevar(tolua_S,"player_type","ac",toluaI_get_player_player_type_ac,toluaI_set_player_player_type_ac);
 tolua_tablevar(tolua_S,"player_type","see_infra",toluaI_get_player_player_type_see_infra,toluaI_set_player_player_type_see_infra);
 tolua_tablevar(tolua_S,"player_type","noise",toluaI_get_player_player_type_noise,toluaI_set_player_player_type_noise);
 tolua_tablevar(tolua_S,"player_type","num_blow",toluaI_get_player_player_type_num_blow,toluaI_set_player_player_type_num_blow);
 tolua_tablevar(tolua_S,"player_type","num_fire",toluaI_get_player_player_type_num_fire,toluaI_set_player_player_type_num_fire);
 tolua_tablevar(tolua_S,"player_type","ammo_mult",toluaI_get_player_player_type_ammo_mult,toluaI_set_player_player_type_ammo_mult);
 tolua_tablevar(tolua_S,"player_type","ammo_tval",toluaI_get_player_player_type_ammo_tval,toluaI_set_player_player_type_ammo_tval);
 tolua_tablevar(tolua_S,"player_type","bow_energy",toluaI_get_player_player_type_bow_energy,toluaI_set_player_player_type_bow_energy);
 tolua_tablevar(tolua_S,"player_type","pspeed",toluaI_get_player_player_type_pspeed,toluaI_set_player_player_type_pspeed);
 tolua_tablevar(tolua_S,"player_type","pet_follow_distance",toluaI_get_player_player_type_pet_follow_distance,toluaI_set_player_player_type_pet_follow_distance);
 tolua_tablevar(tolua_S,"player_type","pet_open_doors",toluaI_get_player_player_type_pet_open_doors,toluaI_set_player_player_type_pet_open_doors);
 tolua_tablevar(tolua_S,"player_type","pet_pickup_items",toluaI_get_player_player_type_pet_pickup_items,toluaI_set_player_player_type_pet_pickup_items);
 tolua_tablearray(tolua_S,"player_type","options",toluaI_get_player_player_type_options,toluaI_set_player_player_type_options);
 tolua_tablearray(tolua_S,"player_type","birth",toluaI_get_player_player_type_birth,toluaI_set_player_player_type_birth);
 tolua_globalvar(tolua_S,"player",toluaI_get_player_player,toluaI_set_player_player);
 tolua_globalvar(tolua_S,"ironman_nightmare",toluaI_get_player_ironman_nightmare,toluaI_set_player_ironman_nightmare);
 tolua_globalvar(tolua_S,"autosave_l",toluaI_get_player_autosave_l,toluaI_set_player_autosave_l);
 TOLUA_FUN(inc_blind,toluaI_player_inc_blind00);
 TOLUA_FUN(clear_blind,toluaI_player_clear_blind00);
 TOLUA_FUN(inc_confused,toluaI_player_inc_confused00);
 TOLUA_FUN(clear_confused,toluaI_player_clear_confused00);
 TOLUA_FUN(inc_poisoned,toluaI_player_inc_poisoned00);
 TOLUA_FUN(clear_poisoned,toluaI_player_clear_poisoned00);
 TOLUA_FUN(inc_afraid,toluaI_player_inc_afraid00);
 TOLUA_FUN(clear_afraid,toluaI_player_clear_afraid00);
 TOLUA_FUN(inc_paralyzed,toluaI_player_inc_paralyzed00);
 TOLUA_FUN(clear_paralyzed,toluaI_player_clear_paralyzed00);
 TOLUA_FUN(inc_image,toluaI_player_inc_image00);
 TOLUA_FUN(clear_image,toluaI_player_clear_image00);
 TOLUA_FUN(inc_fast,toluaI_player_inc_fast00);
 TOLUA_FUN(clear_fast,toluaI_player_clear_fast00);
 TOLUA_FUN(inc_slow,toluaI_player_inc_slow00);
 TOLUA_FUN(clear_slow,toluaI_player_clear_slow00);
 TOLUA_FUN(inc_shield,toluaI_player_inc_shield00);
 TOLUA_FUN(inc_blessed,toluaI_player_inc_blessed00);
 TOLUA_FUN(inc_hero,toluaI_player_inc_hero00);
 TOLUA_FUN(inc_shero,toluaI_player_inc_shero00);
 TOLUA_FUN(inc_protevil,toluaI_player_inc_protevil00);
 TOLUA_FUN(inc_wraith_form,toluaI_player_inc_wraith_form00);
 TOLUA_FUN(inc_tim_esp,toluaI_player_inc_tim_esp00);
 TOLUA_FUN(clear_tim_esp,toluaI_player_clear_tim_esp00);
 TOLUA_FUN(inc_invuln,toluaI_player_inc_invuln00);
 TOLUA_FUN(inc_tim_invis,toluaI_player_inc_tim_invis00);
 TOLUA_FUN(inc_tim_infra,toluaI_player_inc_tim_infra00);
 TOLUA_FUN(inc_oppose_acid,toluaI_player_inc_oppose_acid00);
 TOLUA_FUN(inc_oppose_elec,toluaI_player_inc_oppose_elec00);
 TOLUA_FUN(inc_oppose_fire,toluaI_player_inc_oppose_fire00);
 TOLUA_FUN(inc_oppose_cold,toluaI_player_inc_oppose_cold00);
 TOLUA_FUN(inc_oppose_pois,toluaI_player_inc_oppose_pois00);
 TOLUA_FUN(res_acid_lvl,toluaI_player_res_acid_lvl00);
 TOLUA_FUN(res_elec_lvl,toluaI_player_res_elec_lvl00);
 TOLUA_FUN(res_fire_lvl,toluaI_player_res_fire_lvl00);
 TOLUA_FUN(res_cold_lvl,toluaI_player_res_cold_lvl00);
 TOLUA_FUN(res_pois_lvl,toluaI_player_res_pois_lvl00);
 TOLUA_FUN(inc_stun,toluaI_player_inc_stun00);
 TOLUA_FUN(clear_stun,toluaI_player_clear_stun00);
 TOLUA_FUN(inc_cut,toluaI_player_inc_cut00);
 TOLUA_FUN(clear_cut,toluaI_player_clear_cut00);
 TOLUA_FUN(set_food,toluaI_player_set_food00);
 TOLUA_FUN(gain_exp,toluaI_player_gain_exp00);
 TOLUA_FUN(lose_exp,toluaI_player_lose_exp00);
 TOLUA_FUN(hp_player,toluaI_player_hp_player00);
 TOLUA_FUN(do_dec_stat,toluaI_player_do_dec_stat00);
 TOLUA_FUN(do_res_stat,toluaI_player_do_res_stat00);
 TOLUA_FUN(do_inc_stat,toluaI_player_do_inc_stat00);
 TOLUA_FUN(take_hit,toluaI_player_take_hit00);
 TOLUA_FUN(inc_stat,toluaI_player_inc_stat00);
 TOLUA_FUN(dec_stat,toluaI_player_dec_stat00);
 TOLUA_FUN(res_stat,toluaI_player_res_stat00);
 TOLUA_FUN(lose_all_info,toluaI_player_lose_all_info00);
 TOLUA_FUN(restore_level,toluaI_player_restore_level00);
 TOLUA_FUN(player_res,toluaI_player_player_res00);
 TOLUA_FUN(have_nightmare,toluaI_player_have_nightmare00);
 TOLUA_FUN(do_cmd_save_game,toluaI_player_do_cmd_save_game00);
 return 1;
}
/* Close function */
void tolua_player_close (lua_State* tolua_S)
{
 TOLUA_UNDEF(CLASS_WARRIOR);
 TOLUA_UNDEF(CLASS_MAGE);
 TOLUA_UNDEF(CLASS_PRIEST);
 TOLUA_UNDEF(CLASS_ROGUE);
 TOLUA_UNDEF(CLASS_RANGER);
 TOLUA_UNDEF(CLASS_PALADIN);
 TOLUA_UNDEF(CLASS_WARRIOR_MAGE);
 TOLUA_UNDEF(CLASS_CHAOS_WARRIOR);
 TOLUA_UNDEF(CLASS_MONK);
 TOLUA_UNDEF(CLASS_MINDCRAFTER);
 TOLUA_UNDEF(CLASS_HIGH_MAGE);
 TOLUA_UNDEF(A_STR);
 TOLUA_UNDEF(A_INT);
 TOLUA_UNDEF(A_WIS);
 TOLUA_UNDEF(A_DEX);
 TOLUA_UNDEF(A_CON);
 TOLUA_UNDEF(A_CHR);
 TOLUA_UNDEF(A_MAX);
 TOLUA_UNDEF(SEX_FEMALE);
 TOLUA_UNDEF(SEX_MALE);
 TOLUA_UNDEF(PY_MAX_EXP);
 TOLUA_UNDEF(PY_MAX_GOLD);
 TOLUA_UNDEF(PY_MAX_LEVEL);
 TOLUA_UNDEF(PY_FOOD_MAX);
 TOLUA_UNDEF(PY_FOOD_FULL);
 TOLUA_UNDEF(PY_FOOD_ALERT);
 TOLUA_UNDEF(PY_FOOD_WEAK);
 TOLUA_UNDEF(PY_FOOD_FAINT);
 TOLUA_UNDEF(PY_FOOD_STARVE);
 TOLUA_UNDEF(player_data);
 TOLUA_UNDEF(player_realm);
 TOLUA_UNDEF(player_spell);
 TOLUA_UNDEF(player_timed);
 TOLUA_UNDEF(player_state);
 TOLUA_UNDEF(MAX_SKILL);
 TOLUA_UNDEF(SKILL_DIS);
 TOLUA_UNDEF(SKILL_DEV);
 TOLUA_UNDEF(SKILL_SAV);
 TOLUA_UNDEF(SKILL_STL);
 TOLUA_UNDEF(SKILL_SNS);
 TOLUA_UNDEF(SKILL_FOS);
 TOLUA_UNDEF(SKILL_THN);
 TOLUA_UNDEF(SKILL_THB);
 TOLUA_UNDEF(SKILL_THT);
 TOLUA_UNDEF(SKILL_DIG);
 TOLUA_UNDEF(player_stat);
 TOLUA_UNDEF(player_type);
 lua_getglobals(tolua_S);
 lua_pushstring(tolua_S,"player"); lua_pushnil(tolua_S); lua_rawset(tolua_S,-3);
 lua_pop(tolua_S,1);
 lua_getglobals(tolua_S);
 lua_pushstring(tolua_S,"ironman_nightmare"); lua_pushnil(tolua_S); lua_rawset(tolua_S,-3);
 lua_pop(tolua_S,1);
 lua_getglobals(tolua_S);
 lua_pushstring(tolua_S,"autosave_l"); lua_pushnil(tolua_S); lua_rawset(tolua_S,-3);
 lua_pop(tolua_S,1);
 TOLUA_UNDEF(inc_blind);
 TOLUA_UNDEF(clear_blind);
 TOLUA_UNDEF(inc_confused);
 TOLUA_UNDEF(clear_confused);
 TOLUA_UNDEF(inc_poisoned);
 TOLUA_UNDEF(clear_poisoned);
 TOLUA_UNDEF(inc_afraid);
 TOLUA_UNDEF(clear_afraid);
 TOLUA_UNDEF(inc_paralyzed);
 TOLUA_UNDEF(clear_paralyzed);
 TOLUA_UNDEF(inc_image);
 TOLUA_UNDEF(clear_image);
 TOLUA_UNDEF(inc_fast);
 TOLUA_UNDEF(clear_fast);
 TOLUA_UNDEF(inc_slow);
 TOLUA_UNDEF(clear_slow);
 TOLUA_UNDEF(inc_shield);
 TOLUA_UNDEF(inc_blessed);
 TOLUA_UNDEF(inc_hero);
 TOLUA_UNDEF(inc_shero);
 TOLUA_UNDEF(inc_protevil);
 TOLUA_UNDEF(inc_wraith_form);
 TOLUA_UNDEF(inc_tim_esp);
 TOLUA_UNDEF(clear_tim_esp);
 TOLUA_UNDEF(inc_invuln);
 TOLUA_UNDEF(inc_tim_invis);
 TOLUA_UNDEF(inc_tim_infra);
 TOLUA_UNDEF(inc_oppose_acid);
 TOLUA_UNDEF(inc_oppose_elec);
 TOLUA_UNDEF(inc_oppose_fire);
 TOLUA_UNDEF(inc_oppose_cold);
 TOLUA_UNDEF(inc_oppose_pois);
 TOLUA_UNDEF(res_acid_lvl);
 TOLUA_UNDEF(res_elec_lvl);
 TOLUA_UNDEF(res_fire_lvl);
 TOLUA_UNDEF(res_cold_lvl);
 TOLUA_UNDEF(res_pois_lvl);
 TOLUA_UNDEF(inc_stun);
 TOLUA_UNDEF(clear_stun);
 TOLUA_UNDEF(inc_cut);
 TOLUA_UNDEF(clear_cut);
 TOLUA_UNDEF(set_food);
 TOLUA_UNDEF(gain_exp);
 TOLUA_UNDEF(lose_exp);
 TOLUA_UNDEF(hp_player);
 TOLUA_UNDEF(do_dec_stat);
 TOLUA_UNDEF(do_res_stat);
 TOLUA_UNDEF(do_inc_stat);
 TOLUA_UNDEF(take_hit);
 TOLUA_UNDEF(inc_stat);
 TOLUA_UNDEF(dec_stat);
 TOLUA_UNDEF(res_stat);
 TOLUA_UNDEF(lose_all_info);
 TOLUA_UNDEF(restore_level);
 TOLUA_UNDEF(player_res);
 TOLUA_UNDEF(have_nightmare);
 TOLUA_UNDEF(do_cmd_save_game);
}
