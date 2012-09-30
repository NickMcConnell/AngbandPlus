/*
** Lua binding: spell
** Generated automatically by tolua 4.0a - angband.
*/

#include "lua/tolua.h"

/* Exported function */
int  tolua_spell_open (lua_State* tolua_S);
void tolua_spell_close (lua_State* tolua_S);

#include "angband.h"

/* function to register type */
static void toluaI_reg_types (lua_State* tolua_S)
{
(void) tolua_S;	/* Hack - prevent compiler warnings */
 tolua_usertype(tolua_S,"object_type");
}

/* function: project */
static int toluaI_spell_project00(lua_State* tolua_S)
{
 if (!tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
     !tolua_istype(tolua_S,2,LUA_TNUMBER,0) ||
     !tolua_istype(tolua_S,3,LUA_TNUMBER,0) ||
     !tolua_istype(tolua_S,4,LUA_TNUMBER,0) ||
     !tolua_istype(tolua_S,5,LUA_TNUMBER,0) ||
     !tolua_istype(tolua_S,6,LUA_TNUMBER,0) ||
     !tolua_istype(tolua_S,7,LUA_TNUMBER,0) ||
     !tolua_isnoobj(tolua_S,8))
 {
  TOLUA_ERR_FN(project);
 } else {
  int who = ((int)  tolua_getnumber(tolua_S,1,0));
  int rad = ((int)  tolua_getnumber(tolua_S,2,0));
  int x = ((int)  tolua_getnumber(tolua_S,3,0));
  int y = ((int)  tolua_getnumber(tolua_S,4,0));
  int dam = ((int)  tolua_getnumber(tolua_S,5,0));
  int typ = ((int)  tolua_getnumber(tolua_S,6,0));
  u16b flg = ((u16b)  tolua_getnumber(tolua_S,7,0));
  bool toluaI_ret = (bool)  project(who,rad,x,y,dam,typ,flg);
  tolua_pushbool(tolua_S,(int)toluaI_ret);
 }
 return 1;
}

/* function: self_knowledge */
static int toluaI_spell_self_knowledge00(lua_State* tolua_S)
{
 if (!tolua_isnoobj(tolua_S,1))
 {
  TOLUA_ERR_FN(self_knowledge);
 } else {
  self_knowledge();
 }
 return 0;
}

/* function: detect_traps */
static int toluaI_spell_detect_traps00(lua_State* tolua_S)
{
 if (!tolua_isnoobj(tolua_S,1))
 {
  TOLUA_ERR_FN(detect_traps);
 } else {
  bool toluaI_ret = (bool)  detect_traps();
  tolua_pushbool(tolua_S,(int)toluaI_ret);
 }
 return 1;
}

/* function: detect_doors */
static int toluaI_spell_detect_doors00(lua_State* tolua_S)
{
 if (!tolua_isnoobj(tolua_S,1))
 {
  TOLUA_ERR_FN(detect_doors);
 } else {
  bool toluaI_ret = (bool)  detect_doors();
  tolua_pushbool(tolua_S,(int)toluaI_ret);
 }
 return 1;
}

/* function: detect_stairs */
static int toluaI_spell_detect_stairs00(lua_State* tolua_S)
{
 if (!tolua_isnoobj(tolua_S,1))
 {
  TOLUA_ERR_FN(detect_stairs);
 } else {
  bool toluaI_ret = (bool)  detect_stairs();
  tolua_pushbool(tolua_S,(int)toluaI_ret);
 }
 return 1;
}

/* function: detect_treasure */
static int toluaI_spell_detect_treasure00(lua_State* tolua_S)
{
 if (!tolua_isnoobj(tolua_S,1))
 {
  TOLUA_ERR_FN(detect_treasure);
 } else {
  bool toluaI_ret = (bool)  detect_treasure();
  tolua_pushbool(tolua_S,(int)toluaI_ret);
 }
 return 1;
}

/* function: detect_objects_gold */
static int toluaI_spell_detect_objects_gold00(lua_State* tolua_S)
{
 if (!tolua_isnoobj(tolua_S,1))
 {
  TOLUA_ERR_FN(detect_objects_gold);
 } else {
  bool toluaI_ret = (bool)  detect_objects_gold();
  tolua_pushbool(tolua_S,(int)toluaI_ret);
 }
 return 1;
}

/* function: detect_objects_normal */
static int toluaI_spell_detect_objects_normal00(lua_State* tolua_S)
{
 if (!tolua_isnoobj(tolua_S,1))
 {
  TOLUA_ERR_FN(detect_objects_normal);
 } else {
  bool toluaI_ret = (bool)  detect_objects_normal();
  tolua_pushbool(tolua_S,(int)toluaI_ret);
 }
 return 1;
}

/* function: detect_objects_magic */
static int toluaI_spell_detect_objects_magic00(lua_State* tolua_S)
{
 if (!tolua_isnoobj(tolua_S,1))
 {
  TOLUA_ERR_FN(detect_objects_magic);
 } else {
  bool toluaI_ret = (bool)  detect_objects_magic();
  tolua_pushbool(tolua_S,(int)toluaI_ret);
 }
 return 1;
}

/* function: detect_monsters_normal */
static int toluaI_spell_detect_monsters_normal00(lua_State* tolua_S)
{
 if (!tolua_isnoobj(tolua_S,1))
 {
  TOLUA_ERR_FN(detect_monsters_normal);
 } else {
  bool toluaI_ret = (bool)  detect_monsters_normal();
  tolua_pushbool(tolua_S,(int)toluaI_ret);
 }
 return 1;
}

/* function: detect_monsters_invis */
static int toluaI_spell_detect_monsters_invis00(lua_State* tolua_S)
{
 if (!tolua_isnoobj(tolua_S,1))
 {
  TOLUA_ERR_FN(detect_monsters_invis);
 } else {
  bool toluaI_ret = (bool)  detect_monsters_invis();
  tolua_pushbool(tolua_S,(int)toluaI_ret);
 }
 return 1;
}

/* function: detect_monsters_evil */
static int toluaI_spell_detect_monsters_evil00(lua_State* tolua_S)
{
 if (!tolua_isnoobj(tolua_S,1))
 {
  TOLUA_ERR_FN(detect_monsters_evil);
 } else {
  bool toluaI_ret = (bool)  detect_monsters_evil();
  tolua_pushbool(tolua_S,(int)toluaI_ret);
 }
 return 1;
}

/* function: detect_monsters_xxx */
static int toluaI_spell_detect_monsters_xxx00(lua_State* tolua_S)
{
 if (!tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
     !tolua_isnoobj(tolua_S,2))
 {
  TOLUA_ERR_FN(detect_monsters_xxx);
 } else {
  u32b match_flag = ((u32b)  tolua_getnumber(tolua_S,1,0));
  bool toluaI_ret = (bool)  detect_monsters_xxx(match_flag);
  tolua_pushbool(tolua_S,(int)toluaI_ret);
 }
 return 1;
}

/* function: detect_monsters_string */
static int toluaI_spell_detect_monsters_string00(lua_State* tolua_S)
{
 if (!tolua_istype(tolua_S,1,LUA_TSTRING,0) ||
     !tolua_isnoobj(tolua_S,2))
 {
  TOLUA_ERR_FN(detect_monsters_string);
 } else {
  cptr tolua_var_1 = ((cptr)  tolua_getstring(tolua_S,1,0));
  bool toluaI_ret = (bool)  detect_monsters_string(tolua_var_1);
  tolua_pushbool(tolua_S,(int)toluaI_ret);
 }
 return 1;
}

/* function: detect_monsters_nonliving */
static int toluaI_spell_detect_monsters_nonliving00(lua_State* tolua_S)
{
 if (!tolua_isnoobj(tolua_S,1))
 {
  TOLUA_ERR_FN(detect_monsters_nonliving);
 } else {
  bool toluaI_ret = (bool)  detect_monsters_nonliving();
  tolua_pushbool(tolua_S,(int)toluaI_ret);
 }
 return 1;
}

/* function: detect_monsters_living */
static int toluaI_spell_detect_monsters_living00(lua_State* tolua_S)
{
 if (!tolua_isnoobj(tolua_S,1))
 {
  TOLUA_ERR_FN(detect_monsters_living);
 } else {
  bool toluaI_ret = (bool)  detect_monsters_living();
  tolua_pushbool(tolua_S,(int)toluaI_ret);
 }
 return 1;
}

/* function: detect_all */
static int toluaI_spell_detect_all00(lua_State* tolua_S)
{
 if (!tolua_isnoobj(tolua_S,1))
 {
  TOLUA_ERR_FN(detect_all);
 } else {
  bool toluaI_ret = (bool)  detect_all();
  tolua_pushbool(tolua_S,(int)toluaI_ret);
 }
 return 1;
}

/* function: wall_stone */
static int toluaI_spell_wall_stone00(lua_State* tolua_S)
{
 if (!tolua_isnoobj(tolua_S,1))
 {
  TOLUA_ERR_FN(wall_stone);
 } else {
  bool toluaI_ret = (bool)  wall_stone();
  tolua_pushbool(tolua_S,(int)toluaI_ret);
 }
 return 1;
}

/* function: speed_monsters */
static int toluaI_spell_speed_monsters00(lua_State* tolua_S)
{
 if (!tolua_isnoobj(tolua_S,1))
 {
  TOLUA_ERR_FN(speed_monsters);
 } else {
  bool toluaI_ret = (bool)  speed_monsters();
  tolua_pushbool(tolua_S,(int)toluaI_ret);
 }
 return 1;
}

/* function: slow_monsters */
static int toluaI_spell_slow_monsters00(lua_State* tolua_S)
{
 if (!tolua_isnoobj(tolua_S,1))
 {
  TOLUA_ERR_FN(slow_monsters);
 } else {
  bool toluaI_ret = (bool)  slow_monsters();
  tolua_pushbool(tolua_S,(int)toluaI_ret);
 }
 return 1;
}

/* function: sleep_monsters */
static int toluaI_spell_sleep_monsters00(lua_State* tolua_S)
{
 if (!tolua_isnoobj(tolua_S,1))
 {
  TOLUA_ERR_FN(sleep_monsters);
 } else {
  bool toluaI_ret = (bool)  sleep_monsters();
  tolua_pushbool(tolua_S,(int)toluaI_ret);
 }
 return 1;
}

/* function: aggravate_monsters */
static int toluaI_spell_aggravate_monsters00(lua_State* tolua_S)
{
 if (!tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
     !tolua_isnoobj(tolua_S,2))
 {
  TOLUA_ERR_FN(aggravate_monsters);
 } else {
  int who = ((int)  tolua_getnumber(tolua_S,1,0));
  aggravate_monsters(who);
 }
 return 0;
}

/* function: genocide */
static int toluaI_spell_genocide00(lua_State* tolua_S)
{
 if (!tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
     !tolua_isnoobj(tolua_S,2))
 {
  TOLUA_ERR_FN(genocide);
 } else {
  int player_cast = ((int)  tolua_getnumber(tolua_S,1,0));
  bool toluaI_ret = (bool)  genocide(player_cast);
  tolua_pushbool(tolua_S,(int)toluaI_ret);
 }
 return 1;
}

/* function: mass_genocide */
static int toluaI_spell_mass_genocide00(lua_State* tolua_S)
{
 if (!tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
     !tolua_isnoobj(tolua_S,2))
 {
  TOLUA_ERR_FN(mass_genocide);
 } else {
  int player_cast = ((int)  tolua_getnumber(tolua_S,1,0));
  bool toluaI_ret = (bool)  mass_genocide(player_cast);
  tolua_pushbool(tolua_S,(int)toluaI_ret);
 }
 return 1;
}

/* function: probing */
static int toluaI_spell_probing00(lua_State* tolua_S)
{
 if (!tolua_isnoobj(tolua_S,1))
 {
  TOLUA_ERR_FN(probing);
 } else {
  bool toluaI_ret = (bool)  probing();
  tolua_pushbool(tolua_S,(int)toluaI_ret);
 }
 return 1;
}

/* function: banish_evil */
static int toluaI_spell_banish_evil00(lua_State* tolua_S)
{
 if (!tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
     !tolua_isnoobj(tolua_S,2))
 {
  TOLUA_ERR_FN(banish_evil);
 } else {
  int dist = ((int)  tolua_getnumber(tolua_S,1,0));
  bool toluaI_ret = (bool)  banish_evil(dist);
  tolua_pushbool(tolua_S,(int)toluaI_ret);
 }
 return 1;
}

/* function: dispel_evil */
static int toluaI_spell_dispel_evil00(lua_State* tolua_S)
{
 if (!tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
     !tolua_isnoobj(tolua_S,2))
 {
  TOLUA_ERR_FN(dispel_evil);
 } else {
  int dam = ((int)  tolua_getnumber(tolua_S,1,0));
  bool toluaI_ret = (bool)  dispel_evil(dam);
  tolua_pushbool(tolua_S,(int)toluaI_ret);
 }
 return 1;
}

/* function: dispel_good */
static int toluaI_spell_dispel_good00(lua_State* tolua_S)
{
 if (!tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
     !tolua_isnoobj(tolua_S,2))
 {
  TOLUA_ERR_FN(dispel_good);
 } else {
  int dam = ((int)  tolua_getnumber(tolua_S,1,0));
  bool toluaI_ret = (bool)  dispel_good(dam);
  tolua_pushbool(tolua_S,(int)toluaI_ret);
 }
 return 1;
}

/* function: dispel_undead */
static int toluaI_spell_dispel_undead00(lua_State* tolua_S)
{
 if (!tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
     !tolua_isnoobj(tolua_S,2))
 {
  TOLUA_ERR_FN(dispel_undead);
 } else {
  int dam = ((int)  tolua_getnumber(tolua_S,1,0));
  bool toluaI_ret = (bool)  dispel_undead(dam);
  tolua_pushbool(tolua_S,(int)toluaI_ret);
 }
 return 1;
}

/* function: dispel_monsters */
static int toluaI_spell_dispel_monsters00(lua_State* tolua_S)
{
 if (!tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
     !tolua_isnoobj(tolua_S,2))
 {
  TOLUA_ERR_FN(dispel_monsters);
 } else {
  int dam = ((int)  tolua_getnumber(tolua_S,1,0));
  bool toluaI_ret = (bool)  dispel_monsters(dam);
  tolua_pushbool(tolua_S,(int)toluaI_ret);
 }
 return 1;
}

/* function: dispel_living */
static int toluaI_spell_dispel_living00(lua_State* tolua_S)
{
 if (!tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
     !tolua_isnoobj(tolua_S,2))
 {
  TOLUA_ERR_FN(dispel_living);
 } else {
  int dam = ((int)  tolua_getnumber(tolua_S,1,0));
  bool toluaI_ret = (bool)  dispel_living(dam);
  tolua_pushbool(tolua_S,(int)toluaI_ret);
 }
 return 1;
}

/* function: dispel_demons */
static int toluaI_spell_dispel_demons00(lua_State* tolua_S)
{
 if (!tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
     !tolua_isnoobj(tolua_S,2))
 {
  TOLUA_ERR_FN(dispel_demons);
 } else {
  int dam = ((int)  tolua_getnumber(tolua_S,1,0));
  bool toluaI_ret = (bool)  dispel_demons(dam);
  tolua_pushbool(tolua_S,(int)toluaI_ret);
 }
 return 1;
}

/* function: destroy_area */
static int toluaI_spell_destroy_area00(lua_State* tolua_S)
{
 if (!tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
     !tolua_istype(tolua_S,2,LUA_TNUMBER,0) ||
     !tolua_istype(tolua_S,3,LUA_TNUMBER,0) ||
     !tolua_isnoobj(tolua_S,4))
 {
  TOLUA_ERR_FN(destroy_area);
 } else {
  int x1 = ((int)  tolua_getnumber(tolua_S,1,0));
  int y1 = ((int)  tolua_getnumber(tolua_S,2,0));
  int r = ((int)  tolua_getnumber(tolua_S,3,0));
  bool toluaI_ret = (bool)  destroy_area(x1,y1,r);
  tolua_pushbool(tolua_S,(int)toluaI_ret);
 }
 return 1;
}

/* function: earthquake */
static int toluaI_spell_earthquake00(lua_State* tolua_S)
{
 if (!tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
     !tolua_istype(tolua_S,2,LUA_TNUMBER,0) ||
     !tolua_istype(tolua_S,3,LUA_TNUMBER,0) ||
     !tolua_isnoobj(tolua_S,4))
 {
  TOLUA_ERR_FN(earthquake);
 } else {
  int cx = ((int)  tolua_getnumber(tolua_S,1,0));
  int cy = ((int)  tolua_getnumber(tolua_S,2,0));
  int r = ((int)  tolua_getnumber(tolua_S,3,0));
  bool toluaI_ret = (bool)  earthquake(cx,cy,r);
  tolua_pushbool(tolua_S,(int)toluaI_ret);
 }
 return 1;
}

/* function: lite_room */
static int toluaI_spell_lite_room00(lua_State* tolua_S)
{
 if (!tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
     !tolua_istype(tolua_S,2,LUA_TNUMBER,0) ||
     !tolua_isnoobj(tolua_S,3))
 {
  TOLUA_ERR_FN(lite_room);
 } else {
  int x1 = ((int)  tolua_getnumber(tolua_S,1,0));
  int y1 = ((int)  tolua_getnumber(tolua_S,2,0));
  lite_room(x1,y1);
 }
 return 0;
}

/* function: unlite_room */
static int toluaI_spell_unlite_room00(lua_State* tolua_S)
{
 if (!tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
     !tolua_istype(tolua_S,2,LUA_TNUMBER,0) ||
     !tolua_isnoobj(tolua_S,3))
 {
  TOLUA_ERR_FN(unlite_room);
 } else {
  int x1 = ((int)  tolua_getnumber(tolua_S,1,0));
  int y1 = ((int)  tolua_getnumber(tolua_S,2,0));
  unlite_room(x1,y1);
 }
 return 0;
}

/* function: lite_area */
static int toluaI_spell_lite_area00(lua_State* tolua_S)
{
 if (!tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
     !tolua_istype(tolua_S,2,LUA_TNUMBER,0) ||
     !tolua_isnoobj(tolua_S,3))
 {
  TOLUA_ERR_FN(lite_area);
 } else {
  int dam = ((int)  tolua_getnumber(tolua_S,1,0));
  int rad = ((int)  tolua_getnumber(tolua_S,2,0));
  bool toluaI_ret = (bool)  lite_area(dam,rad);
  tolua_pushbool(tolua_S,(int)toluaI_ret);
 }
 return 1;
}

/* function: unlite_area */
static int toluaI_spell_unlite_area00(lua_State* tolua_S)
{
 if (!tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
     !tolua_istype(tolua_S,2,LUA_TNUMBER,0) ||
     !tolua_isnoobj(tolua_S,3))
 {
  TOLUA_ERR_FN(unlite_area);
 } else {
  int dam = ((int)  tolua_getnumber(tolua_S,1,0));
  int rad = ((int)  tolua_getnumber(tolua_S,2,0));
  bool toluaI_ret = (bool)  unlite_area(dam,rad);
  tolua_pushbool(tolua_S,(int)toluaI_ret);
 }
 return 1;
}

/* function: fire_ball */
static int toluaI_spell_fire_ball00(lua_State* tolua_S)
{
 if (!tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
     !tolua_istype(tolua_S,2,LUA_TNUMBER,0) ||
     !tolua_istype(tolua_S,3,LUA_TNUMBER,0) ||
     !tolua_istype(tolua_S,4,LUA_TNUMBER,0) ||
     !tolua_isnoobj(tolua_S,5))
 {
  TOLUA_ERR_FN(fire_ball);
 } else {
  int typ = ((int)  tolua_getnumber(tolua_S,1,0));
  int dir = ((int)  tolua_getnumber(tolua_S,2,0));
  int dam = ((int)  tolua_getnumber(tolua_S,3,0));
  int rad = ((int)  tolua_getnumber(tolua_S,4,0));
  bool toluaI_ret = (bool)  fire_ball(typ,dir,dam,rad);
  tolua_pushbool(tolua_S,(int)toluaI_ret);
 }
 return 1;
}

/* function: fire_bolt */
static int toluaI_spell_fire_bolt00(lua_State* tolua_S)
{
 if (!tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
     !tolua_istype(tolua_S,2,LUA_TNUMBER,0) ||
     !tolua_istype(tolua_S,3,LUA_TNUMBER,0) ||
     !tolua_isnoobj(tolua_S,4))
 {
  TOLUA_ERR_FN(fire_bolt);
 } else {
  int typ = ((int)  tolua_getnumber(tolua_S,1,0));
  int dir = ((int)  tolua_getnumber(tolua_S,2,0));
  int dam = ((int)  tolua_getnumber(tolua_S,3,0));
  bool toluaI_ret = (bool)  fire_bolt(typ,dir,dam);
  tolua_pushbool(tolua_S,(int)toluaI_ret);
 }
 return 1;
}

/* function: call_chaos */
static int toluaI_spell_call_chaos00(lua_State* tolua_S)
{
 if (!tolua_isnoobj(tolua_S,1))
 {
  TOLUA_ERR_FN(call_chaos);
 } else {
  call_chaos();
 }
 return 0;
}

/* function: fire_beam */
static int toluaI_spell_fire_beam00(lua_State* tolua_S)
{
 if (!tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
     !tolua_istype(tolua_S,2,LUA_TNUMBER,0) ||
     !tolua_istype(tolua_S,3,LUA_TNUMBER,0) ||
     !tolua_isnoobj(tolua_S,4))
 {
  TOLUA_ERR_FN(fire_beam);
 } else {
  int typ = ((int)  tolua_getnumber(tolua_S,1,0));
  int dir = ((int)  tolua_getnumber(tolua_S,2,0));
  int dam = ((int)  tolua_getnumber(tolua_S,3,0));
  bool toluaI_ret = (bool)  fire_beam(typ,dir,dam);
  tolua_pushbool(tolua_S,(int)toluaI_ret);
 }
 return 1;
}

/* function: fire_bolt_or_beam */
static int toluaI_spell_fire_bolt_or_beam00(lua_State* tolua_S)
{
 if (!tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
     !tolua_istype(tolua_S,2,LUA_TNUMBER,0) ||
     !tolua_istype(tolua_S,3,LUA_TNUMBER,0) ||
     !tolua_istype(tolua_S,4,LUA_TNUMBER,0) ||
     !tolua_isnoobj(tolua_S,5))
 {
  TOLUA_ERR_FN(fire_bolt_or_beam);
 } else {
  int prob = ((int)  tolua_getnumber(tolua_S,1,0));
  int typ = ((int)  tolua_getnumber(tolua_S,2,0));
  int dir = ((int)  tolua_getnumber(tolua_S,3,0));
  int dam = ((int)  tolua_getnumber(tolua_S,4,0));
  bool toluaI_ret = (bool)  fire_bolt_or_beam(prob,typ,dir,dam);
  tolua_pushbool(tolua_S,(int)toluaI_ret);
 }
 return 1;
}

/* function: lite_line */
static int toluaI_spell_lite_line00(lua_State* tolua_S)
{
 if (!tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
     !tolua_isnoobj(tolua_S,2))
 {
  TOLUA_ERR_FN(lite_line);
 } else {
  int dir = ((int)  tolua_getnumber(tolua_S,1,0));
  bool toluaI_ret = (bool)  lite_line(dir);
  tolua_pushbool(tolua_S,(int)toluaI_ret);
 }
 return 1;
}

/* function: drain_life */
static int toluaI_spell_drain_life00(lua_State* tolua_S)
{
 if (!tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
     !tolua_istype(tolua_S,2,LUA_TNUMBER,0) ||
     !tolua_isnoobj(tolua_S,3))
 {
  TOLUA_ERR_FN(drain_life);
 } else {
  int dir = ((int)  tolua_getnumber(tolua_S,1,0));
  int dam = ((int)  tolua_getnumber(tolua_S,2,0));
  bool toluaI_ret = (bool)  drain_life(dir,dam);
  tolua_pushbool(tolua_S,(int)toluaI_ret);
 }
 return 1;
}

/* function: drain_gain_life */
static int toluaI_spell_drain_gain_life00(lua_State* tolua_S)
{
 if (!tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
     !tolua_istype(tolua_S,2,LUA_TNUMBER,0) ||
     !tolua_isnoobj(tolua_S,3))
 {
  TOLUA_ERR_FN(drain_gain_life);
 } else {
  int dor = ((int)  tolua_getnumber(tolua_S,1,0));
  int dam = ((int)  tolua_getnumber(tolua_S,2,0));
  bool toluaI_ret = (bool)  drain_gain_life(dor,dam);
  tolua_pushbool(tolua_S,(int)toluaI_ret);
 }
 return 1;
}

/* function: death_ray */
static int toluaI_spell_death_ray00(lua_State* tolua_S)
{
 if (!tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
     !tolua_istype(tolua_S,2,LUA_TNUMBER,0) ||
     !tolua_isnoobj(tolua_S,3))
 {
  TOLUA_ERR_FN(death_ray);
 } else {
  int dir = ((int)  tolua_getnumber(tolua_S,1,0));
  int plev = ((int)  tolua_getnumber(tolua_S,2,0));
  bool toluaI_ret = (bool)  death_ray(dir,plev);
  tolua_pushbool(tolua_S,(int)toluaI_ret);
 }
 return 1;
}

/* function: wall_to_mud */
static int toluaI_spell_wall_to_mud00(lua_State* tolua_S)
{
 if (!tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
     !tolua_isnoobj(tolua_S,2))
 {
  TOLUA_ERR_FN(wall_to_mud);
 } else {
  int dir = ((int)  tolua_getnumber(tolua_S,1,0));
  bool toluaI_ret = (bool)  wall_to_mud(dir);
  tolua_pushbool(tolua_S,(int)toluaI_ret);
 }
 return 1;
}

/* function: destroy_door */
static int toluaI_spell_destroy_door00(lua_State* tolua_S)
{
 if (!tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
     !tolua_isnoobj(tolua_S,2))
 {
  TOLUA_ERR_FN(destroy_door);
 } else {
  int dir = ((int)  tolua_getnumber(tolua_S,1,0));
  bool toluaI_ret = (bool)  destroy_door(dir);
  tolua_pushbool(tolua_S,(int)toluaI_ret);
 }
 return 1;
}

/* function: disarm_trap */
static int toluaI_spell_disarm_trap00(lua_State* tolua_S)
{
 if (!tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
     !tolua_isnoobj(tolua_S,2))
 {
  TOLUA_ERR_FN(disarm_trap);
 } else {
  int dir = ((int)  tolua_getnumber(tolua_S,1,0));
  bool toluaI_ret = (bool)  disarm_trap(dir);
  tolua_pushbool(tolua_S,(int)toluaI_ret);
 }
 return 1;
}

/* function: wizard_lock */
static int toluaI_spell_wizard_lock00(lua_State* tolua_S)
{
 if (!tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
     !tolua_isnoobj(tolua_S,2))
 {
  TOLUA_ERR_FN(wizard_lock);
 } else {
  int dir = ((int)  tolua_getnumber(tolua_S,1,0));
  bool toluaI_ret = (bool)  wizard_lock(dir);
  tolua_pushbool(tolua_S,(int)toluaI_ret);
 }
 return 1;
}

/* function: heal_monster */
static int toluaI_spell_heal_monster00(lua_State* tolua_S)
{
 if (!tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
     !tolua_isnoobj(tolua_S,2))
 {
  TOLUA_ERR_FN(heal_monster);
 } else {
  int dir = ((int)  tolua_getnumber(tolua_S,1,0));
  bool toluaI_ret = (bool)  heal_monster(dir);
  tolua_pushbool(tolua_S,(int)toluaI_ret);
 }
 return 1;
}

/* function: speed_monster */
static int toluaI_spell_speed_monster00(lua_State* tolua_S)
{
 if (!tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
     !tolua_isnoobj(tolua_S,2))
 {
  TOLUA_ERR_FN(speed_monster);
 } else {
  int dir = ((int)  tolua_getnumber(tolua_S,1,0));
  bool toluaI_ret = (bool)  speed_monster(dir);
  tolua_pushbool(tolua_S,(int)toluaI_ret);
 }
 return 1;
}

/* function: slow_monster */
static int toluaI_spell_slow_monster00(lua_State* tolua_S)
{
 if (!tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
     !tolua_isnoobj(tolua_S,2))
 {
  TOLUA_ERR_FN(slow_monster);
 } else {
  int dir = ((int)  tolua_getnumber(tolua_S,1,0));
  bool toluaI_ret = (bool)  slow_monster(dir);
  tolua_pushbool(tolua_S,(int)toluaI_ret);
 }
 return 1;
}

/* function: sleep_monster */
static int toluaI_spell_sleep_monster00(lua_State* tolua_S)
{
 if (!tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
     !tolua_isnoobj(tolua_S,2))
 {
  TOLUA_ERR_FN(sleep_monster);
 } else {
  int dir = ((int)  tolua_getnumber(tolua_S,1,0));
  bool toluaI_ret = (bool)  sleep_monster(dir);
  tolua_pushbool(tolua_S,(int)toluaI_ret);
 }
 return 1;
}

/* function: stasis_monster */
static int toluaI_spell_stasis_monster00(lua_State* tolua_S)
{
 if (!tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
     !tolua_isnoobj(tolua_S,2))
 {
  TOLUA_ERR_FN(stasis_monster);
 } else {
  int dir = ((int)  tolua_getnumber(tolua_S,1,0));
  bool toluaI_ret = (bool)  stasis_monster(dir);
  tolua_pushbool(tolua_S,(int)toluaI_ret);
 }
 return 1;
}

/* function: confuse_monster */
static int toluaI_spell_confuse_monster00(lua_State* tolua_S)
{
 if (!tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
     !tolua_istype(tolua_S,2,LUA_TNUMBER,0) ||
     !tolua_isnoobj(tolua_S,3))
 {
  TOLUA_ERR_FN(confuse_monster);
 } else {
  int dir = ((int)  tolua_getnumber(tolua_S,1,0));
  int plev = ((int)  tolua_getnumber(tolua_S,2,0));
  bool toluaI_ret = (bool)  confuse_monster(dir,plev);
  tolua_pushbool(tolua_S,(int)toluaI_ret);
 }
 return 1;
}

/* function: stun_monster */
static int toluaI_spell_stun_monster00(lua_State* tolua_S)
{
 if (!tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
     !tolua_istype(tolua_S,2,LUA_TNUMBER,0) ||
     !tolua_isnoobj(tolua_S,3))
 {
  TOLUA_ERR_FN(stun_monster);
 } else {
  int dir = ((int)  tolua_getnumber(tolua_S,1,0));
  int plev = ((int)  tolua_getnumber(tolua_S,2,0));
  bool toluaI_ret = (bool)  stun_monster(dir,plev);
  tolua_pushbool(tolua_S,(int)toluaI_ret);
 }
 return 1;
}

/* function: fear_monster */
static int toluaI_spell_fear_monster00(lua_State* tolua_S)
{
 if (!tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
     !tolua_istype(tolua_S,2,LUA_TNUMBER,0) ||
     !tolua_isnoobj(tolua_S,3))
 {
  TOLUA_ERR_FN(fear_monster);
 } else {
  int dir = ((int)  tolua_getnumber(tolua_S,1,0));
  int plev = ((int)  tolua_getnumber(tolua_S,2,0));
  bool toluaI_ret = (bool)  fear_monster(dir,plev);
  tolua_pushbool(tolua_S,(int)toluaI_ret);
 }
 return 1;
}

/* function: poly_monster */
static int toluaI_spell_poly_monster00(lua_State* tolua_S)
{
 if (!tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
     !tolua_isnoobj(tolua_S,2))
 {
  TOLUA_ERR_FN(poly_monster);
 } else {
  int dir = ((int)  tolua_getnumber(tolua_S,1,0));
  bool toluaI_ret = (bool)  poly_monster(dir);
  tolua_pushbool(tolua_S,(int)toluaI_ret);
 }
 return 1;
}

/* function: clone_monster */
static int toluaI_spell_clone_monster00(lua_State* tolua_S)
{
 if (!tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
     !tolua_isnoobj(tolua_S,2))
 {
  TOLUA_ERR_FN(clone_monster);
 } else {
  int dir = ((int)  tolua_getnumber(tolua_S,1,0));
  bool toluaI_ret = (bool)  clone_monster(dir);
  tolua_pushbool(tolua_S,(int)toluaI_ret);
 }
 return 1;
}

/* function: teleport_monster */
static int toluaI_spell_teleport_monster00(lua_State* tolua_S)
{
 if (!tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
     !tolua_isnoobj(tolua_S,2))
 {
  TOLUA_ERR_FN(teleport_monster);
 } else {
  int dir = ((int)  tolua_getnumber(tolua_S,1,0));
  bool toluaI_ret = (bool)  teleport_monster(dir);
  tolua_pushbool(tolua_S,(int)toluaI_ret);
 }
 return 1;
}

/* function: door_creation */
static int toluaI_spell_door_creation00(lua_State* tolua_S)
{
 if (!tolua_isnoobj(tolua_S,1))
 {
  TOLUA_ERR_FN(door_creation);
 } else {
  bool toluaI_ret = (bool)  door_creation();
  tolua_pushbool(tolua_S,(int)toluaI_ret);
 }
 return 1;
}

/* function: trap_creation */
static int toluaI_spell_trap_creation00(lua_State* tolua_S)
{
 if (!tolua_isnoobj(tolua_S,1))
 {
  TOLUA_ERR_FN(trap_creation);
 } else {
  bool toluaI_ret = (bool)  trap_creation();
  tolua_pushbool(tolua_S,(int)toluaI_ret);
 }
 return 1;
}

/* function: glyph_creation */
static int toluaI_spell_glyph_creation00(lua_State* tolua_S)
{
 if (!tolua_isnoobj(tolua_S,1))
 {
  TOLUA_ERR_FN(glyph_creation);
 } else {
  bool toluaI_ret = (bool)  glyph_creation();
  tolua_pushbool(tolua_S,(int)toluaI_ret);
 }
 return 1;
}

/* function: destroy_doors_touch */
static int toluaI_spell_destroy_doors_touch00(lua_State* tolua_S)
{
 if (!tolua_isnoobj(tolua_S,1))
 {
  TOLUA_ERR_FN(destroy_doors_touch);
 } else {
  bool toluaI_ret = (bool)  destroy_doors_touch();
  tolua_pushbool(tolua_S,(int)toluaI_ret);
 }
 return 1;
}

/* function: sleep_monsters_touch */
static int toluaI_spell_sleep_monsters_touch00(lua_State* tolua_S)
{
 if (!tolua_isnoobj(tolua_S,1))
 {
  TOLUA_ERR_FN(sleep_monsters_touch);
 } else {
  bool toluaI_ret = (bool)  sleep_monsters_touch();
  tolua_pushbool(tolua_S,(int)toluaI_ret);
 }
 return 1;
}

/* function: confuse_monsters */
static int toluaI_spell_confuse_monsters00(lua_State* tolua_S)
{
 if (!tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
     !tolua_isnoobj(tolua_S,2))
 {
  TOLUA_ERR_FN(confuse_monsters);
 } else {
  int dam = ((int)  tolua_getnumber(tolua_S,1,0));
  bool toluaI_ret = (bool)  confuse_monsters(dam);
  tolua_pushbool(tolua_S,(int)toluaI_ret);
 }
 return 1;
}

/* function: charm_monsters */
static int toluaI_spell_charm_monsters00(lua_State* tolua_S)
{
 if (!tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
     !tolua_isnoobj(tolua_S,2))
 {
  TOLUA_ERR_FN(charm_monsters);
 } else {
  int dam = ((int)  tolua_getnumber(tolua_S,1,0));
  bool toluaI_ret = (bool)  charm_monsters(dam);
  tolua_pushbool(tolua_S,(int)toluaI_ret);
 }
 return 1;
}

/* function: charm_animals */
static int toluaI_spell_charm_animals00(lua_State* tolua_S)
{
 if (!tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
     !tolua_isnoobj(tolua_S,2))
 {
  TOLUA_ERR_FN(charm_animals);
 } else {
  int dam = ((int)  tolua_getnumber(tolua_S,1,0));
  bool toluaI_ret = (bool)  charm_animals(dam);
  tolua_pushbool(tolua_S,(int)toluaI_ret);
 }
 return 1;
}

/* function: starlite */
static int toluaI_spell_starlite00(lua_State* tolua_S)
{
 if (!tolua_isnoobj(tolua_S,1))
 {
  TOLUA_ERR_FN(starlite);
 } else {
  bool toluaI_ret = (bool)  starlite();
  tolua_pushbool(tolua_S,(int)toluaI_ret);
 }
 return 1;
}

/* function: scatter_ball */
static int toluaI_spell_scatter_ball00(lua_State* tolua_S)
{
 if (!tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
     !tolua_istype(tolua_S,2,LUA_TNUMBER,0) ||
     !tolua_istype(tolua_S,3,LUA_TNUMBER,0) ||
     !tolua_istype(tolua_S,4,LUA_TNUMBER,0) ||
     !tolua_isnoobj(tolua_S,5))
 {
  TOLUA_ERR_FN(scatter_ball);
 } else {
  int num = ((int)  tolua_getnumber(tolua_S,1,0));
  int typ = ((int)  tolua_getnumber(tolua_S,2,0));
  int dam = ((int)  tolua_getnumber(tolua_S,3,0));
  int rad = ((int)  tolua_getnumber(tolua_S,4,0));
  bool toluaI_ret = (bool)  scatter_ball(num,typ,dam,rad);
  tolua_pushbool(tolua_S,(int)toluaI_ret);
 }
 return 1;
}

/* function: create_food */
static int toluaI_spell_create_food00(lua_State* tolua_S)
{
 if (!tolua_isnoobj(tolua_S,1))
 {
  TOLUA_ERR_FN(create_food);
 } else {
  create_food();
 }
 return 0;
}

/* function: whirlwind_attack */
static int toluaI_spell_whirlwind_attack00(lua_State* tolua_S)
{
 if (!tolua_isnoobj(tolua_S,1))
 {
  TOLUA_ERR_FN(whirlwind_attack);
 } else {
  whirlwind_attack();
 }
 return 0;
}

/* function: stun_monsters */
static int toluaI_spell_stun_monsters00(lua_State* tolua_S)
{
 if (!tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
     !tolua_isnoobj(tolua_S,2))
 {
  TOLUA_ERR_FN(stun_monsters);
 } else {
  int dam = ((int)  tolua_getnumber(tolua_S,1,0));
  bool toluaI_ret = (bool)  stun_monsters(dam);
  tolua_pushbool(tolua_S,(int)toluaI_ret);
 }
 return 1;
}

/* function: stasis_monsters */
static int toluaI_spell_stasis_monsters00(lua_State* tolua_S)
{
 if (!tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
     !tolua_isnoobj(tolua_S,2))
 {
  TOLUA_ERR_FN(stasis_monsters);
 } else {
  int dam = ((int)  tolua_getnumber(tolua_S,1,0));
  bool toluaI_ret = (bool)  stasis_monsters(dam);
  tolua_pushbool(tolua_S,(int)toluaI_ret);
 }
 return 1;
}

/* function: banish_monsters */
static int toluaI_spell_banish_monsters00(lua_State* tolua_S)
{
 if (!tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
     !tolua_isnoobj(tolua_S,2))
 {
  TOLUA_ERR_FN(banish_monsters);
 } else {
  int dist = ((int)  tolua_getnumber(tolua_S,1,0));
  bool toluaI_ret = (bool)  banish_monsters(dist);
  tolua_pushbool(tolua_S,(int)toluaI_ret);
 }
 return 1;
}

/* function: turn_monsters */
static int toluaI_spell_turn_monsters00(lua_State* tolua_S)
{
 if (!tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
     !tolua_isnoobj(tolua_S,2))
 {
  TOLUA_ERR_FN(turn_monsters);
 } else {
  int dam = ((int)  tolua_getnumber(tolua_S,1,0));
  bool toluaI_ret = (bool)  turn_monsters(dam);
  tolua_pushbool(tolua_S,(int)toluaI_ret);
 }
 return 1;
}

/* function: turn_evil */
static int toluaI_spell_turn_evil00(lua_State* tolua_S)
{
 if (!tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
     !tolua_isnoobj(tolua_S,2))
 {
  TOLUA_ERR_FN(turn_evil);
 } else {
  int dam = ((int)  tolua_getnumber(tolua_S,1,0));
  bool toluaI_ret = (bool)  turn_evil(dam);
  tolua_pushbool(tolua_S,(int)toluaI_ret);
 }
 return 1;
}

/* function: deathray_monsters */
static int toluaI_spell_deathray_monsters00(lua_State* tolua_S)
{
 if (!tolua_isnoobj(tolua_S,1))
 {
  TOLUA_ERR_FN(deathray_monsters);
 } else {
  bool toluaI_ret = (bool)  deathray_monsters();
  tolua_pushbool(tolua_S,(int)toluaI_ret);
 }
 return 1;
}

/* function: charm_monster */
static int toluaI_spell_charm_monster00(lua_State* tolua_S)
{
 if (!tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
     !tolua_istype(tolua_S,2,LUA_TNUMBER,0) ||
     !tolua_isnoobj(tolua_S,3))
 {
  TOLUA_ERR_FN(charm_monster);
 } else {
  int dir = ((int)  tolua_getnumber(tolua_S,1,0));
  int plev = ((int)  tolua_getnumber(tolua_S,2,0));
  bool toluaI_ret = (bool)  charm_monster(dir,plev);
  tolua_pushbool(tolua_S,(int)toluaI_ret);
 }
 return 1;
}

/* function: control_one_undead */
static int toluaI_spell_control_one_undead00(lua_State* tolua_S)
{
 if (!tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
     !tolua_istype(tolua_S,2,LUA_TNUMBER,0) ||
     !tolua_isnoobj(tolua_S,3))
 {
  TOLUA_ERR_FN(control_one_undead);
 } else {
  int dir = ((int)  tolua_getnumber(tolua_S,1,0));
  int plev = ((int)  tolua_getnumber(tolua_S,2,0));
  bool toluaI_ret = (bool)  control_one_undead(dir,plev);
  tolua_pushbool(tolua_S,(int)toluaI_ret);
 }
 return 1;
}

/* function: charm_animal */
static int toluaI_spell_charm_animal00(lua_State* tolua_S)
{
 if (!tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
     !tolua_istype(tolua_S,2,LUA_TNUMBER,0) ||
     !tolua_isnoobj(tolua_S,3))
 {
  TOLUA_ERR_FN(charm_animal);
 } else {
  int dir = ((int)  tolua_getnumber(tolua_S,1,0));
  int plev = ((int)  tolua_getnumber(tolua_S,2,0));
  bool toluaI_ret = (bool)  charm_animal(dir,plev);
  tolua_pushbool(tolua_S,(int)toluaI_ret);
 }
 return 1;
}

/* function: mindblast_monsters */
static int toluaI_spell_mindblast_monsters00(lua_State* tolua_S)
{
 if (!tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
     !tolua_isnoobj(tolua_S,2))
 {
  TOLUA_ERR_FN(mindblast_monsters);
 } else {
  int dam = ((int)  tolua_getnumber(tolua_S,1,0));
  bool toluaI_ret = (bool)  mindblast_monsters(dam);
  tolua_pushbool(tolua_S,(int)toluaI_ret);
 }
 return 1;
}

/* function: teleport_swap */
static int toluaI_spell_teleport_swap00(lua_State* tolua_S)
{
 if (!tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
     !tolua_isnoobj(tolua_S,2))
 {
  TOLUA_ERR_FN(teleport_swap);
 } else {
  int dir = ((int)  tolua_getnumber(tolua_S,1,0));
  bool toluaI_ret = (bool)  teleport_swap(dir);
  tolua_pushbool(tolua_S,(int)toluaI_ret);
 }
 return 1;
}

/* function: teleport_away */
static int toluaI_spell_teleport_away00(lua_State* tolua_S)
{
 if (!tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
     !tolua_istype(tolua_S,2,LUA_TNUMBER,0) ||
     !tolua_isnoobj(tolua_S,3))
 {
  TOLUA_ERR_FN(teleport_away);
 } else {
  int m_idx = ((int)  tolua_getnumber(tolua_S,1,0));
  int dis = ((int)  tolua_getnumber(tolua_S,2,0));
  bool toluaI_ret = (bool)  teleport_away(m_idx,dis);
  tolua_pushbool(tolua_S,(int)toluaI_ret);
 }
 return 1;
}

/* function: teleport_to_player */
static int toluaI_spell_teleport_to_player00(lua_State* tolua_S)
{
 if (!tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
     !tolua_isnoobj(tolua_S,2))
 {
  TOLUA_ERR_FN(teleport_to_player);
 } else {
  int m_idx = ((int)  tolua_getnumber(tolua_S,1,0));
  teleport_to_player(m_idx);
 }
 return 0;
}

/* function: teleport_player */
static int toluaI_spell_teleport_player00(lua_State* tolua_S)
{
 if (!tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
     !tolua_isnoobj(tolua_S,2))
 {
  TOLUA_ERR_FN(teleport_player);
 } else {
  int dis = ((int)  tolua_getnumber(tolua_S,1,0));
  teleport_player(dis);
 }
 return 0;
}

/* function: teleport_player_to */
static int toluaI_spell_teleport_player_to00(lua_State* tolua_S)
{
 if (!tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
     !tolua_istype(tolua_S,2,LUA_TNUMBER,0) ||
     !tolua_isnoobj(tolua_S,3))
 {
  TOLUA_ERR_FN(teleport_player_to);
 } else {
  int nx = ((int)  tolua_getnumber(tolua_S,1,0));
  int ny = ((int)  tolua_getnumber(tolua_S,2,0));
  teleport_player_to(nx,ny);
 }
 return 0;
}

/* function: teleport_player_level */
static int toluaI_spell_teleport_player_level00(lua_State* tolua_S)
{
 if (!tolua_isnoobj(tolua_S,1))
 {
  TOLUA_ERR_FN(teleport_player_level);
 } else {
  teleport_player_level();
 }
 return 0;
}

/* function: recall_player */
static int toluaI_spell_recall_player00(lua_State* tolua_S)
{
 if (!tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
     !tolua_isnoobj(tolua_S,2))
 {
  TOLUA_ERR_FN(recall_player);
 } else {
  int turns = ((int)  tolua_getnumber(tolua_S,1,0));
  recall_player(turns);
 }
 return 0;
}

/* function: word_of_recall */
static int toluaI_spell_word_of_recall00(lua_State* tolua_S)
{
 if (!tolua_isnoobj(tolua_S,1))
 {
  TOLUA_ERR_FN(word_of_recall);
 } else {
  word_of_recall();
 }
 return 0;
}

/* function: apply_disenchant */
static int toluaI_spell_apply_disenchant00(lua_State* tolua_S)
{
 if (!tolua_isnoobj(tolua_S,1))
 {
  TOLUA_ERR_FN(apply_disenchant);
 } else {
  bool toluaI_ret = (bool)  apply_disenchant();
  tolua_pushbool(tolua_S,(int)toluaI_ret);
 }
 return 1;
}

/* function: mutate_player */
static int toluaI_spell_mutate_player00(lua_State* tolua_S)
{
 if (!tolua_isnoobj(tolua_S,1))
 {
  TOLUA_ERR_FN(mutate_player);
 } else {
  mutate_player();
 }
 return 0;
}

/* function: phlogiston */
static int toluaI_spell_phlogiston00(lua_State* tolua_S)
{
 if (!tolua_isnoobj(tolua_S,1))
 {
  TOLUA_ERR_FN(phlogiston);
 } else {
  phlogiston();
 }
 return 0;
}

/* function: brand_weapon */
static int toluaI_spell_brand_weapon00(lua_State* tolua_S)
{
 if (!tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
     !tolua_isnoobj(tolua_S,2))
 {
  TOLUA_ERR_FN(brand_weapon);
 } else {
  int brand_type = ((int)  tolua_getnumber(tolua_S,1,0));
  brand_weapon(brand_type);
 }
 return 0;
}

/* function: call_the_ */
static int toluaI_spell_call_the_00(lua_State* tolua_S)
{
 if (!tolua_isnoobj(tolua_S,1))
 {
  TOLUA_ERR_FN(call_the_);
 } else {
  call_the_();
 }
 return 0;
}

/* function: fetch */
static int toluaI_spell_fetch00(lua_State* tolua_S)
{
 if (!tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
     !tolua_istype(tolua_S,2,LUA_TNUMBER,0) ||
     !tolua_istype(tolua_S,3,tolua_tag(tolua_S,"bool"),0) ||
     !tolua_isnoobj(tolua_S,4))
 {
  TOLUA_ERR_FN(fetch);
 } else {
  int dir = ((int)  tolua_getnumber(tolua_S,1,0));
  int wgt = ((int)  tolua_getnumber(tolua_S,2,0));
  bool require_los = ((bool)  tolua_getbool(tolua_S,3,0));
  fetch(dir,wgt,require_los);
 }
 return 0;
}

/* function: alter_reality */
static int toluaI_spell_alter_reality00(lua_State* tolua_S)
{
 if (!tolua_isnoobj(tolua_S,1))
 {
  TOLUA_ERR_FN(alter_reality);
 } else {
  alter_reality();
 }
 return 0;
}

/* function: warding_glyph */
static int toluaI_spell_warding_glyph00(lua_State* tolua_S)
{
 if (!tolua_isnoobj(tolua_S,1))
 {
  TOLUA_ERR_FN(warding_glyph);
 } else {
  bool toluaI_ret = (bool)  warding_glyph();
  tolua_pushbool(tolua_S,(int)toluaI_ret);
 }
 return 1;
}

/* function: explosive_rune */
static int toluaI_spell_explosive_rune00(lua_State* tolua_S)
{
 if (!tolua_isnoobj(tolua_S,1))
 {
  TOLUA_ERR_FN(explosive_rune);
 } else {
  bool toluaI_ret = (bool)  explosive_rune();
  tolua_pushbool(tolua_S,(int)toluaI_ret);
 }
 return 1;
}

/* function: identify_pack */
static int toluaI_spell_identify_pack00(lua_State* tolua_S)
{
 if (!tolua_isnoobj(tolua_S,1))
 {
  TOLUA_ERR_FN(identify_pack);
 } else {
  identify_pack();
 }
 return 0;
}

/* function: remove_curse */
static int toluaI_spell_remove_curse00(lua_State* tolua_S)
{
 if (!tolua_isnoobj(tolua_S,1))
 {
  TOLUA_ERR_FN(remove_curse);
 } else {
  bool toluaI_ret = (bool)  remove_curse();
  tolua_pushbool(tolua_S,(int)toluaI_ret);
 }
 return 1;
}

/* function: remove_all_curse */
static int toluaI_spell_remove_all_curse00(lua_State* tolua_S)
{
 if (!tolua_isnoobj(tolua_S,1))
 {
  TOLUA_ERR_FN(remove_all_curse);
 } else {
  bool toluaI_ret = (bool)  remove_all_curse();
  tolua_pushbool(tolua_S,(int)toluaI_ret);
 }
 return 1;
}

/* function: alchemy */
static int toluaI_spell_alchemy00(lua_State* tolua_S)
{
 if (!tolua_isnoobj(tolua_S,1))
 {
  TOLUA_ERR_FN(alchemy);
 } else {
  bool toluaI_ret = (bool)  alchemy();
  tolua_pushbool(tolua_S,(int)toluaI_ret);
 }
 return 1;
}

/* function: stair_creation */
static int toluaI_spell_stair_creation00(lua_State* tolua_S)
{
 if (!tolua_isnoobj(tolua_S,1))
 {
  TOLUA_ERR_FN(stair_creation);
 } else {
  stair_creation();
 }
 return 0;
}

/* function: enchant */
static int toluaI_spell_enchant00(lua_State* tolua_S)
{
 if (!tolua_istype(tolua_S,1,tolua_tag(tolua_S,"object_type"),0) ||
     !tolua_istype(tolua_S,2,LUA_TNUMBER,0) ||
     !tolua_istype(tolua_S,3,LUA_TNUMBER,0) ||
     !tolua_isnoobj(tolua_S,4))
 {
  TOLUA_ERR_FN(enchant);
 } else {
  object_type* o_ptr = ((object_type*)  tolua_getusertype(tolua_S,1,0));
  int n = ((int)  tolua_getnumber(tolua_S,2,0));
  int eflag = ((int)  tolua_getnumber(tolua_S,3,0));
  bool toluaI_ret = (bool)  enchant(o_ptr,n,eflag);
  tolua_pushbool(tolua_S,(int)toluaI_ret);
 }
 return 1;
}

/* function: enchant_spell */
static int toluaI_spell_enchant_spell00(lua_State* tolua_S)
{
 if (!tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
     !tolua_istype(tolua_S,2,LUA_TNUMBER,0) ||
     !tolua_istype(tolua_S,3,LUA_TNUMBER,0) ||
     !tolua_isnoobj(tolua_S,4))
 {
  TOLUA_ERR_FN(enchant_spell);
 } else {
  int num_hit = ((int)  tolua_getnumber(tolua_S,1,0));
  int num_dam = ((int)  tolua_getnumber(tolua_S,2,0));
  int num_ac = ((int)  tolua_getnumber(tolua_S,3,0));
  bool toluaI_ret = (bool)  enchant_spell(num_hit,num_dam,num_ac);
  tolua_pushbool(tolua_S,(int)toluaI_ret);
 }
 return 1;
}

/* function: artifact_scroll */
static int toluaI_spell_artifact_scroll00(lua_State* tolua_S)
{
 if (!tolua_isnoobj(tolua_S,1))
 {
  TOLUA_ERR_FN(artifact_scroll);
 } else {
  bool toluaI_ret = (bool)  artifact_scroll();
  tolua_pushbool(tolua_S,(int)toluaI_ret);
 }
 return 1;
}

/* function: ident_spell */
static int toluaI_spell_ident_spell00(lua_State* tolua_S)
{
 if (!tolua_isnoobj(tolua_S,1))
 {
  TOLUA_ERR_FN(ident_spell);
 } else {
  bool toluaI_ret = (bool)  ident_spell();
  tolua_pushbool(tolua_S,(int)toluaI_ret);
 }
 return 1;
}

/* function: mundane_spell */
static int toluaI_spell_mundane_spell00(lua_State* tolua_S)
{
 if (!tolua_isnoobj(tolua_S,1))
 {
  TOLUA_ERR_FN(mundane_spell);
 } else {
  bool toluaI_ret = (bool)  mundane_spell();
  tolua_pushbool(tolua_S,(int)toluaI_ret);
 }
 return 1;
}

/* function: identify_item */
static int toluaI_spell_identify_item00(lua_State* tolua_S)
{
 if (!tolua_istype(tolua_S,1,tolua_tag(tolua_S,"object_type"),0) ||
     !tolua_isnoobj(tolua_S,2))
 {
  TOLUA_ERR_FN(identify_item);
 } else {
  object_type* o_ptr = ((object_type*)  tolua_getusertype(tolua_S,1,0));
  identify_item(o_ptr);
 }
 return 0;
}

/* function: identify_fully */
static int toluaI_spell_identify_fully00(lua_State* tolua_S)
{
 if (!tolua_isnoobj(tolua_S,1))
 {
  TOLUA_ERR_FN(identify_fully);
 } else {
  bool toluaI_ret = (bool)  identify_fully();
  tolua_pushbool(tolua_S,(int)toluaI_ret);
 }
 return 1;
}

/* function: recharge */
static int toluaI_spell_recharge00(lua_State* tolua_S)
{
 if (!tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
     !tolua_isnoobj(tolua_S,2))
 {
  TOLUA_ERR_FN(recharge);
 } else {
  int num = ((int)  tolua_getnumber(tolua_S,1,0));
  bool toluaI_ret = (bool)  recharge(num);
  tolua_pushbool(tolua_S,(int)toluaI_ret);
 }
 return 1;
}

/* function: bless_weapon */
static int toluaI_spell_bless_weapon00(lua_State* tolua_S)
{
 if (!tolua_isnoobj(tolua_S,1))
 {
  TOLUA_ERR_FN(bless_weapon);
 } else {
  bool toluaI_ret = (bool)  bless_weapon();
  tolua_pushbool(tolua_S,(int)toluaI_ret);
 }
 return 1;
}

/* function: acid_dam */
static int toluaI_spell_acid_dam00(lua_State* tolua_S)
{
 if (!tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
     !tolua_istype(tolua_S,2,LUA_TSTRING,0) ||
     !tolua_isnoobj(tolua_S,3))
 {
  TOLUA_ERR_FN(acid_dam);
 } else {
  int dam = ((int)  tolua_getnumber(tolua_S,1,0));
  cptr kb_str = ((cptr)  tolua_getstring(tolua_S,2,0));
  bool toluaI_ret = (bool)  acid_dam(dam,kb_str);
  tolua_pushbool(tolua_S,(int)toluaI_ret);
 }
 return 1;
}

/* function: elec_dam */
static int toluaI_spell_elec_dam00(lua_State* tolua_S)
{
 if (!tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
     !tolua_istype(tolua_S,2,LUA_TSTRING,0) ||
     !tolua_isnoobj(tolua_S,3))
 {
  TOLUA_ERR_FN(elec_dam);
 } else {
  int dam = ((int)  tolua_getnumber(tolua_S,1,0));
  cptr kb_str = ((cptr)  tolua_getstring(tolua_S,2,0));
  bool toluaI_ret = (bool)  elec_dam(dam,kb_str);
  tolua_pushbool(tolua_S,(int)toluaI_ret);
 }
 return 1;
}

/* function: fire_dam */
static int toluaI_spell_fire_dam00(lua_State* tolua_S)
{
 if (!tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
     !tolua_istype(tolua_S,2,LUA_TSTRING,0) ||
     !tolua_isnoobj(tolua_S,3))
 {
  TOLUA_ERR_FN(fire_dam);
 } else {
  int dam = ((int)  tolua_getnumber(tolua_S,1,0));
  cptr kb_str = ((cptr)  tolua_getstring(tolua_S,2,0));
  bool toluaI_ret = (bool)  fire_dam(dam,kb_str);
  tolua_pushbool(tolua_S,(int)toluaI_ret);
 }
 return 1;
}

/* function: cold_dam */
static int toluaI_spell_cold_dam00(lua_State* tolua_S)
{
 if (!tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
     !tolua_istype(tolua_S,2,LUA_TSTRING,0) ||
     !tolua_isnoobj(tolua_S,3))
 {
  TOLUA_ERR_FN(cold_dam);
 } else {
  int dam = ((int)  tolua_getnumber(tolua_S,1,0));
  cptr kb_str = ((cptr)  tolua_getstring(tolua_S,2,0));
  bool toluaI_ret = (bool)  cold_dam(dam,kb_str);
  tolua_pushbool(tolua_S,(int)toluaI_ret);
 }
 return 1;
}

/* function: pois_dam */
static int toluaI_spell_pois_dam00(lua_State* tolua_S)
{
 if (!tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
     !tolua_istype(tolua_S,2,LUA_TSTRING,0) ||
     !tolua_istype(tolua_S,3,LUA_TNUMBER,0) ||
     !tolua_isnoobj(tolua_S,4))
 {
  TOLUA_ERR_FN(pois_dam);
 } else {
  int dam = ((int)  tolua_getnumber(tolua_S,1,0));
  cptr kb_str = ((cptr)  tolua_getstring(tolua_S,2,0));
  int pois = ((int)  tolua_getnumber(tolua_S,3,0));
  bool toluaI_ret = (bool)  pois_dam(dam,kb_str,pois);
  tolua_pushbool(tolua_S,(int)toluaI_ret);
 }
 return 1;
}

/* function: rustproof */
static int toluaI_spell_rustproof00(lua_State* tolua_S)
{
 if (!tolua_isnoobj(tolua_S,1))
 {
  TOLUA_ERR_FN(rustproof);
 } else {
  bool toluaI_ret = (bool)  rustproof();
  tolua_pushbool(tolua_S,(int)toluaI_ret);
 }
 return 1;
}

/* function: curse_armor */
static int toluaI_spell_curse_armor00(lua_State* tolua_S)
{
 if (!tolua_isnoobj(tolua_S,1))
 {
  TOLUA_ERR_FN(curse_armor);
 } else {
  bool toluaI_ret = (bool)  curse_armor();
  tolua_pushbool(tolua_S,(int)toluaI_ret);
 }
 return 1;
}

/* function: curse_weapon */
static int toluaI_spell_curse_weapon00(lua_State* tolua_S)
{
 if (!tolua_isnoobj(tolua_S,1))
 {
  TOLUA_ERR_FN(curse_weapon);
 } else {
  bool toluaI_ret = (bool)  curse_weapon();
  tolua_pushbool(tolua_S,(int)toluaI_ret);
 }
 return 1;
}

/* function: brand_bolts */
static int toluaI_spell_brand_bolts00(lua_State* tolua_S)
{
 if (!tolua_isnoobj(tolua_S,1))
 {
  TOLUA_ERR_FN(brand_bolts);
 } else {
  bool toluaI_ret = (bool)  brand_bolts();
  tolua_pushbool(tolua_S,(int)toluaI_ret);
 }
 return 1;
}

/* function: polymorph_monster */
static int toluaI_spell_polymorph_monster00(lua_State* tolua_S)
{
 if (!tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
     !tolua_istype(tolua_S,2,LUA_TNUMBER,0) ||
     !tolua_isnoobj(tolua_S,3))
 {
  TOLUA_ERR_FN(polymorph_monster);
 } else {
  int x = ((int)  tolua_getnumber(tolua_S,1,0));
  int y = ((int)  tolua_getnumber(tolua_S,2,0));
  bool toluaI_ret = (bool)  polymorph_monster(x,y);
  tolua_pushbool(tolua_S,(int)toluaI_ret);
 }
 return 1;
}

/* function: dimension_door */
static int toluaI_spell_dimension_door00(lua_State* tolua_S)
{
 if (!tolua_isnoobj(tolua_S,1))
 {
  TOLUA_ERR_FN(dimension_door);
 } else {
  bool toluaI_ret = (bool)  dimension_door();
  tolua_pushbool(tolua_S,(int)toluaI_ret);
 }
 return 1;
}

/* function: map_wilderness */
static int toluaI_spell_map_wilderness00(lua_State* tolua_S)
{
 if (!tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
     !tolua_istype(tolua_S,2,LUA_TNUMBER,0) ||
     !tolua_istype(tolua_S,3,LUA_TNUMBER,0) ||
     !tolua_isnoobj(tolua_S,4))
 {
  TOLUA_ERR_FN(map_wilderness);
 } else {
  int radius = ((int)  tolua_getnumber(tolua_S,1,0));
  s32b x = ((s32b)  tolua_getnumber(tolua_S,2,0));
  s32b y = ((s32b)  tolua_getnumber(tolua_S,3,0));
  map_wilderness(radius,x,y);
 }
 return 0;
}

/* function: map_area */
static int toluaI_spell_map_area00(lua_State* tolua_S)
{
 if (!tolua_isnoobj(tolua_S,1))
 {
  TOLUA_ERR_FN(map_area);
 } else {
  map_area();
 }
 return 0;
}

/* function: wiz_lite */
static int toluaI_spell_wiz_lite00(lua_State* tolua_S)
{
 if (!tolua_isnoobj(tolua_S,1))
 {
  TOLUA_ERR_FN(wiz_lite);
 } else {
  wiz_lite();
 }
 return 0;
}

/* function: wiz_dark */
static int toluaI_spell_wiz_dark00(lua_State* tolua_S)
{
 if (!tolua_isnoobj(tolua_S,1))
 {
  TOLUA_ERR_FN(wiz_dark);
 } else {
  wiz_dark();
 }
 return 0;
}

/* function: do_cmd_rerate */
static int toluaI_spell_do_cmd_rerate00(lua_State* tolua_S)
{
 if (!tolua_isnoobj(tolua_S,1))
 {
  TOLUA_ERR_FN(do_cmd_rerate);
 } else {
  do_cmd_rerate();
 }
 return 0;
}

/* Open function */
int tolua_spell_open (lua_State* tolua_S)
{
 tolua_open(tolua_S);
 toluaI_reg_types(tolua_S);
 TOLUA_DEF(GF_ELEC);
 TOLUA_DEF(GF_POIS);
 TOLUA_DEF(GF_ACID);
 TOLUA_DEF(GF_COLD);
 TOLUA_DEF(GF_FIRE);
 TOLUA_DEF(GF_MISSILE);
 TOLUA_DEF(GF_ARROW);
 TOLUA_DEF(GF_PLASMA);
 TOLUA_DEF(GF_WATER);
 TOLUA_DEF(GF_LITE);
 TOLUA_DEF(GF_DARK);
 TOLUA_DEF(GF_LITE_WEAK);
 TOLUA_DEF(GF_DARK_WEAK);
 TOLUA_DEF(GF_SHARDS);
 TOLUA_DEF(GF_SOUND);
 TOLUA_DEF(GF_CONFUSION);
 TOLUA_DEF(GF_FORCE);
 TOLUA_DEF(GF_INERTIA);
 TOLUA_DEF(GF_MANA);
 TOLUA_DEF(GF_METEOR);
 TOLUA_DEF(GF_ICE);
 TOLUA_DEF(GF_CHAOS);
 TOLUA_DEF(GF_NETHER);
 TOLUA_DEF(GF_DISENCHANT);
 TOLUA_DEF(GF_NEXUS);
 TOLUA_DEF(GF_TIME);
 TOLUA_DEF(GF_GRAVITY);
 TOLUA_DEF(GF_KILL_WALL);
 TOLUA_DEF(GF_KILL_DOOR);
 TOLUA_DEF(GF_KILL_TRAP);
 TOLUA_DEF(GF_MAKE_WALL);
 TOLUA_DEF(GF_MAKE_DOOR);
 TOLUA_DEF(GF_MAKE_TRAP);
 TOLUA_DEF(GF_OLD_CLONE);
 TOLUA_DEF(GF_OLD_POLY);
 TOLUA_DEF(GF_OLD_HEAL);
 TOLUA_DEF(GF_OLD_SPEED);
 TOLUA_DEF(GF_OLD_SLOW);
 TOLUA_DEF(GF_OLD_CONF);
 TOLUA_DEF(GF_OLD_SLEEP);
 TOLUA_DEF(GF_OLD_DRAIN);
 TOLUA_DEF(GF_NEW_DRAIN);
 TOLUA_DEF(GF_AWAY_UNDEAD);
 TOLUA_DEF(GF_AWAY_EVIL);
 TOLUA_DEF(GF_AWAY_ALL);
 TOLUA_DEF(GF_TURN_UNDEAD);
 TOLUA_DEF(GF_TURN_EVIL);
 TOLUA_DEF(GF_TURN_ALL);
 TOLUA_DEF(GF_DISP_UNDEAD);
 TOLUA_DEF(GF_DISP_EVIL);
 TOLUA_DEF(GF_DISP_ALL);
 TOLUA_DEF(GF_DISP_DEMON);
 TOLUA_DEF(GF_DISP_LIVING);
 TOLUA_DEF(GF_ROCKET);
 TOLUA_DEF(GF_NUKE);
 TOLUA_DEF(GF_MAKE_GLYPH);
 TOLUA_DEF(GF_STASIS);
 TOLUA_DEF(GF_STONE_WALL);
 TOLUA_DEF(GF_DEATH_RAY);
 TOLUA_DEF(GF_STUN);
 TOLUA_DEF(GF_HOLY_FIRE);
 TOLUA_DEF(GF_HELL_FIRE);
 TOLUA_DEF(GF_DISINTEGRATE);
 TOLUA_DEF(GF_CHARM);
 TOLUA_DEF(GF_CONTROL_UNDEAD);
 TOLUA_DEF(GF_CONTROL_ANIMAL);
 TOLUA_DEF(GF_PSI);
 TOLUA_DEF(GF_PSI_DRAIN);
 TOLUA_DEF(GF_TELEKINESIS);
 TOLUA_DEF(GF_JAM_DOOR);
 TOLUA_DEF(GF_DOMINATION);
 TOLUA_DEF(GF_DISP_GOOD);
 TOLUA_DEF(MAX_GF);
 TOLUA_DEF(PROJECT_JUMP);
 TOLUA_DEF(PROJECT_BEAM);
 TOLUA_DEF(PROJECT_THRU);
 TOLUA_DEF(PROJECT_STOP);
 TOLUA_DEF(PROJECT_GRID);
 TOLUA_DEF(PROJECT_ITEM);
 TOLUA_DEF(PROJECT_KILL);
 TOLUA_DEF(PROJECT_HIDE);
 TOLUA_DEF(PROJECT_FRND);
 TOLUA_DEF(PROJECT_MFLD);
 TOLUA_FUN(project,toluaI_spell_project00);
 TOLUA_FUN(self_knowledge,toluaI_spell_self_knowledge00);
 TOLUA_FUN(detect_traps,toluaI_spell_detect_traps00);
 TOLUA_FUN(detect_doors,toluaI_spell_detect_doors00);
 TOLUA_FUN(detect_stairs,toluaI_spell_detect_stairs00);
 TOLUA_FUN(detect_treasure,toluaI_spell_detect_treasure00);
 TOLUA_FUN(detect_objects_gold,toluaI_spell_detect_objects_gold00);
 TOLUA_FUN(detect_objects_normal,toluaI_spell_detect_objects_normal00);
 TOLUA_FUN(detect_objects_magic,toluaI_spell_detect_objects_magic00);
 TOLUA_FUN(detect_monsters_normal,toluaI_spell_detect_monsters_normal00);
 TOLUA_FUN(detect_monsters_invis,toluaI_spell_detect_monsters_invis00);
 TOLUA_FUN(detect_monsters_evil,toluaI_spell_detect_monsters_evil00);
 TOLUA_FUN(detect_monsters_xxx,toluaI_spell_detect_monsters_xxx00);
 TOLUA_FUN(detect_monsters_string,toluaI_spell_detect_monsters_string00);
 TOLUA_FUN(detect_monsters_nonliving,toluaI_spell_detect_monsters_nonliving00);
 TOLUA_FUN(detect_monsters_living,toluaI_spell_detect_monsters_living00);
 TOLUA_FUN(detect_all,toluaI_spell_detect_all00);
 TOLUA_FUN(wall_stone,toluaI_spell_wall_stone00);
 TOLUA_FUN(speed_monsters,toluaI_spell_speed_monsters00);
 TOLUA_FUN(slow_monsters,toluaI_spell_slow_monsters00);
 TOLUA_FUN(sleep_monsters,toluaI_spell_sleep_monsters00);
 TOLUA_FUN(aggravate_monsters,toluaI_spell_aggravate_monsters00);
 TOLUA_FUN(genocide,toluaI_spell_genocide00);
 TOLUA_FUN(mass_genocide,toluaI_spell_mass_genocide00);
 TOLUA_FUN(probing,toluaI_spell_probing00);
 TOLUA_FUN(banish_evil,toluaI_spell_banish_evil00);
 TOLUA_FUN(dispel_evil,toluaI_spell_dispel_evil00);
 TOLUA_FUN(dispel_good,toluaI_spell_dispel_good00);
 TOLUA_FUN(dispel_undead,toluaI_spell_dispel_undead00);
 TOLUA_FUN(dispel_monsters,toluaI_spell_dispel_monsters00);
 TOLUA_FUN(dispel_living,toluaI_spell_dispel_living00);
 TOLUA_FUN(dispel_demons,toluaI_spell_dispel_demons00);
 TOLUA_FUN(destroy_area,toluaI_spell_destroy_area00);
 TOLUA_FUN(earthquake,toluaI_spell_earthquake00);
 TOLUA_FUN(lite_room,toluaI_spell_lite_room00);
 TOLUA_FUN(unlite_room,toluaI_spell_unlite_room00);
 TOLUA_FUN(lite_area,toluaI_spell_lite_area00);
 TOLUA_FUN(unlite_area,toluaI_spell_unlite_area00);
 TOLUA_FUN(fire_ball,toluaI_spell_fire_ball00);
 TOLUA_FUN(fire_bolt,toluaI_spell_fire_bolt00);
 TOLUA_FUN(call_chaos,toluaI_spell_call_chaos00);
 TOLUA_FUN(fire_beam,toluaI_spell_fire_beam00);
 TOLUA_FUN(fire_bolt_or_beam,toluaI_spell_fire_bolt_or_beam00);
 TOLUA_FUN(lite_line,toluaI_spell_lite_line00);
 TOLUA_FUN(drain_life,toluaI_spell_drain_life00);
 TOLUA_FUN(drain_gain_life,toluaI_spell_drain_gain_life00);
 TOLUA_FUN(death_ray,toluaI_spell_death_ray00);
 TOLUA_FUN(wall_to_mud,toluaI_spell_wall_to_mud00);
 TOLUA_FUN(destroy_door,toluaI_spell_destroy_door00);
 TOLUA_FUN(disarm_trap,toluaI_spell_disarm_trap00);
 TOLUA_FUN(wizard_lock,toluaI_spell_wizard_lock00);
 TOLUA_FUN(heal_monster,toluaI_spell_heal_monster00);
 TOLUA_FUN(speed_monster,toluaI_spell_speed_monster00);
 TOLUA_FUN(slow_monster,toluaI_spell_slow_monster00);
 TOLUA_FUN(sleep_monster,toluaI_spell_sleep_monster00);
 TOLUA_FUN(stasis_monster,toluaI_spell_stasis_monster00);
 TOLUA_FUN(confuse_monster,toluaI_spell_confuse_monster00);
 TOLUA_FUN(stun_monster,toluaI_spell_stun_monster00);
 TOLUA_FUN(fear_monster,toluaI_spell_fear_monster00);
 TOLUA_FUN(poly_monster,toluaI_spell_poly_monster00);
 TOLUA_FUN(clone_monster,toluaI_spell_clone_monster00);
 TOLUA_FUN(teleport_monster,toluaI_spell_teleport_monster00);
 TOLUA_FUN(door_creation,toluaI_spell_door_creation00);
 TOLUA_FUN(trap_creation,toluaI_spell_trap_creation00);
 TOLUA_FUN(glyph_creation,toluaI_spell_glyph_creation00);
 TOLUA_FUN(destroy_doors_touch,toluaI_spell_destroy_doors_touch00);
 TOLUA_FUN(sleep_monsters_touch,toluaI_spell_sleep_monsters_touch00);
 TOLUA_FUN(confuse_monsters,toluaI_spell_confuse_monsters00);
 TOLUA_FUN(charm_monsters,toluaI_spell_charm_monsters00);
 TOLUA_FUN(charm_animals,toluaI_spell_charm_animals00);
 TOLUA_FUN(starlite,toluaI_spell_starlite00);
 TOLUA_FUN(scatter_ball,toluaI_spell_scatter_ball00);
 TOLUA_FUN(create_food,toluaI_spell_create_food00);
 TOLUA_FUN(whirlwind_attack,toluaI_spell_whirlwind_attack00);
 TOLUA_FUN(stun_monsters,toluaI_spell_stun_monsters00);
 TOLUA_FUN(stasis_monsters,toluaI_spell_stasis_monsters00);
 TOLUA_FUN(banish_monsters,toluaI_spell_banish_monsters00);
 TOLUA_FUN(turn_monsters,toluaI_spell_turn_monsters00);
 TOLUA_FUN(turn_evil,toluaI_spell_turn_evil00);
 TOLUA_FUN(deathray_monsters,toluaI_spell_deathray_monsters00);
 TOLUA_FUN(charm_monster,toluaI_spell_charm_monster00);
 TOLUA_FUN(control_one_undead,toluaI_spell_control_one_undead00);
 TOLUA_FUN(charm_animal,toluaI_spell_charm_animal00);
 TOLUA_FUN(mindblast_monsters,toluaI_spell_mindblast_monsters00);
 TOLUA_FUN(teleport_swap,toluaI_spell_teleport_swap00);
 TOLUA_FUN(teleport_away,toluaI_spell_teleport_away00);
 TOLUA_FUN(teleport_to_player,toluaI_spell_teleport_to_player00);
 TOLUA_FUN(teleport_player,toluaI_spell_teleport_player00);
 TOLUA_FUN(teleport_player_to,toluaI_spell_teleport_player_to00);
 TOLUA_FUN(teleport_player_level,toluaI_spell_teleport_player_level00);
 TOLUA_FUN(recall_player,toluaI_spell_recall_player00);
 TOLUA_FUN(word_of_recall,toluaI_spell_word_of_recall00);
 TOLUA_FUN(apply_disenchant,toluaI_spell_apply_disenchant00);
 TOLUA_FUN(mutate_player,toluaI_spell_mutate_player00);
 TOLUA_FUN(phlogiston,toluaI_spell_phlogiston00);
 TOLUA_FUN(brand_weapon,toluaI_spell_brand_weapon00);
 TOLUA_FUN(call_the_,toluaI_spell_call_the_00);
 TOLUA_FUN(fetch,toluaI_spell_fetch00);
 TOLUA_FUN(alter_reality,toluaI_spell_alter_reality00);
 TOLUA_FUN(warding_glyph,toluaI_spell_warding_glyph00);
 TOLUA_FUN(explosive_rune,toluaI_spell_explosive_rune00);
 TOLUA_FUN(identify_pack,toluaI_spell_identify_pack00);
 TOLUA_FUN(remove_curse,toluaI_spell_remove_curse00);
 TOLUA_FUN(remove_all_curse,toluaI_spell_remove_all_curse00);
 TOLUA_FUN(alchemy,toluaI_spell_alchemy00);
 TOLUA_FUN(stair_creation,toluaI_spell_stair_creation00);
 TOLUA_FUN(enchant,toluaI_spell_enchant00);
 TOLUA_FUN(enchant_spell,toluaI_spell_enchant_spell00);
 TOLUA_FUN(artifact_scroll,toluaI_spell_artifact_scroll00);
 TOLUA_FUN(ident_spell,toluaI_spell_ident_spell00);
 TOLUA_FUN(mundane_spell,toluaI_spell_mundane_spell00);
 TOLUA_FUN(identify_item,toluaI_spell_identify_item00);
 TOLUA_FUN(identify_fully,toluaI_spell_identify_fully00);
 TOLUA_FUN(recharge,toluaI_spell_recharge00);
 TOLUA_FUN(bless_weapon,toluaI_spell_bless_weapon00);
 TOLUA_FUN(acid_dam,toluaI_spell_acid_dam00);
 TOLUA_FUN(elec_dam,toluaI_spell_elec_dam00);
 TOLUA_FUN(fire_dam,toluaI_spell_fire_dam00);
 TOLUA_FUN(cold_dam,toluaI_spell_cold_dam00);
 TOLUA_FUN(pois_dam,toluaI_spell_pois_dam00);
 TOLUA_FUN(rustproof,toluaI_spell_rustproof00);
 TOLUA_FUN(curse_armor,toluaI_spell_curse_armor00);
 TOLUA_FUN(curse_weapon,toluaI_spell_curse_weapon00);
 TOLUA_FUN(brand_bolts,toluaI_spell_brand_bolts00);
 TOLUA_FUN(polymorph_monster,toluaI_spell_polymorph_monster00);
 TOLUA_FUN(dimension_door,toluaI_spell_dimension_door00);
 TOLUA_FUN(map_wilderness,toluaI_spell_map_wilderness00);
 TOLUA_FUN(map_area,toluaI_spell_map_area00);
 TOLUA_FUN(wiz_lite,toluaI_spell_wiz_lite00);
 TOLUA_FUN(wiz_dark,toluaI_spell_wiz_dark00);
 TOLUA_FUN(do_cmd_rerate,toluaI_spell_do_cmd_rerate00);
 return 1;
}
/* Close function */
void tolua_spell_close (lua_State* tolua_S)
{
 TOLUA_UNDEF(GF_ELEC);
 TOLUA_UNDEF(GF_POIS);
 TOLUA_UNDEF(GF_ACID);
 TOLUA_UNDEF(GF_COLD);
 TOLUA_UNDEF(GF_FIRE);
 TOLUA_UNDEF(GF_MISSILE);
 TOLUA_UNDEF(GF_ARROW);
 TOLUA_UNDEF(GF_PLASMA);
 TOLUA_UNDEF(GF_WATER);
 TOLUA_UNDEF(GF_LITE);
 TOLUA_UNDEF(GF_DARK);
 TOLUA_UNDEF(GF_LITE_WEAK);
 TOLUA_UNDEF(GF_DARK_WEAK);
 TOLUA_UNDEF(GF_SHARDS);
 TOLUA_UNDEF(GF_SOUND);
 TOLUA_UNDEF(GF_CONFUSION);
 TOLUA_UNDEF(GF_FORCE);
 TOLUA_UNDEF(GF_INERTIA);
 TOLUA_UNDEF(GF_MANA);
 TOLUA_UNDEF(GF_METEOR);
 TOLUA_UNDEF(GF_ICE);
 TOLUA_UNDEF(GF_CHAOS);
 TOLUA_UNDEF(GF_NETHER);
 TOLUA_UNDEF(GF_DISENCHANT);
 TOLUA_UNDEF(GF_NEXUS);
 TOLUA_UNDEF(GF_TIME);
 TOLUA_UNDEF(GF_GRAVITY);
 TOLUA_UNDEF(GF_KILL_WALL);
 TOLUA_UNDEF(GF_KILL_DOOR);
 TOLUA_UNDEF(GF_KILL_TRAP);
 TOLUA_UNDEF(GF_MAKE_WALL);
 TOLUA_UNDEF(GF_MAKE_DOOR);
 TOLUA_UNDEF(GF_MAKE_TRAP);
 TOLUA_UNDEF(GF_OLD_CLONE);
 TOLUA_UNDEF(GF_OLD_POLY);
 TOLUA_UNDEF(GF_OLD_HEAL);
 TOLUA_UNDEF(GF_OLD_SPEED);
 TOLUA_UNDEF(GF_OLD_SLOW);
 TOLUA_UNDEF(GF_OLD_CONF);
 TOLUA_UNDEF(GF_OLD_SLEEP);
 TOLUA_UNDEF(GF_OLD_DRAIN);
 TOLUA_UNDEF(GF_NEW_DRAIN);
 TOLUA_UNDEF(GF_AWAY_UNDEAD);
 TOLUA_UNDEF(GF_AWAY_EVIL);
 TOLUA_UNDEF(GF_AWAY_ALL);
 TOLUA_UNDEF(GF_TURN_UNDEAD);
 TOLUA_UNDEF(GF_TURN_EVIL);
 TOLUA_UNDEF(GF_TURN_ALL);
 TOLUA_UNDEF(GF_DISP_UNDEAD);
 TOLUA_UNDEF(GF_DISP_EVIL);
 TOLUA_UNDEF(GF_DISP_ALL);
 TOLUA_UNDEF(GF_DISP_DEMON);
 TOLUA_UNDEF(GF_DISP_LIVING);
 TOLUA_UNDEF(GF_ROCKET);
 TOLUA_UNDEF(GF_NUKE);
 TOLUA_UNDEF(GF_MAKE_GLYPH);
 TOLUA_UNDEF(GF_STASIS);
 TOLUA_UNDEF(GF_STONE_WALL);
 TOLUA_UNDEF(GF_DEATH_RAY);
 TOLUA_UNDEF(GF_STUN);
 TOLUA_UNDEF(GF_HOLY_FIRE);
 TOLUA_UNDEF(GF_HELL_FIRE);
 TOLUA_UNDEF(GF_DISINTEGRATE);
 TOLUA_UNDEF(GF_CHARM);
 TOLUA_UNDEF(GF_CONTROL_UNDEAD);
 TOLUA_UNDEF(GF_CONTROL_ANIMAL);
 TOLUA_UNDEF(GF_PSI);
 TOLUA_UNDEF(GF_PSI_DRAIN);
 TOLUA_UNDEF(GF_TELEKINESIS);
 TOLUA_UNDEF(GF_JAM_DOOR);
 TOLUA_UNDEF(GF_DOMINATION);
 TOLUA_UNDEF(GF_DISP_GOOD);
 TOLUA_UNDEF(MAX_GF);
 TOLUA_UNDEF(PROJECT_JUMP);
 TOLUA_UNDEF(PROJECT_BEAM);
 TOLUA_UNDEF(PROJECT_THRU);
 TOLUA_UNDEF(PROJECT_STOP);
 TOLUA_UNDEF(PROJECT_GRID);
 TOLUA_UNDEF(PROJECT_ITEM);
 TOLUA_UNDEF(PROJECT_KILL);
 TOLUA_UNDEF(PROJECT_HIDE);
 TOLUA_UNDEF(PROJECT_FRND);
 TOLUA_UNDEF(PROJECT_MFLD);
 TOLUA_UNDEF(project);
 TOLUA_UNDEF(self_knowledge);
 TOLUA_UNDEF(detect_traps);
 TOLUA_UNDEF(detect_doors);
 TOLUA_UNDEF(detect_stairs);
 TOLUA_UNDEF(detect_treasure);
 TOLUA_UNDEF(detect_objects_gold);
 TOLUA_UNDEF(detect_objects_normal);
 TOLUA_UNDEF(detect_objects_magic);
 TOLUA_UNDEF(detect_monsters_normal);
 TOLUA_UNDEF(detect_monsters_invis);
 TOLUA_UNDEF(detect_monsters_evil);
 TOLUA_UNDEF(detect_monsters_xxx);
 TOLUA_UNDEF(detect_monsters_string);
 TOLUA_UNDEF(detect_monsters_nonliving);
 TOLUA_UNDEF(detect_monsters_living);
 TOLUA_UNDEF(detect_all);
 TOLUA_UNDEF(wall_stone);
 TOLUA_UNDEF(speed_monsters);
 TOLUA_UNDEF(slow_monsters);
 TOLUA_UNDEF(sleep_monsters);
 TOLUA_UNDEF(aggravate_monsters);
 TOLUA_UNDEF(genocide);
 TOLUA_UNDEF(mass_genocide);
 TOLUA_UNDEF(probing);
 TOLUA_UNDEF(banish_evil);
 TOLUA_UNDEF(dispel_evil);
 TOLUA_UNDEF(dispel_good);
 TOLUA_UNDEF(dispel_undead);
 TOLUA_UNDEF(dispel_monsters);
 TOLUA_UNDEF(dispel_living);
 TOLUA_UNDEF(dispel_demons);
 TOLUA_UNDEF(destroy_area);
 TOLUA_UNDEF(earthquake);
 TOLUA_UNDEF(lite_room);
 TOLUA_UNDEF(unlite_room);
 TOLUA_UNDEF(lite_area);
 TOLUA_UNDEF(unlite_area);
 TOLUA_UNDEF(fire_ball);
 TOLUA_UNDEF(fire_bolt);
 TOLUA_UNDEF(call_chaos);
 TOLUA_UNDEF(fire_beam);
 TOLUA_UNDEF(fire_bolt_or_beam);
 TOLUA_UNDEF(lite_line);
 TOLUA_UNDEF(drain_life);
 TOLUA_UNDEF(drain_gain_life);
 TOLUA_UNDEF(death_ray);
 TOLUA_UNDEF(wall_to_mud);
 TOLUA_UNDEF(destroy_door);
 TOLUA_UNDEF(disarm_trap);
 TOLUA_UNDEF(wizard_lock);
 TOLUA_UNDEF(heal_monster);
 TOLUA_UNDEF(speed_monster);
 TOLUA_UNDEF(slow_monster);
 TOLUA_UNDEF(sleep_monster);
 TOLUA_UNDEF(stasis_monster);
 TOLUA_UNDEF(confuse_monster);
 TOLUA_UNDEF(stun_monster);
 TOLUA_UNDEF(fear_monster);
 TOLUA_UNDEF(poly_monster);
 TOLUA_UNDEF(clone_monster);
 TOLUA_UNDEF(teleport_monster);
 TOLUA_UNDEF(door_creation);
 TOLUA_UNDEF(trap_creation);
 TOLUA_UNDEF(glyph_creation);
 TOLUA_UNDEF(destroy_doors_touch);
 TOLUA_UNDEF(sleep_monsters_touch);
 TOLUA_UNDEF(confuse_monsters);
 TOLUA_UNDEF(charm_monsters);
 TOLUA_UNDEF(charm_animals);
 TOLUA_UNDEF(starlite);
 TOLUA_UNDEF(scatter_ball);
 TOLUA_UNDEF(create_food);
 TOLUA_UNDEF(whirlwind_attack);
 TOLUA_UNDEF(stun_monsters);
 TOLUA_UNDEF(stasis_monsters);
 TOLUA_UNDEF(banish_monsters);
 TOLUA_UNDEF(turn_monsters);
 TOLUA_UNDEF(turn_evil);
 TOLUA_UNDEF(deathray_monsters);
 TOLUA_UNDEF(charm_monster);
 TOLUA_UNDEF(control_one_undead);
 TOLUA_UNDEF(charm_animal);
 TOLUA_UNDEF(mindblast_monsters);
 TOLUA_UNDEF(teleport_swap);
 TOLUA_UNDEF(teleport_away);
 TOLUA_UNDEF(teleport_to_player);
 TOLUA_UNDEF(teleport_player);
 TOLUA_UNDEF(teleport_player_to);
 TOLUA_UNDEF(teleport_player_level);
 TOLUA_UNDEF(recall_player);
 TOLUA_UNDEF(word_of_recall);
 TOLUA_UNDEF(apply_disenchant);
 TOLUA_UNDEF(mutate_player);
 TOLUA_UNDEF(phlogiston);
 TOLUA_UNDEF(brand_weapon);
 TOLUA_UNDEF(call_the_);
 TOLUA_UNDEF(fetch);
 TOLUA_UNDEF(alter_reality);
 TOLUA_UNDEF(warding_glyph);
 TOLUA_UNDEF(explosive_rune);
 TOLUA_UNDEF(identify_pack);
 TOLUA_UNDEF(remove_curse);
 TOLUA_UNDEF(remove_all_curse);
 TOLUA_UNDEF(alchemy);
 TOLUA_UNDEF(stair_creation);
 TOLUA_UNDEF(enchant);
 TOLUA_UNDEF(enchant_spell);
 TOLUA_UNDEF(artifact_scroll);
 TOLUA_UNDEF(ident_spell);
 TOLUA_UNDEF(mundane_spell);
 TOLUA_UNDEF(identify_item);
 TOLUA_UNDEF(identify_fully);
 TOLUA_UNDEF(recharge);
 TOLUA_UNDEF(bless_weapon);
 TOLUA_UNDEF(acid_dam);
 TOLUA_UNDEF(elec_dam);
 TOLUA_UNDEF(fire_dam);
 TOLUA_UNDEF(cold_dam);
 TOLUA_UNDEF(pois_dam);
 TOLUA_UNDEF(rustproof);
 TOLUA_UNDEF(curse_armor);
 TOLUA_UNDEF(curse_weapon);
 TOLUA_UNDEF(brand_bolts);
 TOLUA_UNDEF(polymorph_monster);
 TOLUA_UNDEF(dimension_door);
 TOLUA_UNDEF(map_wilderness);
 TOLUA_UNDEF(map_area);
 TOLUA_UNDEF(wiz_lite);
 TOLUA_UNDEF(wiz_dark);
 TOLUA_UNDEF(do_cmd_rerate);
}
