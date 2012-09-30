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
 tolua_usertype(tolua_S,"inven_func");
 tolua_usertype(tolua_S,"monster_type");
 tolua_usertype(tolua_S,"object_type");
}

/* error messages */
#define TOLUA_ERR_SELF tolua_error(tolua_S,"invalid 'self'")
#define TOLUA_ERR_ASSIGN tolua_error(tolua_S,"#vinvalid type in variable assignment.")

/* function: take_hit */
static int toluaI_spell_take_hit00(lua_State* tolua_S)
{
 if (
     !tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
     !tolua_istype(tolua_S,2,LUA_TSTRING,0) ||
     !tolua_isnoobj(tolua_S,3)
 )
 {
  tolua_error(tolua_S,"#ferror in function 'take_hit'.");
  return 0;
 }
 else
 {
  int damage = ((int)  tolua_getnumber(tolua_S,1,0));
  cptr kb_str = ((cptr)  tolua_getstring(tolua_S,2,0));
  {
   take_hit(damage,kb_str);
  }
 }
 return 0;
}

/* function: project */
static int toluaI_spell_project00(lua_State* tolua_S)
{
 if (
     !tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
     !tolua_istype(tolua_S,2,LUA_TNUMBER,0) ||
     !tolua_istype(tolua_S,3,LUA_TNUMBER,0) ||
     !tolua_istype(tolua_S,4,LUA_TNUMBER,0) ||
     !tolua_istype(tolua_S,5,LUA_TNUMBER,0) ||
     !tolua_istype(tolua_S,6,LUA_TNUMBER,0) ||
     !tolua_istype(tolua_S,7,LUA_TNUMBER,0) ||
     !tolua_isnoobj(tolua_S,8)
 )
 {
  tolua_error(tolua_S,"#ferror in function 'project'.");
  return 0;
 }
 else
 {
  int who = ((int)  tolua_getnumber(tolua_S,1,0));
  int rad = ((int)  tolua_getnumber(tolua_S,2,0));
  int x = ((int)  tolua_getnumber(tolua_S,3,0));
  int y = ((int)  tolua_getnumber(tolua_S,4,0));
  int dam = ((int)  tolua_getnumber(tolua_S,5,0));
  int typ = ((int)  tolua_getnumber(tolua_S,6,0));
  u16b flg = ((u16b)  tolua_getnumber(tolua_S,7,0));
  {
   bool toluaI_ret = (bool)  project(who,rad,x,y,dam,typ,flg);
   tolua_pushbool(tolua_S,(int)toluaI_ret);
  }
 }
 return 1;
}

/* function: message_pain */
static int toluaI_spell_message_pain00(lua_State* tolua_S)
{
 if (
     !tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
     !tolua_istype(tolua_S,2,LUA_TNUMBER,0) ||
     !tolua_isnoobj(tolua_S,3)
 )
 {
  tolua_error(tolua_S,"#ferror in function 'message_pain'.");
  return 0;
 }
 else
 {
  int m_idx = ((int)  tolua_getnumber(tolua_S,1,0));
  int dam = ((int)  tolua_getnumber(tolua_S,2,0));
  {
   message_pain(m_idx,dam);
  }
 }
 return 0;
}

/* function: self_knowledge */
static int toluaI_spell_self_knowledge00(lua_State* tolua_S)
{
 if (
     !tolua_isnoobj(tolua_S,1)
 )
 {
  tolua_error(tolua_S,"#ferror in function 'self_knowledge'.");
  return 0;
 }
 else
 {
  {
   self_knowledge();
  }
 }
 return 0;
}

/* function: detect_traps */
static int toluaI_spell_detect_traps00(lua_State* tolua_S)
{
 if (
     !tolua_isnoobj(tolua_S,1)
 )
 {
  tolua_error(tolua_S,"#ferror in function 'detect_traps'.");
  return 0;
 }
 else
 {
  {
   bool toluaI_ret = (bool)  detect_traps();
   tolua_pushbool(tolua_S,(int)toluaI_ret);
  }
 }
 return 1;
}

/* function: detect_doors */
static int toluaI_spell_detect_doors00(lua_State* tolua_S)
{
 if (
     !tolua_isnoobj(tolua_S,1)
 )
 {
  tolua_error(tolua_S,"#ferror in function 'detect_doors'.");
  return 0;
 }
 else
 {
  {
   bool toluaI_ret = (bool)  detect_doors();
   tolua_pushbool(tolua_S,(int)toluaI_ret);
  }
 }
 return 1;
}

/* function: detect_stairs */
static int toluaI_spell_detect_stairs00(lua_State* tolua_S)
{
 if (
     !tolua_isnoobj(tolua_S,1)
 )
 {
  tolua_error(tolua_S,"#ferror in function 'detect_stairs'.");
  return 0;
 }
 else
 {
  {
   bool toluaI_ret = (bool)  detect_stairs();
   tolua_pushbool(tolua_S,(int)toluaI_ret);
  }
 }
 return 1;
}

/* function: detect_treasure */
static int toluaI_spell_detect_treasure00(lua_State* tolua_S)
{
 if (
     !tolua_isnoobj(tolua_S,1)
 )
 {
  tolua_error(tolua_S,"#ferror in function 'detect_treasure'.");
  return 0;
 }
 else
 {
  {
   bool toluaI_ret = (bool)  detect_treasure();
   tolua_pushbool(tolua_S,(int)toluaI_ret);
  }
 }
 return 1;
}

/* function: detect_objects_gold */
static int toluaI_spell_detect_objects_gold00(lua_State* tolua_S)
{
 if (
     !tolua_isnoobj(tolua_S,1)
 )
 {
  tolua_error(tolua_S,"#ferror in function 'detect_objects_gold'.");
  return 0;
 }
 else
 {
  {
   bool toluaI_ret = (bool)  detect_objects_gold();
   tolua_pushbool(tolua_S,(int)toluaI_ret);
  }
 }
 return 1;
}

/* function: detect_objects_normal */
static int toluaI_spell_detect_objects_normal00(lua_State* tolua_S)
{
 if (
     !tolua_isnoobj(tolua_S,1)
 )
 {
  tolua_error(tolua_S,"#ferror in function 'detect_objects_normal'.");
  return 0;
 }
 else
 {
  {
   bool toluaI_ret = (bool)  detect_objects_normal();
   tolua_pushbool(tolua_S,(int)toluaI_ret);
  }
 }
 return 1;
}

/* function: detect_objects_magic */
static int toluaI_spell_detect_objects_magic00(lua_State* tolua_S)
{
 if (
     !tolua_isnoobj(tolua_S,1)
 )
 {
  tolua_error(tolua_S,"#ferror in function 'detect_objects_magic'.");
  return 0;
 }
 else
 {
  {
   bool toluaI_ret = (bool)  detect_objects_magic();
   tolua_pushbool(tolua_S,(int)toluaI_ret);
  }
 }
 return 1;
}

/* function: detect_monsters_normal */
static int toluaI_spell_detect_monsters_normal00(lua_State* tolua_S)
{
 if (
     !tolua_isnoobj(tolua_S,1)
 )
 {
  tolua_error(tolua_S,"#ferror in function 'detect_monsters_normal'.");
  return 0;
 }
 else
 {
  {
   bool toluaI_ret = (bool)  detect_monsters_normal();
   tolua_pushbool(tolua_S,(int)toluaI_ret);
  }
 }
 return 1;
}

/* function: detect_monsters_invis */
static int toluaI_spell_detect_monsters_invis00(lua_State* tolua_S)
{
 if (
     !tolua_isnoobj(tolua_S,1)
 )
 {
  tolua_error(tolua_S,"#ferror in function 'detect_monsters_invis'.");
  return 0;
 }
 else
 {
  {
   bool toluaI_ret = (bool)  detect_monsters_invis();
   tolua_pushbool(tolua_S,(int)toluaI_ret);
  }
 }
 return 1;
}

/* function: detect_monsters_evil */
static int toluaI_spell_detect_monsters_evil00(lua_State* tolua_S)
{
 if (
     !tolua_isnoobj(tolua_S,1)
 )
 {
  tolua_error(tolua_S,"#ferror in function 'detect_monsters_evil'.");
  return 0;
 }
 else
 {
  {
   bool toluaI_ret = (bool)  detect_monsters_evil();
   tolua_pushbool(tolua_S,(int)toluaI_ret);
  }
 }
 return 1;
}

/* function: detect_monsters_xxx */
static int toluaI_spell_detect_monsters_xxx00(lua_State* tolua_S)
{
 if (
     !tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
     !tolua_isnoobj(tolua_S,2)
 )
 {
  tolua_error(tolua_S,"#ferror in function 'detect_monsters_xxx'.");
  return 0;
 }
 else
 {
  u32b match_flag = ((u32b)  tolua_getnumber(tolua_S,1,0));
  {
   bool toluaI_ret = (bool)  detect_monsters_xxx(match_flag);
   tolua_pushbool(tolua_S,(int)toluaI_ret);
  }
 }
 return 1;
}

/* function: detect_monsters_string */
static int toluaI_spell_detect_monsters_string00(lua_State* tolua_S)
{
 if (
     !tolua_istype(tolua_S,1,LUA_TSTRING,0) ||
     !tolua_isnoobj(tolua_S,2)
 )
 {
  tolua_error(tolua_S,"#ferror in function 'detect_monsters_string'.");
  return 0;
 }
 else
 {
  cptr tolua_var_1 = ((cptr)  tolua_getstring(tolua_S,1,0));
  {
   bool toluaI_ret = (bool)  detect_monsters_string(tolua_var_1);
   tolua_pushbool(tolua_S,(int)toluaI_ret);
  }
 }
 return 1;
}

/* function: detect_monsters_nonliving */
static int toluaI_spell_detect_monsters_nonliving00(lua_State* tolua_S)
{
 if (
     !tolua_isnoobj(tolua_S,1)
 )
 {
  tolua_error(tolua_S,"#ferror in function 'detect_monsters_nonliving'.");
  return 0;
 }
 else
 {
  {
   bool toluaI_ret = (bool)  detect_monsters_nonliving();
   tolua_pushbool(tolua_S,(int)toluaI_ret);
  }
 }
 return 1;
}

/* function: detect_monsters_living */
static int toluaI_spell_detect_monsters_living00(lua_State* tolua_S)
{
 if (
     !tolua_isnoobj(tolua_S,1)
 )
 {
  tolua_error(tolua_S,"#ferror in function 'detect_monsters_living'.");
  return 0;
 }
 else
 {
  {
   bool toluaI_ret = (bool)  detect_monsters_living();
   tolua_pushbool(tolua_S,(int)toluaI_ret);
  }
 }
 return 1;
}

/* function: detect_all */
static int toluaI_spell_detect_all00(lua_State* tolua_S)
{
 if (
     !tolua_isnoobj(tolua_S,1)
 )
 {
  tolua_error(tolua_S,"#ferror in function 'detect_all'.");
  return 0;
 }
 else
 {
  {
   bool toluaI_ret = (bool)  detect_all();
   tolua_pushbool(tolua_S,(int)toluaI_ret);
  }
 }
 return 1;
}

/* function: wall_stone */
static int toluaI_spell_wall_stone00(lua_State* tolua_S)
{
 if (
     !tolua_isnoobj(tolua_S,1)
 )
 {
  tolua_error(tolua_S,"#ferror in function 'wall_stone'.");
  return 0;
 }
 else
 {
  {
   bool toluaI_ret = (bool)  wall_stone();
   tolua_pushbool(tolua_S,(int)toluaI_ret);
  }
 }
 return 1;
}

/* function: speed_monsters */
static int toluaI_spell_speed_monsters00(lua_State* tolua_S)
{
 if (
     !tolua_isnoobj(tolua_S,1)
 )
 {
  tolua_error(tolua_S,"#ferror in function 'speed_monsters'.");
  return 0;
 }
 else
 {
  {
   bool toluaI_ret = (bool)  speed_monsters();
   tolua_pushbool(tolua_S,(int)toluaI_ret);
  }
 }
 return 1;
}

/* function: slow_monsters */
static int toluaI_spell_slow_monsters00(lua_State* tolua_S)
{
 if (
     !tolua_isnoobj(tolua_S,1)
 )
 {
  tolua_error(tolua_S,"#ferror in function 'slow_monsters'.");
  return 0;
 }
 else
 {
  {
   bool toluaI_ret = (bool)  slow_monsters();
   tolua_pushbool(tolua_S,(int)toluaI_ret);
  }
 }
 return 1;
}

/* function: sleep_monsters */
static int toluaI_spell_sleep_monsters00(lua_State* tolua_S)
{
 if (
     !tolua_isnoobj(tolua_S,1)
 )
 {
  tolua_error(tolua_S,"#ferror in function 'sleep_monsters'.");
  return 0;
 }
 else
 {
  {
   bool toluaI_ret = (bool)  sleep_monsters();
   tolua_pushbool(tolua_S,(int)toluaI_ret);
  }
 }
 return 1;
}

/* function: aggravate_monsters */
static int toluaI_spell_aggravate_monsters00(lua_State* tolua_S)
{
 if (
     !tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
     !tolua_isnoobj(tolua_S,2)
 )
 {
  tolua_error(tolua_S,"#ferror in function 'aggravate_monsters'.");
  return 0;
 }
 else
 {
  int who = ((int)  tolua_getnumber(tolua_S,1,0));
  {
   aggravate_monsters(who);
  }
 }
 return 0;
}

/* function: genocide */
static int toluaI_spell_genocide00(lua_State* tolua_S)
{
 if (
     !tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
     !tolua_isnoobj(tolua_S,2)
 )
 {
  tolua_error(tolua_S,"#ferror in function 'genocide'.");
  return 0;
 }
 else
 {
  int player_cast = ((int)  tolua_getnumber(tolua_S,1,0));
  {
   bool toluaI_ret = (bool)  genocide(player_cast);
   tolua_pushbool(tolua_S,(int)toluaI_ret);
  }
 }
 return 1;
}

/* function: mass_genocide */
static int toluaI_spell_mass_genocide00(lua_State* tolua_S)
{
 if (
     !tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
     !tolua_isnoobj(tolua_S,2)
 )
 {
  tolua_error(tolua_S,"#ferror in function 'mass_genocide'.");
  return 0;
 }
 else
 {
  int player_cast = ((int)  tolua_getnumber(tolua_S,1,0));
  {
   bool toluaI_ret = (bool)  mass_genocide(player_cast);
   tolua_pushbool(tolua_S,(int)toluaI_ret);
  }
 }
 return 1;
}

/* function: probing */
static int toluaI_spell_probing00(lua_State* tolua_S)
{
 if (
     !tolua_isnoobj(tolua_S,1)
 )
 {
  tolua_error(tolua_S,"#ferror in function 'probing'.");
  return 0;
 }
 else
 {
  {
   bool toluaI_ret = (bool)  probing();
   tolua_pushbool(tolua_S,(int)toluaI_ret);
  }
 }
 return 1;
}

/* function: banish_evil */
static int toluaI_spell_banish_evil00(lua_State* tolua_S)
{
 if (
     !tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
     !tolua_isnoobj(tolua_S,2)
 )
 {
  tolua_error(tolua_S,"#ferror in function 'banish_evil'.");
  return 0;
 }
 else
 {
  int dist = ((int)  tolua_getnumber(tolua_S,1,0));
  {
   bool toluaI_ret = (bool)  banish_evil(dist);
   tolua_pushbool(tolua_S,(int)toluaI_ret);
  }
 }
 return 1;
}

/* function: dispel_evil */
static int toluaI_spell_dispel_evil00(lua_State* tolua_S)
{
 if (
     !tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
     !tolua_isnoobj(tolua_S,2)
 )
 {
  tolua_error(tolua_S,"#ferror in function 'dispel_evil'.");
  return 0;
 }
 else
 {
  int dam = ((int)  tolua_getnumber(tolua_S,1,0));
  {
   bool toluaI_ret = (bool)  dispel_evil(dam);
   tolua_pushbool(tolua_S,(int)toluaI_ret);
  }
 }
 return 1;
}

/* function: dispel_good */
static int toluaI_spell_dispel_good00(lua_State* tolua_S)
{
 if (
     !tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
     !tolua_isnoobj(tolua_S,2)
 )
 {
  tolua_error(tolua_S,"#ferror in function 'dispel_good'.");
  return 0;
 }
 else
 {
  int dam = ((int)  tolua_getnumber(tolua_S,1,0));
  {
   bool toluaI_ret = (bool)  dispel_good(dam);
   tolua_pushbool(tolua_S,(int)toluaI_ret);
  }
 }
 return 1;
}

/* function: dispel_undead */
static int toluaI_spell_dispel_undead00(lua_State* tolua_S)
{
 if (
     !tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
     !tolua_isnoobj(tolua_S,2)
 )
 {
  tolua_error(tolua_S,"#ferror in function 'dispel_undead'.");
  return 0;
 }
 else
 {
  int dam = ((int)  tolua_getnumber(tolua_S,1,0));
  {
   bool toluaI_ret = (bool)  dispel_undead(dam);
   tolua_pushbool(tolua_S,(int)toluaI_ret);
  }
 }
 return 1;
}

/* function: dispel_monsters */
static int toluaI_spell_dispel_monsters00(lua_State* tolua_S)
{
 if (
     !tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
     !tolua_isnoobj(tolua_S,2)
 )
 {
  tolua_error(tolua_S,"#ferror in function 'dispel_monsters'.");
  return 0;
 }
 else
 {
  int dam = ((int)  tolua_getnumber(tolua_S,1,0));
  {
   bool toluaI_ret = (bool)  dispel_monsters(dam);
   tolua_pushbool(tolua_S,(int)toluaI_ret);
  }
 }
 return 1;
}

/* function: dispel_living */
static int toluaI_spell_dispel_living00(lua_State* tolua_S)
{
 if (
     !tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
     !tolua_isnoobj(tolua_S,2)
 )
 {
  tolua_error(tolua_S,"#ferror in function 'dispel_living'.");
  return 0;
 }
 else
 {
  int dam = ((int)  tolua_getnumber(tolua_S,1,0));
  {
   bool toluaI_ret = (bool)  dispel_living(dam);
   tolua_pushbool(tolua_S,(int)toluaI_ret);
  }
 }
 return 1;
}

/* function: dispel_demons */
static int toluaI_spell_dispel_demons00(lua_State* tolua_S)
{
 if (
     !tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
     !tolua_isnoobj(tolua_S,2)
 )
 {
  tolua_error(tolua_S,"#ferror in function 'dispel_demons'.");
  return 0;
 }
 else
 {
  int dam = ((int)  tolua_getnumber(tolua_S,1,0));
  {
   bool toluaI_ret = (bool)  dispel_demons(dam);
   tolua_pushbool(tolua_S,(int)toluaI_ret);
  }
 }
 return 1;
}

/* function: raise_dead */
static int toluaI_spell_raise_dead00(lua_State* tolua_S)
{
 if (
     !tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
     !tolua_istype(tolua_S,2,LUA_TNUMBER,0) ||
     !tolua_istype(tolua_S,3,tolua_tag(tolua_S,"bool"),0) ||
     !tolua_isnoobj(tolua_S,4)
 )
 {
  tolua_error(tolua_S,"#ferror in function 'raise_dead'.");
  return 0;
 }
 else
 {
  int x = ((int)  tolua_getnumber(tolua_S,1,0));
  int y = ((int)  tolua_getnumber(tolua_S,2,0));
  bool pet = ((bool)  tolua_getbool(tolua_S,3,0));
  {
   bool toluaI_ret = (bool)  raise_dead(x,y,pet);
   tolua_pushbool(tolua_S,(int)toluaI_ret);
  }
 }
 return 1;
}

/* function: turn_undead */
static int toluaI_spell_turn_undead00(lua_State* tolua_S)
{
 if (
     !tolua_isnoobj(tolua_S,1)
 )
 {
  tolua_error(tolua_S,"#ferror in function 'turn_undead'.");
  return 0;
 }
 else
 {
  {
   bool toluaI_ret = (bool)  turn_undead();
   tolua_pushbool(tolua_S,(int)toluaI_ret);
  }
 }
 return 1;
}

/* function: destroy_area */
static int toluaI_spell_destroy_area00(lua_State* tolua_S)
{
 if (
     !tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
     !tolua_istype(tolua_S,2,LUA_TNUMBER,0) ||
     !tolua_istype(tolua_S,3,LUA_TNUMBER,0) ||
     !tolua_isnoobj(tolua_S,4)
 )
 {
  tolua_error(tolua_S,"#ferror in function 'destroy_area'.");
  return 0;
 }
 else
 {
  int x1 = ((int)  tolua_getnumber(tolua_S,1,0));
  int y1 = ((int)  tolua_getnumber(tolua_S,2,0));
  int r = ((int)  tolua_getnumber(tolua_S,3,0));
  {
   bool toluaI_ret = (bool)  destroy_area(x1,y1,r);
   tolua_pushbool(tolua_S,(int)toluaI_ret);
  }
 }
 return 1;
}

/* function: earthquake */
static int toluaI_spell_earthquake00(lua_State* tolua_S)
{
 if (
     !tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
     !tolua_istype(tolua_S,2,LUA_TNUMBER,0) ||
     !tolua_istype(tolua_S,3,LUA_TNUMBER,0) ||
     !tolua_isnoobj(tolua_S,4)
 )
 {
  tolua_error(tolua_S,"#ferror in function 'earthquake'.");
  return 0;
 }
 else
 {
  int cx = ((int)  tolua_getnumber(tolua_S,1,0));
  int cy = ((int)  tolua_getnumber(tolua_S,2,0));
  int r = ((int)  tolua_getnumber(tolua_S,3,0));
  {
   bool toluaI_ret = (bool)  earthquake(cx,cy,r);
   tolua_pushbool(tolua_S,(int)toluaI_ret);
  }
 }
 return 1;
}

/* function: lite_room */
static int toluaI_spell_lite_room00(lua_State* tolua_S)
{
 if (
     !tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
     !tolua_istype(tolua_S,2,LUA_TNUMBER,0) ||
     !tolua_isnoobj(tolua_S,3)
 )
 {
  tolua_error(tolua_S,"#ferror in function 'lite_room'.");
  return 0;
 }
 else
 {
  int x1 = ((int)  tolua_getnumber(tolua_S,1,0));
  int y1 = ((int)  tolua_getnumber(tolua_S,2,0));
  {
   lite_room(x1,y1);
  }
 }
 return 0;
}

/* function: unlite_room */
static int toluaI_spell_unlite_room00(lua_State* tolua_S)
{
 if (
     !tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
     !tolua_istype(tolua_S,2,LUA_TNUMBER,0) ||
     !tolua_isnoobj(tolua_S,3)
 )
 {
  tolua_error(tolua_S,"#ferror in function 'unlite_room'.");
  return 0;
 }
 else
 {
  int x1 = ((int)  tolua_getnumber(tolua_S,1,0));
  int y1 = ((int)  tolua_getnumber(tolua_S,2,0));
  {
   unlite_room(x1,y1);
  }
 }
 return 0;
}

/* function: lite_area */
static int toluaI_spell_lite_area00(lua_State* tolua_S)
{
 if (
     !tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
     !tolua_istype(tolua_S,2,LUA_TNUMBER,0) ||
     !tolua_isnoobj(tolua_S,3)
 )
 {
  tolua_error(tolua_S,"#ferror in function 'lite_area'.");
  return 0;
 }
 else
 {
  int dam = ((int)  tolua_getnumber(tolua_S,1,0));
  int rad = ((int)  tolua_getnumber(tolua_S,2,0));
  {
   bool toluaI_ret = (bool)  lite_area(dam,rad);
   tolua_pushbool(tolua_S,(int)toluaI_ret);
  }
 }
 return 1;
}

/* function: unlite_area */
static int toluaI_spell_unlite_area00(lua_State* tolua_S)
{
 if (
     !tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
     !tolua_istype(tolua_S,2,LUA_TNUMBER,0) ||
     !tolua_isnoobj(tolua_S,3)
 )
 {
  tolua_error(tolua_S,"#ferror in function 'unlite_area'.");
  return 0;
 }
 else
 {
  int dam = ((int)  tolua_getnumber(tolua_S,1,0));
  int rad = ((int)  tolua_getnumber(tolua_S,2,0));
  {
   bool toluaI_ret = (bool)  unlite_area(dam,rad);
   tolua_pushbool(tolua_S,(int)toluaI_ret);
  }
 }
 return 1;
}

/* function: fire_ball */
static int toluaI_spell_fire_ball00(lua_State* tolua_S)
{
 if (
     !tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
     !tolua_istype(tolua_S,2,LUA_TNUMBER,0) ||
     !tolua_istype(tolua_S,3,LUA_TNUMBER,0) ||
     !tolua_istype(tolua_S,4,LUA_TNUMBER,0) ||
     !tolua_isnoobj(tolua_S,5)
 )
 {
  tolua_error(tolua_S,"#ferror in function 'fire_ball'.");
  return 0;
 }
 else
 {
  int typ = ((int)  tolua_getnumber(tolua_S,1,0));
  int dir = ((int)  tolua_getnumber(tolua_S,2,0));
  int dam = ((int)  tolua_getnumber(tolua_S,3,0));
  int rad = ((int)  tolua_getnumber(tolua_S,4,0));
  {
   bool toluaI_ret = (bool)  fire_ball(typ,dir,dam,rad);
   tolua_pushbool(tolua_S,(int)toluaI_ret);
  }
 }
 return 1;
}

/* function: fire_bolt */
static int toluaI_spell_fire_bolt00(lua_State* tolua_S)
{
 if (
     !tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
     !tolua_istype(tolua_S,2,LUA_TNUMBER,0) ||
     !tolua_istype(tolua_S,3,LUA_TNUMBER,0) ||
     !tolua_isnoobj(tolua_S,4)
 )
 {
  tolua_error(tolua_S,"#ferror in function 'fire_bolt'.");
  return 0;
 }
 else
 {
  int typ = ((int)  tolua_getnumber(tolua_S,1,0));
  int dir = ((int)  tolua_getnumber(tolua_S,2,0));
  int dam = ((int)  tolua_getnumber(tolua_S,3,0));
  {
   bool toluaI_ret = (bool)  fire_bolt(typ,dir,dam);
   tolua_pushbool(tolua_S,(int)toluaI_ret);
  }
 }
 return 1;
}

/* function: call_chaos */
static int toluaI_spell_call_chaos00(lua_State* tolua_S)
{
 if (
     !tolua_isnoobj(tolua_S,1)
 )
 {
  tolua_error(tolua_S,"#ferror in function 'call_chaos'.");
  return 0;
 }
 else
 {
  {
   call_chaos();
  }
 }
 return 0;
}

/* function: fire_beam */
static int toluaI_spell_fire_beam00(lua_State* tolua_S)
{
 if (
     !tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
     !tolua_istype(tolua_S,2,LUA_TNUMBER,0) ||
     !tolua_istype(tolua_S,3,LUA_TNUMBER,0) ||
     !tolua_isnoobj(tolua_S,4)
 )
 {
  tolua_error(tolua_S,"#ferror in function 'fire_beam'.");
  return 0;
 }
 else
 {
  int typ = ((int)  tolua_getnumber(tolua_S,1,0));
  int dir = ((int)  tolua_getnumber(tolua_S,2,0));
  int dam = ((int)  tolua_getnumber(tolua_S,3,0));
  {
   bool toluaI_ret = (bool)  fire_beam(typ,dir,dam);
   tolua_pushbool(tolua_S,(int)toluaI_ret);
  }
 }
 return 1;
}

/* function: fire_bolt_or_beam */
static int toluaI_spell_fire_bolt_or_beam00(lua_State* tolua_S)
{
 if (
     !tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
     !tolua_istype(tolua_S,2,LUA_TNUMBER,0) ||
     !tolua_istype(tolua_S,3,LUA_TNUMBER,0) ||
     !tolua_istype(tolua_S,4,LUA_TNUMBER,0) ||
     !tolua_isnoobj(tolua_S,5)
 )
 {
  tolua_error(tolua_S,"#ferror in function 'fire_bolt_or_beam'.");
  return 0;
 }
 else
 {
  int prob = ((int)  tolua_getnumber(tolua_S,1,0));
  int typ = ((int)  tolua_getnumber(tolua_S,2,0));
  int dir = ((int)  tolua_getnumber(tolua_S,3,0));
  int dam = ((int)  tolua_getnumber(tolua_S,4,0));
  {
   bool toluaI_ret = (bool)  fire_bolt_or_beam(prob,typ,dir,dam);
   tolua_pushbool(tolua_S,(int)toluaI_ret);
  }
 }
 return 1;
}

/* function: lite_line */
static int toluaI_spell_lite_line00(lua_State* tolua_S)
{
 if (
     !tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
     !tolua_isnoobj(tolua_S,2)
 )
 {
  tolua_error(tolua_S,"#ferror in function 'lite_line'.");
  return 0;
 }
 else
 {
  int dir = ((int)  tolua_getnumber(tolua_S,1,0));
  {
   bool toluaI_ret = (bool)  lite_line(dir);
   tolua_pushbool(tolua_S,(int)toluaI_ret);
  }
 }
 return 1;
}

/* function: drain_life */
static int toluaI_spell_drain_life00(lua_State* tolua_S)
{
 if (
     !tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
     !tolua_istype(tolua_S,2,LUA_TNUMBER,0) ||
     !tolua_isnoobj(tolua_S,3)
 )
 {
  tolua_error(tolua_S,"#ferror in function 'drain_life'.");
  return 0;
 }
 else
 {
  int dir = ((int)  tolua_getnumber(tolua_S,1,0));
  int dam = ((int)  tolua_getnumber(tolua_S,2,0));
  {
   bool toluaI_ret = (bool)  drain_life(dir,dam);
   tolua_pushbool(tolua_S,(int)toluaI_ret);
  }
 }
 return 1;
}

/* function: drain_gain_life */
static int toluaI_spell_drain_gain_life00(lua_State* tolua_S)
{
 if (
     !tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
     !tolua_istype(tolua_S,2,LUA_TNUMBER,0) ||
     !tolua_isnoobj(tolua_S,3)
 )
 {
  tolua_error(tolua_S,"#ferror in function 'drain_gain_life'.");
  return 0;
 }
 else
 {
  int dor = ((int)  tolua_getnumber(tolua_S,1,0));
  int dam = ((int)  tolua_getnumber(tolua_S,2,0));
  {
   bool toluaI_ret = (bool)  drain_gain_life(dor,dam);
   tolua_pushbool(tolua_S,(int)toluaI_ret);
  }
 }
 return 1;
}

/* function: death_ray */
static int toluaI_spell_death_ray00(lua_State* tolua_S)
{
 if (
     !tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
     !tolua_istype(tolua_S,2,LUA_TNUMBER,0) ||
     !tolua_isnoobj(tolua_S,3)
 )
 {
  tolua_error(tolua_S,"#ferror in function 'death_ray'.");
  return 0;
 }
 else
 {
  int dir = ((int)  tolua_getnumber(tolua_S,1,0));
  int plev = ((int)  tolua_getnumber(tolua_S,2,0));
  {
   bool toluaI_ret = (bool)  death_ray(dir,plev);
   tolua_pushbool(tolua_S,(int)toluaI_ret);
  }
 }
 return 1;
}

/* function: wall_to_mud */
static int toluaI_spell_wall_to_mud00(lua_State* tolua_S)
{
 if (
     !tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
     !tolua_isnoobj(tolua_S,2)
 )
 {
  tolua_error(tolua_S,"#ferror in function 'wall_to_mud'.");
  return 0;
 }
 else
 {
  int dir = ((int)  tolua_getnumber(tolua_S,1,0));
  {
   bool toluaI_ret = (bool)  wall_to_mud(dir);
   tolua_pushbool(tolua_S,(int)toluaI_ret);
  }
 }
 return 1;
}

/* function: destroy_door */
static int toluaI_spell_destroy_door00(lua_State* tolua_S)
{
 if (
     !tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
     !tolua_isnoobj(tolua_S,2)
 )
 {
  tolua_error(tolua_S,"#ferror in function 'destroy_door'.");
  return 0;
 }
 else
 {
  int dir = ((int)  tolua_getnumber(tolua_S,1,0));
  {
   bool toluaI_ret = (bool)  destroy_door(dir);
   tolua_pushbool(tolua_S,(int)toluaI_ret);
  }
 }
 return 1;
}

/* function: disarm_trap */
static int toluaI_spell_disarm_trap00(lua_State* tolua_S)
{
 if (
     !tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
     !tolua_isnoobj(tolua_S,2)
 )
 {
  tolua_error(tolua_S,"#ferror in function 'disarm_trap'.");
  return 0;
 }
 else
 {
  int dir = ((int)  tolua_getnumber(tolua_S,1,0));
  {
   bool toluaI_ret = (bool)  disarm_trap(dir);
   tolua_pushbool(tolua_S,(int)toluaI_ret);
  }
 }
 return 1;
}

/* function: wizard_lock */
static int toluaI_spell_wizard_lock00(lua_State* tolua_S)
{
 if (
     !tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
     !tolua_isnoobj(tolua_S,2)
 )
 {
  tolua_error(tolua_S,"#ferror in function 'wizard_lock'.");
  return 0;
 }
 else
 {
  int dir = ((int)  tolua_getnumber(tolua_S,1,0));
  {
   bool toluaI_ret = (bool)  wizard_lock(dir);
   tolua_pushbool(tolua_S,(int)toluaI_ret);
  }
 }
 return 1;
}

/* function: heal_monster */
static int toluaI_spell_heal_monster00(lua_State* tolua_S)
{
 if (
     !tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
     !tolua_isnoobj(tolua_S,2)
 )
 {
  tolua_error(tolua_S,"#ferror in function 'heal_monster'.");
  return 0;
 }
 else
 {
  int dir = ((int)  tolua_getnumber(tolua_S,1,0));
  {
   bool toluaI_ret = (bool)  heal_monster(dir);
   tolua_pushbool(tolua_S,(int)toluaI_ret);
  }
 }
 return 1;
}

/* function: speed_monster */
static int toluaI_spell_speed_monster00(lua_State* tolua_S)
{
 if (
     !tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
     !tolua_isnoobj(tolua_S,2)
 )
 {
  tolua_error(tolua_S,"#ferror in function 'speed_monster'.");
  return 0;
 }
 else
 {
  int dir = ((int)  tolua_getnumber(tolua_S,1,0));
  {
   bool toluaI_ret = (bool)  speed_monster(dir);
   tolua_pushbool(tolua_S,(int)toluaI_ret);
  }
 }
 return 1;
}

/* function: slow_monster */
static int toluaI_spell_slow_monster00(lua_State* tolua_S)
{
 if (
     !tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
     !tolua_isnoobj(tolua_S,2)
 )
 {
  tolua_error(tolua_S,"#ferror in function 'slow_monster'.");
  return 0;
 }
 else
 {
  int dir = ((int)  tolua_getnumber(tolua_S,1,0));
  {
   bool toluaI_ret = (bool)  slow_monster(dir);
   tolua_pushbool(tolua_S,(int)toluaI_ret);
  }
 }
 return 1;
}

/* function: sleep_monster */
static int toluaI_spell_sleep_monster00(lua_State* tolua_S)
{
 if (
     !tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
     !tolua_isnoobj(tolua_S,2)
 )
 {
  tolua_error(tolua_S,"#ferror in function 'sleep_monster'.");
  return 0;
 }
 else
 {
  int dir = ((int)  tolua_getnumber(tolua_S,1,0));
  {
   bool toluaI_ret = (bool)  sleep_monster(dir);
   tolua_pushbool(tolua_S,(int)toluaI_ret);
  }
 }
 return 1;
}

/* function: stasis_monster */
static int toluaI_spell_stasis_monster00(lua_State* tolua_S)
{
 if (
     !tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
     !tolua_isnoobj(tolua_S,2)
 )
 {
  tolua_error(tolua_S,"#ferror in function 'stasis_monster'.");
  return 0;
 }
 else
 {
  int dir = ((int)  tolua_getnumber(tolua_S,1,0));
  {
   bool toluaI_ret = (bool)  stasis_monster(dir);
   tolua_pushbool(tolua_S,(int)toluaI_ret);
  }
 }
 return 1;
}

/* function: confuse_monster */
static int toluaI_spell_confuse_monster00(lua_State* tolua_S)
{
 if (
     !tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
     !tolua_istype(tolua_S,2,LUA_TNUMBER,0) ||
     !tolua_isnoobj(tolua_S,3)
 )
 {
  tolua_error(tolua_S,"#ferror in function 'confuse_monster'.");
  return 0;
 }
 else
 {
  int dir = ((int)  tolua_getnumber(tolua_S,1,0));
  int plev = ((int)  tolua_getnumber(tolua_S,2,0));
  {
   bool toluaI_ret = (bool)  confuse_monster(dir,plev);
   tolua_pushbool(tolua_S,(int)toluaI_ret);
  }
 }
 return 1;
}

/* function: stun_monster */
static int toluaI_spell_stun_monster00(lua_State* tolua_S)
{
 if (
     !tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
     !tolua_istype(tolua_S,2,LUA_TNUMBER,0) ||
     !tolua_isnoobj(tolua_S,3)
 )
 {
  tolua_error(tolua_S,"#ferror in function 'stun_monster'.");
  return 0;
 }
 else
 {
  int dir = ((int)  tolua_getnumber(tolua_S,1,0));
  int plev = ((int)  tolua_getnumber(tolua_S,2,0));
  {
   bool toluaI_ret = (bool)  stun_monster(dir,plev);
   tolua_pushbool(tolua_S,(int)toluaI_ret);
  }
 }
 return 1;
}

/* function: fear_monster */
static int toluaI_spell_fear_monster00(lua_State* tolua_S)
{
 if (
     !tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
     !tolua_istype(tolua_S,2,LUA_TNUMBER,0) ||
     !tolua_isnoobj(tolua_S,3)
 )
 {
  tolua_error(tolua_S,"#ferror in function 'fear_monster'.");
  return 0;
 }
 else
 {
  int dir = ((int)  tolua_getnumber(tolua_S,1,0));
  int plev = ((int)  tolua_getnumber(tolua_S,2,0));
  {
   bool toluaI_ret = (bool)  fear_monster(dir,plev);
   tolua_pushbool(tolua_S,(int)toluaI_ret);
  }
 }
 return 1;
}

/* function: poly_monster */
static int toluaI_spell_poly_monster00(lua_State* tolua_S)
{
 if (
     !tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
     !tolua_isnoobj(tolua_S,2)
 )
 {
  tolua_error(tolua_S,"#ferror in function 'poly_monster'.");
  return 0;
 }
 else
 {
  int dir = ((int)  tolua_getnumber(tolua_S,1,0));
  {
   bool toluaI_ret = (bool)  poly_monster(dir);
   tolua_pushbool(tolua_S,(int)toluaI_ret);
  }
 }
 return 1;
}

/* function: clone_monster */
static int toluaI_spell_clone_monster00(lua_State* tolua_S)
{
 if (
     !tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
     !tolua_isnoobj(tolua_S,2)
 )
 {
  tolua_error(tolua_S,"#ferror in function 'clone_monster'.");
  return 0;
 }
 else
 {
  int dir = ((int)  tolua_getnumber(tolua_S,1,0));
  {
   bool toluaI_ret = (bool)  clone_monster(dir);
   tolua_pushbool(tolua_S,(int)toluaI_ret);
  }
 }
 return 1;
}

/* function: teleport_monster */
static int toluaI_spell_teleport_monster00(lua_State* tolua_S)
{
 if (
     !tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
     !tolua_isnoobj(tolua_S,2)
 )
 {
  tolua_error(tolua_S,"#ferror in function 'teleport_monster'.");
  return 0;
 }
 else
 {
  int dir = ((int)  tolua_getnumber(tolua_S,1,0));
  {
   bool toluaI_ret = (bool)  teleport_monster(dir);
   tolua_pushbool(tolua_S,(int)toluaI_ret);
  }
 }
 return 1;
}

/* function: door_creation */
static int toluaI_spell_door_creation00(lua_State* tolua_S)
{
 if (
     !tolua_isnoobj(tolua_S,1)
 )
 {
  tolua_error(tolua_S,"#ferror in function 'door_creation'.");
  return 0;
 }
 else
 {
  {
   bool toluaI_ret = (bool)  door_creation();
   tolua_pushbool(tolua_S,(int)toluaI_ret);
  }
 }
 return 1;
}

/* function: trap_creation */
static int toluaI_spell_trap_creation00(lua_State* tolua_S)
{
 if (
     !tolua_isnoobj(tolua_S,1)
 )
 {
  tolua_error(tolua_S,"#ferror in function 'trap_creation'.");
  return 0;
 }
 else
 {
  {
   bool toluaI_ret = (bool)  trap_creation();
   tolua_pushbool(tolua_S,(int)toluaI_ret);
  }
 }
 return 1;
}

/* function: glyph_creation */
static int toluaI_spell_glyph_creation00(lua_State* tolua_S)
{
 if (
     !tolua_isnoobj(tolua_S,1)
 )
 {
  tolua_error(tolua_S,"#ferror in function 'glyph_creation'.");
  return 0;
 }
 else
 {
  {
   bool toluaI_ret = (bool)  glyph_creation();
   tolua_pushbool(tolua_S,(int)toluaI_ret);
  }
 }
 return 1;
}

/* function: destroy_doors_touch */
static int toluaI_spell_destroy_doors_touch00(lua_State* tolua_S)
{
 if (
     !tolua_isnoobj(tolua_S,1)
 )
 {
  tolua_error(tolua_S,"#ferror in function 'destroy_doors_touch'.");
  return 0;
 }
 else
 {
  {
   bool toluaI_ret = (bool)  destroy_doors_touch();
   tolua_pushbool(tolua_S,(int)toluaI_ret);
  }
 }
 return 1;
}

/* function: sleep_monsters_touch */
static int toluaI_spell_sleep_monsters_touch00(lua_State* tolua_S)
{
 if (
     !tolua_isnoobj(tolua_S,1)
 )
 {
  tolua_error(tolua_S,"#ferror in function 'sleep_monsters_touch'.");
  return 0;
 }
 else
 {
  {
   bool toluaI_ret = (bool)  sleep_monsters_touch();
   tolua_pushbool(tolua_S,(int)toluaI_ret);
  }
 }
 return 1;
}

/* function: activate_ty_curse */
static int toluaI_spell_activate_ty_curse00(lua_State* tolua_S)
{
 if (
     !tolua_istype(tolua_S,1,tolua_tag(tolua_S,"bool"),0) ||
     !tolua_istype(tolua_S,2,LUA_TNUMBER,0) ||
     !tolua_isnoobj(tolua_S,3)
 )
 {
  tolua_error(tolua_S,"#ferror in function 'activate_ty_curse'.");
  return 0;
 }
 else
 {
  bool stop_ty = ((bool)  tolua_getbool(tolua_S,1,0));
  int count = ((int)  tolua_getnumber(tolua_S,2,0));
  {
   bool toluaI_ret = (bool)  activate_ty_curse(stop_ty,&count);
   tolua_pushbool(tolua_S,(int)toluaI_ret);
   tolua_pushnumber(tolua_S,(long)count);
  }
 }
 return 2;
}

/* function: activate_hi_summon */
static int toluaI_spell_activate_hi_summon00(lua_State* tolua_S)
{
 if (
     !tolua_isnoobj(tolua_S,1)
 )
 {
  tolua_error(tolua_S,"#ferror in function 'activate_hi_summon'.");
  return 0;
 }
 else
 {
  {
   int toluaI_ret = (int)  activate_hi_summon();
   tolua_pushnumber(tolua_S,(long)toluaI_ret);
  }
 }
 return 1;
}

/* function: summon_cyber */
static int toluaI_spell_summon_cyber00(lua_State* tolua_S)
{
 if (
     !tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
     !tolua_istype(tolua_S,2,LUA_TNUMBER,0) ||
     !tolua_istype(tolua_S,3,LUA_TNUMBER,0) ||
     !tolua_isnoobj(tolua_S,4)
 )
 {
  tolua_error(tolua_S,"#ferror in function 'summon_cyber'.");
  return 0;
 }
 else
 {
  int who = ((int)  tolua_getnumber(tolua_S,1,0));
  int x = ((int)  tolua_getnumber(tolua_S,2,0));
  int y = ((int)  tolua_getnumber(tolua_S,3,0));
  {
   int toluaI_ret = (int)  summon_cyber(who,x,y);
   tolua_pushnumber(tolua_S,(long)toluaI_ret);
  }
 }
 return 1;
}

/* function: wall_breaker */
static int toluaI_spell_wall_breaker00(lua_State* tolua_S)
{
 if (
     !tolua_isnoobj(tolua_S,1)
 )
 {
  tolua_error(tolua_S,"#ferror in function 'wall_breaker'.");
  return 0;
 }
 else
 {
  {
   wall_breaker();
  }
 }
 return 0;
}

/* function: confuse_monsters */
static int toluaI_spell_confuse_monsters00(lua_State* tolua_S)
{
 if (
     !tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
     !tolua_isnoobj(tolua_S,2)
 )
 {
  tolua_error(tolua_S,"#ferror in function 'confuse_monsters'.");
  return 0;
 }
 else
 {
  int dam = ((int)  tolua_getnumber(tolua_S,1,0));
  {
   bool toluaI_ret = (bool)  confuse_monsters(dam);
   tolua_pushbool(tolua_S,(int)toluaI_ret);
  }
 }
 return 1;
}

/* function: charm_monsters */
static int toluaI_spell_charm_monsters00(lua_State* tolua_S)
{
 if (
     !tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
     !tolua_isnoobj(tolua_S,2)
 )
 {
  tolua_error(tolua_S,"#ferror in function 'charm_monsters'.");
  return 0;
 }
 else
 {
  int dam = ((int)  tolua_getnumber(tolua_S,1,0));
  {
   bool toluaI_ret = (bool)  charm_monsters(dam);
   tolua_pushbool(tolua_S,(int)toluaI_ret);
  }
 }
 return 1;
}

/* function: charm_animals */
static int toluaI_spell_charm_animals00(lua_State* tolua_S)
{
 if (
     !tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
     !tolua_isnoobj(tolua_S,2)
 )
 {
  tolua_error(tolua_S,"#ferror in function 'charm_animals'.");
  return 0;
 }
 else
 {
  int dam = ((int)  tolua_getnumber(tolua_S,1,0));
  {
   bool toluaI_ret = (bool)  charm_animals(dam);
   tolua_pushbool(tolua_S,(int)toluaI_ret);
  }
 }
 return 1;
}

/* function: starlite */
static int toluaI_spell_starlite00(lua_State* tolua_S)
{
 if (
     !tolua_isnoobj(tolua_S,1)
 )
 {
  tolua_error(tolua_S,"#ferror in function 'starlite'.");
  return 0;
 }
 else
 {
  {
   bool toluaI_ret = (bool)  starlite();
   tolua_pushbool(tolua_S,(int)toluaI_ret);
  }
 }
 return 1;
}

/* function: scatter_ball */
static int toluaI_spell_scatter_ball00(lua_State* tolua_S)
{
 if (
     !tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
     !tolua_istype(tolua_S,2,LUA_TNUMBER,0) ||
     !tolua_istype(tolua_S,3,LUA_TNUMBER,0) ||
     !tolua_istype(tolua_S,4,LUA_TNUMBER,0) ||
     !tolua_isnoobj(tolua_S,5)
 )
 {
  tolua_error(tolua_S,"#ferror in function 'scatter_ball'.");
  return 0;
 }
 else
 {
  int num = ((int)  tolua_getnumber(tolua_S,1,0));
  int typ = ((int)  tolua_getnumber(tolua_S,2,0));
  int dam = ((int)  tolua_getnumber(tolua_S,3,0));
  int rad = ((int)  tolua_getnumber(tolua_S,4,0));
  {
   bool toluaI_ret = (bool)  scatter_ball(num,typ,dam,rad);
   tolua_pushbool(tolua_S,(int)toluaI_ret);
  }
 }
 return 1;
}

/* function: create_food */
static int toluaI_spell_create_food00(lua_State* tolua_S)
{
 if (
     !tolua_isnoobj(tolua_S,1)
 )
 {
  tolua_error(tolua_S,"#ferror in function 'create_food'.");
  return 0;
 }
 else
 {
  {
   create_food();
  }
 }
 return 0;
}

/* function: whirlwind_attack */
static int toluaI_spell_whirlwind_attack00(lua_State* tolua_S)
{
 if (
     !tolua_isnoobj(tolua_S,1)
 )
 {
  tolua_error(tolua_S,"#ferror in function 'whirlwind_attack'.");
  return 0;
 }
 else
 {
  {
   whirlwind_attack();
  }
 }
 return 0;
}

/* function: stun_monsters */
static int toluaI_spell_stun_monsters00(lua_State* tolua_S)
{
 if (
     !tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
     !tolua_isnoobj(tolua_S,2)
 )
 {
  tolua_error(tolua_S,"#ferror in function 'stun_monsters'.");
  return 0;
 }
 else
 {
  int dam = ((int)  tolua_getnumber(tolua_S,1,0));
  {
   bool toluaI_ret = (bool)  stun_monsters(dam);
   tolua_pushbool(tolua_S,(int)toluaI_ret);
  }
 }
 return 1;
}

/* function: stasis_monsters */
static int toluaI_spell_stasis_monsters00(lua_State* tolua_S)
{
 if (
     !tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
     !tolua_isnoobj(tolua_S,2)
 )
 {
  tolua_error(tolua_S,"#ferror in function 'stasis_monsters'.");
  return 0;
 }
 else
 {
  int dam = ((int)  tolua_getnumber(tolua_S,1,0));
  {
   bool toluaI_ret = (bool)  stasis_monsters(dam);
   tolua_pushbool(tolua_S,(int)toluaI_ret);
  }
 }
 return 1;
}

/* function: banish_monsters */
static int toluaI_spell_banish_monsters00(lua_State* tolua_S)
{
 if (
     !tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
     !tolua_isnoobj(tolua_S,2)
 )
 {
  tolua_error(tolua_S,"#ferror in function 'banish_monsters'.");
  return 0;
 }
 else
 {
  int dist = ((int)  tolua_getnumber(tolua_S,1,0));
  {
   bool toluaI_ret = (bool)  banish_monsters(dist);
   tolua_pushbool(tolua_S,(int)toluaI_ret);
  }
 }
 return 1;
}

/* function: turn_monsters */
static int toluaI_spell_turn_monsters00(lua_State* tolua_S)
{
 if (
     !tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
     !tolua_isnoobj(tolua_S,2)
 )
 {
  tolua_error(tolua_S,"#ferror in function 'turn_monsters'.");
  return 0;
 }
 else
 {
  int dam = ((int)  tolua_getnumber(tolua_S,1,0));
  {
   bool toluaI_ret = (bool)  turn_monsters(dam);
   tolua_pushbool(tolua_S,(int)toluaI_ret);
  }
 }
 return 1;
}

/* function: turn_evil */
static int toluaI_spell_turn_evil00(lua_State* tolua_S)
{
 if (
     !tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
     !tolua_isnoobj(tolua_S,2)
 )
 {
  tolua_error(tolua_S,"#ferror in function 'turn_evil'.");
  return 0;
 }
 else
 {
  int dam = ((int)  tolua_getnumber(tolua_S,1,0));
  {
   bool toluaI_ret = (bool)  turn_evil(dam);
   tolua_pushbool(tolua_S,(int)toluaI_ret);
  }
 }
 return 1;
}

/* function: deathray_monsters */
static int toluaI_spell_deathray_monsters00(lua_State* tolua_S)
{
 if (
     !tolua_isnoobj(tolua_S,1)
 )
 {
  tolua_error(tolua_S,"#ferror in function 'deathray_monsters'.");
  return 0;
 }
 else
 {
  {
   bool toluaI_ret = (bool)  deathray_monsters();
   tolua_pushbool(tolua_S,(int)toluaI_ret);
  }
 }
 return 1;
}

/* function: charm_monster */
static int toluaI_spell_charm_monster00(lua_State* tolua_S)
{
 if (
     !tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
     !tolua_istype(tolua_S,2,LUA_TNUMBER,0) ||
     !tolua_isnoobj(tolua_S,3)
 )
 {
  tolua_error(tolua_S,"#ferror in function 'charm_monster'.");
  return 0;
 }
 else
 {
  int dir = ((int)  tolua_getnumber(tolua_S,1,0));
  int plev = ((int)  tolua_getnumber(tolua_S,2,0));
  {
   bool toluaI_ret = (bool)  charm_monster(dir,plev);
   tolua_pushbool(tolua_S,(int)toluaI_ret);
  }
 }
 return 1;
}

/* function: control_one_undead */
static int toluaI_spell_control_one_undead00(lua_State* tolua_S)
{
 if (
     !tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
     !tolua_istype(tolua_S,2,LUA_TNUMBER,0) ||
     !tolua_isnoobj(tolua_S,3)
 )
 {
  tolua_error(tolua_S,"#ferror in function 'control_one_undead'.");
  return 0;
 }
 else
 {
  int dir = ((int)  tolua_getnumber(tolua_S,1,0));
  int plev = ((int)  tolua_getnumber(tolua_S,2,0));
  {
   bool toluaI_ret = (bool)  control_one_undead(dir,plev);
   tolua_pushbool(tolua_S,(int)toluaI_ret);
  }
 }
 return 1;
}

/* function: charm_animal */
static int toluaI_spell_charm_animal00(lua_State* tolua_S)
{
 if (
     !tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
     !tolua_istype(tolua_S,2,LUA_TNUMBER,0) ||
     !tolua_isnoobj(tolua_S,3)
 )
 {
  tolua_error(tolua_S,"#ferror in function 'charm_animal'.");
  return 0;
 }
 else
 {
  int dir = ((int)  tolua_getnumber(tolua_S,1,0));
  int plev = ((int)  tolua_getnumber(tolua_S,2,0));
  {
   bool toluaI_ret = (bool)  charm_animal(dir,plev);
   tolua_pushbool(tolua_S,(int)toluaI_ret);
  }
 }
 return 1;
}

/* function: mindblast_monsters */
static int toluaI_spell_mindblast_monsters00(lua_State* tolua_S)
{
 if (
     !tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
     !tolua_isnoobj(tolua_S,2)
 )
 {
  tolua_error(tolua_S,"#ferror in function 'mindblast_monsters'.");
  return 0;
 }
 else
 {
  int dam = ((int)  tolua_getnumber(tolua_S,1,0));
  {
   bool toluaI_ret = (bool)  mindblast_monsters(dam);
   tolua_pushbool(tolua_S,(int)toluaI_ret);
  }
 }
 return 1;
}

/* function: report_magics */
static int toluaI_spell_report_magics00(lua_State* tolua_S)
{
 if (
     !tolua_isnoobj(tolua_S,1)
 )
 {
  tolua_error(tolua_S,"#ferror in function 'report_magics'.");
  return 0;
 }
 else
 {
  {
   report_magics();
  }
 }
 return 0;
}

/* function: teleport_swap */
static int toluaI_spell_teleport_swap00(lua_State* tolua_S)
{
 if (
     !tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
     !tolua_isnoobj(tolua_S,2)
 )
 {
  tolua_error(tolua_S,"#ferror in function 'teleport_swap'.");
  return 0;
 }
 else
 {
  int dir = ((int)  tolua_getnumber(tolua_S,1,0));
  {
   bool toluaI_ret = (bool)  teleport_swap(dir);
   tolua_pushbool(tolua_S,(int)toluaI_ret);
  }
 }
 return 1;
}

/* function: project_hook */
static int toluaI_spell_project_hook00(lua_State* tolua_S)
{
 if (
     !tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
     !tolua_istype(tolua_S,2,LUA_TNUMBER,0) ||
     !tolua_istype(tolua_S,3,LUA_TNUMBER,0) ||
     !tolua_istype(tolua_S,4,LUA_TNUMBER,0) ||
     !tolua_isnoobj(tolua_S,5)
 )
 {
  tolua_error(tolua_S,"#ferror in function 'project_hook'.");
  return 0;
 }
 else
 {
  int typ = ((int)  tolua_getnumber(tolua_S,1,0));
  int dir = ((int)  tolua_getnumber(tolua_S,2,0));
  int dam = ((int)  tolua_getnumber(tolua_S,3,0));
  u16b flg = ((u16b)  tolua_getnumber(tolua_S,4,0));
  {
   bool toluaI_ret = (bool)  project_hook(typ,dir,dam,flg);
   tolua_pushbool(tolua_S,(int)toluaI_ret);
  }
 }
 return 1;
}

/* function: project_hack */
static int toluaI_spell_project_hack00(lua_State* tolua_S)
{
 if (
     !tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
     !tolua_istype(tolua_S,2,LUA_TNUMBER,0) ||
     !tolua_isnoobj(tolua_S,3)
 )
 {
  tolua_error(tolua_S,"#ferror in function 'project_hack'.");
  return 0;
 }
 else
 {
  int typ = ((int)  tolua_getnumber(tolua_S,1,0));
  int dam = ((int)  tolua_getnumber(tolua_S,2,0));
  {
   bool toluaI_ret = (bool)  project_hack(typ,dam);
   tolua_pushbool(tolua_S,(int)toluaI_ret);
  }
 }
 return 1;
}

/* function: teleport_away */
static int toluaI_spell_teleport_away00(lua_State* tolua_S)
{
 if (
     !tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
     !tolua_istype(tolua_S,2,LUA_TNUMBER,0) ||
     !tolua_isnoobj(tolua_S,3)
 )
 {
  tolua_error(tolua_S,"#ferror in function 'teleport_away'.");
  return 0;
 }
 else
 {
  int m_idx = ((int)  tolua_getnumber(tolua_S,1,0));
  int dis = ((int)  tolua_getnumber(tolua_S,2,0));
  {
   bool toluaI_ret = (bool)  teleport_away(m_idx,dis);
   tolua_pushbool(tolua_S,(int)toluaI_ret);
  }
 }
 return 1;
}

/* function: teleport_to_player */
static int toluaI_spell_teleport_to_player00(lua_State* tolua_S)
{
 if (
     !tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
     !tolua_isnoobj(tolua_S,2)
 )
 {
  tolua_error(tolua_S,"#ferror in function 'teleport_to_player'.");
  return 0;
 }
 else
 {
  int m_idx = ((int)  tolua_getnumber(tolua_S,1,0));
  {
   teleport_to_player(m_idx);
  }
 }
 return 0;
}

/* function: teleport_player */
static int toluaI_spell_teleport_player00(lua_State* tolua_S)
{
 if (
     !tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
     !tolua_isnoobj(tolua_S,2)
 )
 {
  tolua_error(tolua_S,"#ferror in function 'teleport_player'.");
  return 0;
 }
 else
 {
  int dis = ((int)  tolua_getnumber(tolua_S,1,0));
  {
   teleport_player(dis);
  }
 }
 return 0;
}

/* function: teleport_player_to */
static int toluaI_spell_teleport_player_to00(lua_State* tolua_S)
{
 if (
     !tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
     !tolua_istype(tolua_S,2,LUA_TNUMBER,0) ||
     !tolua_isnoobj(tolua_S,3)
 )
 {
  tolua_error(tolua_S,"#ferror in function 'teleport_player_to'.");
  return 0;
 }
 else
 {
  int nx = ((int)  tolua_getnumber(tolua_S,1,0));
  int ny = ((int)  tolua_getnumber(tolua_S,2,0));
  {
   teleport_player_to(nx,ny);
  }
 }
 return 0;
}

/* function: teleport_player_level */
static int toluaI_spell_teleport_player_level00(lua_State* tolua_S)
{
 if (
     !tolua_isnoobj(tolua_S,1)
 )
 {
  tolua_error(tolua_S,"#ferror in function 'teleport_player_level'.");
  return 0;
 }
 else
 {
  {
   teleport_player_level();
  }
 }
 return 0;
}

/* function: recall_player */
static int toluaI_spell_recall_player00(lua_State* tolua_S)
{
 if (
     !tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
     !tolua_isnoobj(tolua_S,2)
 )
 {
  tolua_error(tolua_S,"#ferror in function 'recall_player'.");
  return 0;
 }
 else
 {
  int turns = ((int)  tolua_getnumber(tolua_S,1,0));
  {
   recall_player(turns);
  }
 }
 return 0;
}

/* function: word_of_recall */
static int toluaI_spell_word_of_recall00(lua_State* tolua_S)
{
 if (
     !tolua_isnoobj(tolua_S,1)
 )
 {
  tolua_error(tolua_S,"#ferror in function 'word_of_recall'.");
  return 0;
 }
 else
 {
  {
   word_of_recall();
  }
 }
 return 0;
}

/* function: apply_disenchant */
static int toluaI_spell_apply_disenchant00(lua_State* tolua_S)
{
 if (
     !tolua_isnoobj(tolua_S,1)
 )
 {
  tolua_error(tolua_S,"#ferror in function 'apply_disenchant'.");
  return 0;
 }
 else
 {
  {
   bool toluaI_ret = (bool)  apply_disenchant();
   tolua_pushbool(tolua_S,(int)toluaI_ret);
  }
 }
 return 1;
}

/* function: mutate_player */
static int toluaI_spell_mutate_player00(lua_State* tolua_S)
{
 if (
     !tolua_isnoobj(tolua_S,1)
 )
 {
  tolua_error(tolua_S,"#ferror in function 'mutate_player'.");
  return 0;
 }
 else
 {
  {
   mutate_player();
  }
 }
 return 0;
}

/* function: apply_nexus */
static int toluaI_spell_apply_nexus00(lua_State* tolua_S)
{
 if (
     !tolua_istype(tolua_S,1,tolua_tag(tolua_S,"const monster_type"),0) ||
     !tolua_isnoobj(tolua_S,2)
 )
 {
  tolua_error(tolua_S,"#ferror in function 'apply_nexus'.");
  return 0;
 }
 else
 {
  const monster_type* m_ptr = ((const monster_type*)  tolua_getusertype(tolua_S,1,0));
  {
   apply_nexus(m_ptr);
  }
 }
 return 0;
}

/* function: phlogiston */
static int toluaI_spell_phlogiston00(lua_State* tolua_S)
{
 if (
     !tolua_isnoobj(tolua_S,1)
 )
 {
  tolua_error(tolua_S,"#ferror in function 'phlogiston'.");
  return 0;
 }
 else
 {
  {
   phlogiston();
  }
 }
 return 0;
}

/* function: brand_weapon */
static int toluaI_spell_brand_weapon00(lua_State* tolua_S)
{
 if (
     !tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
     !tolua_isnoobj(tolua_S,2)
 )
 {
  tolua_error(tolua_S,"#ferror in function 'brand_weapon'.");
  return 0;
 }
 else
 {
  int brand_type = ((int)  tolua_getnumber(tolua_S,1,0));
  {
   brand_weapon(brand_type);
  }
 }
 return 0;
}

/* function: call_the_ */
static int toluaI_spell_call_the_00(lua_State* tolua_S)
{
 if (
     !tolua_isnoobj(tolua_S,1)
 )
 {
  tolua_error(tolua_S,"#ferror in function 'call_the_'.");
  return 0;
 }
 else
 {
  {
   call_the_();
  }
 }
 return 0;
}

/* function: fetch */
static int toluaI_spell_fetch00(lua_State* tolua_S)
{
 if (
     !tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
     !tolua_istype(tolua_S,2,LUA_TNUMBER,0) ||
     !tolua_istype(tolua_S,3,tolua_tag(tolua_S,"bool"),0) ||
     !tolua_isnoobj(tolua_S,4)
 )
 {
  tolua_error(tolua_S,"#ferror in function 'fetch'.");
  return 0;
 }
 else
 {
  int dir = ((int)  tolua_getnumber(tolua_S,1,0));
  int wgt = ((int)  tolua_getnumber(tolua_S,2,0));
  bool require_los = ((bool)  tolua_getbool(tolua_S,3,0));
  {
   fetch(dir,wgt,require_los);
  }
 }
 return 0;
}

/* function: alter_reality */
static int toluaI_spell_alter_reality00(lua_State* tolua_S)
{
 if (
     !tolua_isnoobj(tolua_S,1)
 )
 {
  tolua_error(tolua_S,"#ferror in function 'alter_reality'.");
  return 0;
 }
 else
 {
  {
   alter_reality();
  }
 }
 return 0;
}

/* function: warding_glyph */
static int toluaI_spell_warding_glyph00(lua_State* tolua_S)
{
 if (
     !tolua_isnoobj(tolua_S,1)
 )
 {
  tolua_error(tolua_S,"#ferror in function 'warding_glyph'.");
  return 0;
 }
 else
 {
  {
   bool toluaI_ret = (bool)  warding_glyph();
   tolua_pushbool(tolua_S,(int)toluaI_ret);
  }
 }
 return 1;
}

/* function: explosive_rune */
static int toluaI_spell_explosive_rune00(lua_State* tolua_S)
{
 if (
     !tolua_isnoobj(tolua_S,1)
 )
 {
  tolua_error(tolua_S,"#ferror in function 'explosive_rune'.");
  return 0;
 }
 else
 {
  {
   bool toluaI_ret = (bool)  explosive_rune();
   tolua_pushbool(tolua_S,(int)toluaI_ret);
  }
 }
 return 1;
}

/* function: identify_pack */
static int toluaI_spell_identify_pack00(lua_State* tolua_S)
{
 if (
     !tolua_isnoobj(tolua_S,1)
 )
 {
  tolua_error(tolua_S,"#ferror in function 'identify_pack'.");
  return 0;
 }
 else
 {
  {
   identify_pack();
  }
 }
 return 0;
}

/* function: remove_curse */
static int toluaI_spell_remove_curse00(lua_State* tolua_S)
{
 if (
     !tolua_isnoobj(tolua_S,1)
 )
 {
  tolua_error(tolua_S,"#ferror in function 'remove_curse'.");
  return 0;
 }
 else
 {
  {
   bool toluaI_ret = (bool)  remove_curse();
   tolua_pushbool(tolua_S,(int)toluaI_ret);
  }
 }
 return 1;
}

/* function: remove_all_curse */
static int toluaI_spell_remove_all_curse00(lua_State* tolua_S)
{
 if (
     !tolua_isnoobj(tolua_S,1)
 )
 {
  tolua_error(tolua_S,"#ferror in function 'remove_all_curse'.");
  return 0;
 }
 else
 {
  {
   bool toluaI_ret = (bool)  remove_all_curse();
   tolua_pushbool(tolua_S,(int)toluaI_ret);
  }
 }
 return 1;
}

/* function: alchemy */
static int toluaI_spell_alchemy00(lua_State* tolua_S)
{
 if (
     !tolua_isnoobj(tolua_S,1)
 )
 {
  tolua_error(tolua_S,"#ferror in function 'alchemy'.");
  return 0;
 }
 else
 {
  {
   bool toluaI_ret = (bool)  alchemy();
   tolua_pushbool(tolua_S,(int)toluaI_ret);
  }
 }
 return 1;
}

/* function: stair_creation */
static int toluaI_spell_stair_creation00(lua_State* tolua_S)
{
 if (
     !tolua_isnoobj(tolua_S,1)
 )
 {
  tolua_error(tolua_S,"#ferror in function 'stair_creation'.");
  return 0;
 }
 else
 {
  {
   stair_creation();
  }
 }
 return 0;
}

/* function: enchant */
static int toluaI_spell_enchant00(lua_State* tolua_S)
{
 if (
     !tolua_istype(tolua_S,1,tolua_tag(tolua_S,"object_type"),0) ||
     !tolua_istype(tolua_S,2,LUA_TNUMBER,0) ||
     !tolua_istype(tolua_S,3,LUA_TNUMBER,0) ||
     !tolua_isnoobj(tolua_S,4)
 )
 {
  tolua_error(tolua_S,"#ferror in function 'enchant'.");
  return 0;
 }
 else
 {
  object_type* o_ptr = ((object_type*)  tolua_getusertype(tolua_S,1,0));
  int n = ((int)  tolua_getnumber(tolua_S,2,0));
  int eflag = ((int)  tolua_getnumber(tolua_S,3,0));
  {
   bool toluaI_ret = (bool)  enchant(o_ptr,n,eflag);
   tolua_pushbool(tolua_S,(int)toluaI_ret);
  }
 }
 return 1;
}

/* function: enchant_spell */
static int toluaI_spell_enchant_spell00(lua_State* tolua_S)
{
 if (
     !tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
     !tolua_istype(tolua_S,2,LUA_TNUMBER,0) ||
     !tolua_istype(tolua_S,3,LUA_TNUMBER,0) ||
     !tolua_isnoobj(tolua_S,4)
 )
 {
  tolua_error(tolua_S,"#ferror in function 'enchant_spell'.");
  return 0;
 }
 else
 {
  int num_hit = ((int)  tolua_getnumber(tolua_S,1,0));
  int num_dam = ((int)  tolua_getnumber(tolua_S,2,0));
  int num_ac = ((int)  tolua_getnumber(tolua_S,3,0));
  {
   bool toluaI_ret = (bool)  enchant_spell(num_hit,num_dam,num_ac);
   tolua_pushbool(tolua_S,(int)toluaI_ret);
  }
 }
 return 1;
}

/* function: artifact_scroll */
static int toluaI_spell_artifact_scroll00(lua_State* tolua_S)
{
 if (
     !tolua_isnoobj(tolua_S,1)
 )
 {
  tolua_error(tolua_S,"#ferror in function 'artifact_scroll'.");
  return 0;
 }
 else
 {
  {
   bool toluaI_ret = (bool)  artifact_scroll();
   tolua_pushbool(tolua_S,(int)toluaI_ret);
  }
 }
 return 1;
}

/* function: ident_spell */
static int toluaI_spell_ident_spell00(lua_State* tolua_S)
{
 if (
     !tolua_isnoobj(tolua_S,1)
 )
 {
  tolua_error(tolua_S,"#ferror in function 'ident_spell'.");
  return 0;
 }
 else
 {
  {
   bool toluaI_ret = (bool)  ident_spell();
   tolua_pushbool(tolua_S,(int)toluaI_ret);
  }
 }
 return 1;
}

/* function: mundane_spell */
static int toluaI_spell_mundane_spell00(lua_State* tolua_S)
{
 if (
     !tolua_isnoobj(tolua_S,1)
 )
 {
  tolua_error(tolua_S,"#ferror in function 'mundane_spell'.");
  return 0;
 }
 else
 {
  {
   bool toluaI_ret = (bool)  mundane_spell();
   tolua_pushbool(tolua_S,(int)toluaI_ret);
  }
 }
 return 1;
}

/* function: identify_item */
static int toluaI_spell_identify_item00(lua_State* tolua_S)
{
 if (
     !tolua_istype(tolua_S,1,tolua_tag(tolua_S,"object_type"),0) ||
     !tolua_isnoobj(tolua_S,2)
 )
 {
  tolua_error(tolua_S,"#ferror in function 'identify_item'.");
  return 0;
 }
 else
 {
  object_type* o_ptr = ((object_type*)  tolua_getusertype(tolua_S,1,0));
  {
   identify_item(o_ptr);
  }
 }
 return 0;
}

/* function: identify_fully */
static int toluaI_spell_identify_fully00(lua_State* tolua_S)
{
 if (
     !tolua_isnoobj(tolua_S,1)
 )
 {
  tolua_error(tolua_S,"#ferror in function 'identify_fully'.");
  return 0;
 }
 else
 {
  {
   bool toluaI_ret = (bool)  identify_fully();
   tolua_pushbool(tolua_S,(int)toluaI_ret);
  }
 }
 return 1;
}

/* function: recharge */
static int toluaI_spell_recharge00(lua_State* tolua_S)
{
 if (
     !tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
     !tolua_isnoobj(tolua_S,2)
 )
 {
  tolua_error(tolua_S,"#ferror in function 'recharge'.");
  return 0;
 }
 else
 {
  int num = ((int)  tolua_getnumber(tolua_S,1,0));
  {
   bool toluaI_ret = (bool)  recharge(num);
   tolua_pushbool(tolua_S,(int)toluaI_ret);
  }
 }
 return 1;
}

/* function: bless_weapon */
static int toluaI_spell_bless_weapon00(lua_State* tolua_S)
{
 if (
     !tolua_isnoobj(tolua_S,1)
 )
 {
  tolua_error(tolua_S,"#ferror in function 'bless_weapon'.");
  return 0;
 }
 else
 {
  {
   bool toluaI_ret = (bool)  bless_weapon();
   tolua_pushbool(tolua_S,(int)toluaI_ret);
  }
 }
 return 1;
}

/* function: potion_smash_effect */
static int toluaI_spell_potion_smash_effect00(lua_State* tolua_S)
{
 if (
     !tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
     !tolua_istype(tolua_S,2,LUA_TNUMBER,0) ||
     !tolua_istype(tolua_S,3,LUA_TNUMBER,0) ||
     !tolua_istype(tolua_S,4,tolua_tag(tolua_S,"object_type"),0) ||
     !tolua_isnoobj(tolua_S,5)
 )
 {
  tolua_error(tolua_S,"#ferror in function 'potion_smash_effect'.");
  return 0;
 }
 else
 {
  int who = ((int)  tolua_getnumber(tolua_S,1,0));
  int x = ((int)  tolua_getnumber(tolua_S,2,0));
  int y = ((int)  tolua_getnumber(tolua_S,3,0));
  object_type* o_ptr = ((object_type*)  tolua_getusertype(tolua_S,4,0));
  {
   bool toluaI_ret = (bool)  potion_smash_effect(who,x,y,o_ptr);
   tolua_pushbool(tolua_S,(int)toluaI_ret);
  }
 }
 return 1;
}

/* function: display_spell_list */
static int toluaI_spell_display_spell_list00(lua_State* tolua_S)
{
 if (
     !tolua_isnoobj(tolua_S,1)
 )
 {
  tolua_error(tolua_S,"#ferror in function 'display_spell_list'.");
  return 0;
 }
 else
 {
  {
   display_spell_list();
  }
 }
 return 0;
}

/* function: spell_chance */
static int toluaI_spell_spell_chance00(lua_State* tolua_S)
{
 if (
     !tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
     !tolua_istype(tolua_S,2,LUA_TNUMBER,0) ||
     !tolua_isnoobj(tolua_S,3)
 )
 {
  tolua_error(tolua_S,"#ferror in function 'spell_chance'.");
  return 0;
 }
 else
 {
  int spell = ((int)  tolua_getnumber(tolua_S,1,0));
  int realm = ((int)  tolua_getnumber(tolua_S,2,0));
  {
   s16b toluaI_ret = (s16b)  spell_chance(spell,realm);
   tolua_pushnumber(tolua_S,(long)toluaI_ret);
  }
 }
 return 1;
}

/* function: spell_okay */
static int toluaI_spell_spell_okay00(lua_State* tolua_S)
{
 if (
     !tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
     !tolua_istype(tolua_S,2,tolua_tag(tolua_S,"bool"),0) ||
     !tolua_istype(tolua_S,3,LUA_TNUMBER,0) ||
     !tolua_isnoobj(tolua_S,4)
 )
 {
  tolua_error(tolua_S,"#ferror in function 'spell_okay'.");
  return 0;
 }
 else
 {
  int spell = ((int)  tolua_getnumber(tolua_S,1,0));
  bool known = ((bool)  tolua_getbool(tolua_S,2,0));
  int realm = ((int)  tolua_getnumber(tolua_S,3,0));
  {
   bool toluaI_ret = (bool)  spell_okay(spell,known,realm);
   tolua_pushbool(tolua_S,(int)toluaI_ret);
  }
 }
 return 1;
}

/* function: print_spells */
static int toluaI_spell_print_spells00(lua_State* tolua_S)
{
 if (
     !tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
     !tolua_istype(tolua_S,2,LUA_TNUMBER,0) ||
     !tolua_istype(tolua_S,3,LUA_TNUMBER,0) ||
     !tolua_istype(tolua_S,4,LUA_TNUMBER,0) ||
     !tolua_istype(tolua_S,5,LUA_TNUMBER,0) ||
     !tolua_isnoobj(tolua_S,6)
 )
 {
  tolua_error(tolua_S,"#ferror in function 'print_spells'.");
  return 0;
 }
 else
 {
  byte spells = ((byte)  tolua_getnumber(tolua_S,1,0));
  int num = ((int)  tolua_getnumber(tolua_S,2,0));
  int x = ((int)  tolua_getnumber(tolua_S,3,0));
  int y = ((int)  tolua_getnumber(tolua_S,4,0));
  int realm = ((int)  tolua_getnumber(tolua_S,5,0));
  {
   print_spells(&spells,num,x,y,realm);
   tolua_pushnumber(tolua_S,(long)spells);
  }
 }
 return 1;
}

/* function: hates_acid */
static int toluaI_spell_hates_acid00(lua_State* tolua_S)
{
 if (
     !tolua_istype(tolua_S,1,tolua_tag(tolua_S,"const object_type"),0) ||
     !tolua_isnoobj(tolua_S,2)
 )
 {
  tolua_error(tolua_S,"#ferror in function 'hates_acid'.");
  return 0;
 }
 else
 {
  const object_type* o_ptr = ((const object_type*)  tolua_getusertype(tolua_S,1,0));
  {
   bool toluaI_ret = (bool)  hates_acid(o_ptr);
   tolua_pushbool(tolua_S,(int)toluaI_ret);
  }
 }
 return 1;
}

/* function: hates_elec */
static int toluaI_spell_hates_elec00(lua_State* tolua_S)
{
 if (
     !tolua_istype(tolua_S,1,tolua_tag(tolua_S,"const object_type"),0) ||
     !tolua_isnoobj(tolua_S,2)
 )
 {
  tolua_error(tolua_S,"#ferror in function 'hates_elec'.");
  return 0;
 }
 else
 {
  const object_type* o_ptr = ((const object_type*)  tolua_getusertype(tolua_S,1,0));
  {
   bool toluaI_ret = (bool)  hates_elec(o_ptr);
   tolua_pushbool(tolua_S,(int)toluaI_ret);
  }
 }
 return 1;
}

/* function: hates_fire */
static int toluaI_spell_hates_fire00(lua_State* tolua_S)
{
 if (
     !tolua_istype(tolua_S,1,tolua_tag(tolua_S,"const object_type"),0) ||
     !tolua_isnoobj(tolua_S,2)
 )
 {
  tolua_error(tolua_S,"#ferror in function 'hates_fire'.");
  return 0;
 }
 else
 {
  const object_type* o_ptr = ((const object_type*)  tolua_getusertype(tolua_S,1,0));
  {
   bool toluaI_ret = (bool)  hates_fire(o_ptr);
   tolua_pushbool(tolua_S,(int)toluaI_ret);
  }
 }
 return 1;
}

/* function: hates_cold */
static int toluaI_spell_hates_cold00(lua_State* tolua_S)
{
 if (
     !tolua_istype(tolua_S,1,tolua_tag(tolua_S,"const object_type"),0) ||
     !tolua_isnoobj(tolua_S,2)
 )
 {
  tolua_error(tolua_S,"#ferror in function 'hates_cold'.");
  return 0;
 }
 else
 {
  const object_type* o_ptr = ((const object_type*)  tolua_getusertype(tolua_S,1,0));
  {
   bool toluaI_ret = (bool)  hates_cold(o_ptr);
   tolua_pushbool(tolua_S,(int)toluaI_ret);
  }
 }
 return 1;
}

/* function: set_acid_destroy */
static int toluaI_spell_set_acid_destroy00(lua_State* tolua_S)
{
 if (
     !tolua_istype(tolua_S,1,tolua_tag(tolua_S,"object_type"),0) ||
     !tolua_isnoobj(tolua_S,2)
 )
 {
  tolua_error(tolua_S,"#ferror in function 'set_acid_destroy'.");
  return 0;
 }
 else
 {
  object_type* o_ptr = ((object_type*)  tolua_getusertype(tolua_S,1,0));
  {
   int toluaI_ret = (int)  set_acid_destroy(o_ptr);
   tolua_pushnumber(tolua_S,(long)toluaI_ret);
  }
 }
 return 1;
}

/* function: set_elec_destroy */
static int toluaI_spell_set_elec_destroy00(lua_State* tolua_S)
{
 if (
     !tolua_istype(tolua_S,1,tolua_tag(tolua_S,"object_type"),0) ||
     !tolua_isnoobj(tolua_S,2)
 )
 {
  tolua_error(tolua_S,"#ferror in function 'set_elec_destroy'.");
  return 0;
 }
 else
 {
  object_type* o_ptr = ((object_type*)  tolua_getusertype(tolua_S,1,0));
  {
   int toluaI_ret = (int)  set_elec_destroy(o_ptr);
   tolua_pushnumber(tolua_S,(long)toluaI_ret);
  }
 }
 return 1;
}

/* function: set_fire_destroy */
static int toluaI_spell_set_fire_destroy00(lua_State* tolua_S)
{
 if (
     !tolua_istype(tolua_S,1,tolua_tag(tolua_S,"object_type"),0) ||
     !tolua_isnoobj(tolua_S,2)
 )
 {
  tolua_error(tolua_S,"#ferror in function 'set_fire_destroy'.");
  return 0;
 }
 else
 {
  object_type* o_ptr = ((object_type*)  tolua_getusertype(tolua_S,1,0));
  {
   int toluaI_ret = (int)  set_fire_destroy(o_ptr);
   tolua_pushnumber(tolua_S,(long)toluaI_ret);
  }
 }
 return 1;
}

/* function: set_cold_destroy */
static int toluaI_spell_set_cold_destroy00(lua_State* tolua_S)
{
 if (
     !tolua_istype(tolua_S,1,tolua_tag(tolua_S,"object_type"),0) ||
     !tolua_isnoobj(tolua_S,2)
 )
 {
  tolua_error(tolua_S,"#ferror in function 'set_cold_destroy'.");
  return 0;
 }
 else
 {
  object_type* o_ptr = ((object_type*)  tolua_getusertype(tolua_S,1,0));
  {
   int toluaI_ret = (int)  set_cold_destroy(o_ptr);
   tolua_pushnumber(tolua_S,(long)toluaI_ret);
  }
 }
 return 1;
}

/* function: inven_damage */
static int toluaI_spell_inven_damage00(lua_State* tolua_S)
{
 if (
     !tolua_istype(tolua_S,1,tolua_tag(tolua_S,"inven_func"),0) ||
     !tolua_istype(tolua_S,2,LUA_TNUMBER,0) ||
     !tolua_isnoobj(tolua_S,3)
 )
 {
  tolua_error(tolua_S,"#ferror in function 'inven_damage'.");
  return 0;
 }
 else
 {
  inven_func typ = *((inven_func*)  tolua_getusertype(tolua_S,1,0));
  int perc = ((int)  tolua_getnumber(tolua_S,2,0));
  {
   int toluaI_ret = (int)  inven_damage(typ,perc);
   tolua_pushnumber(tolua_S,(long)toluaI_ret);
  }
 }
 return 1;
}

/* function: acid_dam */
static int toluaI_spell_acid_dam00(lua_State* tolua_S)
{
 if (
     !tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
     !tolua_istype(tolua_S,2,LUA_TSTRING,0) ||
     !tolua_isnoobj(tolua_S,3)
 )
 {
  tolua_error(tolua_S,"#ferror in function 'acid_dam'.");
  return 0;
 }
 else
 {
  int dam = ((int)  tolua_getnumber(tolua_S,1,0));
  cptr kb_str = ((cptr)  tolua_getstring(tolua_S,2,0));
  {
   bool toluaI_ret = (bool)  acid_dam(dam,kb_str);
   tolua_pushbool(tolua_S,(int)toluaI_ret);
  }
 }
 return 1;
}

/* function: elec_dam */
static int toluaI_spell_elec_dam00(lua_State* tolua_S)
{
 if (
     !tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
     !tolua_istype(tolua_S,2,LUA_TSTRING,0) ||
     !tolua_isnoobj(tolua_S,3)
 )
 {
  tolua_error(tolua_S,"#ferror in function 'elec_dam'.");
  return 0;
 }
 else
 {
  int dam = ((int)  tolua_getnumber(tolua_S,1,0));
  cptr kb_str = ((cptr)  tolua_getstring(tolua_S,2,0));
  {
   bool toluaI_ret = (bool)  elec_dam(dam,kb_str);
   tolua_pushbool(tolua_S,(int)toluaI_ret);
  }
 }
 return 1;
}

/* function: fire_dam */
static int toluaI_spell_fire_dam00(lua_State* tolua_S)
{
 if (
     !tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
     !tolua_istype(tolua_S,2,LUA_TSTRING,0) ||
     !tolua_isnoobj(tolua_S,3)
 )
 {
  tolua_error(tolua_S,"#ferror in function 'fire_dam'.");
  return 0;
 }
 else
 {
  int dam = ((int)  tolua_getnumber(tolua_S,1,0));
  cptr kb_str = ((cptr)  tolua_getstring(tolua_S,2,0));
  {
   bool toluaI_ret = (bool)  fire_dam(dam,kb_str);
   tolua_pushbool(tolua_S,(int)toluaI_ret);
  }
 }
 return 1;
}

/* function: cold_dam */
static int toluaI_spell_cold_dam00(lua_State* tolua_S)
{
 if (
     !tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
     !tolua_istype(tolua_S,2,LUA_TSTRING,0) ||
     !tolua_isnoobj(tolua_S,3)
 )
 {
  tolua_error(tolua_S,"#ferror in function 'cold_dam'.");
  return 0;
 }
 else
 {
  int dam = ((int)  tolua_getnumber(tolua_S,1,0));
  cptr kb_str = ((cptr)  tolua_getstring(tolua_S,2,0));
  {
   bool toluaI_ret = (bool)  cold_dam(dam,kb_str);
   tolua_pushbool(tolua_S,(int)toluaI_ret);
  }
 }
 return 1;
}

/* function: pois_dam */
static int toluaI_spell_pois_dam00(lua_State* tolua_S)
{
 if (
     !tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
     !tolua_istype(tolua_S,2,LUA_TSTRING,0) ||
     !tolua_istype(tolua_S,3,LUA_TNUMBER,0) ||
     !tolua_isnoobj(tolua_S,4)
 )
 {
  tolua_error(tolua_S,"#ferror in function 'pois_dam'.");
  return 0;
 }
 else
 {
  int dam = ((int)  tolua_getnumber(tolua_S,1,0));
  cptr kb_str = ((cptr)  tolua_getstring(tolua_S,2,0));
  int pois = ((int)  tolua_getnumber(tolua_S,3,0));
  {
   bool toluaI_ret = (bool)  pois_dam(dam,kb_str,pois);
   tolua_pushbool(tolua_S,(int)toluaI_ret);
  }
 }
 return 1;
}

/* function: rustproof */
static int toluaI_spell_rustproof00(lua_State* tolua_S)
{
 if (
     !tolua_isnoobj(tolua_S,1)
 )
 {
  tolua_error(tolua_S,"#ferror in function 'rustproof'.");
  return 0;
 }
 else
 {
  {
   bool toluaI_ret = (bool)  rustproof();
   tolua_pushbool(tolua_S,(int)toluaI_ret);
  }
 }
 return 1;
}

/* function: curse_armor */
static int toluaI_spell_curse_armor00(lua_State* tolua_S)
{
 if (
     !tolua_isnoobj(tolua_S,1)
 )
 {
  tolua_error(tolua_S,"#ferror in function 'curse_armor'.");
  return 0;
 }
 else
 {
  {
   bool toluaI_ret = (bool)  curse_armor();
   tolua_pushbool(tolua_S,(int)toluaI_ret);
  }
 }
 return 1;
}

/* function: curse_weapon */
static int toluaI_spell_curse_weapon00(lua_State* tolua_S)
{
 if (
     !tolua_isnoobj(tolua_S,1)
 )
 {
  tolua_error(tolua_S,"#ferror in function 'curse_weapon'.");
  return 0;
 }
 else
 {
  {
   bool toluaI_ret = (bool)  curse_weapon();
   tolua_pushbool(tolua_S,(int)toluaI_ret);
  }
 }
 return 1;
}

/* function: brand_bolts */
static int toluaI_spell_brand_bolts00(lua_State* tolua_S)
{
 if (
     !tolua_isnoobj(tolua_S,1)
 )
 {
  tolua_error(tolua_S,"#ferror in function 'brand_bolts'.");
  return 0;
 }
 else
 {
  {
   bool toluaI_ret = (bool)  brand_bolts();
   tolua_pushbool(tolua_S,(int)toluaI_ret);
  }
 }
 return 1;
}

/* function: polymorph_monster */
static int toluaI_spell_polymorph_monster00(lua_State* tolua_S)
{
 if (
     !tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
     !tolua_istype(tolua_S,2,LUA_TNUMBER,0) ||
     !tolua_isnoobj(tolua_S,3)
 )
 {
  tolua_error(tolua_S,"#ferror in function 'polymorph_monster'.");
  return 0;
 }
 else
 {
  int x = ((int)  tolua_getnumber(tolua_S,1,0));
  int y = ((int)  tolua_getnumber(tolua_S,2,0));
  {
   bool toluaI_ret = (bool)  polymorph_monster(x,y);
   tolua_pushbool(tolua_S,(int)toluaI_ret);
  }
 }
 return 1;
}

/* function: dimension_door */
static int toluaI_spell_dimension_door00(lua_State* tolua_S)
{
 if (
     !tolua_isnoobj(tolua_S,1)
 )
 {
  tolua_error(tolua_S,"#ferror in function 'dimension_door'.");
  return 0;
 }
 else
 {
  {
   bool toluaI_ret = (bool)  dimension_door();
   tolua_pushbool(tolua_S,(int)toluaI_ret);
  }
 }
 return 1;
}

/* function: map_wilderness */
static int toluaI_spell_map_wilderness00(lua_State* tolua_S)
{
 if (
     !tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
     !tolua_istype(tolua_S,2,LUA_TNUMBER,0) ||
     !tolua_istype(tolua_S,3,LUA_TNUMBER,0) ||
     !tolua_isnoobj(tolua_S,4)
 )
 {
  tolua_error(tolua_S,"#ferror in function 'map_wilderness'.");
  return 0;
 }
 else
 {
  int radius = ((int)  tolua_getnumber(tolua_S,1,0));
  s32b x = ((s32b)  tolua_getnumber(tolua_S,2,0));
  s32b y = ((s32b)  tolua_getnumber(tolua_S,3,0));
  {
   map_wilderness(radius,x,y);
  }
 }
 return 0;
}

/* function: sanity_blast */
static int toluaI_spell_sanity_blast00(lua_State* tolua_S)
{
 if (
     !tolua_istype(tolua_S,1,tolua_tag(tolua_S,"const monster_type"),0) ||
     !tolua_isnoobj(tolua_S,2)
 )
 {
  tolua_error(tolua_S,"#ferror in function 'sanity_blast'.");
  return 0;
 }
 else
 {
  const monster_type* m_ptr = ((const monster_type*)  tolua_getusertype(tolua_S,1,0));
  {
   sanity_blast(m_ptr);
  }
 }
 return 0;
}

/* function: map_area */
static int toluaI_spell_map_area00(lua_State* tolua_S)
{
 if (
     !tolua_isnoobj(tolua_S,1)
 )
 {
  tolua_error(tolua_S,"#ferror in function 'map_area'.");
  return 0;
 }
 else
 {
  {
   map_area();
  }
 }
 return 0;
}

/* function: wiz_lite */
static int toluaI_spell_wiz_lite00(lua_State* tolua_S)
{
 if (
     !tolua_isnoobj(tolua_S,1)
 )
 {
  tolua_error(tolua_S,"#ferror in function 'wiz_lite'.");
  return 0;
 }
 else
 {
  {
   wiz_lite();
  }
 }
 return 0;
}

/* function: wiz_dark */
static int toluaI_spell_wiz_dark00(lua_State* tolua_S)
{
 if (
     !tolua_isnoobj(tolua_S,1)
 )
 {
  tolua_error(tolua_S,"#ferror in function 'wiz_dark'.");
  return 0;
 }
 else
 {
  {
   wiz_dark();
  }
 }
 return 0;
}

/* function: do_cmd_rerate */
static int toluaI_spell_do_cmd_rerate00(lua_State* tolua_S)
{
 if (
     !tolua_isnoobj(tolua_S,1)
 )
 {
  tolua_error(tolua_S,"#ferror in function 'do_cmd_rerate'.");
  return 0;
 }
 else
 {
  {
   do_cmd_rerate();
  }
 }
 return 0;
}

/* Open function */
int tolua_spell_open (lua_State* tolua_S)
{
 tolua_open(tolua_S);
 toluaI_reg_types(tolua_S);
 tolua_constant(tolua_S,NULL,"GF_ELEC",GF_ELEC);
 tolua_constant(tolua_S,NULL,"GF_POIS",GF_POIS);
 tolua_constant(tolua_S,NULL,"GF_ACID",GF_ACID);
 tolua_constant(tolua_S,NULL,"GF_COLD",GF_COLD);
 tolua_constant(tolua_S,NULL,"GF_FIRE",GF_FIRE);
 tolua_constant(tolua_S,NULL,"GF_MISSILE",GF_MISSILE);
 tolua_constant(tolua_S,NULL,"GF_ARROW",GF_ARROW);
 tolua_constant(tolua_S,NULL,"GF_PLASMA",GF_PLASMA);
 tolua_constant(tolua_S,NULL,"GF_WATER",GF_WATER);
 tolua_constant(tolua_S,NULL,"GF_LITE",GF_LITE);
 tolua_constant(tolua_S,NULL,"GF_DARK",GF_DARK);
 tolua_constant(tolua_S,NULL,"GF_LITE_WEAK",GF_LITE_WEAK);
 tolua_constant(tolua_S,NULL,"GF_DARK_WEAK",GF_DARK_WEAK);
 tolua_constant(tolua_S,NULL,"GF_SHARDS",GF_SHARDS);
 tolua_constant(tolua_S,NULL,"GF_SOUND",GF_SOUND);
 tolua_constant(tolua_S,NULL,"GF_CONFUSION",GF_CONFUSION);
 tolua_constant(tolua_S,NULL,"GF_FORCE",GF_FORCE);
 tolua_constant(tolua_S,NULL,"GF_INERTIA",GF_INERTIA);
 tolua_constant(tolua_S,NULL,"GF_MANA",GF_MANA);
 tolua_constant(tolua_S,NULL,"GF_METEOR",GF_METEOR);
 tolua_constant(tolua_S,NULL,"GF_ICE",GF_ICE);
 tolua_constant(tolua_S,NULL,"GF_CHAOS",GF_CHAOS);
 tolua_constant(tolua_S,NULL,"GF_NETHER",GF_NETHER);
 tolua_constant(tolua_S,NULL,"GF_DISENCHANT",GF_DISENCHANT);
 tolua_constant(tolua_S,NULL,"GF_NEXUS",GF_NEXUS);
 tolua_constant(tolua_S,NULL,"GF_TIME",GF_TIME);
 tolua_constant(tolua_S,NULL,"GF_GRAVITY",GF_GRAVITY);
 tolua_constant(tolua_S,NULL,"GF_KILL_WALL",GF_KILL_WALL);
 tolua_constant(tolua_S,NULL,"GF_KILL_DOOR",GF_KILL_DOOR);
 tolua_constant(tolua_S,NULL,"GF_KILL_TRAP",GF_KILL_TRAP);
 tolua_constant(tolua_S,NULL,"GF_MAKE_WALL",GF_MAKE_WALL);
 tolua_constant(tolua_S,NULL,"GF_MAKE_DOOR",GF_MAKE_DOOR);
 tolua_constant(tolua_S,NULL,"GF_MAKE_TRAP",GF_MAKE_TRAP);
 tolua_constant(tolua_S,NULL,"GF_OLD_CLONE",GF_OLD_CLONE);
 tolua_constant(tolua_S,NULL,"GF_OLD_POLY",GF_OLD_POLY);
 tolua_constant(tolua_S,NULL,"GF_OLD_HEAL",GF_OLD_HEAL);
 tolua_constant(tolua_S,NULL,"GF_OLD_SPEED",GF_OLD_SPEED);
 tolua_constant(tolua_S,NULL,"GF_OLD_SLOW",GF_OLD_SLOW);
 tolua_constant(tolua_S,NULL,"GF_OLD_CONF",GF_OLD_CONF);
 tolua_constant(tolua_S,NULL,"GF_OLD_SLEEP",GF_OLD_SLEEP);
 tolua_constant(tolua_S,NULL,"GF_OLD_DRAIN",GF_OLD_DRAIN);
 tolua_constant(tolua_S,NULL,"GF_NEW_DRAIN",GF_NEW_DRAIN);
 tolua_constant(tolua_S,NULL,"GF_AWAY_UNDEAD",GF_AWAY_UNDEAD);
 tolua_constant(tolua_S,NULL,"GF_AWAY_EVIL",GF_AWAY_EVIL);
 tolua_constant(tolua_S,NULL,"GF_AWAY_ALL",GF_AWAY_ALL);
 tolua_constant(tolua_S,NULL,"GF_TURN_UNDEAD",GF_TURN_UNDEAD);
 tolua_constant(tolua_S,NULL,"GF_TURN_EVIL",GF_TURN_EVIL);
 tolua_constant(tolua_S,NULL,"GF_TURN_ALL",GF_TURN_ALL);
 tolua_constant(tolua_S,NULL,"GF_DISP_UNDEAD",GF_DISP_UNDEAD);
 tolua_constant(tolua_S,NULL,"GF_DISP_EVIL",GF_DISP_EVIL);
 tolua_constant(tolua_S,NULL,"GF_DISP_ALL",GF_DISP_ALL);
 tolua_constant(tolua_S,NULL,"GF_DISP_DEMON",GF_DISP_DEMON);
 tolua_constant(tolua_S,NULL,"GF_DISP_LIVING",GF_DISP_LIVING);
 tolua_constant(tolua_S,NULL,"GF_ROCKET",GF_ROCKET);
 tolua_constant(tolua_S,NULL,"GF_NUKE",GF_NUKE);
 tolua_constant(tolua_S,NULL,"GF_MAKE_GLYPH",GF_MAKE_GLYPH);
 tolua_constant(tolua_S,NULL,"GF_STASIS",GF_STASIS);
 tolua_constant(tolua_S,NULL,"GF_STONE_WALL",GF_STONE_WALL);
 tolua_constant(tolua_S,NULL,"GF_DEATH_RAY",GF_DEATH_RAY);
 tolua_constant(tolua_S,NULL,"GF_STUN",GF_STUN);
 tolua_constant(tolua_S,NULL,"GF_HOLY_FIRE",GF_HOLY_FIRE);
 tolua_constant(tolua_S,NULL,"GF_HELL_FIRE",GF_HELL_FIRE);
 tolua_constant(tolua_S,NULL,"GF_DISINTEGRATE",GF_DISINTEGRATE);
 tolua_constant(tolua_S,NULL,"GF_CHARM",GF_CHARM);
 tolua_constant(tolua_S,NULL,"GF_CONTROL_UNDEAD",GF_CONTROL_UNDEAD);
 tolua_constant(tolua_S,NULL,"GF_CONTROL_ANIMAL",GF_CONTROL_ANIMAL);
 tolua_constant(tolua_S,NULL,"GF_PSI",GF_PSI);
 tolua_constant(tolua_S,NULL,"GF_PSI_DRAIN",GF_PSI_DRAIN);
 tolua_constant(tolua_S,NULL,"GF_TELEKINESIS",GF_TELEKINESIS);
 tolua_constant(tolua_S,NULL,"GF_JAM_DOOR",GF_JAM_DOOR);
 tolua_constant(tolua_S,NULL,"GF_DOMINATION",GF_DOMINATION);
 tolua_constant(tolua_S,NULL,"GF_DISP_GOOD",GF_DISP_GOOD);
 tolua_constant(tolua_S,NULL,"MAX_GF",MAX_GF);
 tolua_constant(tolua_S,NULL,"PROJECT_JUMP",PROJECT_JUMP);
 tolua_constant(tolua_S,NULL,"PROJECT_BEAM",PROJECT_BEAM);
 tolua_constant(tolua_S,NULL,"PROJECT_THRU",PROJECT_THRU);
 tolua_constant(tolua_S,NULL,"PROJECT_STOP",PROJECT_STOP);
 tolua_constant(tolua_S,NULL,"PROJECT_GRID",PROJECT_GRID);
 tolua_constant(tolua_S,NULL,"PROJECT_ITEM",PROJECT_ITEM);
 tolua_constant(tolua_S,NULL,"PROJECT_KILL",PROJECT_KILL);
 tolua_constant(tolua_S,NULL,"PROJECT_HIDE",PROJECT_HIDE);
 tolua_constant(tolua_S,NULL,"PROJECT_FRND",PROJECT_FRND);
 tolua_constant(tolua_S,NULL,"PROJECT_MFLD",PROJECT_MFLD);
 tolua_function(tolua_S,NULL,"take_hit",toluaI_spell_take_hit00);
 tolua_function(tolua_S,NULL,"project",toluaI_spell_project00);
 tolua_function(tolua_S,NULL,"message_pain",toluaI_spell_message_pain00);
 tolua_function(tolua_S,NULL,"self_knowledge",toluaI_spell_self_knowledge00);
 tolua_function(tolua_S,NULL,"detect_traps",toluaI_spell_detect_traps00);
 tolua_function(tolua_S,NULL,"detect_doors",toluaI_spell_detect_doors00);
 tolua_function(tolua_S,NULL,"detect_stairs",toluaI_spell_detect_stairs00);
 tolua_function(tolua_S,NULL,"detect_treasure",toluaI_spell_detect_treasure00);
 tolua_function(tolua_S,NULL,"detect_objects_gold",toluaI_spell_detect_objects_gold00);
 tolua_function(tolua_S,NULL,"detect_objects_normal",toluaI_spell_detect_objects_normal00);
 tolua_function(tolua_S,NULL,"detect_objects_magic",toluaI_spell_detect_objects_magic00);
 tolua_function(tolua_S,NULL,"detect_monsters_normal",toluaI_spell_detect_monsters_normal00);
 tolua_function(tolua_S,NULL,"detect_monsters_invis",toluaI_spell_detect_monsters_invis00);
 tolua_function(tolua_S,NULL,"detect_monsters_evil",toluaI_spell_detect_monsters_evil00);
 tolua_function(tolua_S,NULL,"detect_monsters_xxx",toluaI_spell_detect_monsters_xxx00);
 tolua_function(tolua_S,NULL,"detect_monsters_string",toluaI_spell_detect_monsters_string00);
 tolua_function(tolua_S,NULL,"detect_monsters_nonliving",toluaI_spell_detect_monsters_nonliving00);
 tolua_function(tolua_S,NULL,"detect_monsters_living",toluaI_spell_detect_monsters_living00);
 tolua_function(tolua_S,NULL,"detect_all",toluaI_spell_detect_all00);
 tolua_function(tolua_S,NULL,"wall_stone",toluaI_spell_wall_stone00);
 tolua_function(tolua_S,NULL,"speed_monsters",toluaI_spell_speed_monsters00);
 tolua_function(tolua_S,NULL,"slow_monsters",toluaI_spell_slow_monsters00);
 tolua_function(tolua_S,NULL,"sleep_monsters",toluaI_spell_sleep_monsters00);
 tolua_function(tolua_S,NULL,"aggravate_monsters",toluaI_spell_aggravate_monsters00);
 tolua_function(tolua_S,NULL,"genocide",toluaI_spell_genocide00);
 tolua_function(tolua_S,NULL,"mass_genocide",toluaI_spell_mass_genocide00);
 tolua_function(tolua_S,NULL,"probing",toluaI_spell_probing00);
 tolua_function(tolua_S,NULL,"banish_evil",toluaI_spell_banish_evil00);
 tolua_function(tolua_S,NULL,"dispel_evil",toluaI_spell_dispel_evil00);
 tolua_function(tolua_S,NULL,"dispel_good",toluaI_spell_dispel_good00);
 tolua_function(tolua_S,NULL,"dispel_undead",toluaI_spell_dispel_undead00);
 tolua_function(tolua_S,NULL,"dispel_monsters",toluaI_spell_dispel_monsters00);
 tolua_function(tolua_S,NULL,"dispel_living",toluaI_spell_dispel_living00);
 tolua_function(tolua_S,NULL,"dispel_demons",toluaI_spell_dispel_demons00);
 tolua_function(tolua_S,NULL,"raise_dead",toluaI_spell_raise_dead00);
 tolua_function(tolua_S,NULL,"turn_undead",toluaI_spell_turn_undead00);
 tolua_function(tolua_S,NULL,"destroy_area",toluaI_spell_destroy_area00);
 tolua_function(tolua_S,NULL,"earthquake",toluaI_spell_earthquake00);
 tolua_function(tolua_S,NULL,"lite_room",toluaI_spell_lite_room00);
 tolua_function(tolua_S,NULL,"unlite_room",toluaI_spell_unlite_room00);
 tolua_function(tolua_S,NULL,"lite_area",toluaI_spell_lite_area00);
 tolua_function(tolua_S,NULL,"unlite_area",toluaI_spell_unlite_area00);
 tolua_function(tolua_S,NULL,"fire_ball",toluaI_spell_fire_ball00);
 tolua_function(tolua_S,NULL,"fire_bolt",toluaI_spell_fire_bolt00);
 tolua_function(tolua_S,NULL,"call_chaos",toluaI_spell_call_chaos00);
 tolua_function(tolua_S,NULL,"fire_beam",toluaI_spell_fire_beam00);
 tolua_function(tolua_S,NULL,"fire_bolt_or_beam",toluaI_spell_fire_bolt_or_beam00);
 tolua_function(tolua_S,NULL,"lite_line",toluaI_spell_lite_line00);
 tolua_function(tolua_S,NULL,"drain_life",toluaI_spell_drain_life00);
 tolua_function(tolua_S,NULL,"drain_gain_life",toluaI_spell_drain_gain_life00);
 tolua_function(tolua_S,NULL,"death_ray",toluaI_spell_death_ray00);
 tolua_function(tolua_S,NULL,"wall_to_mud",toluaI_spell_wall_to_mud00);
 tolua_function(tolua_S,NULL,"destroy_door",toluaI_spell_destroy_door00);
 tolua_function(tolua_S,NULL,"disarm_trap",toluaI_spell_disarm_trap00);
 tolua_function(tolua_S,NULL,"wizard_lock",toluaI_spell_wizard_lock00);
 tolua_function(tolua_S,NULL,"heal_monster",toluaI_spell_heal_monster00);
 tolua_function(tolua_S,NULL,"speed_monster",toluaI_spell_speed_monster00);
 tolua_function(tolua_S,NULL,"slow_monster",toluaI_spell_slow_monster00);
 tolua_function(tolua_S,NULL,"sleep_monster",toluaI_spell_sleep_monster00);
 tolua_function(tolua_S,NULL,"stasis_monster",toluaI_spell_stasis_monster00);
 tolua_function(tolua_S,NULL,"confuse_monster",toluaI_spell_confuse_monster00);
 tolua_function(tolua_S,NULL,"stun_monster",toluaI_spell_stun_monster00);
 tolua_function(tolua_S,NULL,"fear_monster",toluaI_spell_fear_monster00);
 tolua_function(tolua_S,NULL,"poly_monster",toluaI_spell_poly_monster00);
 tolua_function(tolua_S,NULL,"clone_monster",toluaI_spell_clone_monster00);
 tolua_function(tolua_S,NULL,"teleport_monster",toluaI_spell_teleport_monster00);
 tolua_function(tolua_S,NULL,"door_creation",toluaI_spell_door_creation00);
 tolua_function(tolua_S,NULL,"trap_creation",toluaI_spell_trap_creation00);
 tolua_function(tolua_S,NULL,"glyph_creation",toluaI_spell_glyph_creation00);
 tolua_function(tolua_S,NULL,"destroy_doors_touch",toluaI_spell_destroy_doors_touch00);
 tolua_function(tolua_S,NULL,"sleep_monsters_touch",toluaI_spell_sleep_monsters_touch00);
 tolua_function(tolua_S,NULL,"activate_ty_curse",toluaI_spell_activate_ty_curse00);
 tolua_function(tolua_S,NULL,"activate_hi_summon",toluaI_spell_activate_hi_summon00);
 tolua_function(tolua_S,NULL,"summon_cyber",toluaI_spell_summon_cyber00);
 tolua_function(tolua_S,NULL,"wall_breaker",toluaI_spell_wall_breaker00);
 tolua_function(tolua_S,NULL,"confuse_monsters",toluaI_spell_confuse_monsters00);
 tolua_function(tolua_S,NULL,"charm_monsters",toluaI_spell_charm_monsters00);
 tolua_function(tolua_S,NULL,"charm_animals",toluaI_spell_charm_animals00);
 tolua_function(tolua_S,NULL,"starlite",toluaI_spell_starlite00);
 tolua_function(tolua_S,NULL,"scatter_ball",toluaI_spell_scatter_ball00);
 tolua_function(tolua_S,NULL,"create_food",toluaI_spell_create_food00);
 tolua_function(tolua_S,NULL,"whirlwind_attack",toluaI_spell_whirlwind_attack00);
 tolua_function(tolua_S,NULL,"stun_monsters",toluaI_spell_stun_monsters00);
 tolua_function(tolua_S,NULL,"stasis_monsters",toluaI_spell_stasis_monsters00);
 tolua_function(tolua_S,NULL,"banish_monsters",toluaI_spell_banish_monsters00);
 tolua_function(tolua_S,NULL,"turn_monsters",toluaI_spell_turn_monsters00);
 tolua_function(tolua_S,NULL,"turn_evil",toluaI_spell_turn_evil00);
 tolua_function(tolua_S,NULL,"deathray_monsters",toluaI_spell_deathray_monsters00);
 tolua_function(tolua_S,NULL,"charm_monster",toluaI_spell_charm_monster00);
 tolua_function(tolua_S,NULL,"control_one_undead",toluaI_spell_control_one_undead00);
 tolua_function(tolua_S,NULL,"charm_animal",toluaI_spell_charm_animal00);
 tolua_function(tolua_S,NULL,"mindblast_monsters",toluaI_spell_mindblast_monsters00);
 tolua_function(tolua_S,NULL,"report_magics",toluaI_spell_report_magics00);
 tolua_function(tolua_S,NULL,"teleport_swap",toluaI_spell_teleport_swap00);
 tolua_function(tolua_S,NULL,"project_hook",toluaI_spell_project_hook00);
 tolua_function(tolua_S,NULL,"project_hack",toluaI_spell_project_hack00);
 tolua_function(tolua_S,NULL,"teleport_away",toluaI_spell_teleport_away00);
 tolua_function(tolua_S,NULL,"teleport_to_player",toluaI_spell_teleport_to_player00);
 tolua_function(tolua_S,NULL,"teleport_player",toluaI_spell_teleport_player00);
 tolua_function(tolua_S,NULL,"teleport_player_to",toluaI_spell_teleport_player_to00);
 tolua_function(tolua_S,NULL,"teleport_player_level",toluaI_spell_teleport_player_level00);
 tolua_function(tolua_S,NULL,"recall_player",toluaI_spell_recall_player00);
 tolua_function(tolua_S,NULL,"word_of_recall",toluaI_spell_word_of_recall00);
 tolua_function(tolua_S,NULL,"apply_disenchant",toluaI_spell_apply_disenchant00);
 tolua_function(tolua_S,NULL,"mutate_player",toluaI_spell_mutate_player00);
 tolua_function(tolua_S,NULL,"apply_nexus",toluaI_spell_apply_nexus00);
 tolua_function(tolua_S,NULL,"phlogiston",toluaI_spell_phlogiston00);
 tolua_function(tolua_S,NULL,"brand_weapon",toluaI_spell_brand_weapon00);
 tolua_function(tolua_S,NULL,"call_the_",toluaI_spell_call_the_00);
 tolua_function(tolua_S,NULL,"fetch",toluaI_spell_fetch00);
 tolua_function(tolua_S,NULL,"alter_reality",toluaI_spell_alter_reality00);
 tolua_function(tolua_S,NULL,"warding_glyph",toluaI_spell_warding_glyph00);
 tolua_function(tolua_S,NULL,"explosive_rune",toluaI_spell_explosive_rune00);
 tolua_function(tolua_S,NULL,"identify_pack",toluaI_spell_identify_pack00);
 tolua_function(tolua_S,NULL,"remove_curse",toluaI_spell_remove_curse00);
 tolua_function(tolua_S,NULL,"remove_all_curse",toluaI_spell_remove_all_curse00);
 tolua_function(tolua_S,NULL,"alchemy",toluaI_spell_alchemy00);
 tolua_function(tolua_S,NULL,"stair_creation",toluaI_spell_stair_creation00);
 tolua_function(tolua_S,NULL,"enchant",toluaI_spell_enchant00);
 tolua_function(tolua_S,NULL,"enchant_spell",toluaI_spell_enchant_spell00);
 tolua_function(tolua_S,NULL,"artifact_scroll",toluaI_spell_artifact_scroll00);
 tolua_function(tolua_S,NULL,"ident_spell",toluaI_spell_ident_spell00);
 tolua_function(tolua_S,NULL,"mundane_spell",toluaI_spell_mundane_spell00);
 tolua_function(tolua_S,NULL,"identify_item",toluaI_spell_identify_item00);
 tolua_function(tolua_S,NULL,"identify_fully",toluaI_spell_identify_fully00);
 tolua_function(tolua_S,NULL,"recharge",toluaI_spell_recharge00);
 tolua_function(tolua_S,NULL,"bless_weapon",toluaI_spell_bless_weapon00);
 tolua_function(tolua_S,NULL,"potion_smash_effect",toluaI_spell_potion_smash_effect00);
 tolua_function(tolua_S,NULL,"display_spell_list",toluaI_spell_display_spell_list00);
 tolua_function(tolua_S,NULL,"spell_chance",toluaI_spell_spell_chance00);
 tolua_function(tolua_S,NULL,"spell_okay",toluaI_spell_spell_okay00);
 tolua_function(tolua_S,NULL,"print_spells",toluaI_spell_print_spells00);
 tolua_function(tolua_S,NULL,"hates_acid",toluaI_spell_hates_acid00);
 tolua_function(tolua_S,NULL,"hates_elec",toluaI_spell_hates_elec00);
 tolua_function(tolua_S,NULL,"hates_fire",toluaI_spell_hates_fire00);
 tolua_function(tolua_S,NULL,"hates_cold",toluaI_spell_hates_cold00);
 tolua_function(tolua_S,NULL,"set_acid_destroy",toluaI_spell_set_acid_destroy00);
 tolua_function(tolua_S,NULL,"set_elec_destroy",toluaI_spell_set_elec_destroy00);
 tolua_function(tolua_S,NULL,"set_fire_destroy",toluaI_spell_set_fire_destroy00);
 tolua_function(tolua_S,NULL,"set_cold_destroy",toluaI_spell_set_cold_destroy00);
 tolua_function(tolua_S,NULL,"inven_damage",toluaI_spell_inven_damage00);
 tolua_function(tolua_S,NULL,"acid_dam",toluaI_spell_acid_dam00);
 tolua_function(tolua_S,NULL,"elec_dam",toluaI_spell_elec_dam00);
 tolua_function(tolua_S,NULL,"fire_dam",toluaI_spell_fire_dam00);
 tolua_function(tolua_S,NULL,"cold_dam",toluaI_spell_cold_dam00);
 tolua_function(tolua_S,NULL,"pois_dam",toluaI_spell_pois_dam00);
 tolua_function(tolua_S,NULL,"rustproof",toluaI_spell_rustproof00);
 tolua_function(tolua_S,NULL,"curse_armor",toluaI_spell_curse_armor00);
 tolua_function(tolua_S,NULL,"curse_weapon",toluaI_spell_curse_weapon00);
 tolua_function(tolua_S,NULL,"brand_bolts",toluaI_spell_brand_bolts00);
 tolua_function(tolua_S,NULL,"polymorph_monster",toluaI_spell_polymorph_monster00);
 tolua_function(tolua_S,NULL,"dimension_door",toluaI_spell_dimension_door00);
 tolua_function(tolua_S,NULL,"map_wilderness",toluaI_spell_map_wilderness00);
 tolua_function(tolua_S,NULL,"sanity_blast",toluaI_spell_sanity_blast00);
 tolua_function(tolua_S,NULL,"map_area",toluaI_spell_map_area00);
 tolua_function(tolua_S,NULL,"wiz_lite",toluaI_spell_wiz_lite00);
 tolua_function(tolua_S,NULL,"wiz_dark",toluaI_spell_wiz_dark00);
 tolua_function(tolua_S,NULL,"do_cmd_rerate",toluaI_spell_do_cmd_rerate00);
 return 1;
}
/* Close function */
void tolua_spell_close (lua_State* tolua_S)
{
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"GF_ELEC");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"GF_POIS");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"GF_ACID");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"GF_COLD");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"GF_FIRE");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"GF_MISSILE");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"GF_ARROW");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"GF_PLASMA");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"GF_WATER");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"GF_LITE");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"GF_DARK");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"GF_LITE_WEAK");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"GF_DARK_WEAK");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"GF_SHARDS");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"GF_SOUND");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"GF_CONFUSION");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"GF_FORCE");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"GF_INERTIA");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"GF_MANA");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"GF_METEOR");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"GF_ICE");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"GF_CHAOS");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"GF_NETHER");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"GF_DISENCHANT");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"GF_NEXUS");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"GF_TIME");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"GF_GRAVITY");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"GF_KILL_WALL");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"GF_KILL_DOOR");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"GF_KILL_TRAP");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"GF_MAKE_WALL");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"GF_MAKE_DOOR");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"GF_MAKE_TRAP");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"GF_OLD_CLONE");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"GF_OLD_POLY");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"GF_OLD_HEAL");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"GF_OLD_SPEED");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"GF_OLD_SLOW");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"GF_OLD_CONF");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"GF_OLD_SLEEP");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"GF_OLD_DRAIN");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"GF_NEW_DRAIN");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"GF_AWAY_UNDEAD");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"GF_AWAY_EVIL");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"GF_AWAY_ALL");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"GF_TURN_UNDEAD");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"GF_TURN_EVIL");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"GF_TURN_ALL");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"GF_DISP_UNDEAD");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"GF_DISP_EVIL");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"GF_DISP_ALL");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"GF_DISP_DEMON");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"GF_DISP_LIVING");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"GF_ROCKET");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"GF_NUKE");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"GF_MAKE_GLYPH");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"GF_STASIS");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"GF_STONE_WALL");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"GF_DEATH_RAY");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"GF_STUN");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"GF_HOLY_FIRE");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"GF_HELL_FIRE");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"GF_DISINTEGRATE");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"GF_CHARM");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"GF_CONTROL_UNDEAD");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"GF_CONTROL_ANIMAL");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"GF_PSI");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"GF_PSI_DRAIN");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"GF_TELEKINESIS");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"GF_JAM_DOOR");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"GF_DOMINATION");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"GF_DISP_GOOD");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"MAX_GF");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"PROJECT_JUMP");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"PROJECT_BEAM");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"PROJECT_THRU");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"PROJECT_STOP");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"PROJECT_GRID");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"PROJECT_ITEM");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"PROJECT_KILL");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"PROJECT_HIDE");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"PROJECT_FRND");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"PROJECT_MFLD");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"take_hit");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"project");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"message_pain");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"self_knowledge");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"detect_traps");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"detect_doors");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"detect_stairs");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"detect_treasure");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"detect_objects_gold");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"detect_objects_normal");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"detect_objects_magic");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"detect_monsters_normal");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"detect_monsters_invis");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"detect_monsters_evil");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"detect_monsters_xxx");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"detect_monsters_string");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"detect_monsters_nonliving");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"detect_monsters_living");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"detect_all");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"wall_stone");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"speed_monsters");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"slow_monsters");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"sleep_monsters");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"aggravate_monsters");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"genocide");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"mass_genocide");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"probing");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"banish_evil");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"dispel_evil");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"dispel_good");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"dispel_undead");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"dispel_monsters");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"dispel_living");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"dispel_demons");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"raise_dead");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"turn_undead");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"destroy_area");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"earthquake");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"lite_room");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"unlite_room");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"lite_area");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"unlite_area");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"fire_ball");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"fire_bolt");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"call_chaos");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"fire_beam");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"fire_bolt_or_beam");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"lite_line");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"drain_life");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"drain_gain_life");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"death_ray");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"wall_to_mud");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"destroy_door");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"disarm_trap");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"wizard_lock");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"heal_monster");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"speed_monster");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"slow_monster");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"sleep_monster");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"stasis_monster");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"confuse_monster");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"stun_monster");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"fear_monster");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"poly_monster");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"clone_monster");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"teleport_monster");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"door_creation");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"trap_creation");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"glyph_creation");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"destroy_doors_touch");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"sleep_monsters_touch");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"activate_ty_curse");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"activate_hi_summon");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"summon_cyber");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"wall_breaker");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"confuse_monsters");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"charm_monsters");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"charm_animals");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"starlite");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"scatter_ball");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"create_food");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"whirlwind_attack");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"stun_monsters");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"stasis_monsters");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"banish_monsters");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"turn_monsters");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"turn_evil");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"deathray_monsters");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"charm_monster");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"control_one_undead");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"charm_animal");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"mindblast_monsters");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"report_magics");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"teleport_swap");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"project_hook");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"project_hack");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"teleport_away");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"teleport_to_player");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"teleport_player");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"teleport_player_to");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"teleport_player_level");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"recall_player");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"word_of_recall");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"apply_disenchant");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"mutate_player");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"apply_nexus");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"phlogiston");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"brand_weapon");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"call_the_");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"fetch");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"alter_reality");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"warding_glyph");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"explosive_rune");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"identify_pack");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"remove_curse");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"remove_all_curse");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"alchemy");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"stair_creation");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"enchant");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"enchant_spell");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"artifact_scroll");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"ident_spell");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"mundane_spell");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"identify_item");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"identify_fully");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"recharge");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"bless_weapon");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"potion_smash_effect");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"display_spell_list");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"spell_chance");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"spell_okay");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"print_spells");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"hates_acid");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"hates_elec");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"hates_fire");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"hates_cold");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"set_acid_destroy");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"set_elec_destroy");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"set_fire_destroy");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"set_cold_destroy");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"inven_damage");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"acid_dam");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"elec_dam");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"fire_dam");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"cold_dam");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"pois_dam");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"rustproof");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"curse_armor");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"curse_weapon");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"brand_bolts");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"polymorph_monster");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"dimension_door");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"map_wilderness");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"sanity_blast");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"map_area");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"wiz_lite");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"wiz_dark");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"do_cmd_rerate");
}
