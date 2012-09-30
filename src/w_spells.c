/*
** Lua binding: spells
** Generated automatically by tolua 4.0a - angband on Fri Jan 30 19:43:25 2004.
*/

#include "lua/tolua.h"

/* Exported function */
int tolua_spells_open (lua_State* tolua_S);
void tolua_spells_close (lua_State* tolua_S);

#include "angband.h"
#include "lua.h"

/* function to register type */
static void toluaI_reg_types (lua_State* tolua_S)
{
 tolua_usertype(tolua_S,"magic_power");
 tolua_usertype(tolua_S,"school_type");
 tolua_usertype(tolua_S,"spell_type");
 tolua_usertype(tolua_S,"object_type");
 tolua_usertype(tolua_S,"FILE");
}

/* error messages */
#define TOLUA_ERR_SELF tolua_error(tolua_S,"invalid 'self'")
#define TOLUA_ERR_ASSIGN tolua_error(tolua_S,"#vinvalid type in variable assignment.")

/* get function: project_time */
static int toluaI_get_spells_project_time(lua_State* tolua_S)
{
 tolua_pushnumber(tolua_S,(long)project_time);
 return 1;
}

/* set function: project_time */
static int toluaI_set_spells_project_time(lua_State* tolua_S)
{
 if (!tolua_istype(tolua_S,1,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  project_time = ((int)  tolua_getnumber(tolua_S,1,0));
 return 0;
}

/* function: teleport_player_directed */
static int toluaI_spells_teleport_player_directed00(lua_State* tolua_S)
{
 if (
 !tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
 !tolua_istype(tolua_S,2,LUA_TNUMBER,0) ||
 !tolua_isnoobj(tolua_S,3)
 )
 goto tolua_lerror;
 else
 {
  int rad = ((int)  tolua_getnumber(tolua_S,1,0));
  int dir = ((int)  tolua_getnumber(tolua_S,2,0));
 {
  teleport_player_directed(rad,dir);
 }
 }
 return 0;
tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'teleport_player_directed'.");
 return 0;
}

/* function: teleport_away */
static int toluaI_spells_teleport_away00(lua_State* tolua_S)
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
  int dis = ((int)  tolua_getnumber(tolua_S,2,0));
 {
  teleport_away(m_idx,dis);
 }
 }
 return 0;
tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'teleport_away'.");
 return 0;
}

/* function: teleport_player */
static int toluaI_spells_teleport_player00(lua_State* tolua_S)
{
 if (
 !tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
 !tolua_isnoobj(tolua_S,2)
 )
 goto tolua_lerror;
 else
 {
  int dis = ((int)  tolua_getnumber(tolua_S,1,0));
 {
  teleport_player(dis);
 }
 }
 return 0;
tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'teleport_player'.");
 return 0;
}

/* function: teleport_player_to */
static int toluaI_spells_teleport_player_to00(lua_State* tolua_S)
{
 if (
 !tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
 !tolua_istype(tolua_S,2,LUA_TNUMBER,0) ||
 !tolua_isnoobj(tolua_S,3)
 )
 goto tolua_lerror;
 else
 {
  int ny = ((int)  tolua_getnumber(tolua_S,1,0));
  int nx = ((int)  tolua_getnumber(tolua_S,2,0));
 {
  teleport_player_to(ny,nx);
 }
 }
 return 0;
tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'teleport_player_to'.");
 return 0;
}

/* function: teleport_monster_to */
static int toluaI_spells_teleport_monster_to00(lua_State* tolua_S)
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
  int m_idx = ((int)  tolua_getnumber(tolua_S,1,0));
  int ny = ((int)  tolua_getnumber(tolua_S,2,0));
  int nx = ((int)  tolua_getnumber(tolua_S,3,0));
 {
  teleport_monster_to(m_idx,ny,nx);
 }
 }
 return 0;
tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'teleport_monster_to'.");
 return 0;
}

/* function: teleport_monster */
static int toluaI_spells_teleport_monster00(lua_State* tolua_S)
{
 if (
 !tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
 !tolua_isnoobj(tolua_S,2)
 )
 goto tolua_lerror;
 else
 {
  int dir = ((int)  tolua_getnumber(tolua_S,1,0));
 {
  bool toluaI_ret = (bool)  teleport_monster(dir);
 tolua_pushnumber(tolua_S,(long)toluaI_ret);
 }
 }
 return 1;
tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'teleport_monster'.");
 return 0;
}

/* function: teleport_player_level */
static int toluaI_spells_teleport_player_level00(lua_State* tolua_S)
{
 if (
 !tolua_isnoobj(tolua_S,1)
 )
 goto tolua_lerror;
 else
 {
 {
  teleport_player_level();
 }
 }
 return 0;
tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'teleport_player_level'.");
 return 0;
}

/* function: fetch */
static int toluaI_spells_fetch00(lua_State* tolua_S)
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
  int dir = ((int)  tolua_getnumber(tolua_S,1,0));
  int wgt = ((int)  tolua_getnumber(tolua_S,2,0));
  bool require_los = ((bool)  tolua_getnumber(tolua_S,3,0));
 {
  fetch(dir,wgt,require_los);
 }
 }
 return 0;
tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'fetch'.");
 return 0;
}

/* function: recall_player */
static int toluaI_spells_recall_player00(lua_State* tolua_S)
{
 if (
 !tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
 !tolua_istype(tolua_S,2,LUA_TNUMBER,0) ||
 !tolua_isnoobj(tolua_S,3)
 )
 goto tolua_lerror;
 else
 {
  int d = ((int)  tolua_getnumber(tolua_S,1,0));
  int f = ((int)  tolua_getnumber(tolua_S,2,0));
 {
  recall_player(d,f);
 }
 }
 return 0;
tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'recall_player'.");
 return 0;
}

/* function: take_hit */
static int toluaI_spells_take_hit00(lua_State* tolua_S)
{
 if (
 !tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
 !tolua_istype(tolua_S,2,LUA_TSTRING,0) ||
 !tolua_isnoobj(tolua_S,3)
 )
 goto tolua_lerror;
 else
 {
  int damage = ((int)  tolua_getnumber(tolua_S,1,0));
  cptr kb_str = ((cptr)  tolua_getstring(tolua_S,2,0));
 {
  take_hit(damage,kb_str);
 }
 }
 return 0;
tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'take_hit'.");
 return 0;
}

/* function: take_sanity_hit */
static int toluaI_spells_take_sanity_hit00(lua_State* tolua_S)
{
 if (
 !tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
 !tolua_istype(tolua_S,2,LUA_TSTRING,0) ||
 !tolua_isnoobj(tolua_S,3)
 )
 goto tolua_lerror;
 else
 {
  int damage = ((int)  tolua_getnumber(tolua_S,1,0));
  cptr hit_from = ((cptr)  tolua_getstring(tolua_S,2,0));
 {
  take_sanity_hit(damage,hit_from);
 }
 }
 return 0;
tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'take_sanity_hit'.");
 return 0;
}

/* function: project */
static int toluaI_spells_project00(lua_State* tolua_S)
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
 goto tolua_lerror;
 else
 {
  int who = ((int)  tolua_getnumber(tolua_S,1,0));
  int rad = ((int)  tolua_getnumber(tolua_S,2,0));
  int y = ((int)  tolua_getnumber(tolua_S,3,0));
  int x = ((int)  tolua_getnumber(tolua_S,4,0));
  int dam = ((int)  tolua_getnumber(tolua_S,5,0));
  int typ = ((int)  tolua_getnumber(tolua_S,6,0));
  int flg = ((int)  tolua_getnumber(tolua_S,7,0));
 {
  bool toluaI_ret = (bool)  project(who,rad,y,x,dam,typ,flg);
 tolua_pushnumber(tolua_S,(long)toluaI_ret);
 }
 }
 return 1;
tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'project'.");
 return 0;
}

/* function: corrupt_player */
static int toluaI_spells_corrupt_player00(lua_State* tolua_S)
{
 if (
 !tolua_isnoobj(tolua_S,1)
 )
 goto tolua_lerror;
 else
 {
 {
  corrupt_player();
 }
 }
 return 0;
tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'corrupt_player'.");
 return 0;
}

/* function: grow_things */
static int toluaI_spells_grow_things00(lua_State* tolua_S)
{
 if (
 !tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
 !tolua_istype(tolua_S,2,LUA_TNUMBER,0) ||
 !tolua_isnoobj(tolua_S,3)
 )
 goto tolua_lerror;
 else
 {
  s16b type = ((s16b)  tolua_getnumber(tolua_S,1,0));
  int rad = ((int)  tolua_getnumber(tolua_S,2,0));
 {
  grow_things(type,rad);
 }
 }
 return 0;
tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'grow_things'.");
 return 0;
}

/* function: grow_grass */
static int toluaI_spells_grow_grass00(lua_State* tolua_S)
{
 if (
 !tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
 !tolua_isnoobj(tolua_S,2)
 )
 goto tolua_lerror;
 else
 {
  int rad = ((int)  tolua_getnumber(tolua_S,1,0));
 {
  grow_grass(rad);
 }
 }
 return 0;
tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'grow_grass'.");
 return 0;
}

/* function: grow_trees */
static int toluaI_spells_grow_trees00(lua_State* tolua_S)
{
 if (
 !tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
 !tolua_isnoobj(tolua_S,2)
 )
 goto tolua_lerror;
 else
 {
  int rad = ((int)  tolua_getnumber(tolua_S,1,0));
 {
  grow_trees(rad);
 }
 }
 return 0;
tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'grow_trees'.");
 return 0;
}

/* function: hp_player */
static int toluaI_spells_hp_player00(lua_State* tolua_S)
{
 if (
 !tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
 !tolua_isnoobj(tolua_S,2)
 )
 goto tolua_lerror;
 else
 {
  int num = ((int)  tolua_getnumber(tolua_S,1,0));
 {
  bool toluaI_ret = (bool)  hp_player(num);
 tolua_pushnumber(tolua_S,(long)toluaI_ret);
 }
 }
 return 1;
tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'hp_player'.");
 return 0;
}

/* function: heal_insanity */
static int toluaI_spells_heal_insanity00(lua_State* tolua_S)
{
 if (
 !tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
 !tolua_isnoobj(tolua_S,2)
 )
 goto tolua_lerror;
 else
 {
  int val = ((int)  tolua_getnumber(tolua_S,1,0));
 {
  bool toluaI_ret = (bool)  heal_insanity(val);
 tolua_pushnumber(tolua_S,(long)toluaI_ret);
 }
 }
 return 1;
tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'heal_insanity'.");
 return 0;
}

/* function: warding_glyph */
static int toluaI_spells_warding_glyph00(lua_State* tolua_S)
{
 if (
 !tolua_isnoobj(tolua_S,1)
 )
 goto tolua_lerror;
 else
 {
 {
  warding_glyph();
 }
 }
 return 0;
tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'warding_glyph'.");
 return 0;
}

/* function: explosive_rune */
static int toluaI_spells_explosive_rune00(lua_State* tolua_S)
{
 if (
 !tolua_isnoobj(tolua_S,1)
 )
 goto tolua_lerror;
 else
 {
 {
  explosive_rune();
 }
 }
 return 0;
tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'explosive_rune'.");
 return 0;
}

/* function: do_dec_stat */
static int toluaI_spells_do_dec_stat00(lua_State* tolua_S)
{
 if (
 !tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
 !tolua_istype(tolua_S,2,LUA_TNUMBER,0) ||
 !tolua_isnoobj(tolua_S,3)
 )
 goto tolua_lerror;
 else
 {
  int stat = ((int)  tolua_getnumber(tolua_S,1,0));
  int mode = ((int)  tolua_getnumber(tolua_S,2,0));
 {
  bool toluaI_ret = (bool)  do_dec_stat(stat,mode);
 tolua_pushnumber(tolua_S,(long)toluaI_ret);
 }
 }
 return 1;
tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'do_dec_stat'.");
 return 0;
}

/* function: do_res_stat */
static int toluaI_spells_do_res_stat00(lua_State* tolua_S)
{
 if (
 !tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
 !tolua_istype(tolua_S,2,LUA_TNUMBER,0) ||
 !tolua_isnoobj(tolua_S,3)
 )
 goto tolua_lerror;
 else
 {
  int stat = ((int)  tolua_getnumber(tolua_S,1,0));
  bool full = ((bool)  tolua_getnumber(tolua_S,2,0));
 {
  bool toluaI_ret = (bool)  do_res_stat(stat,full);
 tolua_pushnumber(tolua_S,(long)toluaI_ret);
 }
 }
 return 1;
tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'do_res_stat'.");
 return 0;
}

/* function: do_inc_stat */
static int toluaI_spells_do_inc_stat00(lua_State* tolua_S)
{
 if (
 !tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
 !tolua_isnoobj(tolua_S,2)
 )
 goto tolua_lerror;
 else
 {
  int stat = ((int)  tolua_getnumber(tolua_S,1,0));
 {
  bool toluaI_ret = (bool)  do_inc_stat(stat);
 tolua_pushnumber(tolua_S,(long)toluaI_ret);
 }
 }
 return 1;
tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'do_inc_stat'.");
 return 0;
}

/* function: identify_pack */
static int toluaI_spells_identify_pack00(lua_State* tolua_S)
{
 if (
 !tolua_isnoobj(tolua_S,1)
 )
 goto tolua_lerror;
 else
 {
 {
  identify_pack();
 }
 }
 return 0;
tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'identify_pack'.");
 return 0;
}

/* function: remove_curse */
static int toluaI_spells_remove_curse00(lua_State* tolua_S)
{
 if (
 !tolua_isnoobj(tolua_S,1)
 )
 goto tolua_lerror;
 else
 {
 {
  bool toluaI_ret = (bool)  remove_curse();
 tolua_pushnumber(tolua_S,(long)toluaI_ret);
 }
 }
 return 1;
tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'remove_curse'.");
 return 0;
}

/* function: remove_all_curse */
static int toluaI_spells_remove_all_curse00(lua_State* tolua_S)
{
 if (
 !tolua_isnoobj(tolua_S,1)
 )
 goto tolua_lerror;
 else
 {
 {
  bool toluaI_ret = (bool)  remove_all_curse();
 tolua_pushnumber(tolua_S,(long)toluaI_ret);
 }
 }
 return 1;
tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'remove_all_curse'.");
 return 0;
}

/* function: restore_level */
static int toluaI_spells_restore_level00(lua_State* tolua_S)
{
 if (
 !tolua_isnoobj(tolua_S,1)
 )
 goto tolua_lerror;
 else
 {
 {
  bool toluaI_ret = (bool)  restore_level();
 tolua_pushnumber(tolua_S,(long)toluaI_ret);
 }
 }
 return 1;
tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'restore_level'.");
 return 0;
}

/* function: self_knowledge */
static int toluaI_spells_self_knowledge00(lua_State* tolua_S)
{
 if (
 !tolua_istype(tolua_S,1,tolua_tag(tolua_S,"FILE"),1) ||
 !tolua_isnoobj(tolua_S,2)
 )
 goto tolua_lerror;
 else
 {
  FILE* fff = ((FILE*)  tolua_getusertype(tolua_S,1,NULL));
 {
  self_knowledge(fff);
 }
 }
 return 0;
tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'self_knowledge'.");
 return 0;
}

/* function: lose_all_info */
static int toluaI_spells_lose_all_info00(lua_State* tolua_S)
{
 if (
 !tolua_isnoobj(tolua_S,1)
 )
 goto tolua_lerror;
 else
 {
 {
  bool toluaI_ret = (bool)  lose_all_info();
 tolua_pushnumber(tolua_S,(long)toluaI_ret);
 }
 }
 return 1;
tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'lose_all_info'.");
 return 0;
}

/* function: detect_traps */
static int toluaI_spells_detect_traps00(lua_State* tolua_S)
{
 if (
 !tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
 !tolua_isnoobj(tolua_S,2)
 )
 goto tolua_lerror;
 else
 {
  int rad = ((int)  tolua_getnumber(tolua_S,1,0));
 {
  bool toluaI_ret = (bool)  detect_traps(rad);
 tolua_pushnumber(tolua_S,(long)toluaI_ret);
 }
 }
 return 1;
tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'detect_traps'.");
 return 0;
}

/* function: detect_doors */
static int toluaI_spells_detect_doors00(lua_State* tolua_S)
{
 if (
 !tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
 !tolua_isnoobj(tolua_S,2)
 )
 goto tolua_lerror;
 else
 {
  int rad = ((int)  tolua_getnumber(tolua_S,1,0));
 {
  bool toluaI_ret = (bool)  detect_doors(rad);
 tolua_pushnumber(tolua_S,(long)toluaI_ret);
 }
 }
 return 1;
tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'detect_doors'.");
 return 0;
}

/* function: detect_stairs */
static int toluaI_spells_detect_stairs00(lua_State* tolua_S)
{
 if (
 !tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
 !tolua_isnoobj(tolua_S,2)
 )
 goto tolua_lerror;
 else
 {
  int rad = ((int)  tolua_getnumber(tolua_S,1,0));
 {
  bool toluaI_ret = (bool)  detect_stairs(rad);
 tolua_pushnumber(tolua_S,(long)toluaI_ret);
 }
 }
 return 1;
tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'detect_stairs'.");
 return 0;
}

/* function: detect_treasure */
static int toluaI_spells_detect_treasure00(lua_State* tolua_S)
{
 if (
 !tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
 !tolua_isnoobj(tolua_S,2)
 )
 goto tolua_lerror;
 else
 {
  int rad = ((int)  tolua_getnumber(tolua_S,1,0));
 {
  bool toluaI_ret = (bool)  detect_treasure(rad);
 tolua_pushnumber(tolua_S,(long)toluaI_ret);
 }
 }
 return 1;
tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'detect_treasure'.");
 return 0;
}

/* get function: hack_no_detect_message */
static int toluaI_get_spells_hack_no_detect_message(lua_State* tolua_S)
{
 tolua_pushnumber(tolua_S,(long)hack_no_detect_message);
 return 1;
}

/* set function: hack_no_detect_message */
static int toluaI_set_spells_hack_no_detect_message(lua_State* tolua_S)
{
 if (!tolua_istype(tolua_S,1,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  hack_no_detect_message = ((bool)  tolua_getnumber(tolua_S,1,0));
 return 0;
}

/* function: detect_objects_gold */
static int toluaI_spells_detect_objects_gold00(lua_State* tolua_S)
{
 if (
 !tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
 !tolua_isnoobj(tolua_S,2)
 )
 goto tolua_lerror;
 else
 {
  int rad = ((int)  tolua_getnumber(tolua_S,1,0));
 {
  bool toluaI_ret = (bool)  detect_objects_gold(rad);
 tolua_pushnumber(tolua_S,(long)toluaI_ret);
 }
 }
 return 1;
tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'detect_objects_gold'.");
 return 0;
}

/* function: detect_objects_normal */
static int toluaI_spells_detect_objects_normal00(lua_State* tolua_S)
{
 if (
 !tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
 !tolua_isnoobj(tolua_S,2)
 )
 goto tolua_lerror;
 else
 {
  int rad = ((int)  tolua_getnumber(tolua_S,1,0));
 {
  bool toluaI_ret = (bool)  detect_objects_normal(rad);
 tolua_pushnumber(tolua_S,(long)toluaI_ret);
 }
 }
 return 1;
tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'detect_objects_normal'.");
 return 0;
}

/* function: detect_objects_magic */
static int toluaI_spells_detect_objects_magic00(lua_State* tolua_S)
{
 if (
 !tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
 !tolua_isnoobj(tolua_S,2)
 )
 goto tolua_lerror;
 else
 {
  int rad = ((int)  tolua_getnumber(tolua_S,1,0));
 {
  bool toluaI_ret = (bool)  detect_objects_magic(rad);
 tolua_pushnumber(tolua_S,(long)toluaI_ret);
 }
 }
 return 1;
tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'detect_objects_magic'.");
 return 0;
}

/* function: detect_monsters_normal */
static int toluaI_spells_detect_monsters_normal00(lua_State* tolua_S)
{
 if (
 !tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
 !tolua_isnoobj(tolua_S,2)
 )
 goto tolua_lerror;
 else
 {
  int rad = ((int)  tolua_getnumber(tolua_S,1,0));
 {
  bool toluaI_ret = (bool)  detect_monsters_normal(rad);
 tolua_pushnumber(tolua_S,(long)toluaI_ret);
 }
 }
 return 1;
tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'detect_monsters_normal'.");
 return 0;
}

/* function: detect_monsters_invis */
static int toluaI_spells_detect_monsters_invis00(lua_State* tolua_S)
{
 if (
 !tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
 !tolua_isnoobj(tolua_S,2)
 )
 goto tolua_lerror;
 else
 {
  int rad = ((int)  tolua_getnumber(tolua_S,1,0));
 {
  bool toluaI_ret = (bool)  detect_monsters_invis(rad);
 tolua_pushnumber(tolua_S,(long)toluaI_ret);
 }
 }
 return 1;
tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'detect_monsters_invis'.");
 return 0;
}

/* function: detect_monsters_evil */
static int toluaI_spells_detect_monsters_evil00(lua_State* tolua_S)
{
 if (
 !tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
 !tolua_isnoobj(tolua_S,2)
 )
 goto tolua_lerror;
 else
 {
  int rad = ((int)  tolua_getnumber(tolua_S,1,0));
 {
  bool toluaI_ret = (bool)  detect_monsters_evil(rad);
 tolua_pushnumber(tolua_S,(long)toluaI_ret);
 }
 }
 return 1;
tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'detect_monsters_evil'.");
 return 0;
}

/* function: detect_monsters_good */
static int toluaI_spells_detect_monsters_good00(lua_State* tolua_S)
{
 if (
 !tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
 !tolua_isnoobj(tolua_S,2)
 )
 goto tolua_lerror;
 else
 {
  int rad = ((int)  tolua_getnumber(tolua_S,1,0));
 {
  bool toluaI_ret = (bool)  detect_monsters_good(rad);
 tolua_pushnumber(tolua_S,(long)toluaI_ret);
 }
 }
 return 1;
tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'detect_monsters_good'.");
 return 0;
}

/* function: detect_monsters_xxx */
static int toluaI_spells_detect_monsters_xxx00(lua_State* tolua_S)
{
 if (
 !tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
 !tolua_istype(tolua_S,2,LUA_TNUMBER,0) ||
 !tolua_isnoobj(tolua_S,3)
 )
 goto tolua_lerror;
 else
 {
  u32b match_flag = ((u32b)  tolua_getnumber(tolua_S,1,0));
  int rad = ((int)  tolua_getnumber(tolua_S,2,0));
 {
  bool toluaI_ret = (bool)  detect_monsters_xxx(match_flag,rad);
 tolua_pushnumber(tolua_S,(long)toluaI_ret);
 }
 }
 return 1;
tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'detect_monsters_xxx'.");
 return 0;
}

/* function: detect_monsters_string */
static int toluaI_spells_detect_monsters_string00(lua_State* tolua_S)
{
 if (
 !tolua_istype(tolua_S,1,LUA_TSTRING,0) ||
 !tolua_istype(tolua_S,2,LUA_TNUMBER,0) ||
 !tolua_isnoobj(tolua_S,3)
 )
 goto tolua_lerror;
 else
 {
  cptr chars = ((cptr)  tolua_getstring(tolua_S,1,0));
  int rad = ((int)  tolua_getnumber(tolua_S,2,0));
 {
  bool toluaI_ret = (bool)  detect_monsters_string(chars,rad);
 tolua_pushnumber(tolua_S,(long)toluaI_ret);
 }
 }
 return 1;
tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'detect_monsters_string'.");
 return 0;
}

/* function: detect_monsters_nonliving */
static int toluaI_spells_detect_monsters_nonliving00(lua_State* tolua_S)
{
 if (
 !tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
 !tolua_isnoobj(tolua_S,2)
 )
 goto tolua_lerror;
 else
 {
  int rad = ((int)  tolua_getnumber(tolua_S,1,0));
 {
  bool toluaI_ret = (bool)  detect_monsters_nonliving(rad);
 tolua_pushnumber(tolua_S,(long)toluaI_ret);
 }
 }
 return 1;
tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'detect_monsters_nonliving'.");
 return 0;
}

/* function: detect_all */
static int toluaI_spells_detect_all00(lua_State* tolua_S)
{
 if (
 !tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
 !tolua_isnoobj(tolua_S,2)
 )
 goto tolua_lerror;
 else
 {
  int rad = ((int)  tolua_getnumber(tolua_S,1,0));
 {
  bool toluaI_ret = (bool)  detect_all(rad);
 tolua_pushnumber(tolua_S,(long)toluaI_ret);
 }
 }
 return 1;
tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'detect_all'.");
 return 0;
}

/* function: stair_creation */
static int toluaI_spells_stair_creation00(lua_State* tolua_S)
{
 if (
 !tolua_isnoobj(tolua_S,1)
 )
 goto tolua_lerror;
 else
 {
 {
  stair_creation();
 }
 }
 return 0;
tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'stair_creation'.");
 return 0;
}

/* function: tgt_pt */
static int toluaI_spells_tgt_pt00(lua_State* tolua_S)
{
 if (
 !tolua_istype(tolua_S,1,LUA_TNUMBER,1) ||
 !tolua_istype(tolua_S,2,LUA_TNUMBER,1) ||
 !tolua_isnoobj(tolua_S,3)
 )
 goto tolua_lerror;
 else
 {
  int x = ((int)  tolua_getnumber(tolua_S,1,0));
  int y = ((int)  tolua_getnumber(tolua_S,2,0));
 {
  bool toluaI_ret = (bool)  tgt_pt(&x,&y);
 tolua_pushnumber(tolua_S,(long)toluaI_ret);
 tolua_pushnumber(tolua_S,(long)x);
 tolua_pushnumber(tolua_S,(long)y);
 }
 }
 return 3;
tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'tgt_pt'.");
 return 0;
}

/* function: wall_stone */
static int toluaI_spells_wall_stone00(lua_State* tolua_S)
{
 if (
 !tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
 !tolua_istype(tolua_S,2,LUA_TNUMBER,0) ||
 !tolua_isnoobj(tolua_S,3)
 )
 goto tolua_lerror;
 else
 {
  int y = ((int)  tolua_getnumber(tolua_S,1,0));
  int x = ((int)  tolua_getnumber(tolua_S,2,0));
 {
  bool toluaI_ret = (bool)  wall_stone(y,x);
 tolua_pushnumber(tolua_S,(long)toluaI_ret);
 }
 }
 return 1;
tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'wall_stone'.");
 return 0;
}

/* function: create_artifact */
static int toluaI_spells_create_artifact00(lua_State* tolua_S)
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
  bool a_scroll = ((bool)  tolua_getnumber(tolua_S,2,0));
  bool get_name = ((bool)  tolua_getnumber(tolua_S,3,0));
 {
  bool toluaI_ret = (bool)  create_artifact(o_ptr,a_scroll,get_name);
 tolua_pushnumber(tolua_S,(long)toluaI_ret);
 }
 }
 return 1;
tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'create_artifact'.");
 return 0;
}

/* function: wall_to_mud */
static int toluaI_spells_wall_to_mud00(lua_State* tolua_S)
{
 if (
 !tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
 !tolua_isnoobj(tolua_S,2)
 )
 goto tolua_lerror;
 else
 {
  int dir = ((int)  tolua_getnumber(tolua_S,1,0));
 {
  bool toluaI_ret = (bool)  wall_to_mud(dir);
 tolua_pushnumber(tolua_S,(long)toluaI_ret);
 }
 }
 return 1;
tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'wall_to_mud'.");
 return 0;
}

/* function: ident_spell */
static int toluaI_spells_ident_spell00(lua_State* tolua_S)
{
 if (
 !tolua_isnoobj(tolua_S,1)
 )
 goto tolua_lerror;
 else
 {
 {
  bool toluaI_ret = (bool)  ident_spell();
 tolua_pushnumber(tolua_S,(long)toluaI_ret);
 }
 }
 return 1;
tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'ident_spell'.");
 return 0;
}

/* function: identify_fully */
static int toluaI_spells_identify_fully00(lua_State* tolua_S)
{
 if (
 !tolua_isnoobj(tolua_S,1)
 )
 goto tolua_lerror;
 else
 {
 {
  bool toluaI_ret = (bool)  identify_fully();
 tolua_pushnumber(tolua_S,(long)toluaI_ret);
 }
 }
 return 1;
tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'identify_fully'.");
 return 0;
}

/* function: recharge */
static int toluaI_spells_recharge00(lua_State* tolua_S)
{
 if (
 !tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
 !tolua_isnoobj(tolua_S,2)
 )
 goto tolua_lerror;
 else
 {
  int num = ((int)  tolua_getnumber(tolua_S,1,0));
 {
  bool toluaI_ret = (bool)  recharge(num);
 tolua_pushnumber(tolua_S,(long)toluaI_ret);
 }
 }
 return 1;
tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'recharge'.");
 return 0;
}

/* function: aggravate_monsters */
static int toluaI_spells_aggravate_monsters00(lua_State* tolua_S)
{
 if (
 !tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
 !tolua_isnoobj(tolua_S,2)
 )
 goto tolua_lerror;
 else
 {
  int who = ((int)  tolua_getnumber(tolua_S,1,0));
 {
  aggravate_monsters(who);
 }
 }
 return 0;
tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'aggravate_monsters'.");
 return 0;
}

/* function: genocide_aux */
static int toluaI_spells_genocide_aux00(lua_State* tolua_S)
{
 if (
 !tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
 !tolua_istype(tolua_S,2,LUA_TNUMBER,0) ||
 !tolua_isnoobj(tolua_S,3)
 )
 goto tolua_lerror;
 else
 {
  bool player_cast = ((bool)  tolua_getnumber(tolua_S,1,0));
  char typ = ((char)  tolua_getnumber(tolua_S,2,0));
 {
  bool toluaI_ret = (bool)  genocide_aux(player_cast,typ);
 tolua_pushnumber(tolua_S,(long)toluaI_ret);
 }
 }
 return 1;
tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'genocide_aux'.");
 return 0;
}

/* function: genocide */
static int toluaI_spells_genocide00(lua_State* tolua_S)
{
 if (
 !tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
 !tolua_isnoobj(tolua_S,2)
 )
 goto tolua_lerror;
 else
 {
  bool player_cast = ((bool)  tolua_getnumber(tolua_S,1,0));
 {
  bool toluaI_ret = (bool)  genocide(player_cast);
 tolua_pushnumber(tolua_S,(long)toluaI_ret);
 }
 }
 return 1;
tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'genocide'.");
 return 0;
}

/* function: mass_genocide */
static int toluaI_spells_mass_genocide00(lua_State* tolua_S)
{
 if (
 !tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
 !tolua_isnoobj(tolua_S,2)
 )
 goto tolua_lerror;
 else
 {
  bool player_cast = ((bool)  tolua_getnumber(tolua_S,1,0));
 {
  bool toluaI_ret = (bool)  mass_genocide(player_cast);
 tolua_pushnumber(tolua_S,(long)toluaI_ret);
 }
 }
 return 1;
tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'mass_genocide'.");
 return 0;
}

/* function: probing */
static int toluaI_spells_probing00(lua_State* tolua_S)
{
 if (
 !tolua_isnoobj(tolua_S,1)
 )
 goto tolua_lerror;
 else
 {
 {
  bool toluaI_ret = (bool)  probing();
 tolua_pushnumber(tolua_S,(long)toluaI_ret);
 }
 }
 return 1;
tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'probing'.");
 return 0;
}

/* function: banish_evil */
static int toluaI_spells_banish_evil00(lua_State* tolua_S)
{
 if (
 !tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
 !tolua_isnoobj(tolua_S,2)
 )
 goto tolua_lerror;
 else
 {
  int dist = ((int)  tolua_getnumber(tolua_S,1,0));
 {
  bool toluaI_ret = (bool)  banish_evil(dist);
 tolua_pushnumber(tolua_S,(long)toluaI_ret);
 }
 }
 return 1;
tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'banish_evil'.");
 return 0;
}

/* function: dispel_evil */
static int toluaI_spells_dispel_evil00(lua_State* tolua_S)
{
 if (
 !tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
 !tolua_isnoobj(tolua_S,2)
 )
 goto tolua_lerror;
 else
 {
  int dam = ((int)  tolua_getnumber(tolua_S,1,0));
 {
  bool toluaI_ret = (bool)  dispel_evil(dam);
 tolua_pushnumber(tolua_S,(long)toluaI_ret);
 }
 }
 return 1;
tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'dispel_evil'.");
 return 0;
}

/* function: dispel_good */
static int toluaI_spells_dispel_good00(lua_State* tolua_S)
{
 if (
 !tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
 !tolua_isnoobj(tolua_S,2)
 )
 goto tolua_lerror;
 else
 {
  int dam = ((int)  tolua_getnumber(tolua_S,1,0));
 {
  bool toluaI_ret = (bool)  dispel_good(dam);
 tolua_pushnumber(tolua_S,(long)toluaI_ret);
 }
 }
 return 1;
tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'dispel_good'.");
 return 0;
}

/* function: dispel_undead */
static int toluaI_spells_dispel_undead00(lua_State* tolua_S)
{
 if (
 !tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
 !tolua_isnoobj(tolua_S,2)
 )
 goto tolua_lerror;
 else
 {
  int dam = ((int)  tolua_getnumber(tolua_S,1,0));
 {
  bool toluaI_ret = (bool)  dispel_undead(dam);
 tolua_pushnumber(tolua_S,(long)toluaI_ret);
 }
 }
 return 1;
tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'dispel_undead'.");
 return 0;
}

/* function: dispel_monsters */
static int toluaI_spells_dispel_monsters00(lua_State* tolua_S)
{
 if (
 !tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
 !tolua_isnoobj(tolua_S,2)
 )
 goto tolua_lerror;
 else
 {
  int dam = ((int)  tolua_getnumber(tolua_S,1,0));
 {
  bool toluaI_ret = (bool)  dispel_monsters(dam);
 tolua_pushnumber(tolua_S,(long)toluaI_ret);
 }
 }
 return 1;
tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'dispel_monsters'.");
 return 0;
}

/* function: dispel_living */
static int toluaI_spells_dispel_living00(lua_State* tolua_S)
{
 if (
 !tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
 !tolua_isnoobj(tolua_S,2)
 )
 goto tolua_lerror;
 else
 {
  int dam = ((int)  tolua_getnumber(tolua_S,1,0));
 {
  bool toluaI_ret = (bool)  dispel_living(dam);
 tolua_pushnumber(tolua_S,(long)toluaI_ret);
 }
 }
 return 1;
tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'dispel_living'.");
 return 0;
}

/* function: dispel_demons */
static int toluaI_spells_dispel_demons00(lua_State* tolua_S)
{
 if (
 !tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
 !tolua_isnoobj(tolua_S,2)
 )
 goto tolua_lerror;
 else
 {
  int dam = ((int)  tolua_getnumber(tolua_S,1,0));
 {
  bool toluaI_ret = (bool)  dispel_demons(dam);
 tolua_pushnumber(tolua_S,(long)toluaI_ret);
 }
 }
 return 1;
tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'dispel_demons'.");
 return 0;
}

/* function: turn_undead */
static int toluaI_spells_turn_undead00(lua_State* tolua_S)
{
 if (
 !tolua_isnoobj(tolua_S,1)
 )
 goto tolua_lerror;
 else
 {
 {
  bool toluaI_ret = (bool)  turn_undead();
 tolua_pushnumber(tolua_S,(long)toluaI_ret);
 }
 }
 return 1;
tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'turn_undead'.");
 return 0;
}

/* function: door_creation */
static int toluaI_spells_door_creation00(lua_State* tolua_S)
{
 if (
 !tolua_isnoobj(tolua_S,1)
 )
 goto tolua_lerror;
 else
 {
 {
  bool toluaI_ret = (bool)  door_creation();
 tolua_pushnumber(tolua_S,(long)toluaI_ret);
 }
 }
 return 1;
tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'door_creation'.");
 return 0;
}

/* function: trap_creation */
static int toluaI_spells_trap_creation00(lua_State* tolua_S)
{
 if (
 !tolua_isnoobj(tolua_S,1)
 )
 goto tolua_lerror;
 else
 {
 {
  bool toluaI_ret = (bool)  trap_creation();
 tolua_pushnumber(tolua_S,(long)toluaI_ret);
 }
 }
 return 1;
tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'trap_creation'.");
 return 0;
}

/* function: glyph_creation */
static int toluaI_spells_glyph_creation00(lua_State* tolua_S)
{
 if (
 !tolua_isnoobj(tolua_S,1)
 )
 goto tolua_lerror;
 else
 {
 {
  bool toluaI_ret = (bool)  glyph_creation();
 tolua_pushnumber(tolua_S,(long)toluaI_ret);
 }
 }
 return 1;
tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'glyph_creation'.");
 return 0;
}

/* function: wipe */
static int toluaI_spells_wipe00(lua_State* tolua_S)
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
  int y1 = ((int)  tolua_getnumber(tolua_S,1,0));
  int x1 = ((int)  tolua_getnumber(tolua_S,2,0));
  int r = ((int)  tolua_getnumber(tolua_S,3,0));
 {
  wipe(y1,x1,r);
 }
 }
 return 0;
tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'wipe'.");
 return 0;
}

/* function: destroy_area */
static int toluaI_spells_destroy_area00(lua_State* tolua_S)
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
  int r = ((int)  tolua_getnumber(tolua_S,3,0));
  bool full = ((bool)  tolua_getnumber(tolua_S,4,0));
  bool bypass = ((bool)  tolua_getnumber(tolua_S,5,0));
 {
  destroy_area(y1,x1,r,full,bypass);
 }
 }
 return 0;
tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'destroy_area'.");
 return 0;
}

/* function: earthquake */
static int toluaI_spells_earthquake00(lua_State* tolua_S)
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
  int cy = ((int)  tolua_getnumber(tolua_S,1,0));
  int cx = ((int)  tolua_getnumber(tolua_S,2,0));
  int r = ((int)  tolua_getnumber(tolua_S,3,0));
 {
  earthquake(cy,cx,r);
 }
 }
 return 0;
tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'earthquake'.");
 return 0;
}

/* function: lite_room */
static int toluaI_spells_lite_room00(lua_State* tolua_S)
{
 if (
 !tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
 !tolua_istype(tolua_S,2,LUA_TNUMBER,0) ||
 !tolua_isnoobj(tolua_S,3)
 )
 goto tolua_lerror;
 else
 {
  int y1 = ((int)  tolua_getnumber(tolua_S,1,0));
  int x1 = ((int)  tolua_getnumber(tolua_S,2,0));
 {
  lite_room(y1,x1);
 }
 }
 return 0;
tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'lite_room'.");
 return 0;
}

/* function: unlite_room */
static int toluaI_spells_unlite_room00(lua_State* tolua_S)
{
 if (
 !tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
 !tolua_istype(tolua_S,2,LUA_TNUMBER,0) ||
 !tolua_isnoobj(tolua_S,3)
 )
 goto tolua_lerror;
 else
 {
  int y1 = ((int)  tolua_getnumber(tolua_S,1,0));
  int x1 = ((int)  tolua_getnumber(tolua_S,2,0));
 {
  unlite_room(y1,x1);
 }
 }
 return 0;
tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'unlite_room'.");
 return 0;
}

/* function: lite_area */
static int toluaI_spells_lite_area00(lua_State* tolua_S)
{
 if (
 !tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
 !tolua_istype(tolua_S,2,LUA_TNUMBER,0) ||
 !tolua_isnoobj(tolua_S,3)
 )
 goto tolua_lerror;
 else
 {
  int dam = ((int)  tolua_getnumber(tolua_S,1,0));
  int rad = ((int)  tolua_getnumber(tolua_S,2,0));
 {
  bool toluaI_ret = (bool)  lite_area(dam,rad);
 tolua_pushnumber(tolua_S,(long)toluaI_ret);
 }
 }
 return 1;
tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'lite_area'.");
 return 0;
}

/* function: unlite_area */
static int toluaI_spells_unlite_area00(lua_State* tolua_S)
{
 if (
 !tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
 !tolua_istype(tolua_S,2,LUA_TNUMBER,0) ||
 !tolua_isnoobj(tolua_S,3)
 )
 goto tolua_lerror;
 else
 {
  int dam = ((int)  tolua_getnumber(tolua_S,1,0));
  int rad = ((int)  tolua_getnumber(tolua_S,2,0));
 {
  bool toluaI_ret = (bool)  unlite_area(dam,rad);
 tolua_pushnumber(tolua_S,(long)toluaI_ret);
 }
 }
 return 1;
tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'unlite_area'.");
 return 0;
}

/* function: fire_ball_beam */
static int toluaI_spells_fire_ball_beam00(lua_State* tolua_S)
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
  int typ = ((int)  tolua_getnumber(tolua_S,1,0));
  int dir = ((int)  tolua_getnumber(tolua_S,2,0));
  int dam = ((int)  tolua_getnumber(tolua_S,3,0));
  int rad = ((int)  tolua_getnumber(tolua_S,4,0));
 {
  bool toluaI_ret = (bool)  fire_ball_beam(typ,dir,dam,rad);
 tolua_pushnumber(tolua_S,(long)toluaI_ret);
 }
 }
 return 1;
tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'fire_ball_beam'.");
 return 0;
}

/* function: make_wish */
static int toluaI_spells_make_wish00(lua_State* tolua_S)
{
 if (
 !tolua_isnoobj(tolua_S,1)
 )
 goto tolua_lerror;
 else
 {
 {
  make_wish();
 }
 }
 return 0;
tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'make_wish'.");
 return 0;
}

/* function: fire_wave */
static int toluaI_spells_fire_wave00(lua_State* tolua_S)
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
  int typ = ((int)  tolua_getnumber(tolua_S,1,0));
  int dir = ((int)  tolua_getnumber(tolua_S,2,0));
  int dam = ((int)  tolua_getnumber(tolua_S,3,0));
  int rad = ((int)  tolua_getnumber(tolua_S,4,0));
  int time = ((int)  tolua_getnumber(tolua_S,5,0));
  s32b eff = ((s32b)  tolua_getnumber(tolua_S,6,0));
 {
  bool toluaI_ret = (bool)  fire_wave(typ,dir,dam,rad,time,eff);
 tolua_pushnumber(tolua_S,(long)toluaI_ret);
 }
 }
 return 1;
tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'fire_wave'.");
 return 0;
}

/* function: fire_cloud */
static int toluaI_spells_fire_cloud00(lua_State* tolua_S)
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
  int typ = ((int)  tolua_getnumber(tolua_S,1,0));
  int dir = ((int)  tolua_getnumber(tolua_S,2,0));
  int dam = ((int)  tolua_getnumber(tolua_S,3,0));
  int rad = ((int)  tolua_getnumber(tolua_S,4,0));
  int time = ((int)  tolua_getnumber(tolua_S,5,0));
 {
  bool toluaI_ret = (bool)  fire_cloud(typ,dir,dam,rad,time);
 tolua_pushnumber(tolua_S,(long)toluaI_ret);
 }
 }
 return 1;
tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'fire_cloud'.");
 return 0;
}

/* function: fire_wall */
static int toluaI_spells_fire_wall00(lua_State* tolua_S)
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
  int typ = ((int)  tolua_getnumber(tolua_S,1,0));
  int dir = ((int)  tolua_getnumber(tolua_S,2,0));
  int dam = ((int)  tolua_getnumber(tolua_S,3,0));
  int time = ((int)  tolua_getnumber(tolua_S,4,0));
 {
  bool toluaI_ret = (bool)  fire_wall(typ,dir,dam,time);
 tolua_pushnumber(tolua_S,(long)toluaI_ret);
 }
 }
 return 1;
tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'fire_wall'.");
 return 0;
}

/* function: fire_ball */
static int toluaI_spells_fire_ball00(lua_State* tolua_S)
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
  int typ = ((int)  tolua_getnumber(tolua_S,1,0));
  int dir = ((int)  tolua_getnumber(tolua_S,2,0));
  int dam = ((int)  tolua_getnumber(tolua_S,3,0));
  int rad = ((int)  tolua_getnumber(tolua_S,4,0));
 {
  bool toluaI_ret = (bool)  fire_ball(typ,dir,dam,rad);
 tolua_pushnumber(tolua_S,(long)toluaI_ret);
 }
 }
 return 1;
tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'fire_ball'.");
 return 0;
}

/* function: fire_bolt */
static int toluaI_spells_fire_bolt00(lua_State* tolua_S)
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
  int typ = ((int)  tolua_getnumber(tolua_S,1,0));
  int dir = ((int)  tolua_getnumber(tolua_S,2,0));
  int dam = ((int)  tolua_getnumber(tolua_S,3,0));
 {
  bool toluaI_ret = (bool)  fire_bolt(typ,dir,dam);
 tolua_pushnumber(tolua_S,(long)toluaI_ret);
 }
 }
 return 1;
tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'fire_bolt'.");
 return 0;
}

/* function: fire_beam */
static int toluaI_spells_fire_beam00(lua_State* tolua_S)
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
  int typ = ((int)  tolua_getnumber(tolua_S,1,0));
  int dir = ((int)  tolua_getnumber(tolua_S,2,0));
  int dam = ((int)  tolua_getnumber(tolua_S,3,0));
 {
  bool toluaI_ret = (bool)  fire_beam(typ,dir,dam);
 tolua_pushnumber(tolua_S,(long)toluaI_ret);
 }
 }
 return 1;
tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'fire_beam'.");
 return 0;
}

/* function: fire_druid_ball */
static int toluaI_spells_fire_druid_ball00(lua_State* tolua_S)
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
  int typ = ((int)  tolua_getnumber(tolua_S,1,0));
  int dir = ((int)  tolua_getnumber(tolua_S,2,0));
  int dam = ((int)  tolua_getnumber(tolua_S,3,0));
  int rad = ((int)  tolua_getnumber(tolua_S,4,0));
 {
  bool toluaI_ret = (bool)  fire_druid_ball(typ,dir,dam,rad);
 tolua_pushnumber(tolua_S,(long)toluaI_ret);
 }
 }
 return 1;
tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'fire_druid_ball'.");
 return 0;
}

/* function: fire_druid_bolt */
static int toluaI_spells_fire_druid_bolt00(lua_State* tolua_S)
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
  int typ = ((int)  tolua_getnumber(tolua_S,1,0));
  int dir = ((int)  tolua_getnumber(tolua_S,2,0));
  int dam = ((int)  tolua_getnumber(tolua_S,3,0));
 {
  bool toluaI_ret = (bool)  fire_druid_bolt(typ,dir,dam);
 tolua_pushnumber(tolua_S,(long)toluaI_ret);
 }
 }
 return 1;
tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'fire_druid_bolt'.");
 return 0;
}

/* function: fire_druid_beam */
static int toluaI_spells_fire_druid_beam00(lua_State* tolua_S)
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
  int typ = ((int)  tolua_getnumber(tolua_S,1,0));
  int dir = ((int)  tolua_getnumber(tolua_S,2,0));
  int dam = ((int)  tolua_getnumber(tolua_S,3,0));
 {
  bool toluaI_ret = (bool)  fire_druid_beam(typ,dir,dam);
 tolua_pushnumber(tolua_S,(long)toluaI_ret);
 }
 }
 return 1;
tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'fire_druid_beam'.");
 return 0;
}

/* function: fire_bolt_or_beam */
static int toluaI_spells_fire_bolt_or_beam00(lua_State* tolua_S)
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
  int prob = ((int)  tolua_getnumber(tolua_S,1,0));
  int typ = ((int)  tolua_getnumber(tolua_S,2,0));
  int dir = ((int)  tolua_getnumber(tolua_S,3,0));
  int dam = ((int)  tolua_getnumber(tolua_S,4,0));
 {
  bool toluaI_ret = (bool)  fire_bolt_or_beam(prob,typ,dir,dam);
 tolua_pushnumber(tolua_S,(long)toluaI_ret);
 }
 }
 return 1;
tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'fire_bolt_or_beam'.");
 return 0;
}

/* function: alchemy */
static int toluaI_spells_alchemy00(lua_State* tolua_S)
{
 if (
 !tolua_isnoobj(tolua_S,1)
 )
 goto tolua_lerror;
 else
 {
 {
  bool toluaI_ret = (bool)  alchemy();
 tolua_pushnumber(tolua_S,(long)toluaI_ret);
 }
 }
 return 1;
tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'alchemy'.");
 return 0;
}

/* function: alter_reality */
static int toluaI_spells_alter_reality00(lua_State* tolua_S)
{
 if (
 !tolua_isnoobj(tolua_S,1)
 )
 goto tolua_lerror;
 else
 {
 {
  alter_reality();
 }
 }
 return 0;
tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'alter_reality'.");
 return 0;
}

/* function: swap_position */
static int toluaI_spells_swap_position00(lua_State* tolua_S)
{
 if (
 !tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
 !tolua_istype(tolua_S,2,LUA_TNUMBER,0) ||
 !tolua_isnoobj(tolua_S,3)
 )
 goto tolua_lerror;
 else
 {
  int lty = ((int)  tolua_getnumber(tolua_S,1,0));
  int ltx = ((int)  tolua_getnumber(tolua_S,2,0));
 {
  swap_position(lty,ltx);
 }
 }
 return 0;
tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'swap_position'.");
 return 0;
}

/* function: teleport_swap */
static int toluaI_spells_teleport_swap00(lua_State* tolua_S)
{
 if (
 !tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
 !tolua_isnoobj(tolua_S,2)
 )
 goto tolua_lerror;
 else
 {
  int dir = ((int)  tolua_getnumber(tolua_S,1,0));
 {
  teleport_swap(dir);
 }
 }
 return 0;
tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'teleport_swap'.");
 return 0;
}

/* function: project_meteor */
static int toluaI_spells_project_meteor00(lua_State* tolua_S)
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
  int radius = ((int)  tolua_getnumber(tolua_S,1,0));
  int typ = ((int)  tolua_getnumber(tolua_S,2,0));
  int dam = ((int)  tolua_getnumber(tolua_S,3,0));
  u32b flg = ((u32b)  tolua_getnumber(tolua_S,4,0));
 {
  project_meteor(radius,typ,dam,flg);
 }
 }
 return 0;
tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'project_meteor'.");
 return 0;
}

/* function: passwall */
static int toluaI_spells_passwall00(lua_State* tolua_S)
{
 if (
 !tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
 !tolua_istype(tolua_S,2,LUA_TNUMBER,0) ||
 !tolua_isnoobj(tolua_S,3)
 )
 goto tolua_lerror;
 else
 {
  int dir = ((int)  tolua_getnumber(tolua_S,1,0));
  bool safe = ((bool)  tolua_getnumber(tolua_S,2,0));
 {
  bool toluaI_ret = (bool)  passwall(dir,safe);
 tolua_pushnumber(tolua_S,(long)toluaI_ret);
 }
 }
 return 1;
tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'passwall'.");
 return 0;
}

/* function: project_hook */
static int toluaI_spells_project_hook00(lua_State* tolua_S)
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
  int typ = ((int)  tolua_getnumber(tolua_S,1,0));
  int dir = ((int)  tolua_getnumber(tolua_S,2,0));
  int dam = ((int)  tolua_getnumber(tolua_S,3,0));
  int flg = ((int)  tolua_getnumber(tolua_S,4,0));
 {
  bool toluaI_ret = (bool)  project_hook(typ,dir,dam,flg);
 tolua_pushnumber(tolua_S,(long)toluaI_ret);
 }
 }
 return 1;
tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'project_hook'.");
 return 0;
}

/* function: wizard_lock */
static int toluaI_spells_wizard_lock00(lua_State* tolua_S)
{
 if (
 !tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
 !tolua_isnoobj(tolua_S,2)
 )
 goto tolua_lerror;
 else
 {
  int dir = ((int)  tolua_getnumber(tolua_S,1,0));
 {
  bool toluaI_ret = (bool)  wizard_lock(dir);
 tolua_pushnumber(tolua_S,(long)toluaI_ret);
 }
 }
 return 1;
tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'wizard_lock'.");
 return 0;
}

/* function: reset_recall */
static int toluaI_spells_reset_recall00(lua_State* tolua_S)
{
 if (
 !tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
 !tolua_isnoobj(tolua_S,2)
 )
 goto tolua_lerror;
 else
 {
  bool no_trepas_max_depth = ((bool)  tolua_getnumber(tolua_S,1,0));
 {
  bool toluaI_ret = (bool)  reset_recall(no_trepas_max_depth);
 tolua_pushnumber(tolua_S,(long)toluaI_ret);
 }
 }
 return 1;
tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'reset_recall'.");
 return 0;
}

/* function: get_aim_dir */
static int toluaI_spells_get_aim_dir00(lua_State* tolua_S)
{
 if (
 !tolua_istype(tolua_S,1,LUA_TNUMBER,1) ||
 !tolua_isnoobj(tolua_S,2)
 )
 goto tolua_lerror;
 else
 {
  int dp = ((int)  tolua_getnumber(tolua_S,1,0));
 {
  bool toluaI_ret = (bool)  get_aim_dir(&dp);
 tolua_pushnumber(tolua_S,(long)toluaI_ret);
 tolua_pushnumber(tolua_S,(long)dp);
 }
 }
 return 2;
tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'get_aim_dir'.");
 return 0;
}

/* function: get_rep_dir */
static int toluaI_spells_get_rep_dir00(lua_State* tolua_S)
{
 if (
 !tolua_istype(tolua_S,1,LUA_TNUMBER,1) ||
 !tolua_isnoobj(tolua_S,2)
 )
 goto tolua_lerror;
 else
 {
  int dp = ((int)  tolua_getnumber(tolua_S,1,0));
 {
  bool toluaI_ret = (bool)  get_rep_dir(&dp);
 tolua_pushnumber(tolua_S,(long)toluaI_ret);
 tolua_pushnumber(tolua_S,(long)dp);
 }
 }
 return 2;
tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'get_rep_dir'.");
 return 0;
}

/* function: project_hack */
static int toluaI_spells_project_los00(lua_State* tolua_S)
{
 if (
 !tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
 !tolua_istype(tolua_S,2,LUA_TNUMBER,0) ||
 !tolua_isnoobj(tolua_S,3)
 )
 goto tolua_lerror;
 else
 {
  int typ = ((int)  tolua_getnumber(tolua_S,1,0));
  int dam = ((int)  tolua_getnumber(tolua_S,2,0));
 {
  bool toluaI_ret = (bool)  project_hack(typ,dam);
 tolua_pushnumber(tolua_S,(long)toluaI_ret);
 }
 }
 return 1;
tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'project_los'.");
 return 0;
}

/* function: map_area */
static int toluaI_spells_map_area00(lua_State* tolua_S)
{
 if (
 !tolua_isnoobj(tolua_S,1)
 )
 goto tolua_lerror;
 else
 {
 {
  map_area();
 }
 }
 return 0;
tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'map_area'.");
 return 0;
}

/* function: wiz_lite */
static int toluaI_spells_wiz_lite00(lua_State* tolua_S)
{
 if (
 !tolua_isnoobj(tolua_S,1)
 )
 goto tolua_lerror;
 else
 {
 {
  wiz_lite();
 }
 }
 return 0;
tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'wiz_lite'.");
 return 0;
}

/* function: wiz_lite_extra */
static int toluaI_spells_wiz_lite_extra00(lua_State* tolua_S)
{
 if (
 !tolua_isnoobj(tolua_S,1)
 )
 goto tolua_lerror;
 else
 {
 {
  wiz_lite_extra();
 }
 }
 return 0;
tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'wiz_lite_extra'.");
 return 0;
}

/* function: wiz_dark */
static int toluaI_spells_wiz_dark00(lua_State* tolua_S)
{
 if (
 !tolua_isnoobj(tolua_S,1)
 )
 goto tolua_lerror;
 else
 {
 {
  wiz_dark();
 }
 }
 return 0;
tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'wiz_dark'.");
 return 0;
}

/* function: create_between_gate */
static int toluaI_spells_create_between_gate00(lua_State* tolua_S)
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
  int dist = ((int)  tolua_getnumber(tolua_S,1,0));
  int y = ((int)  tolua_getnumber(tolua_S,2,0));
  int x = ((int)  tolua_getnumber(tolua_S,3,0));
 {
  create_between_gate(dist,y,x);
 }
 }
 return 0;
tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'create_between_gate'.");
 return 0;
}

/* function: destroy_doors_touch */
static int toluaI_spells_destroy_doors_touch00(lua_State* tolua_S)
{
 if (
 !tolua_isnoobj(tolua_S,1)
 )
 goto tolua_lerror;
 else
 {
 {
  bool toluaI_ret = (bool)  destroy_doors_touch();
 tolua_pushnumber(tolua_S,(long)toluaI_ret);
 }
 }
 return 1;
tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'destroy_doors_touch'.");
 return 0;
}

/* function: destroy_traps_touch */
static int toluaI_spells_destroy_traps_touch00(lua_State* tolua_S)
{
 if (
 !tolua_isnoobj(tolua_S,1)
 )
 goto tolua_lerror;
 else
 {
 {
  bool toluaI_ret = (bool)  destroy_traps_touch();
 tolua_pushnumber(tolua_S,(long)toluaI_ret);
 }
 }
 return 1;
tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'destroy_traps_touch'.");
 return 0;
}

/* get function: min_lev of class  magic_power */
static int toluaI_get_spells_magic_power_min_lev(lua_State* tolua_S)
{
  magic_power* self = (magic_power*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->min_lev);
 return 1;
}

/* set function: min_lev of class  magic_power */
static int toluaI_set_spells_magic_power_min_lev(lua_State* tolua_S)
{
  magic_power* self = (magic_power*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->min_lev = ((int)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: mana_cost of class  magic_power */
static int toluaI_get_spells_magic_power_mana_cost(lua_State* tolua_S)
{
  magic_power* self = (magic_power*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->mana_cost);
 return 1;
}

/* set function: mana_cost of class  magic_power */
static int toluaI_set_spells_magic_power_mana_cost(lua_State* tolua_S)
{
  magic_power* self = (magic_power*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->mana_cost = ((int)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: fail of class  magic_power */
static int toluaI_get_spells_magic_power_fail(lua_State* tolua_S)
{
  magic_power* self = (magic_power*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->fail);
 return 1;
}

/* set function: fail of class  magic_power */
static int toluaI_set_spells_magic_power_fail(lua_State* tolua_S)
{
  magic_power* self = (magic_power*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->fail = ((int)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: name of class  magic_power */
static int toluaI_get_spells_magic_power_name(lua_State* tolua_S)
{
  magic_power* self = (magic_power*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushstring(tolua_S,(const char*)self->name);
 return 1;
}

/* set function: name of class  magic_power */
static int toluaI_set_spells_magic_power_name(lua_State* tolua_S)
{
  magic_power* self = (magic_power*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TSTRING,0))
 TOLUA_ERR_ASSIGN;
  self->name = ((cptr)  tolua_getstring(tolua_S,2,0));
 return 0;
}

/* get function: desc of class  magic_power */
static int toluaI_get_spells_magic_power_desc(lua_State* tolua_S)
{
  magic_power* self = (magic_power*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushstring(tolua_S,(const char*)self->desc);
 return 1;
}

/* set function: desc of class  magic_power */
static int toluaI_set_spells_magic_power_desc(lua_State* tolua_S)
{
  magic_power* self = (magic_power*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TSTRING,0))
 TOLUA_ERR_ASSIGN;
  self->desc = ((cptr)  tolua_getstring(tolua_S,2,0));
 return 0;
}

/* function: new_magic_power */
static int toluaI_spells_new_magic_power00(lua_State* tolua_S)
{
 if (
 !tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
 !tolua_isnoobj(tolua_S,2)
 )
 goto tolua_lerror;
 else
 {
  int num = ((int)  tolua_getnumber(tolua_S,1,0));
 {
  magic_power* toluaI_ret = (magic_power*)  new_magic_power(num);
 tolua_pushusertype(tolua_S,(void*)toluaI_ret,tolua_tag(tolua_S,"magic_power"));
 }
 }
 return 1;
tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'new_magic_power'.");
 return 0;
}

/* function: grab_magic_power */
static int toluaI_spells_get_magic_power00(lua_State* tolua_S)
{
 if (
 !tolua_istype(tolua_S,1,tolua_tag(tolua_S,"magic_power"),0) ||
 !tolua_istype(tolua_S,2,LUA_TNUMBER,0) ||
 !tolua_isnoobj(tolua_S,3)
 )
 goto tolua_lerror;
 else
 {
  magic_power* m_ptr = ((magic_power*)  tolua_getusertype(tolua_S,1,0));
  int num = ((int)  tolua_getnumber(tolua_S,2,0));
 {
  magic_power* toluaI_ret = (magic_power*)  grab_magic_power(m_ptr,num);
 tolua_pushusertype(tolua_S,(void*)toluaI_ret,tolua_tag(tolua_S,"magic_power"));
 }
 }
 return 1;
tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'get_magic_power'.");
 return 0;
}

/* function: get_magic_power_lua */
static int toluaI_spells_select_magic_power00(lua_State* tolua_S)
{
 if (
 !tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
 !tolua_istype(tolua_S,2,tolua_tag(tolua_S,"magic_power"),0) ||
 !tolua_istype(tolua_S,3,LUA_TNUMBER,0) ||
 !tolua_istype(tolua_S,4,LUA_TSTRING,0) ||
 !tolua_istype(tolua_S,5,LUA_TNUMBER,0) ||
 !tolua_istype(tolua_S,6,LUA_TNUMBER,0) ||
 !tolua_isnoobj(tolua_S,7)
 )
 goto tolua_lerror;
 else
 {
  int sn = ((int)  tolua_getnumber(tolua_S,1,0));
  magic_power* powers = ((magic_power*)  tolua_getusertype(tolua_S,2,0));
  int max_powers = ((int)  tolua_getnumber(tolua_S,3,0));
  char* info_fct = ((char*)  tolua_getstring(tolua_S,4,0));
  int plev = ((int)  tolua_getnumber(tolua_S,5,0));
  int cast_stat = ((int)  tolua_getnumber(tolua_S,6,0));
 {
  bool toluaI_ret = (bool)  get_magic_power_lua(&sn,powers,max_powers,info_fct,plev,cast_stat);
 tolua_pushnumber(tolua_S,(long)toluaI_ret);
 tolua_pushnumber(tolua_S,(long)sn);
 }
 }
 return 2;
tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'select_magic_power'.");
 return 0;
}

/* function: lua_spell_success */
static int toluaI_spells_magic_power_sucess00(lua_State* tolua_S)
{
 if (
 !tolua_istype(tolua_S,1,tolua_tag(tolua_S,"magic_power"),0) ||
 !tolua_istype(tolua_S,2,LUA_TNUMBER,0) ||
 !tolua_istype(tolua_S,3,LUA_TSTRING,1) ||
 !tolua_isnoobj(tolua_S,4)
 )
 goto tolua_lerror;
 else
 {
  magic_power* spell = ((magic_power*)  tolua_getusertype(tolua_S,1,0));
  int stat = ((int)  tolua_getnumber(tolua_S,2,0));
  char* oups_fct = ((char*)  tolua_getstring(tolua_S,3,NULL));
 {
  bool toluaI_ret = (bool)  lua_spell_success(spell,stat,oups_fct);
 tolua_pushnumber(tolua_S,(long)toluaI_ret);
 }
 }
 return 1;
tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'magic_power_sucess'.");
 return 0;
}

/* function: add_new_power */
static int toluaI_spells_add_new_power00(lua_State* tolua_S)
{
 if (
 !tolua_istype(tolua_S,1,LUA_TSTRING,0) ||
 !tolua_istype(tolua_S,2,LUA_TSTRING,0) ||
 !tolua_istype(tolua_S,3,LUA_TSTRING,0) ||
 !tolua_istype(tolua_S,4,LUA_TSTRING,0) ||
 !tolua_istype(tolua_S,5,LUA_TNUMBER,0) ||
 !tolua_istype(tolua_S,6,LUA_TNUMBER,0) ||
 !tolua_istype(tolua_S,7,LUA_TNUMBER,0) ||
 !tolua_istype(tolua_S,8,LUA_TNUMBER,0) ||
 !tolua_isnoobj(tolua_S,9)
 )
 goto tolua_lerror;
 else
 {
  cptr name = ((cptr)  tolua_getstring(tolua_S,1,0));
  cptr desc = ((cptr)  tolua_getstring(tolua_S,2,0));
  cptr gain = ((cptr)  tolua_getstring(tolua_S,3,0));
  cptr lose = ((cptr)  tolua_getstring(tolua_S,4,0));
  byte level = ((byte)  tolua_getnumber(tolua_S,5,0));
  byte cost = ((byte)  tolua_getnumber(tolua_S,6,0));
  byte stat = ((byte)  tolua_getnumber(tolua_S,7,0));
  byte diff = ((byte)  tolua_getnumber(tolua_S,8,0));
 {
  s16b toluaI_ret = (s16b)  add_new_power(name,desc,gain,lose,level,cost,stat,diff);
 tolua_pushnumber(tolua_S,(long)toluaI_ret);
 }
 }
 return 1;
tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'add_new_power'.");
 return 0;
}

/* get function: power_max */
static int toluaI_get_spells_power_max(lua_State* tolua_S)
{
 tolua_pushnumber(tolua_S,(long)power_max);
 return 1;
}

/* set function: power_max */
static int toluaI_set_spells_power_max(lua_State* tolua_S)
{
 if (!tolua_istype(tolua_S,1,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  power_max = ((s16b)  tolua_getnumber(tolua_S,1,0));
 return 0;
}

/* get function: name of class  spell_type */
static int toluaI_get_spells_spell_type_name(lua_State* tolua_S)
{
  spell_type* self = (spell_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushstring(tolua_S,(const char*)self->name);
 return 1;
}

/* set function: name of class  spell_type */
static int toluaI_set_spells_spell_type_name(lua_State* tolua_S)
{
  spell_type* self = (spell_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TSTRING,0))
 TOLUA_ERR_ASSIGN;
  self->name = ((cptr)  tolua_getstring(tolua_S,2,0));
 return 0;
}

/* get function: skill_level of class  spell_type */
static int toluaI_get_spells_spell_type_skill_level(lua_State* tolua_S)
{
  spell_type* self = (spell_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->skill_level);
 return 1;
}

/* set function: skill_level of class  spell_type */
static int toluaI_set_spells_spell_type_skill_level(lua_State* tolua_S)
{
  spell_type* self = (spell_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->skill_level = ((byte)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: mana of class  spell_type */
static int toluaI_get_spells_spell_type_mana(lua_State* tolua_S)
{
  spell_type* self = (spell_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->mana);
 return 1;
}

/* set function: mana of class  spell_type */
static int toluaI_set_spells_spell_type_mana(lua_State* tolua_S)
{
  spell_type* self = (spell_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->mana = ((byte)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: mana_max of class  spell_type */
static int toluaI_get_spells_spell_type_mana_max(lua_State* tolua_S)
{
  spell_type* self = (spell_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->mana_max);
 return 1;
}

/* set function: mana_max of class  spell_type */
static int toluaI_set_spells_spell_type_mana_max(lua_State* tolua_S)
{
  spell_type* self = (spell_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->mana_max = ((byte)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: fail of class  spell_type */
static int toluaI_get_spells_spell_type_fail(lua_State* tolua_S)
{
  spell_type* self = (spell_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->fail);
 return 1;
}

/* set function: fail of class  spell_type */
static int toluaI_set_spells_spell_type_fail(lua_State* tolua_S)
{
  spell_type* self = (spell_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->fail = ((s16b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: level of class  spell_type */
static int toluaI_get_spells_spell_type_level(lua_State* tolua_S)
{
  spell_type* self = (spell_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->level);
 return 1;
}

/* set function: level of class  spell_type */
static int toluaI_set_spells_spell_type_level(lua_State* tolua_S)
{
  spell_type* self = (spell_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->level = ((s16b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: name of class  school_type */
static int toluaI_get_spells_school_type_name(lua_State* tolua_S)
{
  school_type* self = (school_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushstring(tolua_S,(const char*)self->name);
 return 1;
}

/* set function: name of class  school_type */
static int toluaI_set_spells_school_type_name(lua_State* tolua_S)
{
  school_type* self = (school_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TSTRING,0))
 TOLUA_ERR_ASSIGN;
  self->name = ((cptr)  tolua_getstring(tolua_S,2,0));
 return 0;
}

/* get function: skill of class  school_type */
static int toluaI_get_spells_school_type_skill(lua_State* tolua_S)
{
  school_type* self = (school_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->skill);
 return 1;
}

/* set function: skill of class  school_type */
static int toluaI_set_spells_school_type_skill(lua_State* tolua_S)
{
  school_type* self = (school_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->skill = ((s16b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* function: new_school */
static int toluaI_spells_new_school00(lua_State* tolua_S)
{
 if (
 !tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
 !tolua_istype(tolua_S,2,LUA_TSTRING,0) ||
 !tolua_istype(tolua_S,3,LUA_TNUMBER,0) ||
 !tolua_isnoobj(tolua_S,4)
 )
 goto tolua_lerror;
 else
 {
  int i = ((int)  tolua_getnumber(tolua_S,1,0));
  cptr name = ((cptr)  tolua_getstring(tolua_S,2,0));
  s16b skill = ((s16b)  tolua_getnumber(tolua_S,3,0));
 {
  s16b toluaI_ret = (s16b)  new_school(i,name,skill);
 tolua_pushnumber(tolua_S,(long)toluaI_ret);
 }
 }
 return 1;
tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'new_school'.");
 return 0;
}

/* function: new_spell */
static int toluaI_spells_new_spell00(lua_State* tolua_S)
{
 if (
 !tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
 !tolua_istype(tolua_S,2,LUA_TSTRING,0) ||
 !tolua_isnoobj(tolua_S,3)
 )
 goto tolua_lerror;
 else
 {
  int i = ((int)  tolua_getnumber(tolua_S,1,0));
  cptr name = ((cptr)  tolua_getstring(tolua_S,2,0));
 {
  s16b toluaI_ret = (s16b)  new_spell(i,name);
 tolua_pushnumber(tolua_S,(long)toluaI_ret);
 }
 }
 return 1;
tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'new_spell'.");
 return 0;
}

/* function: grab_spell_type */
static int toluaI_spells_spell00(lua_State* tolua_S)
{
 if (
 !tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
 !tolua_isnoobj(tolua_S,2)
 )
 goto tolua_lerror;
 else
 {
  s16b num = ((s16b)  tolua_getnumber(tolua_S,1,0));
 {
  spell_type* toluaI_ret = (spell_type*)  grab_spell_type(num);
 tolua_pushusertype(tolua_S,(void*)toluaI_ret,tolua_tag(tolua_S,"spell_type"));
 }
 }
 return 1;
tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'spell'.");
 return 0;
}

/* function: grab_school_type */
static int toluaI_spells_school00(lua_State* tolua_S)
{
 if (
 !tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
 !tolua_isnoobj(tolua_S,2)
 )
 goto tolua_lerror;
 else
 {
  s16b num = ((s16b)  tolua_getnumber(tolua_S,1,0));
 {
  school_type* toluaI_ret = (school_type*)  grab_school_type(num);
 tolua_pushusertype(tolua_S,(void*)toluaI_ret,tolua_tag(tolua_S,"school_type"));
 }
 }
 return 1;
tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'school'.");
 return 0;
}

/* function: lua_get_level */
static int toluaI_spells_lua_get_level00(lua_State* tolua_S)
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
  s32b s = ((s32b)  tolua_getnumber(tolua_S,1,0));
  s32b lvl = ((s32b)  tolua_getnumber(tolua_S,2,0));
  s32b max = ((s32b)  tolua_getnumber(tolua_S,3,0));
  s32b min = ((s32b)  tolua_getnumber(tolua_S,4,0));
  s32b bonus = ((s32b)  tolua_getnumber(tolua_S,5,0));
 {
  s32b toluaI_ret = (s32b)  lua_get_level(s,lvl,max,min,bonus);
 tolua_pushnumber(tolua_S,(long)toluaI_ret);
 }
 }
 return 1;
tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'lua_get_level'.");
 return 0;
}

/* function: lua_spell_chance */
static int toluaI_spells_lua_spell_chance00(lua_State* tolua_S)
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
  s32b chance = ((s32b)  tolua_getnumber(tolua_S,1,0));
  int level = ((int)  tolua_getnumber(tolua_S,2,0));
  int skill_level = ((int)  tolua_getnumber(tolua_S,3,0));
  int mana = ((int)  tolua_getnumber(tolua_S,4,0));
  int cur_mana = ((int)  tolua_getnumber(tolua_S,5,0));
  int stat = ((int)  tolua_getnumber(tolua_S,6,0));
 {
  s32b toluaI_ret = (s32b)  lua_spell_chance(chance,level,skill_level,mana,cur_mana,stat);
 tolua_pushnumber(tolua_S,(long)toluaI_ret);
 }
 }
 return 1;
tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'lua_spell_chance'.");
 return 0;
}

/* function: lua_spell_device_chance */
static int toluaI_spells_lua_spell_device_chance00(lua_State* tolua_S)
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
  s32b chance = ((s32b)  tolua_getnumber(tolua_S,1,0));
  int level = ((int)  tolua_getnumber(tolua_S,2,0));
  int base_level = ((int)  tolua_getnumber(tolua_S,3,0));
 {
  s32b toluaI_ret = (s32b)  lua_spell_device_chance(chance,level,base_level);
 tolua_pushnumber(tolua_S,(long)toluaI_ret);
 }
 }
 return 1;
tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'lua_spell_device_chance'.");
 return 0;
}

/* function: get_school_spell */
static int toluaI_spells_get_school_spell00(lua_State* tolua_S)
{
 if (
 !tolua_istype(tolua_S,1,LUA_TSTRING,0) ||
 !tolua_istype(tolua_S,2,LUA_TSTRING,0) ||
 !tolua_istype(tolua_S,3,LUA_TNUMBER,0) ||
 !tolua_isnoobj(tolua_S,4)
 )
 goto tolua_lerror;
 else
 {
  cptr do_what = ((cptr)  tolua_getstring(tolua_S,1,0));
  cptr check_fct = ((cptr)  tolua_getstring(tolua_S,2,0));
  s16b force_book = ((s16b)  tolua_getnumber(tolua_S,3,0));
 {
  u32b toluaI_ret = (u32b)  get_school_spell(do_what,check_fct,force_book);
 tolua_pushnumber(tolua_S,(long)toluaI_ret);
 }
 }
 return 1;
tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'get_school_spell'.");
 return 0;
}

/* get function: last_teleportation_y */
static int toluaI_get_spells_last_teleportation_y(lua_State* tolua_S)
{
 tolua_pushnumber(tolua_S,(long)last_teleportation_y);
 return 1;
}

/* set function: last_teleportation_y */
static int toluaI_set_spells_last_teleportation_y(lua_State* tolua_S)
{
 if (!tolua_istype(tolua_S,1,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  last_teleportation_y = ((s16b)  tolua_getnumber(tolua_S,1,0));
 return 0;
}

/* get function: last_teleportation_x */
static int toluaI_get_spells_last_teleportation_x(lua_State* tolua_S)
{
 tolua_pushnumber(tolua_S,(long)last_teleportation_x);
 return 1;
}

/* set function: last_teleportation_x */
static int toluaI_set_spells_last_teleportation_x(lua_State* tolua_S)
{
 if (!tolua_istype(tolua_S,1,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  last_teleportation_x = ((s16b)  tolua_getnumber(tolua_S,1,0));
 return 0;
}

/* function: get_pos_player */
static int toluaI_spells_get_pos_player00(lua_State* tolua_S)
{
 if (
 !tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
 !tolua_istype(tolua_S,2,LUA_TNUMBER,1) ||
 !tolua_istype(tolua_S,3,LUA_TNUMBER,1) ||
 !tolua_isnoobj(tolua_S,4)
 )
 goto tolua_lerror;
 else
 {
  int dis = ((int)  tolua_getnumber(tolua_S,1,0));
  int ny = ((int)  tolua_getnumber(tolua_S,2,0));
  int nx = ((int)  tolua_getnumber(tolua_S,3,0));
 {
  get_pos_player(dis,&ny,&nx);
 tolua_pushnumber(tolua_S,(long)ny);
 tolua_pushnumber(tolua_S,(long)nx);
 }
 }
 return 2;
tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'get_pos_player'.");
 return 0;
}

/* Open function */
int tolua_spells_open (lua_State* tolua_S)
{
 tolua_open(tolua_S);
 toluaI_reg_types(tolua_S);
 tolua_constant(tolua_S,NULL,"DEFAULT_RADIUS",DEFAULT_RADIUS);
 tolua_constant(tolua_S,NULL,"GF_ELEC",GF_ELEC);
 tolua_constant(tolua_S,NULL,"GF_POIS",GF_POIS);
 tolua_constant(tolua_S,NULL,"GF_ACID",GF_ACID);
 tolua_constant(tolua_S,NULL,"GF_COLD",GF_COLD);
 tolua_constant(tolua_S,NULL,"GF_FIRE",GF_FIRE);
 tolua_constant(tolua_S,NULL,"GF_UNBREATH",GF_UNBREATH);
 tolua_constant(tolua_S,NULL,"GF_CORPSE_EXPL",GF_CORPSE_EXPL);
 tolua_constant(tolua_S,NULL,"GF_MISSILE",GF_MISSILE);
 tolua_constant(tolua_S,NULL,"GF_ARROW",GF_ARROW);
 tolua_constant(tolua_S,NULL,"GF_PLASMA",GF_PLASMA);
 tolua_constant(tolua_S,NULL,"GF_WAVE",GF_WAVE);
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
 tolua_constant(tolua_S,NULL,"GF_IDENTIFY",GF_IDENTIFY);
 tolua_constant(tolua_S,NULL,"GF_RAISE",GF_RAISE);
 tolua_constant(tolua_S,NULL,"GF_STAR_IDENTIFY",GF_STAR_IDENTIFY);
 tolua_constant(tolua_S,NULL,"GF_DESTRUCTION",GF_DESTRUCTION);
 tolua_constant(tolua_S,NULL,"GF_STUN_CONF",GF_STUN_CONF);
 tolua_constant(tolua_S,NULL,"GF_STUN_DAM",GF_STUN_DAM);
 tolua_constant(tolua_S,NULL,"GF_CONF_DAM",GF_CONF_DAM);
 tolua_constant(tolua_S,NULL,"GF_STAR_CHARM",GF_STAR_CHARM);
 tolua_constant(tolua_S,NULL,"GF_IMPLOSION",GF_IMPLOSION);
 tolua_constant(tolua_S,NULL,"GF_LAVA_FLOW",GF_LAVA_FLOW);
 tolua_constant(tolua_S,NULL,"GF_FEAR",GF_FEAR);
 tolua_constant(tolua_S,NULL,"GF_BETWEEN_GATE",GF_BETWEEN_GATE);
 tolua_constant(tolua_S,NULL,"GF_WINDS_MANA",GF_WINDS_MANA);
 tolua_constant(tolua_S,NULL,"GF_DEATH",GF_DEATH);
 tolua_constant(tolua_S,NULL,"GF_CONTROL_DEMON",GF_CONTROL_DEMON);
 tolua_constant(tolua_S,NULL,"GF_RAISE_DEMON",GF_RAISE_DEMON);
 tolua_constant(tolua_S,NULL,"GF_TRAP_DEMONSOUL",GF_TRAP_DEMONSOUL);
 tolua_constant(tolua_S,NULL,"GF_ATTACK",GF_ATTACK);
 tolua_constant(tolua_S,NULL,"GF_CHARM_UNMOVING",GF_CHARM_UNMOVING);
 tolua_constant(tolua_S,NULL,"MAX_GF",MAX_GF);
 tolua_constant(tolua_S,NULL,"PROJECT_JUMP",PROJECT_JUMP);
 tolua_constant(tolua_S,NULL,"PROJECT_BEAM",PROJECT_BEAM);
 tolua_constant(tolua_S,NULL,"PROJECT_THRU",PROJECT_THRU);
 tolua_constant(tolua_S,NULL,"PROJECT_STOP",PROJECT_STOP);
 tolua_constant(tolua_S,NULL,"PROJECT_GRID",PROJECT_GRID);
 tolua_constant(tolua_S,NULL,"PROJECT_ITEM",PROJECT_ITEM);
 tolua_constant(tolua_S,NULL,"PROJECT_KILL",PROJECT_KILL);
 tolua_constant(tolua_S,NULL,"PROJECT_HIDE",PROJECT_HIDE);
 tolua_constant(tolua_S,NULL,"PROJECT_VIEWABLE",PROJECT_VIEWABLE);
 tolua_constant(tolua_S,NULL,"PROJECT_METEOR_SHOWER",PROJECT_METEOR_SHOWER);
 tolua_constant(tolua_S,NULL,"PROJECT_BLAST",PROJECT_BLAST);
 tolua_constant(tolua_S,NULL,"PROJECT_PANEL",PROJECT_PANEL);
 tolua_constant(tolua_S,NULL,"PROJECT_ALL",PROJECT_ALL);
 tolua_constant(tolua_S,NULL,"PROJECT_WALL",PROJECT_WALL);
 tolua_constant(tolua_S,NULL,"PROJECT_MANA_PATH",PROJECT_MANA_PATH);
 tolua_constant(tolua_S,NULL,"PROJECT_ABSORB_MANA",PROJECT_ABSORB_MANA);
 tolua_constant(tolua_S,NULL,"PROJECT_STAY",PROJECT_STAY);
 tolua_globalvar(tolua_S,"project_time",toluaI_get_spells_project_time,toluaI_set_spells_project_time);
 tolua_function(tolua_S,NULL,"teleport_player_directed",toluaI_spells_teleport_player_directed00);
 tolua_function(tolua_S,NULL,"teleport_away",toluaI_spells_teleport_away00);
 tolua_function(tolua_S,NULL,"teleport_player",toluaI_spells_teleport_player00);
 tolua_function(tolua_S,NULL,"teleport_player_to",toluaI_spells_teleport_player_to00);
 tolua_function(tolua_S,NULL,"teleport_monster_to",toluaI_spells_teleport_monster_to00);
 tolua_function(tolua_S,NULL,"teleport_monster",toluaI_spells_teleport_monster00);
 tolua_function(tolua_S,NULL,"teleport_player_level",toluaI_spells_teleport_player_level00);
 tolua_function(tolua_S,NULL,"fetch",toluaI_spells_fetch00);
 tolua_function(tolua_S,NULL,"recall_player",toluaI_spells_recall_player00);
 tolua_function(tolua_S,NULL,"take_hit",toluaI_spells_take_hit00);
 tolua_function(tolua_S,NULL,"take_sanity_hit",toluaI_spells_take_sanity_hit00);
 tolua_function(tolua_S,NULL,"project",toluaI_spells_project00);
 tolua_function(tolua_S,NULL,"corrupt_player",toluaI_spells_corrupt_player00);
 tolua_function(tolua_S,NULL,"grow_things",toluaI_spells_grow_things00);
 tolua_function(tolua_S,NULL,"grow_grass",toluaI_spells_grow_grass00);
 tolua_function(tolua_S,NULL,"grow_trees",toluaI_spells_grow_trees00);
 tolua_function(tolua_S,NULL,"hp_player",toluaI_spells_hp_player00);
 tolua_function(tolua_S,NULL,"heal_insanity",toluaI_spells_heal_insanity00);
 tolua_function(tolua_S,NULL,"warding_glyph",toluaI_spells_warding_glyph00);
 tolua_function(tolua_S,NULL,"explosive_rune",toluaI_spells_explosive_rune00);
 tolua_function(tolua_S,NULL,"do_dec_stat",toluaI_spells_do_dec_stat00);
 tolua_function(tolua_S,NULL,"do_res_stat",toluaI_spells_do_res_stat00);
 tolua_function(tolua_S,NULL,"do_inc_stat",toluaI_spells_do_inc_stat00);
 tolua_function(tolua_S,NULL,"identify_pack",toluaI_spells_identify_pack00);
 tolua_function(tolua_S,NULL,"remove_curse",toluaI_spells_remove_curse00);
 tolua_function(tolua_S,NULL,"remove_all_curse",toluaI_spells_remove_all_curse00);
 tolua_function(tolua_S,NULL,"restore_level",toluaI_spells_restore_level00);
 tolua_function(tolua_S,NULL,"self_knowledge",toluaI_spells_self_knowledge00);
 tolua_function(tolua_S,NULL,"lose_all_info",toluaI_spells_lose_all_info00);
 tolua_function(tolua_S,NULL,"detect_traps",toluaI_spells_detect_traps00);
 tolua_function(tolua_S,NULL,"detect_doors",toluaI_spells_detect_doors00);
 tolua_function(tolua_S,NULL,"detect_stairs",toluaI_spells_detect_stairs00);
 tolua_function(tolua_S,NULL,"detect_treasure",toluaI_spells_detect_treasure00);
 tolua_globalvar(tolua_S,"hack_no_detect_message",toluaI_get_spells_hack_no_detect_message,toluaI_set_spells_hack_no_detect_message);
 tolua_function(tolua_S,NULL,"detect_objects_gold",toluaI_spells_detect_objects_gold00);
 tolua_function(tolua_S,NULL,"detect_objects_normal",toluaI_spells_detect_objects_normal00);
 tolua_function(tolua_S,NULL,"detect_objects_magic",toluaI_spells_detect_objects_magic00);
 tolua_function(tolua_S,NULL,"detect_monsters_normal",toluaI_spells_detect_monsters_normal00);
 tolua_function(tolua_S,NULL,"detect_monsters_invis",toluaI_spells_detect_monsters_invis00);
 tolua_function(tolua_S,NULL,"detect_monsters_evil",toluaI_spells_detect_monsters_evil00);
 tolua_function(tolua_S,NULL,"detect_monsters_good",toluaI_spells_detect_monsters_good00);
 tolua_function(tolua_S,NULL,"detect_monsters_xxx",toluaI_spells_detect_monsters_xxx00);
 tolua_function(tolua_S,NULL,"detect_monsters_string",toluaI_spells_detect_monsters_string00);
 tolua_function(tolua_S,NULL,"detect_monsters_nonliving",toluaI_spells_detect_monsters_nonliving00);
 tolua_function(tolua_S,NULL,"detect_all",toluaI_spells_detect_all00);
 tolua_function(tolua_S,NULL,"stair_creation",toluaI_spells_stair_creation00);
 tolua_function(tolua_S,NULL,"tgt_pt",toluaI_spells_tgt_pt00);
 tolua_function(tolua_S,NULL,"wall_stone",toluaI_spells_wall_stone00);
 tolua_function(tolua_S,NULL,"create_artifact",toluaI_spells_create_artifact00);
 tolua_function(tolua_S,NULL,"wall_to_mud",toluaI_spells_wall_to_mud00);
 tolua_function(tolua_S,NULL,"ident_spell",toluaI_spells_ident_spell00);
 tolua_function(tolua_S,NULL,"identify_fully",toluaI_spells_identify_fully00);
 tolua_function(tolua_S,NULL,"recharge",toluaI_spells_recharge00);
 tolua_function(tolua_S,NULL,"aggravate_monsters",toluaI_spells_aggravate_monsters00);
 tolua_function(tolua_S,NULL,"genocide_aux",toluaI_spells_genocide_aux00);
 tolua_function(tolua_S,NULL,"genocide",toluaI_spells_genocide00);
 tolua_function(tolua_S,NULL,"mass_genocide",toluaI_spells_mass_genocide00);
 tolua_function(tolua_S,NULL,"probing",toluaI_spells_probing00);
 tolua_function(tolua_S,NULL,"banish_evil",toluaI_spells_banish_evil00);
 tolua_function(tolua_S,NULL,"dispel_evil",toluaI_spells_dispel_evil00);
 tolua_function(tolua_S,NULL,"dispel_good",toluaI_spells_dispel_good00);
 tolua_function(tolua_S,NULL,"dispel_undead",toluaI_spells_dispel_undead00);
 tolua_function(tolua_S,NULL,"dispel_monsters",toluaI_spells_dispel_monsters00);
 tolua_function(tolua_S,NULL,"dispel_living",toluaI_spells_dispel_living00);
 tolua_function(tolua_S,NULL,"dispel_demons",toluaI_spells_dispel_demons00);
 tolua_function(tolua_S,NULL,"turn_undead",toluaI_spells_turn_undead00);
 tolua_function(tolua_S,NULL,"door_creation",toluaI_spells_door_creation00);
 tolua_function(tolua_S,NULL,"trap_creation",toluaI_spells_trap_creation00);
 tolua_function(tolua_S,NULL,"glyph_creation",toluaI_spells_glyph_creation00);
 tolua_function(tolua_S,NULL,"wipe",toluaI_spells_wipe00);
 tolua_function(tolua_S,NULL,"destroy_area",toluaI_spells_destroy_area00);
 tolua_function(tolua_S,NULL,"earthquake",toluaI_spells_earthquake00);
 tolua_function(tolua_S,NULL,"lite_room",toluaI_spells_lite_room00);
 tolua_function(tolua_S,NULL,"unlite_room",toluaI_spells_unlite_room00);
 tolua_function(tolua_S,NULL,"lite_area",toluaI_spells_lite_area00);
 tolua_function(tolua_S,NULL,"unlite_area",toluaI_spells_unlite_area00);
 tolua_function(tolua_S,NULL,"fire_ball_beam",toluaI_spells_fire_ball_beam00);
 tolua_function(tolua_S,NULL,"make_wish",toluaI_spells_make_wish00);
 tolua_function(tolua_S,NULL,"fire_wave",toluaI_spells_fire_wave00);
 tolua_constant(tolua_S,NULL,"EFF_WAVE",EFF_WAVE);
 tolua_constant(tolua_S,NULL,"EFF_LAST",EFF_LAST);
 tolua_constant(tolua_S,NULL,"EFF_STORM",EFF_STORM);
 tolua_constant(tolua_S,NULL,"EFF_DIR1",EFF_DIR1);
 tolua_constant(tolua_S,NULL,"EFF_DIR2",EFF_DIR2);
 tolua_constant(tolua_S,NULL,"EFF_DIR3",EFF_DIR3);
 tolua_constant(tolua_S,NULL,"EFF_DIR4",EFF_DIR4);
 tolua_constant(tolua_S,NULL,"EFF_DIR6",EFF_DIR6);
 tolua_constant(tolua_S,NULL,"EFF_DIR7",EFF_DIR7);
 tolua_constant(tolua_S,NULL,"EFF_DIR8",EFF_DIR8);
 tolua_constant(tolua_S,NULL,"EFF_DIR9",EFF_DIR9);
 tolua_function(tolua_S,NULL,"fire_cloud",toluaI_spells_fire_cloud00);
 tolua_function(tolua_S,NULL,"fire_wall",toluaI_spells_fire_wall00);
 tolua_function(tolua_S,NULL,"fire_ball",toluaI_spells_fire_ball00);
 tolua_function(tolua_S,NULL,"fire_bolt",toluaI_spells_fire_bolt00);
 tolua_function(tolua_S,NULL,"fire_beam",toluaI_spells_fire_beam00);
 tolua_function(tolua_S,NULL,"fire_druid_ball",toluaI_spells_fire_druid_ball00);
 tolua_function(tolua_S,NULL,"fire_druid_bolt",toluaI_spells_fire_druid_bolt00);
 tolua_function(tolua_S,NULL,"fire_druid_beam",toluaI_spells_fire_druid_beam00);
 tolua_function(tolua_S,NULL,"fire_bolt_or_beam",toluaI_spells_fire_bolt_or_beam00);
 tolua_function(tolua_S,NULL,"alchemy",toluaI_spells_alchemy00);
 tolua_function(tolua_S,NULL,"alter_reality",toluaI_spells_alter_reality00);
 tolua_function(tolua_S,NULL,"swap_position",toluaI_spells_swap_position00);
 tolua_function(tolua_S,NULL,"teleport_swap",toluaI_spells_teleport_swap00);
 tolua_function(tolua_S,NULL,"project_meteor",toluaI_spells_project_meteor00);
 tolua_function(tolua_S,NULL,"passwall",toluaI_spells_passwall00);
 tolua_function(tolua_S,NULL,"project_hook",toluaI_spells_project_hook00);
 tolua_function(tolua_S,NULL,"wizard_lock",toluaI_spells_wizard_lock00);
 tolua_function(tolua_S,NULL,"reset_recall",toluaI_spells_reset_recall00);
 tolua_function(tolua_S,NULL,"get_aim_dir",toluaI_spells_get_aim_dir00);
 tolua_function(tolua_S,NULL,"get_rep_dir",toluaI_spells_get_rep_dir00);
 tolua_function(tolua_S,NULL,"project_los",toluaI_spells_project_los00);
 tolua_function(tolua_S,NULL,"map_area",toluaI_spells_map_area00);
 tolua_function(tolua_S,NULL,"wiz_lite",toluaI_spells_wiz_lite00);
 tolua_function(tolua_S,NULL,"wiz_lite_extra",toluaI_spells_wiz_lite_extra00);
 tolua_function(tolua_S,NULL,"wiz_dark",toluaI_spells_wiz_dark00);
 tolua_function(tolua_S,NULL,"create_between_gate",toluaI_spells_create_between_gate00);
 tolua_function(tolua_S,NULL,"destroy_doors_touch",toluaI_spells_destroy_doors_touch00);
 tolua_function(tolua_S,NULL,"destroy_traps_touch",toluaI_spells_destroy_traps_touch00);
 tolua_cclass(tolua_S,"magic_power","");
 tolua_tablevar(tolua_S,"magic_power","min_lev",toluaI_get_spells_magic_power_min_lev,toluaI_set_spells_magic_power_min_lev);
 tolua_tablevar(tolua_S,"magic_power","mana_cost",toluaI_get_spells_magic_power_mana_cost,toluaI_set_spells_magic_power_mana_cost);
 tolua_tablevar(tolua_S,"magic_power","fail",toluaI_get_spells_magic_power_fail,toluaI_set_spells_magic_power_fail);
 tolua_tablevar(tolua_S,"magic_power","name",toluaI_get_spells_magic_power_name,toluaI_set_spells_magic_power_name);
 tolua_tablevar(tolua_S,"magic_power","desc",toluaI_get_spells_magic_power_desc,toluaI_set_spells_magic_power_desc);
 tolua_function(tolua_S,NULL,"new_magic_power",toluaI_spells_new_magic_power00);
 tolua_function(tolua_S,NULL,"get_magic_power",toluaI_spells_get_magic_power00);
 tolua_function(tolua_S,NULL,"select_magic_power",toluaI_spells_select_magic_power00);
 tolua_function(tolua_S,NULL,"magic_power_sucess",toluaI_spells_magic_power_sucess00);
 tolua_function(tolua_S,NULL,"add_new_power",toluaI_spells_add_new_power00);
 tolua_globalvar(tolua_S,"power_max",toluaI_get_spells_power_max,toluaI_set_spells_power_max);
 tolua_cclass(tolua_S,"spell_type","school_spell_type");
 tolua_tablevar(tolua_S,"spell_type","name",toluaI_get_spells_spell_type_name,toluaI_set_spells_spell_type_name);
 tolua_tablevar(tolua_S,"spell_type","skill_level",toluaI_get_spells_spell_type_skill_level,toluaI_set_spells_spell_type_skill_level);
 tolua_tablevar(tolua_S,"spell_type","mana",toluaI_get_spells_spell_type_mana,toluaI_set_spells_spell_type_mana);
 tolua_tablevar(tolua_S,"spell_type","mana_max",toluaI_get_spells_spell_type_mana_max,toluaI_set_spells_spell_type_mana_max);
 tolua_tablevar(tolua_S,"spell_type","fail",toluaI_get_spells_spell_type_fail,toluaI_set_spells_spell_type_fail);
 tolua_tablevar(tolua_S,"spell_type","level",toluaI_get_spells_spell_type_level,toluaI_set_spells_spell_type_level);
 tolua_cclass(tolua_S,"school_type","");
 tolua_tablevar(tolua_S,"school_type","name",toluaI_get_spells_school_type_name,toluaI_set_spells_school_type_name);
 tolua_tablevar(tolua_S,"school_type","skill",toluaI_get_spells_school_type_skill,toluaI_set_spells_school_type_skill);
 tolua_function(tolua_S,NULL,"new_school",toluaI_spells_new_school00);
 tolua_function(tolua_S,NULL,"new_spell",toluaI_spells_new_spell00);
 tolua_function(tolua_S,NULL,"spell",toluaI_spells_spell00);
 tolua_function(tolua_S,NULL,"school",toluaI_spells_school00);
 tolua_function(tolua_S,NULL,"lua_get_level",toluaI_spells_lua_get_level00);
 tolua_function(tolua_S,NULL,"lua_spell_chance",toluaI_spells_lua_spell_chance00);
 tolua_function(tolua_S,NULL,"lua_spell_device_chance",toluaI_spells_lua_spell_device_chance00);
 tolua_function(tolua_S,NULL,"get_school_spell",toluaI_spells_get_school_spell00);
 tolua_globalvar(tolua_S,"last_teleportation_y",toluaI_get_spells_last_teleportation_y,toluaI_set_spells_last_teleportation_y);
 tolua_globalvar(tolua_S,"last_teleportation_x",toluaI_get_spells_last_teleportation_x,toluaI_set_spells_last_teleportation_x);
 tolua_function(tolua_S,NULL,"get_pos_player",toluaI_spells_get_pos_player00);
 return 1;
}
/* Close function */
void tolua_spells_close (lua_State* tolua_S)
{
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"DEFAULT_RADIUS");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"GF_ELEC");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"GF_POIS");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"GF_ACID");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"GF_COLD");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"GF_FIRE");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"GF_UNBREATH");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"GF_CORPSE_EXPL");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"GF_MISSILE");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"GF_ARROW");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"GF_PLASMA");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"GF_WAVE");
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
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"GF_IDENTIFY");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"GF_RAISE");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"GF_STAR_IDENTIFY");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"GF_DESTRUCTION");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"GF_STUN_CONF");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"GF_STUN_DAM");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"GF_CONF_DAM");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"GF_STAR_CHARM");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"GF_IMPLOSION");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"GF_LAVA_FLOW");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"GF_FEAR");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"GF_BETWEEN_GATE");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"GF_WINDS_MANA");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"GF_DEATH");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"GF_CONTROL_DEMON");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"GF_RAISE_DEMON");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"GF_TRAP_DEMONSOUL");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"GF_ATTACK");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"GF_CHARM_UNMOVING");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"MAX_GF");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"PROJECT_JUMP");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"PROJECT_BEAM");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"PROJECT_THRU");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"PROJECT_STOP");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"PROJECT_GRID");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"PROJECT_ITEM");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"PROJECT_KILL");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"PROJECT_HIDE");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"PROJECT_VIEWABLE");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"PROJECT_METEOR_SHOWER");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"PROJECT_BLAST");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"PROJECT_PANEL");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"PROJECT_ALL");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"PROJECT_WALL");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"PROJECT_MANA_PATH");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"PROJECT_ABSORB_MANA");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"PROJECT_STAY");
 lua_getglobals(tolua_S);
 lua_pushstring(tolua_S,"project_time"); lua_pushnil(tolua_S); lua_rawset(tolua_S,-3);
 lua_pop(tolua_S,1);
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"teleport_player_directed");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"teleport_away");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"teleport_player");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"teleport_player_to");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"teleport_monster_to");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"teleport_monster");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"teleport_player_level");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"fetch");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"recall_player");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"take_hit");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"take_sanity_hit");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"project");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"corrupt_player");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"grow_things");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"grow_grass");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"grow_trees");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"hp_player");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"heal_insanity");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"warding_glyph");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"explosive_rune");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"do_dec_stat");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"do_res_stat");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"do_inc_stat");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"identify_pack");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"remove_curse");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"remove_all_curse");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"restore_level");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"self_knowledge");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"lose_all_info");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"detect_traps");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"detect_doors");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"detect_stairs");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"detect_treasure");
 lua_getglobals(tolua_S);
 lua_pushstring(tolua_S,"hack_no_detect_message"); lua_pushnil(tolua_S); lua_rawset(tolua_S,-3);
 lua_pop(tolua_S,1);
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"detect_objects_gold");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"detect_objects_normal");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"detect_objects_magic");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"detect_monsters_normal");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"detect_monsters_invis");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"detect_monsters_evil");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"detect_monsters_good");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"detect_monsters_xxx");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"detect_monsters_string");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"detect_monsters_nonliving");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"detect_all");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"stair_creation");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"tgt_pt");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"wall_stone");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"create_artifact");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"wall_to_mud");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"ident_spell");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"identify_fully");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"recharge");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"aggravate_monsters");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"genocide_aux");
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
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"turn_undead");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"door_creation");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"trap_creation");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"glyph_creation");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"wipe");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"destroy_area");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"earthquake");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"lite_room");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"unlite_room");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"lite_area");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"unlite_area");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"fire_ball_beam");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"make_wish");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"fire_wave");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"EFF_WAVE");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"EFF_LAST");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"EFF_STORM");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"EFF_DIR1");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"EFF_DIR2");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"EFF_DIR3");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"EFF_DIR4");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"EFF_DIR6");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"EFF_DIR7");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"EFF_DIR8");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"EFF_DIR9");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"fire_cloud");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"fire_wall");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"fire_ball");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"fire_bolt");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"fire_beam");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"fire_druid_ball");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"fire_druid_bolt");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"fire_druid_beam");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"fire_bolt_or_beam");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"alchemy");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"alter_reality");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"swap_position");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"teleport_swap");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"project_meteor");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"passwall");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"project_hook");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"wizard_lock");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"reset_recall");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"get_aim_dir");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"get_rep_dir");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"project_los");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"map_area");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"wiz_lite");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"wiz_lite_extra");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"wiz_dark");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"create_between_gate");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"destroy_doors_touch");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"destroy_traps_touch");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"magic_power");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"new_magic_power");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"get_magic_power");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"select_magic_power");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"magic_power_sucess");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"add_new_power");
 lua_getglobals(tolua_S);
 lua_pushstring(tolua_S,"power_max"); lua_pushnil(tolua_S); lua_rawset(tolua_S,-3);
 lua_pop(tolua_S,1);
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"spell_type");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"school_type");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"new_school");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"new_spell");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"spell");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"school");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"lua_get_level");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"lua_spell_chance");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"lua_spell_device_chance");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"get_school_spell");
 lua_getglobals(tolua_S);
 lua_pushstring(tolua_S,"last_teleportation_y"); lua_pushnil(tolua_S); lua_rawset(tolua_S,-3);
 lua_pop(tolua_S,1);
 lua_getglobals(tolua_S);
 lua_pushstring(tolua_S,"last_teleportation_x"); lua_pushnil(tolua_S); lua_rawset(tolua_S,-3);
 lua_pop(tolua_S,1);
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"get_pos_player");
}
