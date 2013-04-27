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
 tolua_usertype(tolua_S,"monster_race");
}

/* get function: flags of class  monster_race */
static int toluaI_get_monst_monster_race_flags(lua_State* tolua_S)
{
 int toluaI_index;
 TOLUA_ARRAY_SELF(monster_race);
 TOLUA_ARRAY_INDEX("monster_race: flags",9);
 tolua_pushnumber(tolua_S,(long)self->flags[toluaI_index]);
 return 1;
}

/* set function: flags of class  monster_race */
static int toluaI_set_monst_monster_race_flags(lua_State* tolua_S)
{
 int toluaI_index;
 TOLUA_ARRAY_SELF(monster_race);
 TOLUA_ARRAY_INDEX("monster_race: flags",9);
  self->flags[toluaI_index] = ((u32b)  tolua_getnumber(tolua_S,3,0));
 return 0;
}

/* get function: d_char of class  monster_race */
static int toluaI_get_monst_monster_race_d_char(lua_State* tolua_S)
{
  TOLUA_GET_SELF(monster_race);
  tolua_pushnumber(tolua_S,(long)self->d_char);
 return 1;
}

/* set function: d_char of class  monster_race */
static int toluaI_set_monst_monster_race_d_char(lua_State* tolua_S)
{
  TOLUA_GET_SELF(monster_race);
  if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
   TOLUA_ERR_ASSIGN;
  self->d_char = ((char)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: level of class  monster_race */
static int toluaI_get_monst_monster_race_level(lua_State* tolua_S)
{
  TOLUA_GET_SELF(monster_race);
  tolua_pushnumber(tolua_S,(long)self->level);
 return 1;
}

/* set function: level of class  monster_race */
static int toluaI_set_monst_monster_race_level(lua_State* tolua_S)
{
  TOLUA_GET_SELF(monster_race);
  if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
   TOLUA_ERR_ASSIGN;
  self->level = ((byte)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: r_info */
static int toluaI_get_monst_r_info(lua_State* tolua_S)
{
  tolua_pushusertype(tolua_S,(void*)r_info,tolua_tag(tolua_S,"monster_race"));
 return 1;
}

/* set function: r_info */
static int toluaI_set_monst_r_info(lua_State* tolua_S)
{
  if (!tolua_istype(tolua_S,1,tolua_tag(tolua_S,"monster_race"),0))
   TOLUA_ERR_ASSIGN;
  r_info = ((monster_race*)  tolua_getusertype(tolua_S,1,0));
 return 0;
}

/* function: summon_specific */
static int toluaI_monst_summon_specific00(lua_State* tolua_S)
{
 if (!tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
     !tolua_istype(tolua_S,2,LUA_TNUMBER,0) ||
     !tolua_istype(tolua_S,3,LUA_TNUMBER,0) ||
     !tolua_istype(tolua_S,4,LUA_TNUMBER,0) ||
     !tolua_istype(tolua_S,5,LUA_TNUMBER,0) ||
     !tolua_istype(tolua_S,6,tolua_tag(tolua_S,"bool"),0) ||
     !tolua_istype(tolua_S,7,tolua_tag(tolua_S,"bool"),0) ||
     !tolua_istype(tolua_S,8,tolua_tag(tolua_S,"bool"),0) ||
     !tolua_isnoobj(tolua_S,9))
 {
  TOLUA_ERR_FN(summon_specific);
 } else {
  int who = ((int)  tolua_getnumber(tolua_S,1,0));
  int x1 = ((int)  tolua_getnumber(tolua_S,2,0));
  int y1 = ((int)  tolua_getnumber(tolua_S,3,0));
  int lev = ((int)  tolua_getnumber(tolua_S,4,0));
  int type = ((int)  tolua_getnumber(tolua_S,5,0));
  bool group = ((bool)  tolua_getbool(tolua_S,6,0));
  bool friendly = ((bool)  tolua_getbool(tolua_S,7,0));
  bool pet = ((bool)  tolua_getbool(tolua_S,8,0));
  summon_specific(who,x1,y1,lev,type,group,friendly,pet);
 }
 return 0;
}

/* function: summon_cloned_creature */
static int toluaI_monst_summon_cloned_creature00(lua_State* tolua_S)
{
 if (!tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
     !tolua_istype(tolua_S,2,LUA_TNUMBER,0) ||
     !tolua_istype(tolua_S,3,LUA_TNUMBER,0) ||
     !tolua_istype(tolua_S,4,tolua_tag(tolua_S,"bool"),0) ||
     !tolua_isnoobj(tolua_S,5))
 {
  TOLUA_ERR_FN(summon_cloned_creature);
 } else {
  int x1 = ((int)  tolua_getnumber(tolua_S,1,0));
  int y1 = ((int)  tolua_getnumber(tolua_S,2,0));
  int r_idx = ((int)  tolua_getnumber(tolua_S,3,0));
  bool pet = ((bool)  tolua_getbool(tolua_S,4,0));
  summon_cloned_creature(x1,y1,r_idx,pet);
 }
 return 0;
}

/* function: monst_race */
static int toluaI_monst_monst_race00(lua_State* tolua_S)
{
 if (!tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
     !tolua_isnoobj(tolua_S,2))
 {
  TOLUA_ERR_FN(monst_race);
 } else {
  int r_idx = ((int)  tolua_getnumber(tolua_S,1,0));
  monster_race* toluaI_ret = (monster_race*)  monst_race(r_idx);
   tolua_pushusertype(tolua_S,(void*)toluaI_ret,tolua_tag(tolua_S,"monster_race"));
 }
 return 1;
}

/* function: mon_race_name */
static int toluaI_monst_mon_race_name00(lua_State* tolua_S)
{
 if (!tolua_istype(tolua_S,1,tolua_tag(tolua_S,"monster_race"),0) ||
     !tolua_isnoobj(tolua_S,2))
 {
  TOLUA_ERR_FN(mon_race_name);
 } else {
  monster_race* r_ptr = ((monster_race*)  tolua_getusertype(tolua_S,1,0));
  cptr toluaI_ret = (cptr)  mon_race_name(r_ptr);
  tolua_pushstring(tolua_S,(const char*)toluaI_ret);
 }
 return 1;
}

/* function: monster_can_open */
static int toluaI_monst_monster_can_open00(lua_State* tolua_S)
{
 if (!tolua_istype(tolua_S,1,tolua_tag(tolua_S,"monster_race"),0) ||
     !tolua_istype(tolua_S,2,LUA_TNUMBER,0) ||
     !tolua_isnoobj(tolua_S,3))
 {
  TOLUA_ERR_FN(monster_can_open);
 } else {
  monster_race* r_ptr = ((monster_race*)  tolua_getusertype(tolua_S,1,0));
  int power = ((int)  tolua_getnumber(tolua_S,2,0));
  bool toluaI_ret = (bool)  monster_can_open(r_ptr,power);
  tolua_pushbool(tolua_S,(int)toluaI_ret);
 }
 return 1;
}

/* function: summon_monsters_near_player */
static int toluaI_monst_summon_monsters_near_player00(lua_State* tolua_S)
{
 if (!tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
     !tolua_istype(tolua_S,2,LUA_TNUMBER,0) ||
     !tolua_isnoobj(tolua_S,3))
 {
  TOLUA_ERR_FN(summon_monsters_near_player);
 } else {
  int num = ((int)  tolua_getnumber(tolua_S,1,0));
  int type = ((int)  tolua_getnumber(tolua_S,2,0));
  bool toluaI_ret = (bool)  summon_monsters_near_player(num,type);
  tolua_pushbool(tolua_S,(int)toluaI_ret);
 }
 return 1;
}

/* Open function */
int tolua_monst_open (lua_State* tolua_S)
{
 tolua_open(tolua_S);
 toluaI_reg_types(tolua_S);
 TOLUA_DEF(RF0_UNIQUE);
 TOLUA_DEF(RF0_QUESTOR);
 TOLUA_DEF(RF0_MALE);
 TOLUA_DEF(RF0_FEMALE);
 TOLUA_DEF(RF0_CHAR_CLEAR);
 TOLUA_DEF(RF0_CHAR_MIMIC);
 TOLUA_DEF(RF0_ATTR_CLEAR);
 TOLUA_DEF(RF0_ATTR_MULTI);
 TOLUA_DEF(RF0_FORCE_DEPTH);
 TOLUA_DEF(RF0_FORCE_MAXHP);
 TOLUA_DEF(RF0_FORCE_SLEEP);
 TOLUA_DEF(RF0_FORCE_EXTRA);
 TOLUA_DEF(RF0_XXX_1);
 TOLUA_DEF(RF0_FRIENDS);
 TOLUA_DEF(RF0_ESCORT);
 TOLUA_DEF(RF0_ESCORTS);
 TOLUA_DEF(RF0_NEVER_BLOW);
 TOLUA_DEF(RF0_NEVER_MOVE);
 TOLUA_DEF(RF0_RAND_25);
 TOLUA_DEF(RF0_RAND_50);
 TOLUA_DEF(RF0_ONLY_GOLD);
 TOLUA_DEF(RF0_ONLY_ITEM);
 TOLUA_DEF(RF0_DROP_60);
 TOLUA_DEF(RF0_DROP_90);
 TOLUA_DEF(RF0_DROP_1D2);
 TOLUA_DEF(RF0_DROP_2D2);
 TOLUA_DEF(RF0_DROP_3D2);
 TOLUA_DEF(RF0_DROP_4D2);
 TOLUA_DEF(RF0_DROP_GOOD);
 TOLUA_DEF(RF0_DROP_GREAT);
 TOLUA_DEF(RF0_DROP_USEFUL);
 TOLUA_DEF(RF0_DROP_CHOSEN);
 TOLUA_DEF(RF1_STUPID);
 TOLUA_DEF(RF1_SMART);
 TOLUA_DEF(RF1_CAN_SPEAK);
 TOLUA_DEF(RF1_REFLECTING);
 TOLUA_DEF(RF1_INVISIBLE);
 TOLUA_DEF(RF1_COLD_BLOOD);
 TOLUA_DEF(RF1_EMPTY_MIND);
 TOLUA_DEF(RF1_WEIRD_MIND);
 TOLUA_DEF(RF1_MULTIPLY);
 TOLUA_DEF(RF1_REGENERATE);
 TOLUA_DEF(RF1_SHAPECHANGER);
 TOLUA_DEF(RF1_ATTR_ANY);
 TOLUA_DEF(RF1_POWERFUL);
 TOLUA_DEF(RF1_XXX_1);
 TOLUA_DEF(RF1_AURA_FIRE);
 TOLUA_DEF(RF1_AURA_ELEC);
 TOLUA_DEF(RF1_OPEN_DOOR);
 TOLUA_DEF(RF1_BASH_DOOR);
 TOLUA_DEF(RF1_PASS_WALL);
 TOLUA_DEF(RF1_KILL_WALL);
 TOLUA_DEF(RF1_MOVE_BODY);
 TOLUA_DEF(RF1_KILL_BODY);
 TOLUA_DEF(RF1_TAKE_ITEM);
 TOLUA_DEF(RF1_KILL_ITEM);
 TOLUA_DEF(RF1_BRAIN_1);
 TOLUA_DEF(RF1_BRAIN_2);
 TOLUA_DEF(RF1_BRAIN_3);
 TOLUA_DEF(RF1_BRAIN_4);
 TOLUA_DEF(RF1_BRAIN_5);
 TOLUA_DEF(RF1_BRAIN_6);
 TOLUA_DEF(RF1_BRAIN_7);
 TOLUA_DEF(RF1_QUANTUM);
 TOLUA_DEF(RF2_ORC);
 TOLUA_DEF(RF2_TROLL);
 TOLUA_DEF(RF2_GIANT);
 TOLUA_DEF(RF2_DRAGON);
 TOLUA_DEF(RF2_DEMON);
 TOLUA_DEF(RF2_UNDEAD);
 TOLUA_DEF(RF2_EVIL);
 TOLUA_DEF(RF2_ANIMAL);
 TOLUA_DEF(RF2_AMBERITE);
 TOLUA_DEF(RF2_GOOD);
 TOLUA_DEF(RF2_AURA_COLD);
 TOLUA_DEF(RF2_NONLIVING);
 TOLUA_DEF(RF2_HURT_LITE);
 TOLUA_DEF(RF2_HURT_ROCK);
 TOLUA_DEF(RF2_HURT_FIRE);
 TOLUA_DEF(RF2_HURT_COLD);
 TOLUA_DEF(RF2_IM_ACID);
 TOLUA_DEF(RF2_IM_ELEC);
 TOLUA_DEF(RF2_IM_FIRE);
 TOLUA_DEF(RF2_IM_COLD);
 TOLUA_DEF(RF2_IM_POIS);
 TOLUA_DEF(RF2_RES_TELE);
 TOLUA_DEF(RF2_RES_NETH);
 TOLUA_DEF(RF2_RES_WATE);
 TOLUA_DEF(RF2_RES_PLAS);
 TOLUA_DEF(RF2_RES_NEXU);
 TOLUA_DEF(RF2_RES_DISE);
 TOLUA_DEF(RF2_UNIQUE_7);
 TOLUA_DEF(RF2_NO_FEAR);
 TOLUA_DEF(RF2_NO_STUN);
 TOLUA_DEF(RF2_NO_CONF);
 TOLUA_DEF(RF2_NO_SLEEP);
 TOLUA_DEF(RF3_SHRIEK);
 TOLUA_DEF(RF3_ELDRITCH_HORROR);
 TOLUA_DEF(RF3_XXX3);
 TOLUA_DEF(RF3_ROCKET);
 TOLUA_DEF(RF3_ARROW);
 TOLUA_DEF(RF3_XXX6);
 TOLUA_DEF(RF3_XXX7);
 TOLUA_DEF(RF3_XXX8);
 TOLUA_DEF(RF3_BR_ACID);
 TOLUA_DEF(RF3_BR_ELEC);
 TOLUA_DEF(RF3_BR_FIRE);
 TOLUA_DEF(RF3_BR_COLD);
 TOLUA_DEF(RF3_BR_POIS);
 TOLUA_DEF(RF3_BR_NETH);
 TOLUA_DEF(RF3_BR_LITE);
 TOLUA_DEF(RF3_BR_DARK);
 TOLUA_DEF(RF3_BR_CONF);
 TOLUA_DEF(RF3_BR_SOUN);
 TOLUA_DEF(RF3_BR_CHAO);
 TOLUA_DEF(RF3_BR_DISE);
 TOLUA_DEF(RF3_BR_NEXU);
 TOLUA_DEF(RF3_BR_TIME);
 TOLUA_DEF(RF3_BR_INER);
 TOLUA_DEF(RF3_BR_GRAV);
 TOLUA_DEF(RF3_BR_SHAR);
 TOLUA_DEF(RF3_BR_PLAS);
 TOLUA_DEF(RF3_BR_WALL);
 TOLUA_DEF(RF3_BR_MANA);
 TOLUA_DEF(RF3_BA_NUKE);
 TOLUA_DEF(RF3_BR_NUKE);
 TOLUA_DEF(RF3_BA_CHAO);
 TOLUA_DEF(RF3_BR_DISI);
 TOLUA_DEF(RF4_BA_ACID);
 TOLUA_DEF(RF4_BA_ELEC);
 TOLUA_DEF(RF4_BA_FIRE);
 TOLUA_DEF(RF4_BA_COLD);
 TOLUA_DEF(RF4_BA_POIS);
 TOLUA_DEF(RF4_BA_NETH);
 TOLUA_DEF(RF4_BA_WATE);
 TOLUA_DEF(RF4_BA_MANA);
 TOLUA_DEF(RF4_BA_DARK);
 TOLUA_DEF(RF4_DRAIN_MANA);
 TOLUA_DEF(RF4_MIND_BLAST);
 TOLUA_DEF(RF4_BRAIN_SMASH);
 TOLUA_DEF(RF4_CAUSE_1);
 TOLUA_DEF(RF4_CAUSE_2);
 TOLUA_DEF(RF4_CAUSE_3);
 TOLUA_DEF(RF4_CAUSE_4);
 TOLUA_DEF(RF4_BO_ACID);
 TOLUA_DEF(RF4_BO_ELEC);
 TOLUA_DEF(RF4_BO_FIRE);
 TOLUA_DEF(RF4_BO_COLD);
 TOLUA_DEF(RF4_BO_POIS);
 TOLUA_DEF(RF4_BO_NETH);
 TOLUA_DEF(RF4_BO_WATE);
 TOLUA_DEF(RF4_BO_MANA);
 TOLUA_DEF(RF4_BO_PLAS);
 TOLUA_DEF(RF4_BO_ICEE);
 TOLUA_DEF(RF4_MISSILE);
 TOLUA_DEF(RF4_SCARE);
 TOLUA_DEF(RF4_BLIND);
 TOLUA_DEF(RF4_CONF);
 TOLUA_DEF(RF4_SLOW);
 TOLUA_DEF(RF4_HOLD);
 TOLUA_DEF(RF5_HASTE);
 TOLUA_DEF(RF5_HAND_DOOM);
 TOLUA_DEF(RF5_HEAL);
 TOLUA_DEF(RF5_INVULNER);
 TOLUA_DEF(RF5_BLINK);
 TOLUA_DEF(RF5_TPORT);
 TOLUA_DEF(RF5_XXX3);
 TOLUA_DEF(RF5_XXX4);
 TOLUA_DEF(RF5_TELE_TO);
 TOLUA_DEF(RF5_TELE_AWAY);
 TOLUA_DEF(RF5_TELE_LEVEL);
 TOLUA_DEF(RF5_XXX5);
 TOLUA_DEF(RF5_DARKNESS);
 TOLUA_DEF(RF5_TRAPS);
 TOLUA_DEF(RF5_FORGET);
 TOLUA_DEF(RF5_RAISE_DEAD);
 TOLUA_DEF(RF5_S_KIN);
 TOLUA_DEF(RF5_S_CYBER);
 TOLUA_DEF(RF5_S_MONSTER);
 TOLUA_DEF(RF5_S_MONSTERS);
 TOLUA_DEF(RF5_S_ANT);
 TOLUA_DEF(RF5_S_SPIDER);
 TOLUA_DEF(RF5_S_HOUND);
 TOLUA_DEF(RF5_S_HYDRA);
 TOLUA_DEF(RF5_S_ANGEL);
 TOLUA_DEF(RF5_S_DEMON);
 TOLUA_DEF(RF5_S_UNDEAD);
 TOLUA_DEF(RF5_S_DRAGON);
 TOLUA_DEF(RF5_S_HI_UNDEAD);
 TOLUA_DEF(RF5_S_HI_DRAGON);
 TOLUA_DEF(RF5_S_AMBERITES);
 TOLUA_DEF(RF5_S_UNIQUE);
 TOLUA_DEF(RF6_AQUATIC);
 TOLUA_DEF(RF6_CAN_SWIM);
 TOLUA_DEF(RF6_CAN_FLY);
 TOLUA_DEF(RF6_FRIENDLY);
 TOLUA_DEF(RF6_SILLY);
 TOLUA_DEF(RF6_LITE_1);
 TOLUA_DEF(RF6_LITE_2);
 TOLUA_DEF(RF7_WILD);
 TOLUA_DEF(RF8_DROP_CORPSE);
 TOLUA_DEF(RF8_DROP_SKELETON);
 TOLUA_DEF(SUMMON_ANT);
 TOLUA_DEF(SUMMON_SPIDER);
 TOLUA_DEF(SUMMON_HOUND);
 TOLUA_DEF(SUMMON_HYDRA);
 TOLUA_DEF(SUMMON_ANGEL);
 TOLUA_DEF(SUMMON_DEMON);
 TOLUA_DEF(SUMMON_UNDEAD);
 TOLUA_DEF(SUMMON_DRAGON);
 TOLUA_DEF(SUMMON_HI_UNDEAD);
 TOLUA_DEF(SUMMON_HI_DRAGON);
 TOLUA_DEF(SUMMON_AMBERITES);
 TOLUA_DEF(SUMMON_UNIQUE);
 TOLUA_DEF(SUMMON_BIZARRE1);
 TOLUA_DEF(SUMMON_BIZARRE2);
 TOLUA_DEF(SUMMON_BIZARRE3);
 TOLUA_DEF(SUMMON_BIZARRE4);
 TOLUA_DEF(SUMMON_BIZARRE5);
 TOLUA_DEF(SUMMON_BIZARRE6);
 TOLUA_DEF(SUMMON_CYBER);
 TOLUA_DEF(SUMMON_KIN);
 TOLUA_DEF(SUMMON_DAWN);
 TOLUA_DEF(SUMMON_ANIMAL);
 TOLUA_DEF(SUMMON_ANIMAL_RANGER);
 TOLUA_DEF(SUMMON_HI_UNDEAD_NO_UNIQUES);
 TOLUA_DEF(SUMMON_HI_DRAGON_NO_UNIQUES);
 TOLUA_DEF(SUMMON_NO_UNIQUES);
 TOLUA_DEF(SUMMON_PHANTOM);
 TOLUA_DEF(SUMMON_ELEMENTAL);
 TOLUA_DEF(SUMMON_BLUE_HORROR);
 tolua_cclass(tolua_S,"monster_race","");
 tolua_tablearray(tolua_S,"monster_race","flags",toluaI_get_monst_monster_race_flags,toluaI_set_monst_monster_race_flags);
 tolua_tablevar(tolua_S,"monster_race","d_char",toluaI_get_monst_monster_race_d_char,toluaI_set_monst_monster_race_d_char);
 tolua_tablevar(tolua_S,"monster_race","level",toluaI_get_monst_monster_race_level,toluaI_set_monst_monster_race_level);
 tolua_globalvar(tolua_S,"r_info",toluaI_get_monst_r_info,toluaI_set_monst_r_info);
 TOLUA_FUN(summon_specific,toluaI_monst_summon_specific00);
 TOLUA_FUN(summon_cloned_creature,toluaI_monst_summon_cloned_creature00);
 TOLUA_FUN(monst_race,toluaI_monst_monst_race00);
 TOLUA_FUN(mon_race_name,toluaI_monst_mon_race_name00);
 TOLUA_FUN(monster_can_open,toluaI_monst_monster_can_open00);
 TOLUA_FUN(summon_monsters_near_player,toluaI_monst_summon_monsters_near_player00);
 return 1;
}
/* Close function */
void tolua_monst_close (lua_State* tolua_S)
{
 TOLUA_UNDEF(RF0_UNIQUE);
 TOLUA_UNDEF(RF0_QUESTOR);
 TOLUA_UNDEF(RF0_MALE);
 TOLUA_UNDEF(RF0_FEMALE);
 TOLUA_UNDEF(RF0_CHAR_CLEAR);
 TOLUA_UNDEF(RF0_CHAR_MIMIC);
 TOLUA_UNDEF(RF0_ATTR_CLEAR);
 TOLUA_UNDEF(RF0_ATTR_MULTI);
 TOLUA_UNDEF(RF0_FORCE_DEPTH);
 TOLUA_UNDEF(RF0_FORCE_MAXHP);
 TOLUA_UNDEF(RF0_FORCE_SLEEP);
 TOLUA_UNDEF(RF0_FORCE_EXTRA);
 TOLUA_UNDEF(RF0_XXX_1);
 TOLUA_UNDEF(RF0_FRIENDS);
 TOLUA_UNDEF(RF0_ESCORT);
 TOLUA_UNDEF(RF0_ESCORTS);
 TOLUA_UNDEF(RF0_NEVER_BLOW);
 TOLUA_UNDEF(RF0_NEVER_MOVE);
 TOLUA_UNDEF(RF0_RAND_25);
 TOLUA_UNDEF(RF0_RAND_50);
 TOLUA_UNDEF(RF0_ONLY_GOLD);
 TOLUA_UNDEF(RF0_ONLY_ITEM);
 TOLUA_UNDEF(RF0_DROP_60);
 TOLUA_UNDEF(RF0_DROP_90);
 TOLUA_UNDEF(RF0_DROP_1D2);
 TOLUA_UNDEF(RF0_DROP_2D2);
 TOLUA_UNDEF(RF0_DROP_3D2);
 TOLUA_UNDEF(RF0_DROP_4D2);
 TOLUA_UNDEF(RF0_DROP_GOOD);
 TOLUA_UNDEF(RF0_DROP_GREAT);
 TOLUA_UNDEF(RF0_DROP_USEFUL);
 TOLUA_UNDEF(RF0_DROP_CHOSEN);
 TOLUA_UNDEF(RF1_STUPID);
 TOLUA_UNDEF(RF1_SMART);
 TOLUA_UNDEF(RF1_CAN_SPEAK);
 TOLUA_UNDEF(RF1_REFLECTING);
 TOLUA_UNDEF(RF1_INVISIBLE);
 TOLUA_UNDEF(RF1_COLD_BLOOD);
 TOLUA_UNDEF(RF1_EMPTY_MIND);
 TOLUA_UNDEF(RF1_WEIRD_MIND);
 TOLUA_UNDEF(RF1_MULTIPLY);
 TOLUA_UNDEF(RF1_REGENERATE);
 TOLUA_UNDEF(RF1_SHAPECHANGER);
 TOLUA_UNDEF(RF1_ATTR_ANY);
 TOLUA_UNDEF(RF1_POWERFUL);
 TOLUA_UNDEF(RF1_XXX_1);
 TOLUA_UNDEF(RF1_AURA_FIRE);
 TOLUA_UNDEF(RF1_AURA_ELEC);
 TOLUA_UNDEF(RF1_OPEN_DOOR);
 TOLUA_UNDEF(RF1_BASH_DOOR);
 TOLUA_UNDEF(RF1_PASS_WALL);
 TOLUA_UNDEF(RF1_KILL_WALL);
 TOLUA_UNDEF(RF1_MOVE_BODY);
 TOLUA_UNDEF(RF1_KILL_BODY);
 TOLUA_UNDEF(RF1_TAKE_ITEM);
 TOLUA_UNDEF(RF1_KILL_ITEM);
 TOLUA_UNDEF(RF1_BRAIN_1);
 TOLUA_UNDEF(RF1_BRAIN_2);
 TOLUA_UNDEF(RF1_BRAIN_3);
 TOLUA_UNDEF(RF1_BRAIN_4);
 TOLUA_UNDEF(RF1_BRAIN_5);
 TOLUA_UNDEF(RF1_BRAIN_6);
 TOLUA_UNDEF(RF1_BRAIN_7);
 TOLUA_UNDEF(RF1_QUANTUM);
 TOLUA_UNDEF(RF2_ORC);
 TOLUA_UNDEF(RF2_TROLL);
 TOLUA_UNDEF(RF2_GIANT);
 TOLUA_UNDEF(RF2_DRAGON);
 TOLUA_UNDEF(RF2_DEMON);
 TOLUA_UNDEF(RF2_UNDEAD);
 TOLUA_UNDEF(RF2_EVIL);
 TOLUA_UNDEF(RF2_ANIMAL);
 TOLUA_UNDEF(RF2_AMBERITE);
 TOLUA_UNDEF(RF2_GOOD);
 TOLUA_UNDEF(RF2_AURA_COLD);
 TOLUA_UNDEF(RF2_NONLIVING);
 TOLUA_UNDEF(RF2_HURT_LITE);
 TOLUA_UNDEF(RF2_HURT_ROCK);
 TOLUA_UNDEF(RF2_HURT_FIRE);
 TOLUA_UNDEF(RF2_HURT_COLD);
 TOLUA_UNDEF(RF2_IM_ACID);
 TOLUA_UNDEF(RF2_IM_ELEC);
 TOLUA_UNDEF(RF2_IM_FIRE);
 TOLUA_UNDEF(RF2_IM_COLD);
 TOLUA_UNDEF(RF2_IM_POIS);
 TOLUA_UNDEF(RF2_RES_TELE);
 TOLUA_UNDEF(RF2_RES_NETH);
 TOLUA_UNDEF(RF2_RES_WATE);
 TOLUA_UNDEF(RF2_RES_PLAS);
 TOLUA_UNDEF(RF2_RES_NEXU);
 TOLUA_UNDEF(RF2_RES_DISE);
 TOLUA_UNDEF(RF2_UNIQUE_7);
 TOLUA_UNDEF(RF2_NO_FEAR);
 TOLUA_UNDEF(RF2_NO_STUN);
 TOLUA_UNDEF(RF2_NO_CONF);
 TOLUA_UNDEF(RF2_NO_SLEEP);
 TOLUA_UNDEF(RF3_SHRIEK);
 TOLUA_UNDEF(RF3_ELDRITCH_HORROR);
 TOLUA_UNDEF(RF3_XXX3);
 TOLUA_UNDEF(RF3_ROCKET);
 TOLUA_UNDEF(RF3_ARROW);
 TOLUA_UNDEF(RF3_XXX6);
 TOLUA_UNDEF(RF3_XXX7);
 TOLUA_UNDEF(RF3_XXX8);
 TOLUA_UNDEF(RF3_BR_ACID);
 TOLUA_UNDEF(RF3_BR_ELEC);
 TOLUA_UNDEF(RF3_BR_FIRE);
 TOLUA_UNDEF(RF3_BR_COLD);
 TOLUA_UNDEF(RF3_BR_POIS);
 TOLUA_UNDEF(RF3_BR_NETH);
 TOLUA_UNDEF(RF3_BR_LITE);
 TOLUA_UNDEF(RF3_BR_DARK);
 TOLUA_UNDEF(RF3_BR_CONF);
 TOLUA_UNDEF(RF3_BR_SOUN);
 TOLUA_UNDEF(RF3_BR_CHAO);
 TOLUA_UNDEF(RF3_BR_DISE);
 TOLUA_UNDEF(RF3_BR_NEXU);
 TOLUA_UNDEF(RF3_BR_TIME);
 TOLUA_UNDEF(RF3_BR_INER);
 TOLUA_UNDEF(RF3_BR_GRAV);
 TOLUA_UNDEF(RF3_BR_SHAR);
 TOLUA_UNDEF(RF3_BR_PLAS);
 TOLUA_UNDEF(RF3_BR_WALL);
 TOLUA_UNDEF(RF3_BR_MANA);
 TOLUA_UNDEF(RF3_BA_NUKE);
 TOLUA_UNDEF(RF3_BR_NUKE);
 TOLUA_UNDEF(RF3_BA_CHAO);
 TOLUA_UNDEF(RF3_BR_DISI);
 TOLUA_UNDEF(RF4_BA_ACID);
 TOLUA_UNDEF(RF4_BA_ELEC);
 TOLUA_UNDEF(RF4_BA_FIRE);
 TOLUA_UNDEF(RF4_BA_COLD);
 TOLUA_UNDEF(RF4_BA_POIS);
 TOLUA_UNDEF(RF4_BA_NETH);
 TOLUA_UNDEF(RF4_BA_WATE);
 TOLUA_UNDEF(RF4_BA_MANA);
 TOLUA_UNDEF(RF4_BA_DARK);
 TOLUA_UNDEF(RF4_DRAIN_MANA);
 TOLUA_UNDEF(RF4_MIND_BLAST);
 TOLUA_UNDEF(RF4_BRAIN_SMASH);
 TOLUA_UNDEF(RF4_CAUSE_1);
 TOLUA_UNDEF(RF4_CAUSE_2);
 TOLUA_UNDEF(RF4_CAUSE_3);
 TOLUA_UNDEF(RF4_CAUSE_4);
 TOLUA_UNDEF(RF4_BO_ACID);
 TOLUA_UNDEF(RF4_BO_ELEC);
 TOLUA_UNDEF(RF4_BO_FIRE);
 TOLUA_UNDEF(RF4_BO_COLD);
 TOLUA_UNDEF(RF4_BO_POIS);
 TOLUA_UNDEF(RF4_BO_NETH);
 TOLUA_UNDEF(RF4_BO_WATE);
 TOLUA_UNDEF(RF4_BO_MANA);
 TOLUA_UNDEF(RF4_BO_PLAS);
 TOLUA_UNDEF(RF4_BO_ICEE);
 TOLUA_UNDEF(RF4_MISSILE);
 TOLUA_UNDEF(RF4_SCARE);
 TOLUA_UNDEF(RF4_BLIND);
 TOLUA_UNDEF(RF4_CONF);
 TOLUA_UNDEF(RF4_SLOW);
 TOLUA_UNDEF(RF4_HOLD);
 TOLUA_UNDEF(RF5_HASTE);
 TOLUA_UNDEF(RF5_HAND_DOOM);
 TOLUA_UNDEF(RF5_HEAL);
 TOLUA_UNDEF(RF5_INVULNER);
 TOLUA_UNDEF(RF5_BLINK);
 TOLUA_UNDEF(RF5_TPORT);
 TOLUA_UNDEF(RF5_XXX3);
 TOLUA_UNDEF(RF5_XXX4);
 TOLUA_UNDEF(RF5_TELE_TO);
 TOLUA_UNDEF(RF5_TELE_AWAY);
 TOLUA_UNDEF(RF5_TELE_LEVEL);
 TOLUA_UNDEF(RF5_XXX5);
 TOLUA_UNDEF(RF5_DARKNESS);
 TOLUA_UNDEF(RF5_TRAPS);
 TOLUA_UNDEF(RF5_FORGET);
 TOLUA_UNDEF(RF5_RAISE_DEAD);
 TOLUA_UNDEF(RF5_S_KIN);
 TOLUA_UNDEF(RF5_S_CYBER);
 TOLUA_UNDEF(RF5_S_MONSTER);
 TOLUA_UNDEF(RF5_S_MONSTERS);
 TOLUA_UNDEF(RF5_S_ANT);
 TOLUA_UNDEF(RF5_S_SPIDER);
 TOLUA_UNDEF(RF5_S_HOUND);
 TOLUA_UNDEF(RF5_S_HYDRA);
 TOLUA_UNDEF(RF5_S_ANGEL);
 TOLUA_UNDEF(RF5_S_DEMON);
 TOLUA_UNDEF(RF5_S_UNDEAD);
 TOLUA_UNDEF(RF5_S_DRAGON);
 TOLUA_UNDEF(RF5_S_HI_UNDEAD);
 TOLUA_UNDEF(RF5_S_HI_DRAGON);
 TOLUA_UNDEF(RF5_S_AMBERITES);
 TOLUA_UNDEF(RF5_S_UNIQUE);
 TOLUA_UNDEF(RF6_AQUATIC);
 TOLUA_UNDEF(RF6_CAN_SWIM);
 TOLUA_UNDEF(RF6_CAN_FLY);
 TOLUA_UNDEF(RF6_FRIENDLY);
 TOLUA_UNDEF(RF6_SILLY);
 TOLUA_UNDEF(RF6_LITE_1);
 TOLUA_UNDEF(RF6_LITE_2);
 TOLUA_UNDEF(RF7_WILD);
 TOLUA_UNDEF(RF8_DROP_CORPSE);
 TOLUA_UNDEF(RF8_DROP_SKELETON);
 TOLUA_UNDEF(SUMMON_ANT);
 TOLUA_UNDEF(SUMMON_SPIDER);
 TOLUA_UNDEF(SUMMON_HOUND);
 TOLUA_UNDEF(SUMMON_HYDRA);
 TOLUA_UNDEF(SUMMON_ANGEL);
 TOLUA_UNDEF(SUMMON_DEMON);
 TOLUA_UNDEF(SUMMON_UNDEAD);
 TOLUA_UNDEF(SUMMON_DRAGON);
 TOLUA_UNDEF(SUMMON_HI_UNDEAD);
 TOLUA_UNDEF(SUMMON_HI_DRAGON);
 TOLUA_UNDEF(SUMMON_AMBERITES);
 TOLUA_UNDEF(SUMMON_UNIQUE);
 TOLUA_UNDEF(SUMMON_BIZARRE1);
 TOLUA_UNDEF(SUMMON_BIZARRE2);
 TOLUA_UNDEF(SUMMON_BIZARRE3);
 TOLUA_UNDEF(SUMMON_BIZARRE4);
 TOLUA_UNDEF(SUMMON_BIZARRE5);
 TOLUA_UNDEF(SUMMON_BIZARRE6);
 TOLUA_UNDEF(SUMMON_CYBER);
 TOLUA_UNDEF(SUMMON_KIN);
 TOLUA_UNDEF(SUMMON_DAWN);
 TOLUA_UNDEF(SUMMON_ANIMAL);
 TOLUA_UNDEF(SUMMON_ANIMAL_RANGER);
 TOLUA_UNDEF(SUMMON_HI_UNDEAD_NO_UNIQUES);
 TOLUA_UNDEF(SUMMON_HI_DRAGON_NO_UNIQUES);
 TOLUA_UNDEF(SUMMON_NO_UNIQUES);
 TOLUA_UNDEF(SUMMON_PHANTOM);
 TOLUA_UNDEF(SUMMON_ELEMENTAL);
 TOLUA_UNDEF(SUMMON_BLUE_HORROR);
 TOLUA_UNDEF(monster_race);
 lua_getglobals(tolua_S);
 lua_pushstring(tolua_S,"r_info"); lua_pushnil(tolua_S); lua_rawset(tolua_S,-3);
 lua_pop(tolua_S,1);
 TOLUA_UNDEF(summon_specific);
 TOLUA_UNDEF(summon_cloned_creature);
 TOLUA_UNDEF(monst_race);
 TOLUA_UNDEF(mon_race_name);
 TOLUA_UNDEF(monster_can_open);
 TOLUA_UNDEF(summon_monsters_near_player);
}
