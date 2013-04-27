/*
** Lua binding: field
** Generated automatically by tolua 4.0a - angband.
*/

#include "lua/tolua.h"

/* Exported function */
int  tolua_field_open (lua_State* tolua_S);
void tolua_field_close (lua_State* tolua_S);

#include "angband.h"
#include "script.h"

/* function to register type */
static void toluaI_reg_types (lua_State* tolua_S)
{
(void) tolua_S;	/* Hack - prevent compiler warnings */
 tolua_usertype(tolua_S,"field_type");
}

/* get function: f_attr of class  field_type */
static int toluaI_get_field_field_type_f_attr(lua_State* tolua_S)
{
  TOLUA_GET_SELF(field_type);
  tolua_pushnumber(tolua_S,(long)self->f_attr);
 return 1;
}

/* set function: f_attr of class  field_type */
static int toluaI_set_field_field_type_f_attr(lua_State* tolua_S)
{
  TOLUA_GET_SELF(field_type);
  if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
   TOLUA_ERR_ASSIGN;
  self->f_attr = ((byte)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: f_char of class  field_type */
static int toluaI_get_field_field_type_f_char(lua_State* tolua_S)
{
  TOLUA_GET_SELF(field_type);
  tolua_pushnumber(tolua_S,(long)self->f_char);
 return 1;
}

/* set function: f_char of class  field_type */
static int toluaI_set_field_field_type_f_char(lua_State* tolua_S)
{
  TOLUA_GET_SELF(field_type);
  if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
   TOLUA_ERR_ASSIGN;
  self->f_char = ((char)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: t_idx of class  field_type */
static int toluaI_get_field_field_type_t_idx(lua_State* tolua_S)
{
  TOLUA_GET_SELF(field_type);
  tolua_pushnumber(tolua_S,(long)self->t_idx);
 return 1;
}

/* set function: t_idx of class  field_type */
static int toluaI_set_field_field_type_t_idx(lua_State* tolua_S)
{
  TOLUA_GET_SELF(field_type);
  if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
   TOLUA_ERR_ASSIGN;
  self->t_idx = ((s16b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: fy of class  field_type */
static int toluaI_get_field_field_type_fy(lua_State* tolua_S)
{
  TOLUA_GET_SELF(field_type);
  tolua_pushnumber(tolua_S,(long)self->fy);
 return 1;
}

/* set function: fy of class  field_type */
static int toluaI_set_field_field_type_fy(lua_State* tolua_S)
{
  TOLUA_GET_SELF(field_type);
  if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
   TOLUA_ERR_ASSIGN;
  self->fy = ((s16b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: fx of class  field_type */
static int toluaI_get_field_field_type_fx(lua_State* tolua_S)
{
  TOLUA_GET_SELF(field_type);
  tolua_pushnumber(tolua_S,(long)self->fx);
 return 1;
}

/* set function: fx of class  field_type */
static int toluaI_set_field_field_type_fx(lua_State* tolua_S)
{
  TOLUA_GET_SELF(field_type);
  if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
   TOLUA_ERR_ASSIGN;
  self->fx = ((s16b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: info of class  field_type */
static int toluaI_get_field_field_type_info(lua_State* tolua_S)
{
  TOLUA_GET_SELF(field_type);
  tolua_pushnumber(tolua_S,(long)self->info);
 return 1;
}

/* set function: info of class  field_type */
static int toluaI_set_field_field_type_info(lua_State* tolua_S)
{
  TOLUA_GET_SELF(field_type);
  if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
   TOLUA_ERR_ASSIGN;
  self->info = ((u16b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: data of class  field_type */
static int toluaI_get_field_field_type_data(lua_State* tolua_S)
{
 int toluaI_index;
 TOLUA_ARRAY_SELF(field_type);
 TOLUA_ARRAY_INDEX("field_type: data",8);
 tolua_pushnumber(tolua_S,(long)self->data[toluaI_index]);
 return 1;
}

/* set function: data of class  field_type */
static int toluaI_set_field_field_type_data(lua_State* tolua_S)
{
 int toluaI_index;
 TOLUA_ARRAY_SELF(field_type);
 TOLUA_ARRAY_INDEX("field_type: data",8);
  self->data[toluaI_index] = ((byte)  tolua_getnumber(tolua_S,3,0));
 return 0;
}

/* get function: counter of class  field_type */
static int toluaI_get_field_field_type_counter(lua_State* tolua_S)
{
  TOLUA_GET_SELF(field_type);
  tolua_pushnumber(tolua_S,(long)self->counter);
 return 1;
}

/* set function: counter of class  field_type */
static int toluaI_set_field_field_type_counter(lua_State* tolua_S)
{
  TOLUA_GET_SELF(field_type);
  if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
   TOLUA_ERR_ASSIGN;
  self->counter = ((s16b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* function: deleteme */
static int toluaI_field_deleteme00(lua_State* tolua_S)
{
 if (!tolua_isnoobj(tolua_S,1))
 {
  TOLUA_ERR_FN(deleteme);
 } else {
  deleteme();
 }
 return 0;
}

/* function: set_corpse_size */
static int toluaI_field_set_corpse_size00(lua_State* tolua_S)
{
 if (!tolua_istype(tolua_S,1,tolua_tag(tolua_S,"field_type"),0) ||
     !tolua_istype(tolua_S,2,LUA_TNUMBER,0) ||
     !tolua_isnoobj(tolua_S,3))
 {
  TOLUA_ERR_FN(set_corpse_size);
 } else {
  field_type* f_ptr = ((field_type*)  tolua_getusertype(tolua_S,1,0));
  int size = ((int)  tolua_getnumber(tolua_S,2,0));
  set_corpse_size(f_ptr,size);
 }
 return 0;
}

/* function: notice_field */
static int toluaI_field_notice_field00(lua_State* tolua_S)
{
 if (!tolua_istype(tolua_S,1,tolua_tag(tolua_S,"field_type"),0) ||
     !tolua_isnoobj(tolua_S,2))
 {
  TOLUA_ERR_FN(notice_field);
 } else {
  field_type* f_ptr = ((field_type*)  tolua_getusertype(tolua_S,1,0));
  notice_field(f_ptr);
 }
 return 0;
}

/* function: field_name */
static int toluaI_field_field_name00(lua_State* tolua_S)
{
 if (!tolua_istype(tolua_S,1,tolua_tag(tolua_S,"const field_type"),0) ||
     !tolua_isnoobj(tolua_S,2))
 {
  TOLUA_ERR_FN(field_name);
 } else {
  const field_type* f_ptr = ((const field_type*)  tolua_getusertype(tolua_S,1,0));
  cptr toluaI_ret = (cptr)  field_name(f_ptr);
  tolua_pushstring(tolua_S,(const char*)toluaI_ret);
 }
 return 1;
}

/* function: check_trap_hit */
static int toluaI_field_check_trap_hit00(lua_State* tolua_S)
{
 if (!tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
     !tolua_isnoobj(tolua_S,2))
 {
  TOLUA_ERR_FN(check_trap_hit);
 } else {
  int power = ((int)  tolua_getnumber(tolua_S,1,0));
  bool toluaI_ret = (bool)  check_trap_hit(power);
  tolua_pushbool(tolua_S,(int)toluaI_ret);
 }
 return 1;
}

/* function: hit_trap */
static int toluaI_field_hit_trap00(lua_State* tolua_S)
{
 if (!tolua_istype(tolua_S,1,tolua_tag(tolua_S,"field_type"),0) ||
     !tolua_isnoobj(tolua_S,2))
 {
  TOLUA_ERR_FN(hit_trap);
 } else {
  field_type* f_ptr = ((field_type*)  tolua_getusertype(tolua_S,1,0));
  hit_trap(f_ptr);
 }
 return 0;
}

/* function: evil_trap */
static int toluaI_field_evil_trap00(lua_State* tolua_S)
{
 if (!tolua_isnoobj(tolua_S,1))
 {
  TOLUA_ERR_FN(evil_trap);
 } else {
  evil_trap();
 }
 return 0;
}

/* function: player_save */
static int toluaI_field_player_save00(lua_State* tolua_S)
{
 if (!tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
     !tolua_isnoobj(tolua_S,2))
 {
  TOLUA_ERR_FN(player_save);
 } else {
  int power = ((int)  tolua_getnumber(tolua_S,1,0));
  bool toluaI_ret = (bool)  player_save(power);
  tolua_pushbool(tolua_S,(int)toluaI_ret);
 }
 return 1;
}

/* function: drop_random_item */
static int toluaI_field_drop_random_item00(lua_State* tolua_S)
{
 if (!tolua_isnoobj(tolua_S,1))
 {
  TOLUA_ERR_FN(drop_random_item);
 } else {
  drop_random_item();
 }
 return 0;
}

/* function: drain_lite */
static int toluaI_field_drain_lite00(lua_State* tolua_S)
{
 if (!tolua_isnoobj(tolua_S,1))
 {
  TOLUA_ERR_FN(drain_lite);
 } else {
  drain_lite();
 }
 return 0;
}

/* function: drain_food */
static int toluaI_field_drain_food00(lua_State* tolua_S)
{
 if (!tolua_isnoobj(tolua_S,1))
 {
  TOLUA_ERR_FN(drain_food);
 } else {
  drain_food();
 }
 return 0;
}

/* function: drain_magic */
static int toluaI_field_drain_magic00(lua_State* tolua_S)
{
 if (!tolua_isnoobj(tolua_S,1))
 {
  TOLUA_ERR_FN(drain_magic);
 } else {
  drain_magic();
 }
 return 0;
}

/* function: raise_dead */
static int toluaI_field_raise_dead00(lua_State* tolua_S)
{
 if (!tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
     !tolua_istype(tolua_S,2,LUA_TNUMBER,0) ||
     !tolua_istype(tolua_S,3,tolua_tag(tolua_S,"bool"),0) ||
     !tolua_isnoobj(tolua_S,4))
 {
  TOLUA_ERR_FN(raise_dead);
 } else {
  int x = ((int)  tolua_getnumber(tolua_S,1,0));
  int y = ((int)  tolua_getnumber(tolua_S,2,0));
  bool pet = ((bool)  tolua_getbool(tolua_S,3,0));
  bool toluaI_ret = (bool)  raise_dead(x,y,pet);
  tolua_pushbool(tolua_S,(int)toluaI_ret);
 }
 return 1;
}

/* function: do_cmd_store */
static int toluaI_field_do_cmd_store00(lua_State* tolua_S)
{
 if (!tolua_istype(tolua_S,1,tolua_tag(tolua_S,"const field_type"),0) ||
     !tolua_isnoobj(tolua_S,2))
 {
  TOLUA_ERR_FN(do_cmd_store);
 } else {
  const field_type* f_ptr = ((const field_type*)  tolua_getusertype(tolua_S,1,0));
  do_cmd_store(f_ptr);
 }
 return 0;
}

/* function: do_cmd_bldg */
static int toluaI_field_do_cmd_bldg00(lua_State* tolua_S)
{
 if (!tolua_istype(tolua_S,1,tolua_tag(tolua_S,"const field_type"),0) ||
     !tolua_isnoobj(tolua_S,2))
 {
  TOLUA_ERR_FN(do_cmd_bldg);
 } else {
  const field_type* f_ptr = ((const field_type*)  tolua_getusertype(tolua_S,1,0));
  do_cmd_bldg(f_ptr);
 }
 return 0;
}

/* function: place_sb */
static int toluaI_field_place_sb00(lua_State* tolua_S)
{
 if (!tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
     !tolua_istype(tolua_S,2,LUA_TNUMBER,0) ||
     !tolua_isnoobj(tolua_S,3))
 {
  TOLUA_ERR_FN(place_sb);
 } else {
  int greed = ((int)  tolua_getnumber(tolua_S,1,0));
  int max_cost = ((int)  tolua_getnumber(tolua_S,2,0));
  place_sb(greed,max_cost);
 }
 return 0;
}

/* function: display_build */
static int toluaI_field_display_build00(lua_State* tolua_S)
{
 if (!tolua_istype(tolua_S,1,tolua_tag(tolua_S,"const field_type"),0) ||
     !tolua_isnoobj(tolua_S,2))
 {
  TOLUA_ERR_FN(display_build);
 } else {
  const field_type* f_ptr = ((const field_type*)  tolua_getusertype(tolua_S,1,0));
  display_build(f_ptr);
 }
 return 0;
}

/* function: test_gold */
static int toluaI_field_test_gold00(lua_State* tolua_S)
{
 if (!tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
     !tolua_isnoobj(tolua_S,2))
 {
  TOLUA_ERR_FN(test_gold);
 } else {
  s32b cost = ((s32b)  tolua_getnumber(tolua_S,1,0));
  bool toluaI_ret = (bool)  test_gold(cost);
  tolua_pushbool(tolua_S,(int)toluaI_ret);
 }
 return 1;
}

/* function: build_has_quest */
static int toluaI_field_build_has_quest00(lua_State* tolua_S)
{
 if (!tolua_isnoobj(tolua_S,1))
 {
  TOLUA_ERR_FN(build_has_quest);
 } else {
  bool toluaI_ret = (bool)  build_has_quest();
  tolua_pushbool(tolua_S,(int)toluaI_ret);
 }
 return 1;
}

/* function: build_cmd_quest */
static int toluaI_field_build_cmd_quest00(lua_State* tolua_S)
{
 if (!tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
     !tolua_istype(tolua_S,2,tolua_tag(tolua_S,"field_type"),0) ||
     !tolua_isnoobj(tolua_S,3))
 {
  TOLUA_ERR_FN(build_cmd_quest);
 } else {
  int level = ((int)  tolua_getnumber(tolua_S,1,0));
  field_type* f_ptr = ((field_type*)  tolua_getusertype(tolua_S,2,0));
  build_cmd_quest(level,f_ptr);
 }
 return 0;
}

/* function: record_aura */
static int toluaI_field_record_aura00(lua_State* tolua_S)
{
 if (!tolua_isnoobj(tolua_S,1))
 {
  TOLUA_ERR_FN(record_aura);
 } else {
  record_aura();
 }
 return 0;
}

/* function: building_healer */
static int toluaI_field_building_healer00(lua_State* tolua_S)
{
 if (!tolua_isnoobj(tolua_S,1))
 {
  TOLUA_ERR_FN(building_healer);
 } else {
  bool toluaI_ret = (bool)  building_healer();
  tolua_pushbool(tolua_S,(int)toluaI_ret);
 }
 return 1;
}

/* function: building_magetower */
static int toluaI_field_building_magetower00(lua_State* tolua_S)
{
 if (!tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
     !tolua_istype(tolua_S,2,tolua_tag(tolua_S,"bool"),0) ||
     !tolua_isnoobj(tolua_S,3))
 {
  TOLUA_ERR_FN(building_magetower);
 } else {
  int factor = ((int)  tolua_getnumber(tolua_S,1,0));
  bool display = ((bool)  tolua_getbool(tolua_S,2,0));
  bool toluaI_ret = (bool)  building_magetower(factor,display);
  tolua_pushbool(tolua_S,(int)toluaI_ret);
 }
 return 1;
}

/* function: research_mon */
static int toluaI_field_research_mon00(lua_State* tolua_S)
{
 if (!tolua_isnoobj(tolua_S,1))
 {
  TOLUA_ERR_FN(research_mon);
 } else {
  bool toluaI_ret = (bool)  research_mon();
  tolua_pushbool(tolua_S,(int)toluaI_ret);
 }
 return 1;
}

/* function: gamble_help */
static int toluaI_field_gamble_help00(lua_State* tolua_S)
{
 if (!tolua_isnoobj(tolua_S,1))
 {
  TOLUA_ERR_FN(gamble_help);
 } else {
  gamble_help();
 }
 return 0;
}

/* function: gamble_in_between */
static int toluaI_field_gamble_in_between00(lua_State* tolua_S)
{
 if (!tolua_isnoobj(tolua_S,1))
 {
  TOLUA_ERR_FN(gamble_in_between);
 } else {
  gamble_in_between();
 }
 return 0;
}

/* function: gamble_craps */
static int toluaI_field_gamble_craps00(lua_State* tolua_S)
{
 if (!tolua_isnoobj(tolua_S,1))
 {
  TOLUA_ERR_FN(gamble_craps);
 } else {
  gamble_craps();
 }
 return 0;
}

/* function: gamble_spin_wheel */
static int toluaI_field_gamble_spin_wheel00(lua_State* tolua_S)
{
 if (!tolua_isnoobj(tolua_S,1))
 {
  TOLUA_ERR_FN(gamble_spin_wheel);
 } else {
  gamble_spin_wheel();
 }
 return 0;
}

/* function: gamble_dice_slots */
static int toluaI_field_gamble_dice_slots00(lua_State* tolua_S)
{
 if (!tolua_isnoobj(tolua_S,1))
 {
  TOLUA_ERR_FN(gamble_dice_slots);
 } else {
  gamble_dice_slots();
 }
 return 0;
}

/* function: inn_rest */
static int toluaI_field_inn_rest00(lua_State* tolua_S)
{
 if (!tolua_isnoobj(tolua_S,1))
 {
  TOLUA_ERR_FN(inn_rest);
 } else {
  bool toluaI_ret = (bool)  inn_rest();
  tolua_pushbool(tolua_S,(int)toluaI_ret);
 }
 return 1;
}

/* function: compare_weapons */
static int toluaI_field_compare_weapons00(lua_State* tolua_S)
{
 if (!tolua_isnoobj(tolua_S,1))
 {
  TOLUA_ERR_FN(compare_weapons);
 } else {
  bool toluaI_ret = (bool)  compare_weapons();
  tolua_pushbool(tolua_S,(int)toluaI_ret);
 }
 return 1;
}

/* function: building_recharge */
static int toluaI_field_building_recharge00(lua_State* tolua_S)
{
 if (!tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
     !tolua_isnoobj(tolua_S,2))
 {
  TOLUA_ERR_FN(building_recharge);
 } else {
  s32b cost = ((s32b)  tolua_getnumber(tolua_S,1,0));
  building_recharge(cost);
 }
 return 0;
}

/* function: enchant_item */
static int toluaI_field_enchant_item00(lua_State* tolua_S)
{
 if (!tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
     !tolua_istype(tolua_S,2,tolua_tag(tolua_S,"bool"),0) ||
     !tolua_istype(tolua_S,3,tolua_tag(tolua_S,"bool"),0) ||
     !tolua_istype(tolua_S,4,tolua_tag(tolua_S,"bool"),0) ||
     !tolua_istype(tolua_S,5,tolua_tag(tolua_S,"bool"),0) ||
     !tolua_isnoobj(tolua_S,6))
 {
  TOLUA_ERR_FN(enchant_item);
 } else {
  s32b cost = ((s32b)  tolua_getnumber(tolua_S,1,0));
  bool to_hit = ((bool)  tolua_getbool(tolua_S,2,0));
  bool to_dam = ((bool)  tolua_getbool(tolua_S,3,0));
  bool to_ac = ((bool)  tolua_getbool(tolua_S,4,0));
  bool weap = ((bool)  tolua_getbool(tolua_S,5,0));
  bool toluaI_ret = (bool)  enchant_item(cost,to_hit,to_dam,to_ac,weap);
  tolua_pushbool(tolua_S,(int)toluaI_ret);
 }
 return 1;
}

/* function: build_cmd_repair */
static int toluaI_field_build_cmd_repair00(lua_State* tolua_S)
{
 if (!tolua_isnoobj(tolua_S,1))
 {
  TOLUA_ERR_FN(build_cmd_repair);
 } else {
  build_cmd_repair();
 }
 return 0;
}

/* function: build_cmd_loan */
static int toluaI_field_build_cmd_loan00(lua_State* tolua_S)
{
 if (!tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
     !tolua_isnoobj(tolua_S,2))
 {
  TOLUA_ERR_FN(build_cmd_loan);
 } else {
  int factor = ((int)  tolua_getnumber(tolua_S,1,0));
  build_cmd_loan(factor);
 }
 return 0;
}

/* function: build_set_qlevel */
static int toluaI_field_build_set_qlevel00(lua_State* tolua_S)
{
 if (!tolua_isnoobj(tolua_S,1))
 {
  TOLUA_ERR_FN(build_set_qlevel);
 } else {
  build_set_qlevel();
 }
 return 0;
}

/* function: build_get_qlevel */
static int toluaI_field_build_get_qlevel00(lua_State* tolua_S)
{
 if (!tolua_isnoobj(tolua_S,1))
 {
  TOLUA_ERR_FN(build_get_qlevel);
 } else {
  int toluaI_ret = (int)  build_get_qlevel();
  tolua_pushnumber(tolua_S,(long)toluaI_ret);
 }
 return 1;
}

/* function: place_qinit */
static int toluaI_field_place_qinit00(lua_State* tolua_S)
{
 if (!tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
     !tolua_istype(tolua_S,2,LUA_TNUMBER,0) ||
     !tolua_isnoobj(tolua_S,3))
 {
  TOLUA_ERR_FN(place_qinit);
 } else {
  int member = ((int)  tolua_getnumber(tolua_S,1,0));
  int base = ((int)  tolua_getnumber(tolua_S,2,0));
  place_qinit(member,base);
 }
 return 0;
}

/* function: build_cmd_spellbooks */
static int toluaI_field_build_cmd_spellbooks00(lua_State* tolua_S)
{
 if (!tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
     !tolua_isnoobj(tolua_S,2))
 {
  TOLUA_ERR_FN(build_cmd_spellbooks);
 } else {
  int price = ((int)  tolua_getnumber(tolua_S,1,0));
  build_cmd_spellbooks(price);
 }
 return 0;
}

/* function: build_cmd_recall */
static int toluaI_field_build_cmd_recall00(lua_State* tolua_S)
{
 if (!tolua_isnoobj(tolua_S,1))
 {
  TOLUA_ERR_FN(build_cmd_recall);
 } else {
  bool toluaI_ret = (bool)  build_cmd_recall();
  tolua_pushbool(tolua_S,(int)toluaI_ret);
 }
 return 1;
}

/* function: build_cmd_grave */
static int toluaI_field_build_cmd_grave00(lua_State* tolua_S)
{
 if (!tolua_isnoobj(tolua_S,1))
 {
  TOLUA_ERR_FN(build_cmd_grave);
 } else {
  build_cmd_grave();
 }
 return 0;
}

/* function: is_member */
static int toluaI_field_is_member00(lua_State* tolua_S)
{
 if (!tolua_isnoobj(tolua_S,1))
 {
  TOLUA_ERR_FN(is_member);
 } else {
  bool toluaI_ret = (bool)  is_member();
  tolua_pushbool(tolua_S,(int)toluaI_ret);
 }
 return 1;
}

/* function: is_full_member */
static int toluaI_field_is_full_member00(lua_State* tolua_S)
{
 if (!tolua_isnoobj(tolua_S,1))
 {
  TOLUA_ERR_FN(is_full_member);
 } else {
  bool toluaI_ret = (bool)  is_full_member();
  tolua_pushbool(tolua_S,(int)toluaI_ret);
 }
 return 1;
}

/* function: get_loan_amount */
static int toluaI_field_get_loan_amount00(lua_State* tolua_S)
{
 if (!tolua_isnoobj(tolua_S,1))
 {
  TOLUA_ERR_FN(get_loan_amount);
 } else {
  int toluaI_ret = (int)  get_loan_amount();
  tolua_pushnumber(tolua_S,(long)toluaI_ret);
 }
 return 1;
}

/* function: check_mortgage */
static int toluaI_field_check_mortgage00(lua_State* tolua_S)
{
 if (!tolua_isnoobj(tolua_S,1))
 {
  TOLUA_ERR_FN(check_mortgage);
 } else {
  bool toluaI_ret = (bool)  check_mortgage();
  tolua_pushbool(tolua_S,(int)toluaI_ret);
 }
 return 1;
}

/* function: build_next_quest */
static int toluaI_field_build_next_quest00(lua_State* tolua_S)
{
 if (!tolua_isnoobj(tolua_S,1))
 {
  TOLUA_ERR_FN(build_next_quest);
 } else {
  bool toluaI_ret = (bool)  build_next_quest();
  tolua_pushbool(tolua_S,(int)toluaI_ret);
 }
 return 1;
}

/* function: build_cmd_food */
static int toluaI_field_build_cmd_food00(lua_State* tolua_S)
{
 if (!tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
     !tolua_isnoobj(tolua_S,2))
 {
  TOLUA_ERR_FN(build_cmd_food);
 } else {
  int price = ((int)  tolua_getnumber(tolua_S,1,0));
  build_cmd_food(price);
 }
 return 0;
}

/* function: in_quest */
static int toluaI_field_in_quest00(lua_State* tolua_S)
{
 if (!tolua_isnoobj(tolua_S,1))
 {
  TOLUA_ERR_FN(in_quest);
 } else {
  int toluaI_ret = (int)  in_quest();
  tolua_pushnumber(tolua_S,(long)toluaI_ret);
 }
 return 1;
}

/* Open function */
int tolua_field_open (lua_State* tolua_S)
{
 tolua_open(tolua_S);
 toluaI_reg_types(tolua_S);
 TOLUA_DEF(ACT_TUNNEL);
 TOLUA_DEF(ACT_DISARM);
 TOLUA_DEF(ACT_OPEN);
 TOLUA_DEF(BREAK_GLYPH);
 TOLUA_DEF(BREAK_MINOR_GLYPH);
 TOLUA_DEF(FIELD_INFO_TEMP);
 TOLUA_DEF(FIELD_INFO_FEAT);
 TOLUA_DEF(FIELD_INFO_VIS);
 TOLUA_DEF(FIELD_INFO_MARK);
 TOLUA_DEF(FIELD_INFO_TRANS);
 TOLUA_DEF(FIELD_INFO_NO_LOOK);
 TOLUA_DEF(FIELD_INFO_NFT_LOOK);
 TOLUA_DEF(FIELD_INFO_MERGE);
 TOLUA_DEF(FIELD_INFO_NO_ENTER);
 TOLUA_DEF(FIELD_INFO_NO_MAGIC);
 TOLUA_DEF(FIELD_INFO_NO_OBJCT);
 TOLUA_DEF(FIELD_INFO_PERM);
 TOLUA_DEF(FIELD_INFO_IGNORE);
 TOLUA_DEF(FIELD_INFO_NO_MPLACE);
 TOLUA_DEF(FIELD_INFO_DUMMY13);
 TOLUA_DEF(FIELD_INFO_DUMMY14);
 TOLUA_DEF(GS_NONE);
 TOLUA_DEF(GS_NONMEMBER);
 TOLUA_DEF(GS_LOW_MEMBER);
 TOLUA_DEF(GS_MEMBER);
 tolua_cclass(tolua_S,"field_type","");
 tolua_tablevar(tolua_S,"field_type","f_attr",toluaI_get_field_field_type_f_attr,toluaI_set_field_field_type_f_attr);
 tolua_tablevar(tolua_S,"field_type","f_char",toluaI_get_field_field_type_f_char,toluaI_set_field_field_type_f_char);
 tolua_tablevar(tolua_S,"field_type","t_idx",toluaI_get_field_field_type_t_idx,toluaI_set_field_field_type_t_idx);
 tolua_tablevar(tolua_S,"field_type","fy",toluaI_get_field_field_type_fy,toluaI_set_field_field_type_fy);
 tolua_tablevar(tolua_S,"field_type","fx",toluaI_get_field_field_type_fx,toluaI_set_field_field_type_fx);
 tolua_tablevar(tolua_S,"field_type","info",toluaI_get_field_field_type_info,toluaI_set_field_field_type_info);
 tolua_tablearray(tolua_S,"field_type","data",toluaI_get_field_field_type_data,toluaI_set_field_field_type_data);
 tolua_tablevar(tolua_S,"field_type","counter",toluaI_get_field_field_type_counter,toluaI_set_field_field_type_counter);
 TOLUA_FUN(deleteme,toluaI_field_deleteme00);
 TOLUA_FUN(set_corpse_size,toluaI_field_set_corpse_size00);
 TOLUA_FUN(notice_field,toluaI_field_notice_field00);
 TOLUA_FUN(field_name,toluaI_field_field_name00);
 TOLUA_FUN(check_trap_hit,toluaI_field_check_trap_hit00);
 TOLUA_FUN(hit_trap,toluaI_field_hit_trap00);
 TOLUA_FUN(evil_trap,toluaI_field_evil_trap00);
 TOLUA_FUN(player_save,toluaI_field_player_save00);
 TOLUA_FUN(drop_random_item,toluaI_field_drop_random_item00);
 TOLUA_FUN(drain_lite,toluaI_field_drain_lite00);
 TOLUA_FUN(drain_food,toluaI_field_drain_food00);
 TOLUA_FUN(drain_magic,toluaI_field_drain_magic00);
 TOLUA_FUN(raise_dead,toluaI_field_raise_dead00);
 TOLUA_FUN(do_cmd_store,toluaI_field_do_cmd_store00);
 TOLUA_FUN(do_cmd_bldg,toluaI_field_do_cmd_bldg00);
 TOLUA_FUN(place_sb,toluaI_field_place_sb00);
 TOLUA_FUN(display_build,toluaI_field_display_build00);
 TOLUA_FUN(test_gold,toluaI_field_test_gold00);
 TOLUA_FUN(build_has_quest,toluaI_field_build_has_quest00);
 TOLUA_FUN(build_cmd_quest,toluaI_field_build_cmd_quest00);
 TOLUA_FUN(record_aura,toluaI_field_record_aura00);
 TOLUA_FUN(building_healer,toluaI_field_building_healer00);
 TOLUA_FUN(building_magetower,toluaI_field_building_magetower00);
 TOLUA_FUN(research_mon,toluaI_field_research_mon00);
 TOLUA_FUN(gamble_help,toluaI_field_gamble_help00);
 TOLUA_FUN(gamble_in_between,toluaI_field_gamble_in_between00);
 TOLUA_FUN(gamble_craps,toluaI_field_gamble_craps00);
 TOLUA_FUN(gamble_spin_wheel,toluaI_field_gamble_spin_wheel00);
 TOLUA_FUN(gamble_dice_slots,toluaI_field_gamble_dice_slots00);
 TOLUA_FUN(inn_rest,toluaI_field_inn_rest00);
 TOLUA_FUN(compare_weapons,toluaI_field_compare_weapons00);
 TOLUA_FUN(building_recharge,toluaI_field_building_recharge00);
 TOLUA_FUN(enchant_item,toluaI_field_enchant_item00);
 TOLUA_FUN(build_cmd_repair,toluaI_field_build_cmd_repair00);
 TOLUA_FUN(build_cmd_loan,toluaI_field_build_cmd_loan00);
 TOLUA_FUN(build_set_qlevel,toluaI_field_build_set_qlevel00);
 TOLUA_FUN(build_get_qlevel,toluaI_field_build_get_qlevel00);
 TOLUA_FUN(place_qinit,toluaI_field_place_qinit00);
 TOLUA_FUN(build_cmd_spellbooks,toluaI_field_build_cmd_spellbooks00);
 TOLUA_FUN(build_cmd_recall,toluaI_field_build_cmd_recall00);
 TOLUA_FUN(build_cmd_grave,toluaI_field_build_cmd_grave00);
 TOLUA_FUN(is_member,toluaI_field_is_member00);
 TOLUA_FUN(is_full_member,toluaI_field_is_full_member00);
 TOLUA_FUN(get_loan_amount,toluaI_field_get_loan_amount00);
 TOLUA_FUN(check_mortgage,toluaI_field_check_mortgage00);
 TOLUA_FUN(build_next_quest,toluaI_field_build_next_quest00);
 TOLUA_FUN(build_cmd_food,toluaI_field_build_cmd_food00);
 TOLUA_FUN(in_quest,toluaI_field_in_quest00);
 return 1;
}
/* Close function */
void tolua_field_close (lua_State* tolua_S)
{
 TOLUA_UNDEF(ACT_TUNNEL);
 TOLUA_UNDEF(ACT_DISARM);
 TOLUA_UNDEF(ACT_OPEN);
 TOLUA_UNDEF(BREAK_GLYPH);
 TOLUA_UNDEF(BREAK_MINOR_GLYPH);
 TOLUA_UNDEF(FIELD_INFO_TEMP);
 TOLUA_UNDEF(FIELD_INFO_FEAT);
 TOLUA_UNDEF(FIELD_INFO_VIS);
 TOLUA_UNDEF(FIELD_INFO_MARK);
 TOLUA_UNDEF(FIELD_INFO_TRANS);
 TOLUA_UNDEF(FIELD_INFO_NO_LOOK);
 TOLUA_UNDEF(FIELD_INFO_NFT_LOOK);
 TOLUA_UNDEF(FIELD_INFO_MERGE);
 TOLUA_UNDEF(FIELD_INFO_NO_ENTER);
 TOLUA_UNDEF(FIELD_INFO_NO_MAGIC);
 TOLUA_UNDEF(FIELD_INFO_NO_OBJCT);
 TOLUA_UNDEF(FIELD_INFO_PERM);
 TOLUA_UNDEF(FIELD_INFO_IGNORE);
 TOLUA_UNDEF(FIELD_INFO_NO_MPLACE);
 TOLUA_UNDEF(FIELD_INFO_DUMMY13);
 TOLUA_UNDEF(FIELD_INFO_DUMMY14);
 TOLUA_UNDEF(GS_NONE);
 TOLUA_UNDEF(GS_NONMEMBER);
 TOLUA_UNDEF(GS_LOW_MEMBER);
 TOLUA_UNDEF(GS_MEMBER);
 TOLUA_UNDEF(field_type);
 TOLUA_UNDEF(deleteme);
 TOLUA_UNDEF(set_corpse_size);
 TOLUA_UNDEF(notice_field);
 TOLUA_UNDEF(field_name);
 TOLUA_UNDEF(check_trap_hit);
 TOLUA_UNDEF(hit_trap);
 TOLUA_UNDEF(evil_trap);
 TOLUA_UNDEF(player_save);
 TOLUA_UNDEF(drop_random_item);
 TOLUA_UNDEF(drain_lite);
 TOLUA_UNDEF(drain_food);
 TOLUA_UNDEF(drain_magic);
 TOLUA_UNDEF(raise_dead);
 TOLUA_UNDEF(do_cmd_store);
 TOLUA_UNDEF(do_cmd_bldg);
 TOLUA_UNDEF(place_sb);
 TOLUA_UNDEF(display_build);
 TOLUA_UNDEF(test_gold);
 TOLUA_UNDEF(build_has_quest);
 TOLUA_UNDEF(build_cmd_quest);
 TOLUA_UNDEF(record_aura);
 TOLUA_UNDEF(building_healer);
 TOLUA_UNDEF(building_magetower);
 TOLUA_UNDEF(research_mon);
 TOLUA_UNDEF(gamble_help);
 TOLUA_UNDEF(gamble_in_between);
 TOLUA_UNDEF(gamble_craps);
 TOLUA_UNDEF(gamble_spin_wheel);
 TOLUA_UNDEF(gamble_dice_slots);
 TOLUA_UNDEF(inn_rest);
 TOLUA_UNDEF(compare_weapons);
 TOLUA_UNDEF(building_recharge);
 TOLUA_UNDEF(enchant_item);
 TOLUA_UNDEF(build_cmd_repair);
 TOLUA_UNDEF(build_cmd_loan);
 TOLUA_UNDEF(build_set_qlevel);
 TOLUA_UNDEF(build_get_qlevel);
 TOLUA_UNDEF(place_qinit);
 TOLUA_UNDEF(build_cmd_spellbooks);
 TOLUA_UNDEF(build_cmd_recall);
 TOLUA_UNDEF(build_cmd_grave);
 TOLUA_UNDEF(is_member);
 TOLUA_UNDEF(is_full_member);
 TOLUA_UNDEF(get_loan_amount);
 TOLUA_UNDEF(check_mortgage);
 TOLUA_UNDEF(build_next_quest);
 TOLUA_UNDEF(build_cmd_food);
 TOLUA_UNDEF(in_quest);
}
