/*
** Lua binding: object
** Generated automatically by tolua 4.0a - angband.
*/

#include "lua/tolua.h"

/* Exported function */
int  tolua_object_open (lua_State* tolua_S);
void tolua_object_close (lua_State* tolua_S);

#include "angband.h"
#include "script.h"

/* function to register type */
static void toluaI_reg_types (lua_State* tolua_S)
{
(void) tolua_S;	/* Hack - prevent compiler warnings */
 tolua_usertype(tolua_S,"object_type");
}

/* get function: k_idx of class  object_type */
static int toluaI_get_object_object_type_k_idx(lua_State* tolua_S)
{
  TOLUA_GET_SELF(object_type);
  tolua_pushnumber(tolua_S,(long)self->k_idx);
 return 1;
}

/* set function: k_idx of class  object_type */
static int toluaI_set_object_object_type_k_idx(lua_State* tolua_S)
{
  TOLUA_GET_SELF(object_type);
  if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
   TOLUA_ERR_ASSIGN;
  self->k_idx = ((s16b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: iy of class  object_type */
static int toluaI_get_object_object_type_iy(lua_State* tolua_S)
{
  TOLUA_GET_SELF(object_type);
  tolua_pushnumber(tolua_S,(long)self->iy);
 return 1;
}

/* set function: iy of class  object_type */
static int toluaI_set_object_object_type_iy(lua_State* tolua_S)
{
  TOLUA_GET_SELF(object_type);
  if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
   TOLUA_ERR_ASSIGN;
  self->iy = ((s16b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: ix of class  object_type */
static int toluaI_get_object_object_type_ix(lua_State* tolua_S)
{
  TOLUA_GET_SELF(object_type);
  tolua_pushnumber(tolua_S,(long)self->ix);
 return 1;
}

/* set function: ix of class  object_type */
static int toluaI_set_object_object_type_ix(lua_State* tolua_S)
{
  TOLUA_GET_SELF(object_type);
  if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
   TOLUA_ERR_ASSIGN;
  self->ix = ((s16b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: tval of class  object_type */
static int toluaI_get_object_object_type_tval(lua_State* tolua_S)
{
  TOLUA_GET_SELF(object_type);
  tolua_pushnumber(tolua_S,(long)self->tval);
 return 1;
}

/* set function: tval of class  object_type */
static int toluaI_set_object_object_type_tval(lua_State* tolua_S)
{
  TOLUA_GET_SELF(object_type);
  if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
   TOLUA_ERR_ASSIGN;
  self->tval = ((byte)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: sval of class  object_type */
static int toluaI_get_object_object_type_sval(lua_State* tolua_S)
{
  TOLUA_GET_SELF(object_type);
  tolua_pushnumber(tolua_S,(long)self->sval);
 return 1;
}

/* set function: sval of class  object_type */
static int toluaI_set_object_object_type_sval(lua_State* tolua_S)
{
  TOLUA_GET_SELF(object_type);
  if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
   TOLUA_ERR_ASSIGN;
  self->sval = ((byte)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: pval of class  object_type */
static int toluaI_get_object_object_type_pval(lua_State* tolua_S)
{
  TOLUA_GET_SELF(object_type);
  tolua_pushnumber(tolua_S,(long)self->pval);
 return 1;
}

/* set function: pval of class  object_type */
static int toluaI_set_object_object_type_pval(lua_State* tolua_S)
{
  TOLUA_GET_SELF(object_type);
  if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
   TOLUA_ERR_ASSIGN;
  self->pval = ((s16b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: discount of class  object_type */
static int toluaI_get_object_object_type_discount(lua_State* tolua_S)
{
  TOLUA_GET_SELF(object_type);
  tolua_pushnumber(tolua_S,(long)self->discount);
 return 1;
}

/* set function: discount of class  object_type */
static int toluaI_set_object_object_type_discount(lua_State* tolua_S)
{
  TOLUA_GET_SELF(object_type);
  if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
   TOLUA_ERR_ASSIGN;
  self->discount = ((byte)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: number of class  object_type */
static int toluaI_get_object_object_type_number(lua_State* tolua_S)
{
  TOLUA_GET_SELF(object_type);
  tolua_pushnumber(tolua_S,(long)self->number);
 return 1;
}

/* set function: number of class  object_type */
static int toluaI_set_object_object_type_number(lua_State* tolua_S)
{
  TOLUA_GET_SELF(object_type);
  if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
   TOLUA_ERR_ASSIGN;
  self->number = ((byte)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: weight of class  object_type */
static int toluaI_get_object_object_type_weight(lua_State* tolua_S)
{
  TOLUA_GET_SELF(object_type);
  tolua_pushnumber(tolua_S,(long)self->weight);
 return 1;
}

/* set function: weight of class  object_type */
static int toluaI_set_object_object_type_weight(lua_State* tolua_S)
{
  TOLUA_GET_SELF(object_type);
  if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
   TOLUA_ERR_ASSIGN;
  self->weight = ((s16b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: to_h of class  object_type */
static int toluaI_get_object_object_type_to_h(lua_State* tolua_S)
{
  TOLUA_GET_SELF(object_type);
  tolua_pushnumber(tolua_S,(long)self->to_h);
 return 1;
}

/* set function: to_h of class  object_type */
static int toluaI_set_object_object_type_to_h(lua_State* tolua_S)
{
  TOLUA_GET_SELF(object_type);
  if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
   TOLUA_ERR_ASSIGN;
  self->to_h = ((s16b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: to_d of class  object_type */
static int toluaI_get_object_object_type_to_d(lua_State* tolua_S)
{
  TOLUA_GET_SELF(object_type);
  tolua_pushnumber(tolua_S,(long)self->to_d);
 return 1;
}

/* set function: to_d of class  object_type */
static int toluaI_set_object_object_type_to_d(lua_State* tolua_S)
{
  TOLUA_GET_SELF(object_type);
  if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
   TOLUA_ERR_ASSIGN;
  self->to_d = ((s16b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: to_a of class  object_type */
static int toluaI_get_object_object_type_to_a(lua_State* tolua_S)
{
  TOLUA_GET_SELF(object_type);
  tolua_pushnumber(tolua_S,(long)self->to_a);
 return 1;
}

/* set function: to_a of class  object_type */
static int toluaI_set_object_object_type_to_a(lua_State* tolua_S)
{
  TOLUA_GET_SELF(object_type);
  if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
   TOLUA_ERR_ASSIGN;
  self->to_a = ((s16b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: ac of class  object_type */
static int toluaI_get_object_object_type_ac(lua_State* tolua_S)
{
  TOLUA_GET_SELF(object_type);
  tolua_pushnumber(tolua_S,(long)self->ac);
 return 1;
}

/* set function: ac of class  object_type */
static int toluaI_set_object_object_type_ac(lua_State* tolua_S)
{
  TOLUA_GET_SELF(object_type);
  if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
   TOLUA_ERR_ASSIGN;
  self->ac = ((s16b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: timeout of class  object_type */
static int toluaI_get_object_object_type_timeout(lua_State* tolua_S)
{
  TOLUA_GET_SELF(object_type);
  tolua_pushnumber(tolua_S,(long)self->timeout);
 return 1;
}

/* set function: timeout of class  object_type */
static int toluaI_set_object_object_type_timeout(lua_State* tolua_S)
{
  TOLUA_GET_SELF(object_type);
  if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
   TOLUA_ERR_ASSIGN;
  self->timeout = ((s16b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: dd of class  object_type */
static int toluaI_get_object_object_type_dd(lua_State* tolua_S)
{
  TOLUA_GET_SELF(object_type);
  tolua_pushnumber(tolua_S,(long)self->dd);
 return 1;
}

/* set function: dd of class  object_type */
static int toluaI_set_object_object_type_dd(lua_State* tolua_S)
{
  TOLUA_GET_SELF(object_type);
  if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
   TOLUA_ERR_ASSIGN;
  self->dd = ((byte)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: ds of class  object_type */
static int toluaI_get_object_object_type_ds(lua_State* tolua_S)
{
  TOLUA_GET_SELF(object_type);
  tolua_pushnumber(tolua_S,(long)self->ds);
 return 1;
}

/* set function: ds of class  object_type */
static int toluaI_set_object_object_type_ds(lua_State* tolua_S)
{
  TOLUA_GET_SELF(object_type);
  if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
   TOLUA_ERR_ASSIGN;
  self->ds = ((byte)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: inscription of class  object_type */
static int toluaI_get_object_object_type_inscription(lua_State* tolua_S)
{
  TOLUA_GET_SELF(object_type);
  tolua_pushnumber(tolua_S,(long)self->inscription);
 return 1;
}

/* set function: inscription of class  object_type */
static int toluaI_set_object_object_type_inscription(lua_State* tolua_S)
{
  TOLUA_GET_SELF(object_type);
  if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
   TOLUA_ERR_ASSIGN;
  self->inscription = ((u16b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: xtra_name of class  object_type */
static int toluaI_get_object_object_type_xtra_name(lua_State* tolua_S)
{
  TOLUA_GET_SELF(object_type);
  tolua_pushnumber(tolua_S,(long)self->xtra_name);
 return 1;
}

/* set function: xtra_name of class  object_type */
static int toluaI_set_object_object_type_xtra_name(lua_State* tolua_S)
{
  TOLUA_GET_SELF(object_type);
  if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
   TOLUA_ERR_ASSIGN;
  self->xtra_name = ((u16b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: flags of class  object_type */
static int toluaI_get_object_object_type_flags(lua_State* tolua_S)
{
 int toluaI_index;
 TOLUA_ARRAY_SELF(object_type);
 TOLUA_ARRAY_INDEX("object_type: flags",4);
 tolua_pushnumber(tolua_S,(long)self->flags[toluaI_index]);
 return 1;
}

/* set function: flags of class  object_type */
static int toluaI_set_object_object_type_flags(lua_State* tolua_S)
{
 int toluaI_index;
 TOLUA_ARRAY_SELF(object_type);
 TOLUA_ARRAY_INDEX("object_type: flags",4);
  self->flags[toluaI_index] = ((u32b)  tolua_getnumber(tolua_S,3,0));
 return 0;
}

/* get function: kn_flags of class  object_type */
static int toluaI_get_object_object_type_kn_flags(lua_State* tolua_S)
{
 int toluaI_index;
 TOLUA_ARRAY_SELF(object_type);
 TOLUA_ARRAY_INDEX("object_type: kn_flags",4);
 tolua_pushnumber(tolua_S,(long)self->kn_flags[toluaI_index]);
 return 1;
}

/* set function: kn_flags of class  object_type */
static int toluaI_set_object_object_type_kn_flags(lua_State* tolua_S)
{
 int toluaI_index;
 TOLUA_ARRAY_SELF(object_type);
 TOLUA_ARRAY_INDEX("object_type: kn_flags",4);
  self->kn_flags[toluaI_index] = ((u32b)  tolua_getnumber(tolua_S,3,0));
 return 0;
}

/* get function: next_o_idx of class  object_type */
static int toluaI_get_object_object_type_next_o_idx(lua_State* tolua_S)
{
  TOLUA_GET_SELF(object_type);
  tolua_pushnumber(tolua_S,(long)self->next_o_idx);
 return 1;
}

/* set function: next_o_idx of class  object_type */
static int toluaI_set_object_object_type_next_o_idx(lua_State* tolua_S)
{
  TOLUA_GET_SELF(object_type);
  if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
   TOLUA_ERR_ASSIGN;
  self->next_o_idx = ((s16b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: cost of class  object_type */
static int toluaI_get_object_object_type_cost(lua_State* tolua_S)
{
  TOLUA_GET_SELF(object_type);
  tolua_pushnumber(tolua_S,(long)self->cost);
 return 1;
}

/* set function: cost of class  object_type */
static int toluaI_set_object_object_type_cost(lua_State* tolua_S)
{
  TOLUA_GET_SELF(object_type);
  if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
   TOLUA_ERR_ASSIGN;
  self->cost = ((s32b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: feeling of class  object_type */
static int toluaI_get_object_object_type_feeling(lua_State* tolua_S)
{
  TOLUA_GET_SELF(object_type);
  tolua_pushnumber(tolua_S,(long)self->feeling);
 return 1;
}

/* set function: feeling of class  object_type */
static int toluaI_set_object_object_type_feeling(lua_State* tolua_S)
{
  TOLUA_GET_SELF(object_type);
  if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
   TOLUA_ERR_ASSIGN;
  self->feeling = ((byte)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: a_idx of class  object_type */
static int toluaI_get_object_object_type_a_idx(lua_State* tolua_S)
{
  TOLUA_GET_SELF(object_type);
  tolua_pushnumber(tolua_S,(long)self->a_idx);
 return 1;
}

/* set function: a_idx of class  object_type */
static int toluaI_set_object_object_type_a_idx(lua_State* tolua_S)
{
  TOLUA_GET_SELF(object_type);
  if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
   TOLUA_ERR_ASSIGN;
  self->a_idx = ((byte)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: info of class  object_type */
static int toluaI_get_object_object_type_info(lua_State* tolua_S)
{
  TOLUA_GET_SELF(object_type);
  tolua_pushnumber(tolua_S,(long)self->info);
 return 1;
}

/* set function: info of class  object_type */
static int toluaI_set_object_object_type_info(lua_State* tolua_S)
{
  TOLUA_GET_SELF(object_type);
  if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
   TOLUA_ERR_ASSIGN;
  self->info = ((byte)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: allocated of class  object_type */
static int toluaI_get_object_object_type_allocated(lua_State* tolua_S)
{
  TOLUA_GET_SELF(object_type);
  tolua_pushbool(tolua_S,(int)self->allocated);
 return 1;
}

/* set function: allocated of class  object_type */
static int toluaI_set_object_object_type_allocated(lua_State* tolua_S)
{
  TOLUA_GET_SELF(object_type);
  if (!tolua_istype(tolua_S,2,tolua_tag(tolua_S,"bool"),0))
   TOLUA_ERR_ASSIGN;
  self->allocated = ((bool)  tolua_getbool(tolua_S,2,0));
 return 0;
}

/* function: item_tester_hook_weapon */
static int toluaI_object_item_tester_hook_weapon00(lua_State* tolua_S)
{
 if (!tolua_istype(tolua_S,1,tolua_tag(tolua_S,"const object_type"),0) ||
     !tolua_isnoobj(tolua_S,2))
 {
  TOLUA_ERR_FN(item_tester_hook_weapon);
 } else {
  const object_type* o_ptr = ((const object_type*)  tolua_getusertype(tolua_S,1,0));
  bool toluaI_ret = (bool)  item_tester_hook_weapon(o_ptr);
  tolua_pushbool(tolua_S,(int)toluaI_ret);
 }
 return 1;
}

/* function: item_tester_hook_melee_weapon */
static int toluaI_object_item_tester_hook_melee_weapon00(lua_State* tolua_S)
{
 if (!tolua_istype(tolua_S,1,tolua_tag(tolua_S,"const object_type"),0) ||
     !tolua_isnoobj(tolua_S,2))
 {
  TOLUA_ERR_FN(item_tester_hook_melee_weapon);
 } else {
  const object_type* o_ptr = ((const object_type*)  tolua_getusertype(tolua_S,1,0));
  bool toluaI_ret = (bool)  item_tester_hook_melee_weapon(o_ptr);
  tolua_pushbool(tolua_S,(int)toluaI_ret);
 }
 return 1;
}

/* function: item_tester_hook_nonsword */
static int toluaI_object_item_tester_hook_nonsword00(lua_State* tolua_S)
{
 if (!tolua_istype(tolua_S,1,tolua_tag(tolua_S,"const object_type"),0) ||
     !tolua_isnoobj(tolua_S,2))
 {
  TOLUA_ERR_FN(item_tester_hook_nonsword);
 } else {
  const object_type* o_ptr = ((const object_type*)  tolua_getusertype(tolua_S,1,0));
  bool toluaI_ret = (bool)  item_tester_hook_nonsword(o_ptr);
  tolua_pushbool(tolua_S,(int)toluaI_ret);
 }
 return 1;
}

/* function: item_tester_hook_ammo */
static int toluaI_object_item_tester_hook_ammo00(lua_State* tolua_S)
{
 if (!tolua_istype(tolua_S,1,tolua_tag(tolua_S,"const object_type"),0) ||
     !tolua_isnoobj(tolua_S,2))
 {
  TOLUA_ERR_FN(item_tester_hook_ammo);
 } else {
  const object_type* o_ptr = ((const object_type*)  tolua_getusertype(tolua_S,1,0));
  bool toluaI_ret = (bool)  item_tester_hook_ammo(o_ptr);
  tolua_pushbool(tolua_S,(int)toluaI_ret);
 }
 return 1;
}

/* function: item_tester_hook_fletcher */
static int toluaI_object_item_tester_hook_fletcher00(lua_State* tolua_S)
{
 if (!tolua_istype(tolua_S,1,tolua_tag(tolua_S,"const object_type"),0) ||
     !tolua_isnoobj(tolua_S,2))
 {
  TOLUA_ERR_FN(item_tester_hook_fletcher);
 } else {
  const object_type* o_ptr = ((const object_type*)  tolua_getusertype(tolua_S,1,0));
  bool toluaI_ret = (bool)  item_tester_hook_fletcher(o_ptr);
  tolua_pushbool(tolua_S,(int)toluaI_ret);
 }
 return 1;
}

/* function: item_tester_hook_armour */
static int toluaI_object_item_tester_hook_armour00(lua_State* tolua_S)
{
 if (!tolua_istype(tolua_S,1,tolua_tag(tolua_S,"const object_type"),0) ||
     !tolua_isnoobj(tolua_S,2))
 {
  TOLUA_ERR_FN(item_tester_hook_armour);
 } else {
  const object_type* o_ptr = ((const object_type*)  tolua_getusertype(tolua_S,1,0));
  bool toluaI_ret = (bool)  item_tester_hook_armour(o_ptr);
  tolua_pushbool(tolua_S,(int)toluaI_ret);
 }
 return 1;
}

/* function: item_tester_hook_soft_armour */
static int toluaI_object_item_tester_hook_soft_armour00(lua_State* tolua_S)
{
 if (!tolua_istype(tolua_S,1,tolua_tag(tolua_S,"const object_type"),0) ||
     !tolua_isnoobj(tolua_S,2))
 {
  TOLUA_ERR_FN(item_tester_hook_soft_armour);
 } else {
  const object_type* o_ptr = ((const object_type*)  tolua_getusertype(tolua_S,1,0));
  bool toluaI_ret = (bool)  item_tester_hook_soft_armour(o_ptr);
  tolua_pushbool(tolua_S,(int)toluaI_ret);
 }
 return 1;
}

/* function: item_tester_hook_hard_armour */
static int toluaI_object_item_tester_hook_hard_armour00(lua_State* tolua_S)
{
 if (!tolua_istype(tolua_S,1,tolua_tag(tolua_S,"const object_type"),0) ||
     !tolua_isnoobj(tolua_S,2))
 {
  TOLUA_ERR_FN(item_tester_hook_hard_armour);
 } else {
  const object_type* o_ptr = ((const object_type*)  tolua_getusertype(tolua_S,1,0));
  bool toluaI_ret = (bool)  item_tester_hook_hard_armour(o_ptr);
  tolua_pushbool(tolua_S,(int)toluaI_ret);
 }
 return 1;
}

/* function: item_tester_hook_helm */
static int toluaI_object_item_tester_hook_helm00(lua_State* tolua_S)
{
 if (!tolua_istype(tolua_S,1,tolua_tag(tolua_S,"const object_type"),0) ||
     !tolua_isnoobj(tolua_S,2))
 {
  TOLUA_ERR_FN(item_tester_hook_helm);
 } else {
  const object_type* o_ptr = ((const object_type*)  tolua_getusertype(tolua_S,1,0));
  bool toluaI_ret = (bool)  item_tester_hook_helm(o_ptr);
  tolua_pushbool(tolua_S,(int)toluaI_ret);
 }
 return 1;
}

/* function: item_tester_hook_pure_hard_armour */
static int toluaI_object_item_tester_hook_pure_hard_armour00(lua_State* tolua_S)
{
 if (!tolua_istype(tolua_S,1,tolua_tag(tolua_S,"const object_type"),0) ||
     !tolua_isnoobj(tolua_S,2))
 {
  TOLUA_ERR_FN(item_tester_hook_pure_hard_armour);
 } else {
  const object_type* o_ptr = ((const object_type*)  tolua_getusertype(tolua_S,1,0));
  bool toluaI_ret = (bool)  item_tester_hook_pure_hard_armour(o_ptr);
  tolua_pushbool(tolua_S,(int)toluaI_ret);
 }
 return 1;
}

/* function: item_tester_hook_weapon_armour */
static int toluaI_object_item_tester_hook_weapon_armour00(lua_State* tolua_S)
{
 if (!tolua_istype(tolua_S,1,tolua_tag(tolua_S,"const object_type"),0) ||
     !tolua_isnoobj(tolua_S,2))
 {
  TOLUA_ERR_FN(item_tester_hook_weapon_armour);
 } else {
  const object_type* o_ptr = ((const object_type*)  tolua_getusertype(tolua_S,1,0));
  bool toluaI_ret = (bool)  item_tester_hook_weapon_armour(o_ptr);
  tolua_pushbool(tolua_S,(int)toluaI_ret);
 }
 return 1;
}

/* function: item_tester_hook_wear */
static int toluaI_object_item_tester_hook_wear00(lua_State* tolua_S)
{
 if (!tolua_istype(tolua_S,1,tolua_tag(tolua_S,"const object_type"),0) ||
     !tolua_isnoobj(tolua_S,2))
 {
  TOLUA_ERR_FN(item_tester_hook_wear);
 } else {
  const object_type* o_ptr = ((const object_type*)  tolua_getusertype(tolua_S,1,0));
  bool toluaI_ret = (bool)  item_tester_hook_wear(o_ptr);
  tolua_pushbool(tolua_S,(int)toluaI_ret);
 }
 return 1;
}

/* function: item_tester_hook_recharge */
static int toluaI_object_item_tester_hook_recharge00(lua_State* tolua_S)
{
 if (!tolua_istype(tolua_S,1,tolua_tag(tolua_S,"const object_type"),0) ||
     !tolua_isnoobj(tolua_S,2))
 {
  TOLUA_ERR_FN(item_tester_hook_recharge);
 } else {
  const object_type* o_ptr = ((const object_type*)  tolua_getusertype(tolua_S,1,0));
  bool toluaI_ret = (bool)  item_tester_hook_recharge(o_ptr);
  tolua_pushbool(tolua_S,(int)toluaI_ret);
 }
 return 1;
}

/* function: item_tester_hook_jewel */
static int toluaI_object_item_tester_hook_jewel00(lua_State* tolua_S)
{
 if (!tolua_istype(tolua_S,1,tolua_tag(tolua_S,"const object_type"),0) ||
     !tolua_isnoobj(tolua_S,2))
 {
  TOLUA_ERR_FN(item_tester_hook_jewel);
 } else {
  const object_type* o_ptr = ((const object_type*)  tolua_getusertype(tolua_S,1,0));
  bool toluaI_ret = (bool)  item_tester_hook_jewel(o_ptr);
  tolua_pushbool(tolua_S,(int)toluaI_ret);
 }
 return 1;
}

/* function: item_tester_hook_tval */
static int toluaI_object_item_tester_hook_tval00(lua_State* tolua_S)
{
 if (!tolua_istype(tolua_S,1,tolua_tag(tolua_S,"const object_type"),0) ||
     !tolua_isnoobj(tolua_S,2))
 {
  TOLUA_ERR_FN(item_tester_hook_tval);
 } else {
  const object_type* o_ptr = ((const object_type*)  tolua_getusertype(tolua_S,1,0));
  bool toluaI_ret = (bool)  item_tester_hook_tval(o_ptr);
  tolua_pushbool(tolua_S,(int)toluaI_ret);
 }
 return 1;
}

/* function: item_tester_hook_is_blessed */
static int toluaI_object_item_tester_hook_is_blessed00(lua_State* tolua_S)
{
 if (!tolua_istype(tolua_S,1,tolua_tag(tolua_S,"const object_type"),0) ||
     !tolua_isnoobj(tolua_S,2))
 {
  TOLUA_ERR_FN(item_tester_hook_is_blessed);
 } else {
  const object_type* o_ptr = ((const object_type*)  tolua_getusertype(tolua_S,1,0));
  bool toluaI_ret = (bool)  item_tester_hook_is_blessed(o_ptr);
  tolua_pushbool(tolua_S,(int)toluaI_ret);
 }
 return 1;
}

/* function: item_tester_hook_is_good */
static int toluaI_object_item_tester_hook_is_good00(lua_State* tolua_S)
{
 if (!tolua_istype(tolua_S,1,tolua_tag(tolua_S,"const object_type"),0) ||
     !tolua_isnoobj(tolua_S,2))
 {
  TOLUA_ERR_FN(item_tester_hook_is_good);
 } else {
  const object_type* o_ptr = ((const object_type*)  tolua_getusertype(tolua_S,1,0));
  bool toluaI_ret = (bool)  item_tester_hook_is_good(o_ptr);
  tolua_pushbool(tolua_S,(int)toluaI_ret);
 }
 return 1;
}

/* function: item_tester_hook_is_great */
static int toluaI_object_item_tester_hook_is_great00(lua_State* tolua_S)
{
 if (!tolua_istype(tolua_S,1,tolua_tag(tolua_S,"const object_type"),0) ||
     !tolua_isnoobj(tolua_S,2))
 {
  TOLUA_ERR_FN(item_tester_hook_is_great);
 } else {
  const object_type* o_ptr = ((const object_type*)  tolua_getusertype(tolua_S,1,0));
  bool toluaI_ret = (bool)  item_tester_hook_is_great(o_ptr);
  tolua_pushbool(tolua_S,(int)toluaI_ret);
 }
 return 1;
}

/* function: item_tester_hook_is_book */
static int toluaI_object_item_tester_hook_is_book00(lua_State* tolua_S)
{
 if (!tolua_istype(tolua_S,1,tolua_tag(tolua_S,"const object_type"),0) ||
     !tolua_isnoobj(tolua_S,2))
 {
  TOLUA_ERR_FN(item_tester_hook_is_book);
 } else {
  const object_type* o_ptr = ((const object_type*)  tolua_getusertype(tolua_S,1,0));
  bool toluaI_ret = (bool)  item_tester_hook_is_book(o_ptr);
  tolua_pushbool(tolua_S,(int)toluaI_ret);
 }
 return 1;
}

/* function: add_ego_flags */
static int toluaI_object_add_ego_flags00(lua_State* tolua_S)
{
 if (!tolua_istype(tolua_S,1,tolua_tag(tolua_S,"object_type"),0) ||
     !tolua_istype(tolua_S,2,LUA_TNUMBER,0) ||
     !tolua_isnoobj(tolua_S,3))
 {
  TOLUA_ERR_FN(add_ego_flags);
 } else {
  object_type* o_ptr = ((object_type*)  tolua_getusertype(tolua_S,1,0));
  byte ego = ((byte)  tolua_getnumber(tolua_S,2,0));
  add_ego_flags(o_ptr,ego);
 }
 return 0;
}

/* function: add_ego_power */
static int toluaI_object_add_ego_power00(lua_State* tolua_S)
{
 if (!tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
     !tolua_istype(tolua_S,2,tolua_tag(tolua_S,"object_type"),0) ||
     !tolua_isnoobj(tolua_S,3))
 {
  TOLUA_ERR_FN(add_ego_power);
 } else {
  int power = ((int)  tolua_getnumber(tolua_S,1,0));
  object_type* o_ptr = ((object_type*)  tolua_getusertype(tolua_S,2,0));
  add_ego_power(power,o_ptr);
 }
 return 0;
}

/* function: acquirement */
static int toluaI_object_acquirement00(lua_State* tolua_S)
{
 if (!tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
     !tolua_istype(tolua_S,2,LUA_TNUMBER,0) ||
     !tolua_istype(tolua_S,3,LUA_TNUMBER,0) ||
     !tolua_istype(tolua_S,4,tolua_tag(tolua_S,"bool"),0) ||
     !tolua_istype(tolua_S,5,tolua_tag(tolua_S,"bool"),0) ||
     !tolua_isnoobj(tolua_S,6))
 {
  TOLUA_ERR_FN(acquirement);
 } else {
  int x1 = ((int)  tolua_getnumber(tolua_S,1,0));
  int y1 = ((int)  tolua_getnumber(tolua_S,2,0));
  int num = ((int)  tolua_getnumber(tolua_S,3,0));
  bool great = ((bool)  tolua_getbool(tolua_S,4,0));
  bool known = ((bool)  tolua_getbool(tolua_S,5,0));
  acquirement(x1,y1,num,great,known);
 }
 return 0;
}

/* function: ring_of_power */
static int toluaI_object_ring_of_power00(lua_State* tolua_S)
{
 if (!tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
     !tolua_isnoobj(tolua_S,2))
 {
  TOLUA_ERR_FN(ring_of_power);
 } else {
  int dir = ((int)  tolua_getnumber(tolua_S,1,0));
  ring_of_power(dir);
 }
 return 0;
}

/* Open function */
int tolua_object_open (lua_State* tolua_S)
{
 tolua_open(tolua_S);
 toluaI_reg_types(tolua_S);
 TOLUA_DEF(EGO_XTRA_SUSTAIN);
 TOLUA_DEF(EGO_XTRA_LO_RESIST);
 TOLUA_DEF(EGO_XTRA_HI_RESIST);
 TOLUA_DEF(EGO_XTRA_ANY_RESIST);
 TOLUA_DEF(EGO_XTRA_ABILITY);
 TOLUA_DEF(EGO_XTRA_POWER);
 TOLUA_DEF(SV_WAND_HEAL_MONSTER);
 TOLUA_DEF(SV_WAND_HASTE_MONSTER);
 TOLUA_DEF(SV_WAND_CLONE_MONSTER);
 TOLUA_DEF(SV_WAND_TELEPORT_AWAY);
 TOLUA_DEF(SV_WAND_DISARMING);
 TOLUA_DEF(SV_WAND_TRAP_DOOR_DEST);
 TOLUA_DEF(SV_WAND_STONE_TO_MUD);
 TOLUA_DEF(SV_WAND_LITE);
 TOLUA_DEF(SV_WAND_SLEEP_MONSTER);
 TOLUA_DEF(SV_WAND_SLOW_MONSTER);
 TOLUA_DEF(SV_WAND_CONFUSE_MONSTER);
 TOLUA_DEF(SV_WAND_FEAR_MONSTER);
 TOLUA_DEF(SV_WAND_DRAIN_LIFE);
 TOLUA_DEF(SV_WAND_POLYMORPH);
 TOLUA_DEF(SV_WAND_STINKING_CLOUD);
 TOLUA_DEF(SV_WAND_MAGIC_MISSILE);
 TOLUA_DEF(SV_WAND_ACID_BOLT);
 TOLUA_DEF(SV_WAND_CHARM_MONSTER);
 TOLUA_DEF(SV_WAND_FIRE_BOLT);
 TOLUA_DEF(SV_WAND_COLD_BOLT);
 TOLUA_DEF(SV_WAND_ACID_BALL);
 TOLUA_DEF(SV_WAND_ELEC_BALL);
 TOLUA_DEF(SV_WAND_FIRE_BALL);
 TOLUA_DEF(SV_WAND_COLD_BALL);
 TOLUA_DEF(SV_WAND_WONDER);
 TOLUA_DEF(TR0_STR);
 TOLUA_DEF(TR0_INT);
 TOLUA_DEF(TR0_WIS);
 TOLUA_DEF(TR0_DEX);
 TOLUA_DEF(TR0_CON);
 TOLUA_DEF(TR0_CHR);
 TOLUA_DEF(TR0_XXX1);
 TOLUA_DEF(TR0_SP);
 TOLUA_DEF(TR0_STEALTH);
 TOLUA_DEF(TR0_SEARCH);
 TOLUA_DEF(TR0_INFRA);
 TOLUA_DEF(TR0_TUNNEL);
 TOLUA_DEF(TR0_SPEED);
 TOLUA_DEF(TR0_BLOWS);
 TOLUA_DEF(TR0_CHAOTIC);
 TOLUA_DEF(TR0_VAMPIRIC);
 TOLUA_DEF(TR0_SLAY_ANIMAL);
 TOLUA_DEF(TR0_SLAY_EVIL);
 TOLUA_DEF(TR0_SLAY_UNDEAD);
 TOLUA_DEF(TR0_SLAY_DEMON);
 TOLUA_DEF(TR0_SLAY_ORC);
 TOLUA_DEF(TR0_SLAY_TROLL);
 TOLUA_DEF(TR0_SLAY_GIANT);
 TOLUA_DEF(TR0_SLAY_DRAGON);
 TOLUA_DEF(TR0_KILL_DRAGON);
 TOLUA_DEF(TR0_VORPAL);
 TOLUA_DEF(TR0_IMPACT);
 TOLUA_DEF(TR0_BRAND_POIS);
 TOLUA_DEF(TR0_BRAND_ACID);
 TOLUA_DEF(TR0_BRAND_ELEC);
 TOLUA_DEF(TR0_BRAND_FIRE);
 TOLUA_DEF(TR0_BRAND_COLD);
 TOLUA_DEF(TR1_SUST_STR);
 TOLUA_DEF(TR1_SUST_INT);
 TOLUA_DEF(TR1_SUST_WIS);
 TOLUA_DEF(TR1_SUST_DEX);
 TOLUA_DEF(TR1_SUST_CON);
 TOLUA_DEF(TR1_SUST_CHR);
 TOLUA_DEF(TR1_XXX1);
 TOLUA_DEF(TR1_IM_POIS);
 TOLUA_DEF(TR1_IM_ACID);
 TOLUA_DEF(TR1_IM_ELEC);
 TOLUA_DEF(TR1_IM_FIRE);
 TOLUA_DEF(TR1_IM_COLD);
 TOLUA_DEF(TR1_THROW);
 TOLUA_DEF(TR1_REFLECT);
 TOLUA_DEF(TR1_FREE_ACT);
 TOLUA_DEF(TR1_HOLD_LIFE);
 TOLUA_DEF(TR1_RES_ACID);
 TOLUA_DEF(TR1_RES_ELEC);
 TOLUA_DEF(TR1_RES_FIRE);
 TOLUA_DEF(TR1_RES_COLD);
 TOLUA_DEF(TR1_RES_POIS);
 TOLUA_DEF(TR1_RES_FEAR);
 TOLUA_DEF(TR1_RES_LITE);
 TOLUA_DEF(TR1_RES_DARK);
 TOLUA_DEF(TR1_RES_BLIND);
 TOLUA_DEF(TR1_RES_CONF);
 TOLUA_DEF(TR1_RES_SOUND);
 TOLUA_DEF(TR1_RES_SHARDS);
 TOLUA_DEF(TR1_RES_NETHER);
 TOLUA_DEF(TR1_RES_NEXUS);
 TOLUA_DEF(TR1_RES_CHAOS);
 TOLUA_DEF(TR1_RES_DISEN);
 TOLUA_DEF(TR2_SH_FIRE);
 TOLUA_DEF(TR2_SH_ELEC);
 TOLUA_DEF(TR2_QUESTITEM);
 TOLUA_DEF(TR2_XXX4);
 TOLUA_DEF(TR2_NO_TELE);
 TOLUA_DEF(TR2_NO_MAGIC);
 TOLUA_DEF(TR2_XXX7);
 TOLUA_DEF(TR2_TY_CURSE);
 TOLUA_DEF(TR2_EASY_KNOW);
 TOLUA_DEF(TR2_HIDE_TYPE);
 TOLUA_DEF(TR2_SHOW_MODS);
 TOLUA_DEF(TR2_INSTA_ART);
 TOLUA_DEF(TR2_FEATHER);
 TOLUA_DEF(TR2_LITE);
 TOLUA_DEF(TR2_SEE_INVIS);
 TOLUA_DEF(TR2_TELEPATHY);
 TOLUA_DEF(TR2_SLOW_DIGEST);
 TOLUA_DEF(TR2_REGEN);
 TOLUA_DEF(TR2_XTRA_MIGHT);
 TOLUA_DEF(TR2_XTRA_SHOTS);
 TOLUA_DEF(TR2_IGNORE_ACID);
 TOLUA_DEF(TR2_IGNORE_ELEC);
 TOLUA_DEF(TR2_IGNORE_FIRE);
 TOLUA_DEF(TR2_IGNORE_COLD);
 TOLUA_DEF(TR2_ACTIVATE);
 TOLUA_DEF(TR2_DRAIN_EXP);
 TOLUA_DEF(TR2_TELEPORT);
 TOLUA_DEF(TR2_AGGRAVATE);
 TOLUA_DEF(TR2_BLESSED);
 TOLUA_DEF(TR2_CURSED);
 TOLUA_DEF(TR2_HEAVY_CURSE);
 TOLUA_DEF(TR2_PERMA_CURSE);
 TOLUA_DEF(TR3_LUCK_10);
 TOLUA_DEF(TR3_XXX2);
 TOLUA_DEF(TR3_XXX3);
 TOLUA_DEF(TR3_XXX4);
 TOLUA_DEF(TR3_XXX5);
 TOLUA_DEF(TR3_XXX6);
 TOLUA_DEF(TR3_XXX7);
 TOLUA_DEF(TR3_XXX8);
 TOLUA_DEF(TR3_IM_LITE);
 TOLUA_DEF(TR3_IM_DARK);
 TOLUA_DEF(TR3_SH_ACID);
 TOLUA_DEF(TR3_SH_COLD);
 TOLUA_DEF(TR3_MUTATE);
 TOLUA_DEF(TR3_PATRON);
 TOLUA_DEF(TR3_STRANGE_LUCK);
 TOLUA_DEF(TR3_PASS_WALL);
 TOLUA_DEF(TR3_GHOUL_TOUCH);
 TOLUA_DEF(TR3_PSI_CRIT);
 TOLUA_DEF(TR3_RETURN);
 TOLUA_DEF(TR3_EXPLODE);
 TOLUA_DEF(TR3_HURT_ACID);
 TOLUA_DEF(TR3_HURT_ELEC);
 TOLUA_DEF(TR3_HURT_FIRE);
 TOLUA_DEF(TR3_HURT_COLD);
 TOLUA_DEF(TR3_HURT_LITE);
 TOLUA_DEF(TR3_HURT_DARK);
 TOLUA_DEF(TR3_XXX27);
 TOLUA_DEF(TR3_XXX28);
 TOLUA_DEF(TR3_AUTO_CURSE);
 TOLUA_DEF(TR3_DRAIN_STATS);
 TOLUA_DEF(TR3_CANT_EAT);
 TOLUA_DEF(TR3_SLOW_HEAL);
 tolua_cclass(tolua_S,"object_type","");
 tolua_tablevar(tolua_S,"object_type","k_idx",toluaI_get_object_object_type_k_idx,toluaI_set_object_object_type_k_idx);
 tolua_tablevar(tolua_S,"object_type","iy",toluaI_get_object_object_type_iy,toluaI_set_object_object_type_iy);
 tolua_tablevar(tolua_S,"object_type","ix",toluaI_get_object_object_type_ix,toluaI_set_object_object_type_ix);
 tolua_tablevar(tolua_S,"object_type","tval",toluaI_get_object_object_type_tval,toluaI_set_object_object_type_tval);
 tolua_tablevar(tolua_S,"object_type","sval",toluaI_get_object_object_type_sval,toluaI_set_object_object_type_sval);
 tolua_tablevar(tolua_S,"object_type","pval",toluaI_get_object_object_type_pval,toluaI_set_object_object_type_pval);
 tolua_tablevar(tolua_S,"object_type","discount",toluaI_get_object_object_type_discount,toluaI_set_object_object_type_discount);
 tolua_tablevar(tolua_S,"object_type","number",toluaI_get_object_object_type_number,toluaI_set_object_object_type_number);
 tolua_tablevar(tolua_S,"object_type","weight",toluaI_get_object_object_type_weight,toluaI_set_object_object_type_weight);
 tolua_tablevar(tolua_S,"object_type","to_h",toluaI_get_object_object_type_to_h,toluaI_set_object_object_type_to_h);
 tolua_tablevar(tolua_S,"object_type","to_d",toluaI_get_object_object_type_to_d,toluaI_set_object_object_type_to_d);
 tolua_tablevar(tolua_S,"object_type","to_a",toluaI_get_object_object_type_to_a,toluaI_set_object_object_type_to_a);
 tolua_tablevar(tolua_S,"object_type","ac",toluaI_get_object_object_type_ac,toluaI_set_object_object_type_ac);
 tolua_tablevar(tolua_S,"object_type","timeout",toluaI_get_object_object_type_timeout,toluaI_set_object_object_type_timeout);
 tolua_tablevar(tolua_S,"object_type","dd",toluaI_get_object_object_type_dd,toluaI_set_object_object_type_dd);
 tolua_tablevar(tolua_S,"object_type","ds",toluaI_get_object_object_type_ds,toluaI_set_object_object_type_ds);
 tolua_tablevar(tolua_S,"object_type","inscription",toluaI_get_object_object_type_inscription,toluaI_set_object_object_type_inscription);
 tolua_tablevar(tolua_S,"object_type","xtra_name",toluaI_get_object_object_type_xtra_name,toluaI_set_object_object_type_xtra_name);
 tolua_tablearray(tolua_S,"object_type","flags",toluaI_get_object_object_type_flags,toluaI_set_object_object_type_flags);
 tolua_tablearray(tolua_S,"object_type","kn_flags",toluaI_get_object_object_type_kn_flags,toluaI_set_object_object_type_kn_flags);
 tolua_tablevar(tolua_S,"object_type","next_o_idx",toluaI_get_object_object_type_next_o_idx,toluaI_set_object_object_type_next_o_idx);
 tolua_tablevar(tolua_S,"object_type","cost",toluaI_get_object_object_type_cost,toluaI_set_object_object_type_cost);
 tolua_tablevar(tolua_S,"object_type","feeling",toluaI_get_object_object_type_feeling,toluaI_set_object_object_type_feeling);
 tolua_tablevar(tolua_S,"object_type","a_idx",toluaI_get_object_object_type_a_idx,toluaI_set_object_object_type_a_idx);
 tolua_tablevar(tolua_S,"object_type","info",toluaI_get_object_object_type_info,toluaI_set_object_object_type_info);
 tolua_tablevar(tolua_S,"object_type","allocated",toluaI_get_object_object_type_allocated,toluaI_set_object_object_type_allocated);
 TOLUA_FUN(item_tester_hook_weapon,toluaI_object_item_tester_hook_weapon00);
 TOLUA_FUN(item_tester_hook_melee_weapon,toluaI_object_item_tester_hook_melee_weapon00);
 TOLUA_FUN(item_tester_hook_nonsword,toluaI_object_item_tester_hook_nonsword00);
 TOLUA_FUN(item_tester_hook_ammo,toluaI_object_item_tester_hook_ammo00);
 TOLUA_FUN(item_tester_hook_fletcher,toluaI_object_item_tester_hook_fletcher00);
 TOLUA_FUN(item_tester_hook_armour,toluaI_object_item_tester_hook_armour00);
 TOLUA_FUN(item_tester_hook_soft_armour,toluaI_object_item_tester_hook_soft_armour00);
 TOLUA_FUN(item_tester_hook_hard_armour,toluaI_object_item_tester_hook_hard_armour00);
 TOLUA_FUN(item_tester_hook_helm,toluaI_object_item_tester_hook_helm00);
 TOLUA_FUN(item_tester_hook_pure_hard_armour,toluaI_object_item_tester_hook_pure_hard_armour00);
 TOLUA_FUN(item_tester_hook_weapon_armour,toluaI_object_item_tester_hook_weapon_armour00);
 TOLUA_FUN(item_tester_hook_wear,toluaI_object_item_tester_hook_wear00);
 TOLUA_FUN(item_tester_hook_recharge,toluaI_object_item_tester_hook_recharge00);
 TOLUA_FUN(item_tester_hook_jewel,toluaI_object_item_tester_hook_jewel00);
 TOLUA_FUN(item_tester_hook_tval,toluaI_object_item_tester_hook_tval00);
 TOLUA_FUN(item_tester_hook_is_blessed,toluaI_object_item_tester_hook_is_blessed00);
 TOLUA_FUN(item_tester_hook_is_good,toluaI_object_item_tester_hook_is_good00);
 TOLUA_FUN(item_tester_hook_is_great,toluaI_object_item_tester_hook_is_great00);
 TOLUA_FUN(item_tester_hook_is_book,toluaI_object_item_tester_hook_is_book00);
 TOLUA_FUN(add_ego_flags,toluaI_object_add_ego_flags00);
 TOLUA_FUN(add_ego_power,toluaI_object_add_ego_power00);
 TOLUA_FUN(acquirement,toluaI_object_acquirement00);
 TOLUA_FUN(ring_of_power,toluaI_object_ring_of_power00);
 return 1;
}
/* Close function */
void tolua_object_close (lua_State* tolua_S)
{
 TOLUA_UNDEF(EGO_XTRA_SUSTAIN);
 TOLUA_UNDEF(EGO_XTRA_LO_RESIST);
 TOLUA_UNDEF(EGO_XTRA_HI_RESIST);
 TOLUA_UNDEF(EGO_XTRA_ANY_RESIST);
 TOLUA_UNDEF(EGO_XTRA_ABILITY);
 TOLUA_UNDEF(EGO_XTRA_POWER);
 TOLUA_UNDEF(SV_WAND_HEAL_MONSTER);
 TOLUA_UNDEF(SV_WAND_HASTE_MONSTER);
 TOLUA_UNDEF(SV_WAND_CLONE_MONSTER);
 TOLUA_UNDEF(SV_WAND_TELEPORT_AWAY);
 TOLUA_UNDEF(SV_WAND_DISARMING);
 TOLUA_UNDEF(SV_WAND_TRAP_DOOR_DEST);
 TOLUA_UNDEF(SV_WAND_STONE_TO_MUD);
 TOLUA_UNDEF(SV_WAND_LITE);
 TOLUA_UNDEF(SV_WAND_SLEEP_MONSTER);
 TOLUA_UNDEF(SV_WAND_SLOW_MONSTER);
 TOLUA_UNDEF(SV_WAND_CONFUSE_MONSTER);
 TOLUA_UNDEF(SV_WAND_FEAR_MONSTER);
 TOLUA_UNDEF(SV_WAND_DRAIN_LIFE);
 TOLUA_UNDEF(SV_WAND_POLYMORPH);
 TOLUA_UNDEF(SV_WAND_STINKING_CLOUD);
 TOLUA_UNDEF(SV_WAND_MAGIC_MISSILE);
 TOLUA_UNDEF(SV_WAND_ACID_BOLT);
 TOLUA_UNDEF(SV_WAND_CHARM_MONSTER);
 TOLUA_UNDEF(SV_WAND_FIRE_BOLT);
 TOLUA_UNDEF(SV_WAND_COLD_BOLT);
 TOLUA_UNDEF(SV_WAND_ACID_BALL);
 TOLUA_UNDEF(SV_WAND_ELEC_BALL);
 TOLUA_UNDEF(SV_WAND_FIRE_BALL);
 TOLUA_UNDEF(SV_WAND_COLD_BALL);
 TOLUA_UNDEF(SV_WAND_WONDER);
 TOLUA_UNDEF(TR0_STR);
 TOLUA_UNDEF(TR0_INT);
 TOLUA_UNDEF(TR0_WIS);
 TOLUA_UNDEF(TR0_DEX);
 TOLUA_UNDEF(TR0_CON);
 TOLUA_UNDEF(TR0_CHR);
 TOLUA_UNDEF(TR0_XXX1);
 TOLUA_UNDEF(TR0_SP);
 TOLUA_UNDEF(TR0_STEALTH);
 TOLUA_UNDEF(TR0_SEARCH);
 TOLUA_UNDEF(TR0_INFRA);
 TOLUA_UNDEF(TR0_TUNNEL);
 TOLUA_UNDEF(TR0_SPEED);
 TOLUA_UNDEF(TR0_BLOWS);
 TOLUA_UNDEF(TR0_CHAOTIC);
 TOLUA_UNDEF(TR0_VAMPIRIC);
 TOLUA_UNDEF(TR0_SLAY_ANIMAL);
 TOLUA_UNDEF(TR0_SLAY_EVIL);
 TOLUA_UNDEF(TR0_SLAY_UNDEAD);
 TOLUA_UNDEF(TR0_SLAY_DEMON);
 TOLUA_UNDEF(TR0_SLAY_ORC);
 TOLUA_UNDEF(TR0_SLAY_TROLL);
 TOLUA_UNDEF(TR0_SLAY_GIANT);
 TOLUA_UNDEF(TR0_SLAY_DRAGON);
 TOLUA_UNDEF(TR0_KILL_DRAGON);
 TOLUA_UNDEF(TR0_VORPAL);
 TOLUA_UNDEF(TR0_IMPACT);
 TOLUA_UNDEF(TR0_BRAND_POIS);
 TOLUA_UNDEF(TR0_BRAND_ACID);
 TOLUA_UNDEF(TR0_BRAND_ELEC);
 TOLUA_UNDEF(TR0_BRAND_FIRE);
 TOLUA_UNDEF(TR0_BRAND_COLD);
 TOLUA_UNDEF(TR1_SUST_STR);
 TOLUA_UNDEF(TR1_SUST_INT);
 TOLUA_UNDEF(TR1_SUST_WIS);
 TOLUA_UNDEF(TR1_SUST_DEX);
 TOLUA_UNDEF(TR1_SUST_CON);
 TOLUA_UNDEF(TR1_SUST_CHR);
 TOLUA_UNDEF(TR1_XXX1);
 TOLUA_UNDEF(TR1_IM_POIS);
 TOLUA_UNDEF(TR1_IM_ACID);
 TOLUA_UNDEF(TR1_IM_ELEC);
 TOLUA_UNDEF(TR1_IM_FIRE);
 TOLUA_UNDEF(TR1_IM_COLD);
 TOLUA_UNDEF(TR1_THROW);
 TOLUA_UNDEF(TR1_REFLECT);
 TOLUA_UNDEF(TR1_FREE_ACT);
 TOLUA_UNDEF(TR1_HOLD_LIFE);
 TOLUA_UNDEF(TR1_RES_ACID);
 TOLUA_UNDEF(TR1_RES_ELEC);
 TOLUA_UNDEF(TR1_RES_FIRE);
 TOLUA_UNDEF(TR1_RES_COLD);
 TOLUA_UNDEF(TR1_RES_POIS);
 TOLUA_UNDEF(TR1_RES_FEAR);
 TOLUA_UNDEF(TR1_RES_LITE);
 TOLUA_UNDEF(TR1_RES_DARK);
 TOLUA_UNDEF(TR1_RES_BLIND);
 TOLUA_UNDEF(TR1_RES_CONF);
 TOLUA_UNDEF(TR1_RES_SOUND);
 TOLUA_UNDEF(TR1_RES_SHARDS);
 TOLUA_UNDEF(TR1_RES_NETHER);
 TOLUA_UNDEF(TR1_RES_NEXUS);
 TOLUA_UNDEF(TR1_RES_CHAOS);
 TOLUA_UNDEF(TR1_RES_DISEN);
 TOLUA_UNDEF(TR2_SH_FIRE);
 TOLUA_UNDEF(TR2_SH_ELEC);
 TOLUA_UNDEF(TR2_QUESTITEM);
 TOLUA_UNDEF(TR2_XXX4);
 TOLUA_UNDEF(TR2_NO_TELE);
 TOLUA_UNDEF(TR2_NO_MAGIC);
 TOLUA_UNDEF(TR2_XXX7);
 TOLUA_UNDEF(TR2_TY_CURSE);
 TOLUA_UNDEF(TR2_EASY_KNOW);
 TOLUA_UNDEF(TR2_HIDE_TYPE);
 TOLUA_UNDEF(TR2_SHOW_MODS);
 TOLUA_UNDEF(TR2_INSTA_ART);
 TOLUA_UNDEF(TR2_FEATHER);
 TOLUA_UNDEF(TR2_LITE);
 TOLUA_UNDEF(TR2_SEE_INVIS);
 TOLUA_UNDEF(TR2_TELEPATHY);
 TOLUA_UNDEF(TR2_SLOW_DIGEST);
 TOLUA_UNDEF(TR2_REGEN);
 TOLUA_UNDEF(TR2_XTRA_MIGHT);
 TOLUA_UNDEF(TR2_XTRA_SHOTS);
 TOLUA_UNDEF(TR2_IGNORE_ACID);
 TOLUA_UNDEF(TR2_IGNORE_ELEC);
 TOLUA_UNDEF(TR2_IGNORE_FIRE);
 TOLUA_UNDEF(TR2_IGNORE_COLD);
 TOLUA_UNDEF(TR2_ACTIVATE);
 TOLUA_UNDEF(TR2_DRAIN_EXP);
 TOLUA_UNDEF(TR2_TELEPORT);
 TOLUA_UNDEF(TR2_AGGRAVATE);
 TOLUA_UNDEF(TR2_BLESSED);
 TOLUA_UNDEF(TR2_CURSED);
 TOLUA_UNDEF(TR2_HEAVY_CURSE);
 TOLUA_UNDEF(TR2_PERMA_CURSE);
 TOLUA_UNDEF(TR3_LUCK_10);
 TOLUA_UNDEF(TR3_XXX2);
 TOLUA_UNDEF(TR3_XXX3);
 TOLUA_UNDEF(TR3_XXX4);
 TOLUA_UNDEF(TR3_XXX5);
 TOLUA_UNDEF(TR3_XXX6);
 TOLUA_UNDEF(TR3_XXX7);
 TOLUA_UNDEF(TR3_XXX8);
 TOLUA_UNDEF(TR3_IM_LITE);
 TOLUA_UNDEF(TR3_IM_DARK);
 TOLUA_UNDEF(TR3_SH_ACID);
 TOLUA_UNDEF(TR3_SH_COLD);
 TOLUA_UNDEF(TR3_MUTATE);
 TOLUA_UNDEF(TR3_PATRON);
 TOLUA_UNDEF(TR3_STRANGE_LUCK);
 TOLUA_UNDEF(TR3_PASS_WALL);
 TOLUA_UNDEF(TR3_GHOUL_TOUCH);
 TOLUA_UNDEF(TR3_PSI_CRIT);
 TOLUA_UNDEF(TR3_RETURN);
 TOLUA_UNDEF(TR3_EXPLODE);
 TOLUA_UNDEF(TR3_HURT_ACID);
 TOLUA_UNDEF(TR3_HURT_ELEC);
 TOLUA_UNDEF(TR3_HURT_FIRE);
 TOLUA_UNDEF(TR3_HURT_COLD);
 TOLUA_UNDEF(TR3_HURT_LITE);
 TOLUA_UNDEF(TR3_HURT_DARK);
 TOLUA_UNDEF(TR3_XXX27);
 TOLUA_UNDEF(TR3_XXX28);
 TOLUA_UNDEF(TR3_AUTO_CURSE);
 TOLUA_UNDEF(TR3_DRAIN_STATS);
 TOLUA_UNDEF(TR3_CANT_EAT);
 TOLUA_UNDEF(TR3_SLOW_HEAL);
 TOLUA_UNDEF(object_type);
 TOLUA_UNDEF(item_tester_hook_weapon);
 TOLUA_UNDEF(item_tester_hook_melee_weapon);
 TOLUA_UNDEF(item_tester_hook_nonsword);
 TOLUA_UNDEF(item_tester_hook_ammo);
 TOLUA_UNDEF(item_tester_hook_fletcher);
 TOLUA_UNDEF(item_tester_hook_armour);
 TOLUA_UNDEF(item_tester_hook_soft_armour);
 TOLUA_UNDEF(item_tester_hook_hard_armour);
 TOLUA_UNDEF(item_tester_hook_helm);
 TOLUA_UNDEF(item_tester_hook_pure_hard_armour);
 TOLUA_UNDEF(item_tester_hook_weapon_armour);
 TOLUA_UNDEF(item_tester_hook_wear);
 TOLUA_UNDEF(item_tester_hook_recharge);
 TOLUA_UNDEF(item_tester_hook_jewel);
 TOLUA_UNDEF(item_tester_hook_tval);
 TOLUA_UNDEF(item_tester_hook_is_blessed);
 TOLUA_UNDEF(item_tester_hook_is_good);
 TOLUA_UNDEF(item_tester_hook_is_great);
 TOLUA_UNDEF(item_tester_hook_is_book);
 TOLUA_UNDEF(add_ego_flags);
 TOLUA_UNDEF(add_ego_power);
 TOLUA_UNDEF(acquirement);
 TOLUA_UNDEF(ring_of_power);
}
