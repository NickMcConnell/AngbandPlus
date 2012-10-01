/*
** Lua binding: types
** Generated automatically by tolua 5.0a on 05/24/09 20:03:29.
*/

#ifndef __cplusplus
#include "stdlib.h"
#endif
#include "string.h"

#include "lua/tolua.h"

/* Exported function */
TOLUA_API int tolua_types_open (lua_State* tolua_S);

#include "angband.h"

/* function to release collected object via destructor */
#ifdef __cplusplus

static int tolua_collect_monster_blow (lua_State* tolua_S)
{
 monster_blow* self = (monster_blow*) tolua_tousertype(tolua_S,1,0);
 delete self;
 return 0;
}

static int tolua_collect_monster_attack (lua_State* tolua_S)
{
 monster_attack* self = (monster_attack*) tolua_tousertype(tolua_S,1,0);
 delete self;
 return 0;
}

static int tolua_collect_monster_spell (lua_State* tolua_S)
{
 monster_spell* self = (monster_spell*) tolua_tousertype(tolua_S,1,0);
 delete self;
 return 0;
}
#endif


/* function to register type */
static void tolua_reg_types (lua_State* tolua_S)
{
 tolua_usertype(tolua_S,"ability_def");
 tolua_usertype(tolua_S,"feature_type");
 tolua_usertype(tolua_S,"music_songs");
 tolua_usertype(tolua_S,"cave_type");
 tolua_usertype(tolua_S,"option_type");
 tolua_usertype(tolua_S,"class_def");
 tolua_usertype(tolua_S,"wild_info");
 tolua_usertype(tolua_S,"dungeon_info_type");
 tolua_usertype(tolua_S,"alloc_entry");
 tolua_usertype(tolua_S,"resist_def");
 tolua_usertype(tolua_S,"magic_spells");
 tolua_usertype(tolua_S,"owner_type");
 tolua_usertype(tolua_S,"store_type");
 tolua_usertype(tolua_S,"border_type");
 tolua_usertype(tolua_S,"monster_type");
 tolua_usertype(tolua_S,"monster_blow");
 tolua_usertype(tolua_S,"player_class");
 tolua_usertype(tolua_S,"artifact_type");
 tolua_usertype(tolua_S,"vault_type");
 tolua_usertype(tolua_S,"monster_magics");
 tolua_usertype(tolua_S,"vault_def");
 tolua_usertype(tolua_S,"player_race");
 tolua_usertype(tolua_S,"player_sex");
 tolua_usertype(tolua_S,"monster_attack");
 tolua_usertype(tolua_S,"player_type");
 tolua_usertype(tolua_S,"monster_spell");
 tolua_usertype(tolua_S,"dialog_answers");
 tolua_usertype(tolua_S,"monster_race");
 tolua_usertype(tolua_S,"header");
 tolua_usertype(tolua_S,"object_kind");
 tolua_usertype(tolua_S,"trap_type");
 tolua_usertype(tolua_S,"random_room");
 tolua_usertype(tolua_S,"ego_item_type");
 tolua_usertype(tolua_S,"object_type");
}

/* get function: v_major of class  header */
static int tolua_get_header_v_major(lua_State* tolua_S)
{
  header* self = (header*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'v_major'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->v_major);
 return 1;
}

/* set function: v_major of class  header */
static int tolua_set_header_v_major(lua_State* tolua_S)
{
  header* self = (header*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'v_major'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->v_major = ((byte)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: v_minor of class  header */
static int tolua_get_header_v_minor(lua_State* tolua_S)
{
  header* self = (header*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'v_minor'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->v_minor);
 return 1;
}

/* set function: v_minor of class  header */
static int tolua_set_header_v_minor(lua_State* tolua_S)
{
  header* self = (header*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'v_minor'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->v_minor = ((byte)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: v_patch of class  header */
static int tolua_get_header_v_patch(lua_State* tolua_S)
{
  header* self = (header*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'v_patch'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->v_patch);
 return 1;
}

/* set function: v_patch of class  header */
static int tolua_set_header_v_patch(lua_State* tolua_S)
{
  header* self = (header*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'v_patch'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->v_patch = ((byte)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: v_extra of class  header */
static int tolua_get_header_v_extra(lua_State* tolua_S)
{
  header* self = (header*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'v_extra'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->v_extra);
 return 1;
}

/* set function: v_extra of class  header */
static int tolua_set_header_v_extra(lua_State* tolua_S)
{
  header* self = (header*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'v_extra'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->v_extra = ((byte)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: info_num of class  header */
static int tolua_get_header_info_num(lua_State* tolua_S)
{
  header* self = (header*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'info_num'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->info_num);
 return 1;
}

/* set function: info_num of class  header */
static int tolua_set_header_info_num(lua_State* tolua_S)
{
  header* self = (header*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'info_num'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->info_num = ((u16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: info_len of class  header */
static int tolua_get_header_info_len(lua_State* tolua_S)
{
  header* self = (header*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'info_len'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->info_len);
 return 1;
}

/* set function: info_len of class  header */
static int tolua_set_header_info_len(lua_State* tolua_S)
{
  header* self = (header*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'info_len'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->info_len = ((u16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: head_size of class  header */
static int tolua_get_header_head_size(lua_State* tolua_S)
{
  header* self = (header*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'head_size'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->head_size);
 return 1;
}

/* set function: head_size of class  header */
static int tolua_set_header_head_size(lua_State* tolua_S)
{
  header* self = (header*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'head_size'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->head_size = ((u32b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: info_size of class  header */
static int tolua_get_header_info_size(lua_State* tolua_S)
{
  header* self = (header*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'info_size'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->info_size);
 return 1;
}

/* set function: info_size of class  header */
static int tolua_set_header_info_size(lua_State* tolua_S)
{
  header* self = (header*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'info_size'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->info_size = ((u32b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: name_size of class  header */
static int tolua_get_header_name_size(lua_State* tolua_S)
{
  header* self = (header*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'name_size'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->name_size);
 return 1;
}

/* set function: name_size of class  header */
static int tolua_set_header_name_size(lua_State* tolua_S)
{
  header* self = (header*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'name_size'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->name_size = ((u32b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: text_size of class  header */
static int tolua_get_header_text_size(lua_State* tolua_S)
{
  header* self = (header*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'text_size'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->text_size);
 return 1;
}

/* set function: text_size of class  header */
static int tolua_set_header_text_size(lua_State* tolua_S)
{
  header* self = (header*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'text_size'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->text_size = ((u32b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: top of class  random_room */
static int tolua_get_random_room_top(lua_State* tolua_S)
{
  random_room* self = (random_room*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'top'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->top);
 return 1;
}

/* set function: top of class  random_room */
static int tolua_set_random_room_top(lua_State* tolua_S)
{
  random_room* self = (random_room*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'top'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->top = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: left of class  random_room */
static int tolua_get_random_room_left(lua_State* tolua_S)
{
  random_room* self = (random_room*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'left'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->left);
 return 1;
}

/* set function: left of class  random_room */
static int tolua_set_random_room_left(lua_State* tolua_S)
{
  random_room* self = (random_room*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'left'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->left = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: right of class  random_room */
static int tolua_get_random_room_right(lua_State* tolua_S)
{
  random_room* self = (random_room*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'right'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->right);
 return 1;
}

/* set function: right of class  random_room */
static int tolua_set_random_room_right(lua_State* tolua_S)
{
  random_room* self = (random_room*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'right'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->right = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: bottom of class  random_room */
static int tolua_get_random_room_bottom(lua_State* tolua_S)
{
  random_room* self = (random_room*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'bottom'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->bottom);
 return 1;
}

/* set function: bottom of class  random_room */
static int tolua_set_random_room_bottom(lua_State* tolua_S)
{
  random_room* self = (random_room*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'bottom'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->bottom = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: centerx of class  random_room */
static int tolua_get_random_room_centerx(lua_State* tolua_S)
{
  random_room* self = (random_room*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'centerx'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->centerx);
 return 1;
}

/* set function: centerx of class  random_room */
static int tolua_set_random_room_centerx(lua_State* tolua_S)
{
  random_room* self = (random_room*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'centerx'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->centerx = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: centery of class  random_room */
static int tolua_get_random_room_centery(lua_State* tolua_S)
{
  random_room* self = (random_room*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'centery'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->centery);
 return 1;
}

/* set function: centery of class  random_room */
static int tolua_set_random_room_centery(lua_State* tolua_S)
{
  random_room* self = (random_room*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'centery'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->centery = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: connectedto of class  random_room */
static int tolua_get_types_random_room_connectedto(lua_State* tolua_S)
{
 int tolua_index;
  random_room* self;
 lua_pushstring(tolua_S,".self");
 lua_rawget(tolua_S,1);
 self = (random_room*)  lua_touserdata(tolua_S,-1);
#ifndef TOLUA_RELEASE
 {
 tolua_Error tolua_err;
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in array indexing.",&tolua_err);
 }
#endif
 tolua_index = (int)tolua_tonumber(tolua_S,2,0)-1;
#ifndef TOLUA_RELEASE
 if (tolua_index<0 || tolua_index>=20)
 tolua_error(tolua_S,"array indexing out of range.",NULL);
#endif
 tolua_pushboolean(tolua_S,(bool)self->connectedto[tolua_index]);
 return 1;
}

/* set function: connectedto of class  random_room */
static int tolua_set_types_random_room_connectedto(lua_State* tolua_S)
{
 int tolua_index;
  random_room* self;
 lua_pushstring(tolua_S,".self");
 lua_rawget(tolua_S,1);
 self = (random_room*)  lua_touserdata(tolua_S,-1);
#ifndef TOLUA_RELEASE
 {
 tolua_Error tolua_err;
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in array indexing.",&tolua_err);
 }
#endif
 tolua_index = (int)tolua_tonumber(tolua_S,2,0)-1;
#ifndef TOLUA_RELEASE
 if (tolua_index<0 || tolua_index>=20)
 tolua_error(tolua_S,"array indexing out of range.",NULL);
#endif
  self->connectedto[tolua_index] = ((bool)  tolua_toboolean(tolua_S,3,0));
 return 0;
}

/* get function: connectx of class  random_room */
static int tolua_get_types_random_room_connectx(lua_State* tolua_S)
{
 int tolua_index;
  random_room* self;
 lua_pushstring(tolua_S,".self");
 lua_rawget(tolua_S,1);
 self = (random_room*)  lua_touserdata(tolua_S,-1);
#ifndef TOLUA_RELEASE
 {
 tolua_Error tolua_err;
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in array indexing.",&tolua_err);
 }
#endif
 tolua_index = (int)tolua_tonumber(tolua_S,2,0)-1;
#ifndef TOLUA_RELEASE
 if (tolua_index<0 || tolua_index>=20)
 tolua_error(tolua_S,"array indexing out of range.",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->connectx[tolua_index]);
 return 1;
}

/* set function: connectx of class  random_room */
static int tolua_set_types_random_room_connectx(lua_State* tolua_S)
{
 int tolua_index;
  random_room* self;
 lua_pushstring(tolua_S,".self");
 lua_rawget(tolua_S,1);
 self = (random_room*)  lua_touserdata(tolua_S,-1);
#ifndef TOLUA_RELEASE
 {
 tolua_Error tolua_err;
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in array indexing.",&tolua_err);
 }
#endif
 tolua_index = (int)tolua_tonumber(tolua_S,2,0)-1;
#ifndef TOLUA_RELEASE
 if (tolua_index<0 || tolua_index>=20)
 tolua_error(tolua_S,"array indexing out of range.",NULL);
#endif
  self->connectx[tolua_index] = ((s16b)  tolua_tonumber(tolua_S,3,0));
 return 0;
}

/* get function: connecty of class  random_room */
static int tolua_get_types_random_room_connecty(lua_State* tolua_S)
{
 int tolua_index;
  random_room* self;
 lua_pushstring(tolua_S,".self");
 lua_rawget(tolua_S,1);
 self = (random_room*)  lua_touserdata(tolua_S,-1);
#ifndef TOLUA_RELEASE
 {
 tolua_Error tolua_err;
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in array indexing.",&tolua_err);
 }
#endif
 tolua_index = (int)tolua_tonumber(tolua_S,2,0)-1;
#ifndef TOLUA_RELEASE
 if (tolua_index<0 || tolua_index>=20)
 tolua_error(tolua_S,"array indexing out of range.",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->connecty[tolua_index]);
 return 1;
}

/* set function: connecty of class  random_room */
static int tolua_set_types_random_room_connecty(lua_State* tolua_S)
{
 int tolua_index;
  random_room* self;
 lua_pushstring(tolua_S,".self");
 lua_rawget(tolua_S,1);
 self = (random_room*)  lua_touserdata(tolua_S,-1);
#ifndef TOLUA_RELEASE
 {
 tolua_Error tolua_err;
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in array indexing.",&tolua_err);
 }
#endif
 tolua_index = (int)tolua_tonumber(tolua_S,2,0)-1;
#ifndef TOLUA_RELEASE
 if (tolua_index<0 || tolua_index>=20)
 tolua_error(tolua_S,"array indexing out of range.",NULL);
#endif
  self->connecty[tolua_index] = ((s16b)  tolua_tonumber(tolua_S,3,0));
 return 0;
}

/* get function: pickeddir of class  random_room */
static int tolua_get_types_random_room_pickeddir(lua_State* tolua_S)
{
 int tolua_index;
  random_room* self;
 lua_pushstring(tolua_S,".self");
 lua_rawget(tolua_S,1);
 self = (random_room*)  lua_touserdata(tolua_S,-1);
#ifndef TOLUA_RELEASE
 {
 tolua_Error tolua_err;
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in array indexing.",&tolua_err);
 }
#endif
 tolua_index = (int)tolua_tonumber(tolua_S,2,0)-1;
#ifndef TOLUA_RELEASE
 if (tolua_index<0 || tolua_index>=10)
 tolua_error(tolua_S,"array indexing out of range.",NULL);
#endif
 tolua_pushboolean(tolua_S,(bool)self->pickeddir[tolua_index]);
 return 1;
}

/* set function: pickeddir of class  random_room */
static int tolua_set_types_random_room_pickeddir(lua_State* tolua_S)
{
 int tolua_index;
  random_room* self;
 lua_pushstring(tolua_S,".self");
 lua_rawget(tolua_S,1);
 self = (random_room*)  lua_touserdata(tolua_S,-1);
#ifndef TOLUA_RELEASE
 {
 tolua_Error tolua_err;
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in array indexing.",&tolua_err);
 }
#endif
 tolua_index = (int)tolua_tonumber(tolua_S,2,0)-1;
#ifndef TOLUA_RELEASE
 if (tolua_index<0 || tolua_index>=10)
 tolua_error(tolua_S,"array indexing out of range.",NULL);
#endif
  self->pickeddir[tolua_index] = ((bool)  tolua_toboolean(tolua_S,3,0));
 return 0;
}

/* get function: created of class  random_room */
static int tolua_get_random_room_created(lua_State* tolua_S)
{
  random_room* self = (random_room*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'created'",NULL);
#endif
 tolua_pushboolean(tolua_S,(bool)self->created);
 return 1;
}

/* set function: created of class  random_room */
static int tolua_set_random_room_created(lua_State* tolua_S)
{
  random_room* self = (random_room*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'created'",NULL);
 if (!tolua_isboolean(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->created = ((bool)  tolua_toboolean(tolua_S,2,0));
 return 0;
}

/* get function: name of class  feature_type */
static int tolua_get_feature_type_name(lua_State* tolua_S)
{
  feature_type* self = (feature_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'name'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->name);
 return 1;
}

/* set function: name of class  feature_type */
static int tolua_set_feature_type_name(lua_State* tolua_S)
{
  feature_type* self = (feature_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'name'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->name = ((u32b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: text of class  feature_type */
static int tolua_get_feature_type_text(lua_State* tolua_S)
{
  feature_type* self = (feature_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'text'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->text);
 return 1;
}

/* set function: text of class  feature_type */
static int tolua_set_feature_type_text(lua_State* tolua_S)
{
  feature_type* self = (feature_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'text'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->text = ((u32b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: mimic of class  feature_type */
static int tolua_get_feature_type_mimic(lua_State* tolua_S)
{
  feature_type* self = (feature_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'mimic'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->mimic);
 return 1;
}

/* set function: mimic of class  feature_type */
static int tolua_set_feature_type_mimic(lua_State* tolua_S)
{
  feature_type* self = (feature_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'mimic'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->mimic = ((byte)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: flags1 of class  feature_type */
static int tolua_get_feature_type_flags1(lua_State* tolua_S)
{
  feature_type* self = (feature_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'flags1'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->flags1);
 return 1;
}

/* set function: flags1 of class  feature_type */
static int tolua_set_feature_type_flags1(lua_State* tolua_S)
{
  feature_type* self = (feature_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'flags1'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->flags1 = ((u32b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: extra of class  feature_type */
static int tolua_get_feature_type_extra(lua_State* tolua_S)
{
  feature_type* self = (feature_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'extra'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->extra);
 return 1;
}

/* set function: extra of class  feature_type */
static int tolua_set_feature_type_extra(lua_State* tolua_S)
{
  feature_type* self = (feature_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'extra'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->extra = ((byte)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: unused of class  feature_type */
static int tolua_get_feature_type_unused(lua_State* tolua_S)
{
  feature_type* self = (feature_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'unused'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->unused);
 return 1;
}

/* set function: unused of class  feature_type */
static int tolua_set_feature_type_unused(lua_State* tolua_S)
{
  feature_type* self = (feature_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'unused'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->unused = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: d_attr of class  feature_type */
static int tolua_get_feature_type_d_attr(lua_State* tolua_S)
{
  feature_type* self = (feature_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'd_attr'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->d_attr);
 return 1;
}

/* set function: d_attr of class  feature_type */
static int tolua_set_feature_type_d_attr(lua_State* tolua_S)
{
  feature_type* self = (feature_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'd_attr'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->d_attr = ((byte)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: d_char of class  feature_type */
static int tolua_get_feature_type_d_char(lua_State* tolua_S)
{
  feature_type* self = (feature_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'd_char'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->d_char);
 return 1;
}

/* set function: d_char of class  feature_type */
static int tolua_set_feature_type_d_char(lua_State* tolua_S)
{
  feature_type* self = (feature_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'd_char'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->d_char = ((char)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: x_attr of class  feature_type */
static int tolua_get_feature_type_x_attr(lua_State* tolua_S)
{
  feature_type* self = (feature_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'x_attr'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->x_attr);
 return 1;
}

/* set function: x_attr of class  feature_type */
static int tolua_set_feature_type_x_attr(lua_State* tolua_S)
{
  feature_type* self = (feature_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'x_attr'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->x_attr = ((byte)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: x_char of class  feature_type */
static int tolua_get_feature_type_x_char(lua_State* tolua_S)
{
  feature_type* self = (feature_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'x_char'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->x_char);
 return 1;
}

/* set function: x_char of class  feature_type */
static int tolua_set_feature_type_x_char(lua_State* tolua_S)
{
  feature_type* self = (feature_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'x_char'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->x_char = ((char)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: name of class  monster_attack */
static int tolua_get_monster_attack_name(lua_State* tolua_S)
{
  monster_attack* self = (monster_attack*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'name'",NULL);
#endif
 tolua_pushstring(tolua_S,(const char*)self->name);
 return 1;
}

/* set function: name of class  monster_attack */
static int tolua_set_monster_attack_name(lua_State* tolua_S)
{
  monster_attack* self = (monster_attack*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'name'",NULL);
 if (!tolua_isstring(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
 strncpy(self->name,tolua_tostring(tolua_S,2,0),80-1);
 return 0;
}

/* get function: act of class  monster_attack */
static int tolua_get_monster_attack_act(lua_State* tolua_S)
{
  monster_attack* self = (monster_attack*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'act'",NULL);
#endif
 tolua_pushstring(tolua_S,(const char*)self->act);
 return 1;
}

/* set function: act of class  monster_attack */
static int tolua_set_monster_attack_act(lua_State* tolua_S)
{
  monster_attack* self = (monster_attack*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'act'",NULL);
 if (!tolua_isstring(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
 strncpy(self->act,tolua_tostring(tolua_S,2,0),80-1);
 return 0;
}

/* get function: type of class  monster_attack */
static int tolua_get_monster_attack_type(lua_State* tolua_S)
{
  monster_attack* self = (monster_attack*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'type'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->type);
 return 1;
}

/* set function: type of class  monster_attack */
static int tolua_set_monster_attack_type(lua_State* tolua_S)
{
  monster_attack* self = (monster_attack*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'type'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->type = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: effect of class  monster_attack */
static int tolua_get_monster_attack_effect(lua_State* tolua_S)
{
  monster_attack* self = (monster_attack*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'effect'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->effect);
 return 1;
}

/* set function: effect of class  monster_attack */
static int tolua_set_monster_attack_effect(lua_State* tolua_S)
{
  monster_attack* self = (monster_attack*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'effect'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->effect = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: ddice of class  monster_attack */
static int tolua_get_monster_attack_ddice(lua_State* tolua_S)
{
  monster_attack* self = (monster_attack*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'ddice'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->ddice);
 return 1;
}

/* set function: ddice of class  monster_attack */
static int tolua_set_monster_attack_ddice(lua_State* tolua_S)
{
  monster_attack* self = (monster_attack*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'ddice'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->ddice = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: dside of class  monster_attack */
static int tolua_get_monster_attack_dside(lua_State* tolua_S)
{
  monster_attack* self = (monster_attack*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'dside'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->dside);
 return 1;
}

/* set function: dside of class  monster_attack */
static int tolua_set_monster_attack_dside(lua_State* tolua_S)
{
  monster_attack* self = (monster_attack*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'dside'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->dside = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: element of class  monster_attack */
static int tolua_get_monster_attack_element(lua_State* tolua_S)
{
  monster_attack* self = (monster_attack*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'element'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->element);
 return 1;
}

/* set function: element of class  monster_attack */
static int tolua_set_monster_attack_element(lua_State* tolua_S)
{
  monster_attack* self = (monster_attack*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'element'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->element = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: special1 of class  monster_attack */
static int tolua_get_monster_attack_special1(lua_State* tolua_S)
{
  monster_attack* self = (monster_attack*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'special1'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->special1);
 return 1;
}

/* set function: special1 of class  monster_attack */
static int tolua_set_monster_attack_special1(lua_State* tolua_S)
{
  monster_attack* self = (monster_attack*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'special1'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->special1 = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: special2 of class  monster_attack */
static int tolua_get_monster_attack_special2(lua_State* tolua_S)
{
  monster_attack* self = (monster_attack*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'special2'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->special2);
 return 1;
}

/* set function: special2 of class  monster_attack */
static int tolua_set_monster_attack_special2(lua_State* tolua_S)
{
  monster_attack* self = (monster_attack*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'special2'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->special2 = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: name of class  monster_spell */
static int tolua_get_monster_spell_name(lua_State* tolua_S)
{
  monster_spell* self = (monster_spell*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'name'",NULL);
#endif
 tolua_pushstring(tolua_S,(const char*)self->name);
 return 1;
}

/* set function: name of class  monster_spell */
static int tolua_set_monster_spell_name(lua_State* tolua_S)
{
  monster_spell* self = (monster_spell*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'name'",NULL);
 if (!tolua_isstring(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
 strncpy(self->name,tolua_tostring(tolua_S,2,0),80-1);
 return 0;
}

/* get function: act of class  monster_spell */
static int tolua_get_monster_spell_act(lua_State* tolua_S)
{
  monster_spell* self = (monster_spell*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'act'",NULL);
#endif
 tolua_pushstring(tolua_S,(const char*)self->act);
 return 1;
}

/* set function: act of class  monster_spell */
static int tolua_set_monster_spell_act(lua_State* tolua_S)
{
  monster_spell* self = (monster_spell*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'act'",NULL);
 if (!tolua_isstring(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
 strncpy(self->act,tolua_tostring(tolua_S,2,0),80-1);
 return 0;
}

/* get function: type of class  monster_spell */
static int tolua_get_monster_spell_type(lua_State* tolua_S)
{
  monster_spell* self = (monster_spell*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'type'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->type);
 return 1;
}

/* set function: type of class  monster_spell */
static int tolua_set_monster_spell_type(lua_State* tolua_S)
{
  monster_spell* self = (monster_spell*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'type'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->type = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: power of class  monster_spell */
static int tolua_get_monster_spell_power(lua_State* tolua_S)
{
  monster_spell* self = (monster_spell*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'power'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->power);
 return 1;
}

/* set function: power of class  monster_spell */
static int tolua_set_monster_spell_power(lua_State* tolua_S)
{
  monster_spell* self = (monster_spell*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'power'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->power = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: special1 of class  monster_spell */
static int tolua_get_monster_spell_special1(lua_State* tolua_S)
{
  monster_spell* self = (monster_spell*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'special1'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->special1);
 return 1;
}

/* set function: special1 of class  monster_spell */
static int tolua_set_monster_spell_special1(lua_State* tolua_S)
{
  monster_spell* self = (monster_spell*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'special1'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->special1 = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: special2 of class  monster_spell */
static int tolua_get_monster_spell_special2(lua_State* tolua_S)
{
  monster_spell* self = (monster_spell*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'special2'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->special2);
 return 1;
}

/* set function: special2 of class  monster_spell */
static int tolua_set_monster_spell_special2(lua_State* tolua_S)
{
  monster_spell* self = (monster_spell*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'special2'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->special2 = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: special3 of class  monster_spell */
static int tolua_get_monster_spell_special3(lua_State* tolua_S)
{
  monster_spell* self = (monster_spell*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'special3'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->special3);
 return 1;
}

/* set function: special3 of class  monster_spell */
static int tolua_set_monster_spell_special3(lua_State* tolua_S)
{
  monster_spell* self = (monster_spell*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'special3'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->special3 = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: summchar of class  monster_spell */
static int tolua_get_monster_spell_summchar(lua_State* tolua_S)
{
  monster_spell* self = (monster_spell*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'summchar'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->summchar);
 return 1;
}

/* set function: summchar of class  monster_spell */
static int tolua_set_monster_spell_summchar(lua_State* tolua_S)
{
  monster_spell* self = (monster_spell*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'summchar'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->summchar = ((char)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: cost of class  monster_spell */
static int tolua_get_monster_spell_cost(lua_State* tolua_S)
{
  monster_spell* self = (monster_spell*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'cost'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->cost);
 return 1;
}

/* set function: cost of class  monster_spell */
static int tolua_set_monster_spell_cost(lua_State* tolua_S)
{
  monster_spell* self = (monster_spell*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'cost'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->cost = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: name of class  magic_spells */
static int tolua_get_magic_spells_name(lua_State* tolua_S)
{
  magic_spells* self = (magic_spells*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'name'",NULL);
#endif
 tolua_pushstring(tolua_S,(const char*)self->name);
 return 1;
}

/* set function: name of class  magic_spells */
static int tolua_set_magic_spells_name(lua_State* tolua_S)
{
  magic_spells* self = (magic_spells*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'name'",NULL);
 if (!tolua_isstring(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
 strncpy(self->name,tolua_tostring(tolua_S,2,0),80-1);
 return 0;
}

/* get function: school of class  magic_spells */
static int tolua_get_types_magic_spells_school(lua_State* tolua_S)
{
 int tolua_index;
  magic_spells* self;
 lua_pushstring(tolua_S,".self");
 lua_rawget(tolua_S,1);
 self = (magic_spells*)  lua_touserdata(tolua_S,-1);
#ifndef TOLUA_RELEASE
 {
 tolua_Error tolua_err;
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in array indexing.",&tolua_err);
 }
#endif
 tolua_index = (int)tolua_tonumber(tolua_S,2,0)-1;
#ifndef TOLUA_RELEASE
 if (tolua_index<0 || tolua_index>=5)
 tolua_error(tolua_S,"array indexing out of range.",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->school[tolua_index]);
 return 1;
}

/* set function: school of class  magic_spells */
static int tolua_set_types_magic_spells_school(lua_State* tolua_S)
{
 int tolua_index;
  magic_spells* self;
 lua_pushstring(tolua_S,".self");
 lua_rawget(tolua_S,1);
 self = (magic_spells*)  lua_touserdata(tolua_S,-1);
#ifndef TOLUA_RELEASE
 {
 tolua_Error tolua_err;
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in array indexing.",&tolua_err);
 }
#endif
 tolua_index = (int)tolua_tonumber(tolua_S,2,0)-1;
#ifndef TOLUA_RELEASE
 if (tolua_index<0 || tolua_index>=5)
 tolua_error(tolua_S,"array indexing out of range.",NULL);
#endif
  self->school[tolua_index] = ((s16b)  tolua_tonumber(tolua_S,3,0));
 return 0;
}

/* get function: effect of class  magic_spells */
static int tolua_get_types_magic_spells_effect(lua_State* tolua_S)
{
 int tolua_index;
  magic_spells* self;
 lua_pushstring(tolua_S,".self");
 lua_rawget(tolua_S,1);
 self = (magic_spells*)  lua_touserdata(tolua_S,-1);
#ifndef TOLUA_RELEASE
 {
 tolua_Error tolua_err;
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in array indexing.",&tolua_err);
 }
#endif
 tolua_index = (int)tolua_tonumber(tolua_S,2,0)-1;
#ifndef TOLUA_RELEASE
 if (tolua_index<0 || tolua_index>=5)
 tolua_error(tolua_S,"array indexing out of range.",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->effect[tolua_index]);
 return 1;
}

/* set function: effect of class  magic_spells */
static int tolua_set_types_magic_spells_effect(lua_State* tolua_S)
{
 int tolua_index;
  magic_spells* self;
 lua_pushstring(tolua_S,".self");
 lua_rawget(tolua_S,1);
 self = (magic_spells*)  lua_touserdata(tolua_S,-1);
#ifndef TOLUA_RELEASE
 {
 tolua_Error tolua_err;
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in array indexing.",&tolua_err);
 }
#endif
 tolua_index = (int)tolua_tonumber(tolua_S,2,0)-1;
#ifndef TOLUA_RELEASE
 if (tolua_index<0 || tolua_index>=5)
 tolua_error(tolua_S,"array indexing out of range.",NULL);
#endif
  self->effect[tolua_index] = ((s16b)  tolua_tonumber(tolua_S,3,0));
 return 0;
}

/* get function: shape of class  magic_spells */
static int tolua_get_types_magic_spells_shape(lua_State* tolua_S)
{
 int tolua_index;
  magic_spells* self;
 lua_pushstring(tolua_S,".self");
 lua_rawget(tolua_S,1);
 self = (magic_spells*)  lua_touserdata(tolua_S,-1);
#ifndef TOLUA_RELEASE
 {
 tolua_Error tolua_err;
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in array indexing.",&tolua_err);
 }
#endif
 tolua_index = (int)tolua_tonumber(tolua_S,2,0)-1;
#ifndef TOLUA_RELEASE
 if (tolua_index<0 || tolua_index>=5)
 tolua_error(tolua_S,"array indexing out of range.",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->shape[tolua_index]);
 return 1;
}

/* set function: shape of class  magic_spells */
static int tolua_set_types_magic_spells_shape(lua_State* tolua_S)
{
 int tolua_index;
  magic_spells* self;
 lua_pushstring(tolua_S,".self");
 lua_rawget(tolua_S,1);
 self = (magic_spells*)  lua_touserdata(tolua_S,-1);
#ifndef TOLUA_RELEASE
 {
 tolua_Error tolua_err;
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in array indexing.",&tolua_err);
 }
#endif
 tolua_index = (int)tolua_tonumber(tolua_S,2,0)-1;
#ifndef TOLUA_RELEASE
 if (tolua_index<0 || tolua_index>=5)
 tolua_error(tolua_S,"array indexing out of range.",NULL);
#endif
  self->shape[tolua_index] = ((s16b)  tolua_tonumber(tolua_S,3,0));
 return 0;
}

/* get function: power of class  magic_spells */
static int tolua_get_types_magic_spells_power(lua_State* tolua_S)
{
 int tolua_index;
  magic_spells* self;
 lua_pushstring(tolua_S,".self");
 lua_rawget(tolua_S,1);
 self = (magic_spells*)  lua_touserdata(tolua_S,-1);
#ifndef TOLUA_RELEASE
 {
 tolua_Error tolua_err;
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in array indexing.",&tolua_err);
 }
#endif
 tolua_index = (int)tolua_tonumber(tolua_S,2,0)-1;
#ifndef TOLUA_RELEASE
 if (tolua_index<0 || tolua_index>=5)
 tolua_error(tolua_S,"array indexing out of range.",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->power[tolua_index]);
 return 1;
}

/* set function: power of class  magic_spells */
static int tolua_set_types_magic_spells_power(lua_State* tolua_S)
{
 int tolua_index;
  magic_spells* self;
 lua_pushstring(tolua_S,".self");
 lua_rawget(tolua_S,1);
 self = (magic_spells*)  lua_touserdata(tolua_S,-1);
#ifndef TOLUA_RELEASE
 {
 tolua_Error tolua_err;
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in array indexing.",&tolua_err);
 }
#endif
 tolua_index = (int)tolua_tonumber(tolua_S,2,0)-1;
#ifndef TOLUA_RELEASE
 if (tolua_index<0 || tolua_index>=5)
 tolua_error(tolua_S,"array indexing out of range.",NULL);
#endif
  self->power[tolua_index] = ((s32b)  tolua_tonumber(tolua_S,3,0));
 return 0;
}

/* get function: radius of class  magic_spells */
static int tolua_get_types_magic_spells_radius(lua_State* tolua_S)
{
 int tolua_index;
  magic_spells* self;
 lua_pushstring(tolua_S,".self");
 lua_rawget(tolua_S,1);
 self = (magic_spells*)  lua_touserdata(tolua_S,-1);
#ifndef TOLUA_RELEASE
 {
 tolua_Error tolua_err;
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in array indexing.",&tolua_err);
 }
#endif
 tolua_index = (int)tolua_tonumber(tolua_S,2,0)-1;
#ifndef TOLUA_RELEASE
 if (tolua_index<0 || tolua_index>=5)
 tolua_error(tolua_S,"array indexing out of range.",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->radius[tolua_index]);
 return 1;
}

/* set function: radius of class  magic_spells */
static int tolua_set_types_magic_spells_radius(lua_State* tolua_S)
{
 int tolua_index;
  magic_spells* self;
 lua_pushstring(tolua_S,".self");
 lua_rawget(tolua_S,1);
 self = (magic_spells*)  lua_touserdata(tolua_S,-1);
#ifndef TOLUA_RELEASE
 {
 tolua_Error tolua_err;
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in array indexing.",&tolua_err);
 }
#endif
 tolua_index = (int)tolua_tonumber(tolua_S,2,0)-1;
#ifndef TOLUA_RELEASE
 if (tolua_index<0 || tolua_index>=5)
 tolua_error(tolua_S,"array indexing out of range.",NULL);
#endif
  self->radius[tolua_index] = ((s16b)  tolua_tonumber(tolua_S,3,0));
 return 0;
}

/* get function: type of class  magic_spells */
static int tolua_get_types_magic_spells_type(lua_State* tolua_S)
{
 int tolua_index;
  magic_spells* self;
 lua_pushstring(tolua_S,".self");
 lua_rawget(tolua_S,1);
 self = (magic_spells*)  lua_touserdata(tolua_S,-1);
#ifndef TOLUA_RELEASE
 {
 tolua_Error tolua_err;
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in array indexing.",&tolua_err);
 }
#endif
 tolua_index = (int)tolua_tonumber(tolua_S,2,0)-1;
#ifndef TOLUA_RELEASE
 if (tolua_index<0 || tolua_index>=5)
 tolua_error(tolua_S,"array indexing out of range.",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->type[tolua_index]);
 return 1;
}

/* set function: type of class  magic_spells */
static int tolua_set_types_magic_spells_type(lua_State* tolua_S)
{
 int tolua_index;
  magic_spells* self;
 lua_pushstring(tolua_S,".self");
 lua_rawget(tolua_S,1);
 self = (magic_spells*)  lua_touserdata(tolua_S,-1);
#ifndef TOLUA_RELEASE
 {
 tolua_Error tolua_err;
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in array indexing.",&tolua_err);
 }
#endif
 tolua_index = (int)tolua_tonumber(tolua_S,2,0)-1;
#ifndef TOLUA_RELEASE
 if (tolua_index<0 || tolua_index>=5)
 tolua_error(tolua_S,"array indexing out of range.",NULL);
#endif
  self->type[tolua_index] = ((s16b)  tolua_tonumber(tolua_S,3,0));
 return 0;
}

/* get function: manacost of class  magic_spells */
static int tolua_get_types_magic_spells_manacost(lua_State* tolua_S)
{
 int tolua_index;
  magic_spells* self;
 lua_pushstring(tolua_S,".self");
 lua_rawget(tolua_S,1);
 self = (magic_spells*)  lua_touserdata(tolua_S,-1);
#ifndef TOLUA_RELEASE
 {
 tolua_Error tolua_err;
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in array indexing.",&tolua_err);
 }
#endif
 tolua_index = (int)tolua_tonumber(tolua_S,2,0)-1;
#ifndef TOLUA_RELEASE
 if (tolua_index<0 || tolua_index>=5)
 tolua_error(tolua_S,"array indexing out of range.",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->manacost[tolua_index]);
 return 1;
}

/* set function: manacost of class  magic_spells */
static int tolua_set_types_magic_spells_manacost(lua_State* tolua_S)
{
 int tolua_index;
  magic_spells* self;
 lua_pushstring(tolua_S,".self");
 lua_rawget(tolua_S,1);
 self = (magic_spells*)  lua_touserdata(tolua_S,-1);
#ifndef TOLUA_RELEASE
 {
 tolua_Error tolua_err;
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in array indexing.",&tolua_err);
 }
#endif
 tolua_index = (int)tolua_tonumber(tolua_S,2,0)-1;
#ifndef TOLUA_RELEASE
 if (tolua_index<0 || tolua_index>=5)
 tolua_error(tolua_S,"array indexing out of range.",NULL);
#endif
  self->manacost[tolua_index] = ((s32b)  tolua_tonumber(tolua_S,3,0));
 return 0;
}

/* get function: schar1 of class  magic_spells */
static int tolua_get_magic_spells_schar1(lua_State* tolua_S)
{
  magic_spells* self = (magic_spells*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'schar1'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->schar1);
 return 1;
}

/* set function: schar1 of class  magic_spells */
static int tolua_set_magic_spells_schar1(lua_State* tolua_S)
{
  magic_spells* self = (magic_spells*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'schar1'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->schar1 = ((char)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: schar2 of class  magic_spells */
static int tolua_get_magic_spells_schar2(lua_State* tolua_S)
{
  magic_spells* self = (magic_spells*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'schar2'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->schar2);
 return 1;
}

/* set function: schar2 of class  magic_spells */
static int tolua_set_magic_spells_schar2(lua_State* tolua_S)
{
  magic_spells* self = (magic_spells*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'schar2'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->schar2 = ((char)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: schar3 of class  magic_spells */
static int tolua_get_magic_spells_schar3(lua_State* tolua_S)
{
  magic_spells* self = (magic_spells*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'schar3'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->schar3);
 return 1;
}

/* set function: schar3 of class  magic_spells */
static int tolua_set_magic_spells_schar3(lua_State* tolua_S)
{
  magic_spells* self = (magic_spells*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'schar3'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->schar3 = ((char)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: schar4 of class  magic_spells */
static int tolua_get_magic_spells_schar4(lua_State* tolua_S)
{
  magic_spells* self = (magic_spells*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'schar4'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->schar4);
 return 1;
}

/* set function: schar4 of class  magic_spells */
static int tolua_set_magic_spells_schar4(lua_State* tolua_S)
{
  magic_spells* self = (magic_spells*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'schar4'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->schar4 = ((char)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: schar5 of class  magic_spells */
static int tolua_get_magic_spells_schar5(lua_State* tolua_S)
{
  magic_spells* self = (magic_spells*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'schar5'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->schar5);
 return 1;
}

/* set function: schar5 of class  magic_spells */
static int tolua_set_magic_spells_schar5(lua_State* tolua_S)
{
  magic_spells* self = (magic_spells*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'schar5'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->schar5 = ((char)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: sspeci1 of class  magic_spells */
static int tolua_get_magic_spells_sspeci1(lua_State* tolua_S)
{
  magic_spells* self = (magic_spells*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'sspeci1'",NULL);
#endif
 tolua_pushstring(tolua_S,(const char*)self->sspeci1);
 return 1;
}

/* set function: sspeci1 of class  magic_spells */
static int tolua_set_magic_spells_sspeci1(lua_State* tolua_S)
{
  magic_spells* self = (magic_spells*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'sspeci1'",NULL);
 if (!tolua_isstring(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
 strncpy(self->sspeci1,tolua_tostring(tolua_S,2,0),80-1);
 return 0;
}

/* get function: sspeci2 of class  magic_spells */
static int tolua_get_magic_spells_sspeci2(lua_State* tolua_S)
{
  magic_spells* self = (magic_spells*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'sspeci2'",NULL);
#endif
 tolua_pushstring(tolua_S,(const char*)self->sspeci2);
 return 1;
}

/* set function: sspeci2 of class  magic_spells */
static int tolua_set_magic_spells_sspeci2(lua_State* tolua_S)
{
  magic_spells* self = (magic_spells*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'sspeci2'",NULL);
 if (!tolua_isstring(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
 strncpy(self->sspeci2,tolua_tostring(tolua_S,2,0),80-1);
 return 0;
}

/* get function: sspeci3 of class  magic_spells */
static int tolua_get_magic_spells_sspeci3(lua_State* tolua_S)
{
  magic_spells* self = (magic_spells*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'sspeci3'",NULL);
#endif
 tolua_pushstring(tolua_S,(const char*)self->sspeci3);
 return 1;
}

/* set function: sspeci3 of class  magic_spells */
static int tolua_set_magic_spells_sspeci3(lua_State* tolua_S)
{
  magic_spells* self = (magic_spells*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'sspeci3'",NULL);
 if (!tolua_isstring(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
 strncpy(self->sspeci3,tolua_tostring(tolua_S,2,0),80-1);
 return 0;
}

/* get function: sspeci4 of class  magic_spells */
static int tolua_get_magic_spells_sspeci4(lua_State* tolua_S)
{
  magic_spells* self = (magic_spells*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'sspeci4'",NULL);
#endif
 tolua_pushstring(tolua_S,(const char*)self->sspeci4);
 return 1;
}

/* set function: sspeci4 of class  magic_spells */
static int tolua_set_magic_spells_sspeci4(lua_State* tolua_S)
{
  magic_spells* self = (magic_spells*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'sspeci4'",NULL);
 if (!tolua_isstring(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
 strncpy(self->sspeci4,tolua_tostring(tolua_S,2,0),80-1);
 return 0;
}

/* get function: sspeci5 of class  magic_spells */
static int tolua_get_magic_spells_sspeci5(lua_State* tolua_S)
{
  magic_spells* self = (magic_spells*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'sspeci5'",NULL);
#endif
 tolua_pushstring(tolua_S,(const char*)self->sspeci5);
 return 1;
}

/* set function: sspeci5 of class  magic_spells */
static int tolua_set_magic_spells_sspeci5(lua_State* tolua_S)
{
  magic_spells* self = (magic_spells*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'sspeci5'",NULL);
 if (!tolua_isstring(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
 strncpy(self->sspeci5,tolua_tostring(tolua_S,2,0),80-1);
 return 0;
}

/* get function: finalcost of class  magic_spells */
static int tolua_get_magic_spells_finalcost(lua_State* tolua_S)
{
  magic_spells* self = (magic_spells*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'finalcost'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->finalcost);
 return 1;
}

/* set function: finalcost of class  magic_spells */
static int tolua_set_magic_spells_finalcost(lua_State* tolua_S)
{
  magic_spells* self = (magic_spells*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'finalcost'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->finalcost = ((s32b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: created of class  magic_spells */
static int tolua_get_magic_spells_created(lua_State* tolua_S)
{
  magic_spells* self = (magic_spells*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'created'",NULL);
#endif
 tolua_pushboolean(tolua_S,(bool)self->created);
 return 1;
}

/* set function: created of class  magic_spells */
static int tolua_set_magic_spells_created(lua_State* tolua_S)
{
  magic_spells* self = (magic_spells*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'created'",NULL);
 if (!tolua_isboolean(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->created = ((bool)  tolua_toboolean(tolua_S,2,0));
 return 0;
}

/* get function: name of class  music_songs */
static int tolua_get_music_songs_name(lua_State* tolua_S)
{
  music_songs* self = (music_songs*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'name'",NULL);
#endif
 tolua_pushstring(tolua_S,(const char*)self->name);
 return 1;
}

/* set function: name of class  music_songs */
static int tolua_set_music_songs_name(lua_State* tolua_S)
{
  music_songs* self = (music_songs*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'name'",NULL);
 if (!tolua_isstring(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
 strncpy(self->name,tolua_tostring(tolua_S,2,0),80-1);
 return 0;
}

/* get function: type of class  music_songs */
static int tolua_get_music_songs_type(lua_State* tolua_S)
{
  music_songs* self = (music_songs*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'type'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->type);
 return 1;
}

/* set function: type of class  music_songs */
static int tolua_set_music_songs_type(lua_State* tolua_S)
{
  music_songs* self = (music_songs*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'type'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->type = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: power of class  music_songs */
static int tolua_get_music_songs_power(lua_State* tolua_S)
{
  music_songs* self = (music_songs*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'power'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->power);
 return 1;
}

/* set function: power of class  music_songs */
static int tolua_set_music_songs_power(lua_State* tolua_S)
{
  music_songs* self = (music_songs*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'power'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->power = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: element of class  music_songs */
static int tolua_get_music_songs_element(lua_State* tolua_S)
{
  music_songs* self = (music_songs*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'element'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->element);
 return 1;
}

/* set function: element of class  music_songs */
static int tolua_set_music_songs_element(lua_State* tolua_S)
{
  music_songs* self = (music_songs*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'element'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->element = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: radius of class  music_songs */
static int tolua_get_music_songs_radius(lua_State* tolua_S)
{
  music_songs* self = (music_songs*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'radius'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->radius);
 return 1;
}

/* set function: radius of class  music_songs */
static int tolua_set_music_songs_radius(lua_State* tolua_S)
{
  music_songs* self = (music_songs*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'radius'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->radius = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: cost of class  music_songs */
static int tolua_get_music_songs_cost(lua_State* tolua_S)
{
  music_songs* self = (music_songs*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'cost'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->cost);
 return 1;
}

/* set function: cost of class  music_songs */
static int tolua_set_music_songs_cost(lua_State* tolua_S)
{
  music_songs* self = (music_songs*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'cost'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->cost = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: created of class  music_songs */
static int tolua_get_music_songs_created(lua_State* tolua_S)
{
  music_songs* self = (music_songs*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'created'",NULL);
#endif
 tolua_pushboolean(tolua_S,(bool)self->created);
 return 1;
}

/* set function: created of class  music_songs */
static int tolua_set_music_songs_created(lua_State* tolua_S)
{
  music_songs* self = (music_songs*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'created'",NULL);
 if (!tolua_isboolean(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->created = ((bool)  tolua_toboolean(tolua_S,2,0));
 return 0;
}

/* get function: name of class  object_kind */
static int tolua_get_object_kind_name(lua_State* tolua_S)
{
  object_kind* self = (object_kind*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'name'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->name);
 return 1;
}

/* set function: name of class  object_kind */
static int tolua_set_object_kind_name(lua_State* tolua_S)
{
  object_kind* self = (object_kind*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'name'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->name = ((u32b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: text of class  object_kind */
static int tolua_get_object_kind_text(lua_State* tolua_S)
{
  object_kind* self = (object_kind*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'text'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->text);
 return 1;
}

/* set function: text of class  object_kind */
static int tolua_set_object_kind_text(lua_State* tolua_S)
{
  object_kind* self = (object_kind*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'text'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->text = ((u32b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: tval of class  object_kind */
static int tolua_get_object_kind_tval(lua_State* tolua_S)
{
  object_kind* self = (object_kind*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'tval'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->tval);
 return 1;
}

/* set function: tval of class  object_kind */
static int tolua_set_object_kind_tval(lua_State* tolua_S)
{
  object_kind* self = (object_kind*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'tval'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->tval = ((byte)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: sval of class  object_kind */
static int tolua_get_object_kind_sval(lua_State* tolua_S)
{
  object_kind* self = (object_kind*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'sval'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->sval);
 return 1;
}

/* set function: sval of class  object_kind */
static int tolua_set_object_kind_sval(lua_State* tolua_S)
{
  object_kind* self = (object_kind*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'sval'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->sval = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: pval of class  object_kind */
static int tolua_get_object_kind_pval(lua_State* tolua_S)
{
  object_kind* self = (object_kind*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'pval'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->pval);
 return 1;
}

/* set function: pval of class  object_kind */
static int tolua_set_object_kind_pval(lua_State* tolua_S)
{
  object_kind* self = (object_kind*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'pval'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->pval = ((s32b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: to_h of class  object_kind */
static int tolua_get_object_kind_to_h(lua_State* tolua_S)
{
  object_kind* self = (object_kind*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'to_h'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->to_h);
 return 1;
}

/* set function: to_h of class  object_kind */
static int tolua_set_object_kind_to_h(lua_State* tolua_S)
{
  object_kind* self = (object_kind*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'to_h'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->to_h = ((s32b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: to_d of class  object_kind */
static int tolua_get_object_kind_to_d(lua_State* tolua_S)
{
  object_kind* self = (object_kind*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'to_d'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->to_d);
 return 1;
}

/* set function: to_d of class  object_kind */
static int tolua_set_object_kind_to_d(lua_State* tolua_S)
{
  object_kind* self = (object_kind*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'to_d'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->to_d = ((s32b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: to_a of class  object_kind */
static int tolua_get_object_kind_to_a(lua_State* tolua_S)
{
  object_kind* self = (object_kind*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'to_a'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->to_a);
 return 1;
}

/* set function: to_a of class  object_kind */
static int tolua_set_object_kind_to_a(lua_State* tolua_S)
{
  object_kind* self = (object_kind*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'to_a'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->to_a = ((s32b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: ac of class  object_kind */
static int tolua_get_object_kind_ac(lua_State* tolua_S)
{
  object_kind* self = (object_kind*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'ac'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->ac);
 return 1;
}

/* set function: ac of class  object_kind */
static int tolua_set_object_kind_ac(lua_State* tolua_S)
{
  object_kind* self = (object_kind*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'ac'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->ac = ((s32b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: dd of class  object_kind */
static int tolua_get_object_kind_dd(lua_State* tolua_S)
{
  object_kind* self = (object_kind*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'dd'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->dd);
 return 1;
}

/* set function: dd of class  object_kind */
static int tolua_set_object_kind_dd(lua_State* tolua_S)
{
  object_kind* self = (object_kind*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'dd'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->dd = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: ds of class  object_kind */
static int tolua_get_object_kind_ds(lua_State* tolua_S)
{
  object_kind* self = (object_kind*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'ds'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->ds);
 return 1;
}

/* set function: ds of class  object_kind */
static int tolua_set_object_kind_ds(lua_State* tolua_S)
{
  object_kind* self = (object_kind*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'ds'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->ds = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: weight of class  object_kind */
static int tolua_get_object_kind_weight(lua_State* tolua_S)
{
  object_kind* self = (object_kind*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'weight'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->weight);
 return 1;
}

/* set function: weight of class  object_kind */
static int tolua_set_object_kind_weight(lua_State* tolua_S)
{
  object_kind* self = (object_kind*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'weight'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->weight = ((s32b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: cost of class  object_kind */
static int tolua_get_object_kind_cost(lua_State* tolua_S)
{
  object_kind* self = (object_kind*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'cost'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->cost);
 return 1;
}

/* set function: cost of class  object_kind */
static int tolua_set_object_kind_cost(lua_State* tolua_S)
{
  object_kind* self = (object_kind*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'cost'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->cost = ((s32b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: flags1 of class  object_kind */
static int tolua_get_object_kind_flags1(lua_State* tolua_S)
{
  object_kind* self = (object_kind*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'flags1'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->flags1);
 return 1;
}

/* set function: flags1 of class  object_kind */
static int tolua_set_object_kind_flags1(lua_State* tolua_S)
{
  object_kind* self = (object_kind*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'flags1'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->flags1 = ((u32b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: flags2 of class  object_kind */
static int tolua_get_object_kind_flags2(lua_State* tolua_S)
{
  object_kind* self = (object_kind*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'flags2'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->flags2);
 return 1;
}

/* set function: flags2 of class  object_kind */
static int tolua_set_object_kind_flags2(lua_State* tolua_S)
{
  object_kind* self = (object_kind*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'flags2'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->flags2 = ((u32b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: flags3 of class  object_kind */
static int tolua_get_object_kind_flags3(lua_State* tolua_S)
{
  object_kind* self = (object_kind*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'flags3'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->flags3);
 return 1;
}

/* set function: flags3 of class  object_kind */
static int tolua_set_object_kind_flags3(lua_State* tolua_S)
{
  object_kind* self = (object_kind*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'flags3'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->flags3 = ((u32b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: flags4 of class  object_kind */
static int tolua_get_object_kind_flags4(lua_State* tolua_S)
{
  object_kind* self = (object_kind*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'flags4'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->flags4);
 return 1;
}

/* set function: flags4 of class  object_kind */
static int tolua_set_object_kind_flags4(lua_State* tolua_S)
{
  object_kind* self = (object_kind*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'flags4'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->flags4 = ((u32b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: locale of class  object_kind */
static int tolua_get_types_object_kind_locale(lua_State* tolua_S)
{
 int tolua_index;
  object_kind* self;
 lua_pushstring(tolua_S,".self");
 lua_rawget(tolua_S,1);
 self = (object_kind*)  lua_touserdata(tolua_S,-1);
#ifndef TOLUA_RELEASE
 {
 tolua_Error tolua_err;
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in array indexing.",&tolua_err);
 }
#endif
 tolua_index = (int)tolua_tonumber(tolua_S,2,0)-1;
#ifndef TOLUA_RELEASE
 if (tolua_index<0 || tolua_index>=4)
 tolua_error(tolua_S,"array indexing out of range.",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->locale[tolua_index]);
 return 1;
}

/* set function: locale of class  object_kind */
static int tolua_set_types_object_kind_locale(lua_State* tolua_S)
{
 int tolua_index;
  object_kind* self;
 lua_pushstring(tolua_S,".self");
 lua_rawget(tolua_S,1);
 self = (object_kind*)  lua_touserdata(tolua_S,-1);
#ifndef TOLUA_RELEASE
 {
 tolua_Error tolua_err;
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in array indexing.",&tolua_err);
 }
#endif
 tolua_index = (int)tolua_tonumber(tolua_S,2,0)-1;
#ifndef TOLUA_RELEASE
 if (tolua_index<0 || tolua_index>=4)
 tolua_error(tolua_S,"array indexing out of range.",NULL);
#endif
  self->locale[tolua_index] = ((s16b)  tolua_tonumber(tolua_S,3,0));
 return 0;
}

/* get function: chance of class  object_kind */
static int tolua_get_types_object_kind_chance(lua_State* tolua_S)
{
 int tolua_index;
  object_kind* self;
 lua_pushstring(tolua_S,".self");
 lua_rawget(tolua_S,1);
 self = (object_kind*)  lua_touserdata(tolua_S,-1);
#ifndef TOLUA_RELEASE
 {
 tolua_Error tolua_err;
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in array indexing.",&tolua_err);
 }
#endif
 tolua_index = (int)tolua_tonumber(tolua_S,2,0)-1;
#ifndef TOLUA_RELEASE
 if (tolua_index<0 || tolua_index>=4)
 tolua_error(tolua_S,"array indexing out of range.",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->chance[tolua_index]);
 return 1;
}

/* set function: chance of class  object_kind */
static int tolua_set_types_object_kind_chance(lua_State* tolua_S)
{
 int tolua_index;
  object_kind* self;
 lua_pushstring(tolua_S,".self");
 lua_rawget(tolua_S,1);
 self = (object_kind*)  lua_touserdata(tolua_S,-1);
#ifndef TOLUA_RELEASE
 {
 tolua_Error tolua_err;
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in array indexing.",&tolua_err);
 }
#endif
 tolua_index = (int)tolua_tonumber(tolua_S,2,0)-1;
#ifndef TOLUA_RELEASE
 if (tolua_index<0 || tolua_index>=4)
 tolua_error(tolua_S,"array indexing out of range.",NULL);
#endif
  self->chance[tolua_index] = ((s16b)  tolua_tonumber(tolua_S,3,0));
 return 0;
}

/* get function: level of class  object_kind */
static int tolua_get_object_kind_level(lua_State* tolua_S)
{
  object_kind* self = (object_kind*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'level'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->level);
 return 1;
}

/* set function: level of class  object_kind */
static int tolua_set_object_kind_level(lua_State* tolua_S)
{
  object_kind* self = (object_kind*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'level'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->level = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: extra of class  object_kind */
static int tolua_get_object_kind_extra(lua_State* tolua_S)
{
  object_kind* self = (object_kind*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'extra'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->extra);
 return 1;
}

/* set function: extra of class  object_kind */
static int tolua_set_object_kind_extra(lua_State* tolua_S)
{
  object_kind* self = (object_kind*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'extra'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->extra = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: d_attr of class  object_kind */
static int tolua_get_object_kind_d_attr(lua_State* tolua_S)
{
  object_kind* self = (object_kind*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'd_attr'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->d_attr);
 return 1;
}

/* set function: d_attr of class  object_kind */
static int tolua_set_object_kind_d_attr(lua_State* tolua_S)
{
  object_kind* self = (object_kind*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'd_attr'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->d_attr = ((byte)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: d_char of class  object_kind */
static int tolua_get_object_kind_d_char(lua_State* tolua_S)
{
  object_kind* self = (object_kind*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'd_char'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->d_char);
 return 1;
}

/* set function: d_char of class  object_kind */
static int tolua_set_object_kind_d_char(lua_State* tolua_S)
{
  object_kind* self = (object_kind*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'd_char'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->d_char = ((char)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: x_attr of class  object_kind */
static int tolua_get_object_kind_x_attr(lua_State* tolua_S)
{
  object_kind* self = (object_kind*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'x_attr'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->x_attr);
 return 1;
}

/* set function: x_attr of class  object_kind */
static int tolua_set_object_kind_x_attr(lua_State* tolua_S)
{
  object_kind* self = (object_kind*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'x_attr'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->x_attr = ((byte)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: x_char of class  object_kind */
static int tolua_get_object_kind_x_char(lua_State* tolua_S)
{
  object_kind* self = (object_kind*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'x_char'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->x_char);
 return 1;
}

/* set function: x_char of class  object_kind */
static int tolua_set_object_kind_x_char(lua_State* tolua_S)
{
  object_kind* self = (object_kind*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'x_char'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->x_char = ((char)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: flavor of class  object_kind */
static int tolua_get_object_kind_flavor(lua_State* tolua_S)
{
  object_kind* self = (object_kind*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'flavor'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->flavor);
 return 1;
}

/* set function: flavor of class  object_kind */
static int tolua_set_object_kind_flavor(lua_State* tolua_S)
{
  object_kind* self = (object_kind*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'flavor'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->flavor = ((byte)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: easy_know of class  object_kind */
static int tolua_get_object_kind_easy_know(lua_State* tolua_S)
{
  object_kind* self = (object_kind*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'easy_know'",NULL);
#endif
 tolua_pushboolean(tolua_S,(bool)self->easy_know);
 return 1;
}

/* set function: easy_know of class  object_kind */
static int tolua_set_object_kind_easy_know(lua_State* tolua_S)
{
  object_kind* self = (object_kind*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'easy_know'",NULL);
 if (!tolua_isboolean(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->easy_know = ((bool)  tolua_toboolean(tolua_S,2,0));
 return 0;
}

/* get function: aware of class  object_kind */
static int tolua_get_object_kind_aware(lua_State* tolua_S)
{
  object_kind* self = (object_kind*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'aware'",NULL);
#endif
 tolua_pushboolean(tolua_S,(bool)self->aware);
 return 1;
}

/* set function: aware of class  object_kind */
static int tolua_set_object_kind_aware(lua_State* tolua_S)
{
  object_kind* self = (object_kind*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'aware'",NULL);
 if (!tolua_isboolean(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->aware = ((bool)  tolua_toboolean(tolua_S,2,0));
 return 0;
}

/* get function: tried of class  object_kind */
static int tolua_get_object_kind_tried(lua_State* tolua_S)
{
  object_kind* self = (object_kind*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'tried'",NULL);
#endif
 tolua_pushboolean(tolua_S,(bool)self->tried);
 return 1;
}

/* set function: tried of class  object_kind */
static int tolua_set_object_kind_tried(lua_State* tolua_S)
{
  object_kind* self = (object_kind*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'tried'",NULL);
 if (!tolua_isboolean(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->tried = ((bool)  tolua_toboolean(tolua_S,2,0));
 return 0;
}

/* get function: know of class  object_kind */
static int tolua_get_object_kind_know(lua_State* tolua_S)
{
  object_kind* self = (object_kind*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'know'",NULL);
#endif
 tolua_pushboolean(tolua_S,(bool)self->know);
 return 1;
}

/* set function: know of class  object_kind */
static int tolua_set_object_kind_know(lua_State* tolua_S)
{
  object_kind* self = (object_kind*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'know'",NULL);
 if (!tolua_isboolean(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->know = ((bool)  tolua_toboolean(tolua_S,2,0));
 return 0;
}

/* get function: recipe1 of class  object_kind */
static int tolua_get_object_kind_recipe1(lua_State* tolua_S)
{
  object_kind* self = (object_kind*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'recipe1'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->recipe1);
 return 1;
}

/* set function: recipe1 of class  object_kind */
static int tolua_set_object_kind_recipe1(lua_State* tolua_S)
{
  object_kind* self = (object_kind*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'recipe1'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->recipe1 = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: recipe2 of class  object_kind */
static int tolua_get_object_kind_recipe2(lua_State* tolua_S)
{
  object_kind* self = (object_kind*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'recipe2'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->recipe2);
 return 1;
}

/* set function: recipe2 of class  object_kind */
static int tolua_set_object_kind_recipe2(lua_State* tolua_S)
{
  object_kind* self = (object_kind*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'recipe2'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->recipe2 = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: brandtype of class  object_kind */
static int tolua_get_object_kind_brandtype(lua_State* tolua_S)
{
  object_kind* self = (object_kind*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'brandtype'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->brandtype);
 return 1;
}

/* set function: brandtype of class  object_kind */
static int tolua_set_object_kind_brandtype(lua_State* tolua_S)
{
  object_kind* self = (object_kind*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'brandtype'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->brandtype = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: branddam of class  object_kind */
static int tolua_get_object_kind_branddam(lua_State* tolua_S)
{
  object_kind* self = (object_kind*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'branddam'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->branddam);
 return 1;
}

/* set function: branddam of class  object_kind */
static int tolua_set_object_kind_branddam(lua_State* tolua_S)
{
  object_kind* self = (object_kind*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'branddam'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->branddam = ((s32b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: brandrad of class  object_kind */
static int tolua_get_object_kind_brandrad(lua_State* tolua_S)
{
  object_kind* self = (object_kind*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'brandrad'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->brandrad);
 return 1;
}

/* set function: brandrad of class  object_kind */
static int tolua_set_object_kind_brandrad(lua_State* tolua_S)
{
  object_kind* self = (object_kind*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'brandrad'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->brandrad = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: resistances of class  object_kind */
static int tolua_get_types_object_kind_resistances(lua_State* tolua_S)
{
 int tolua_index;
  object_kind* self;
 lua_pushstring(tolua_S,".self");
 lua_rawget(tolua_S,1);
 self = (object_kind*)  lua_touserdata(tolua_S,-1);
#ifndef TOLUA_RELEASE
 {
 tolua_Error tolua_err;
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in array indexing.",&tolua_err);
 }
#endif
 tolua_index = (int)tolua_tonumber(tolua_S,2,0)-1;
#ifndef TOLUA_RELEASE
 if (tolua_index<0 || tolua_index>=MAX_RESIST)
 tolua_error(tolua_S,"array indexing out of range.",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->resistances[tolua_index]);
 return 1;
}

/* set function: resistances of class  object_kind */
static int tolua_set_types_object_kind_resistances(lua_State* tolua_S)
{
 int tolua_index;
  object_kind* self;
 lua_pushstring(tolua_S,".self");
 lua_rawget(tolua_S,1);
 self = (object_kind*)  lua_touserdata(tolua_S,-1);
#ifndef TOLUA_RELEASE
 {
 tolua_Error tolua_err;
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in array indexing.",&tolua_err);
 }
#endif
 tolua_index = (int)tolua_tonumber(tolua_S,2,0)-1;
#ifndef TOLUA_RELEASE
 if (tolua_index<0 || tolua_index>=MAX_RESIST)
 tolua_error(tolua_S,"array indexing out of range.",NULL);
#endif
  self->resistances[tolua_index] = ((s16b)  tolua_tonumber(tolua_S,3,0));
 return 0;
}

/* get function: itemtype of class  object_kind */
static int tolua_get_object_kind_itemtype(lua_State* tolua_S)
{
  object_kind* self = (object_kind*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'itemtype'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->itemtype);
 return 1;
}

/* set function: itemtype of class  object_kind */
static int tolua_set_object_kind_itemtype(lua_State* tolua_S)
{
  object_kind* self = (object_kind*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'itemtype'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->itemtype = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: itemskill of class  object_kind */
static int tolua_get_object_kind_itemskill(lua_State* tolua_S)
{
  object_kind* self = (object_kind*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'itemskill'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->itemskill);
 return 1;
}

/* set function: itemskill of class  object_kind */
static int tolua_set_object_kind_itemskill(lua_State* tolua_S)
{
  object_kind* self = (object_kind*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'itemskill'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->itemskill = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: statsbonus of class  object_kind */
static int tolua_get_types_object_kind_statsbonus(lua_State* tolua_S)
{
 int tolua_index;
  object_kind* self;
 lua_pushstring(tolua_S,".self");
 lua_rawget(tolua_S,1);
 self = (object_kind*)  lua_touserdata(tolua_S,-1);
#ifndef TOLUA_RELEASE
 {
 tolua_Error tolua_err;
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in array indexing.",&tolua_err);
 }
#endif
 tolua_index = (int)tolua_tonumber(tolua_S,2,0)-1;
#ifndef TOLUA_RELEASE
 if (tolua_index<0 || tolua_index>=6)
 tolua_error(tolua_S,"array indexing out of range.",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->statsbonus[tolua_index]);
 return 1;
}

/* set function: statsbonus of class  object_kind */
static int tolua_set_types_object_kind_statsbonus(lua_State* tolua_S)
{
 int tolua_index;
  object_kind* self;
 lua_pushstring(tolua_S,".self");
 lua_rawget(tolua_S,1);
 self = (object_kind*)  lua_touserdata(tolua_S,-1);
#ifndef TOLUA_RELEASE
 {
 tolua_Error tolua_err;
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in array indexing.",&tolua_err);
 }
#endif
 tolua_index = (int)tolua_tonumber(tolua_S,2,0)-1;
#ifndef TOLUA_RELEASE
 if (tolua_index<0 || tolua_index>=6)
 tolua_error(tolua_S,"array indexing out of range.",NULL);
#endif
  self->statsbonus[tolua_index] = ((s16b)  tolua_tonumber(tolua_S,3,0));
 return 0;
}

/* get function: skillsbonus of class  object_kind */
static int tolua_get_types_object_kind_skillsbonus(lua_State* tolua_S)
{
 int tolua_index;
  object_kind* self;
 lua_pushstring(tolua_S,".self");
 lua_rawget(tolua_S,1);
 self = (object_kind*)  lua_touserdata(tolua_S,-1);
#ifndef TOLUA_RELEASE
 {
 tolua_Error tolua_err;
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in array indexing.",&tolua_err);
 }
#endif
 tolua_index = (int)tolua_tonumber(tolua_S,2,0)-1;
#ifndef TOLUA_RELEASE
 if (tolua_index<0 || tolua_index>=SKILL_MAX)
 tolua_error(tolua_S,"array indexing out of range.",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->skillsbonus[tolua_index]);
 return 1;
}

/* set function: skillsbonus of class  object_kind */
static int tolua_set_types_object_kind_skillsbonus(lua_State* tolua_S)
{
 int tolua_index;
  object_kind* self;
 lua_pushstring(tolua_S,".self");
 lua_rawget(tolua_S,1);
 self = (object_kind*)  lua_touserdata(tolua_S,-1);
#ifndef TOLUA_RELEASE
 {
 tolua_Error tolua_err;
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in array indexing.",&tolua_err);
 }
#endif
 tolua_index = (int)tolua_tonumber(tolua_S,2,0)-1;
#ifndef TOLUA_RELEASE
 if (tolua_index<0 || tolua_index>=SKILL_MAX)
 tolua_error(tolua_S,"array indexing out of range.",NULL);
#endif
  self->skillsbonus[tolua_index] = ((s16b)  tolua_tonumber(tolua_S,3,0));
 return 0;
}

/* get function: extrablows of class  object_kind */
static int tolua_get_object_kind_extrablows(lua_State* tolua_S)
{
  object_kind* self = (object_kind*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'extrablows'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->extrablows);
 return 1;
}

/* set function: extrablows of class  object_kind */
static int tolua_set_object_kind_extrablows(lua_State* tolua_S)
{
  object_kind* self = (object_kind*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'extrablows'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->extrablows = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: extrashots of class  object_kind */
static int tolua_get_object_kind_extrashots(lua_State* tolua_S)
{
  object_kind* self = (object_kind*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'extrashots'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->extrashots);
 return 1;
}

/* set function: extrashots of class  object_kind */
static int tolua_set_object_kind_extrashots(lua_State* tolua_S)
{
  object_kind* self = (object_kind*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'extrashots'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->extrashots = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: speedbonus of class  object_kind */
static int tolua_get_object_kind_speedbonus(lua_State* tolua_S)
{
  object_kind* self = (object_kind*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'speedbonus'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->speedbonus);
 return 1;
}

/* set function: speedbonus of class  object_kind */
static int tolua_set_object_kind_speedbonus(lua_State* tolua_S)
{
  object_kind* self = (object_kind*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'speedbonus'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->speedbonus = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: lifebonus of class  object_kind */
static int tolua_get_object_kind_lifebonus(lua_State* tolua_S)
{
  object_kind* self = (object_kind*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'lifebonus'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->lifebonus);
 return 1;
}

/* set function: lifebonus of class  object_kind */
static int tolua_set_object_kind_lifebonus(lua_State* tolua_S)
{
  object_kind* self = (object_kind*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'lifebonus'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->lifebonus = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: manabonus of class  object_kind */
static int tolua_get_object_kind_manabonus(lua_State* tolua_S)
{
  object_kind* self = (object_kind*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'manabonus'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->manabonus);
 return 1;
}

/* set function: manabonus of class  object_kind */
static int tolua_set_object_kind_manabonus(lua_State* tolua_S)
{
  object_kind* self = (object_kind*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'manabonus'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->manabonus = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: infravision of class  object_kind */
static int tolua_get_object_kind_infravision(lua_State* tolua_S)
{
  object_kind* self = (object_kind*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'infravision'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->infravision);
 return 1;
}

/* set function: infravision of class  object_kind */
static int tolua_set_object_kind_infravision(lua_State* tolua_S)
{
  object_kind* self = (object_kind*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'infravision'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->infravision = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: spellbonus of class  object_kind */
static int tolua_get_object_kind_spellbonus(lua_State* tolua_S)
{
  object_kind* self = (object_kind*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'spellbonus'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->spellbonus);
 return 1;
}

/* set function: spellbonus of class  object_kind */
static int tolua_set_object_kind_spellbonus(lua_State* tolua_S)
{
  object_kind* self = (object_kind*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'spellbonus'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->spellbonus = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: invisibility of class  object_kind */
static int tolua_get_object_kind_invisibility(lua_State* tolua_S)
{
  object_kind* self = (object_kind*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'invisibility'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->invisibility);
 return 1;
}

/* set function: invisibility of class  object_kind */
static int tolua_set_object_kind_invisibility(lua_State* tolua_S)
{
  object_kind* self = (object_kind*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'invisibility'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->invisibility = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: light of class  object_kind */
static int tolua_get_object_kind_light(lua_State* tolua_S)
{
  object_kind* self = (object_kind*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'light'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->light);
 return 1;
}

/* set function: light of class  object_kind */
static int tolua_set_object_kind_light(lua_State* tolua_S)
{
  object_kind* self = (object_kind*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'light'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->light = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: extra1 of class  object_kind */
static int tolua_get_object_kind_extra1(lua_State* tolua_S)
{
  object_kind* self = (object_kind*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'extra1'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->extra1);
 return 1;
}

/* set function: extra1 of class  object_kind */
static int tolua_set_object_kind_extra1(lua_State* tolua_S)
{
  object_kind* self = (object_kind*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'extra1'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->extra1 = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: extra2 of class  object_kind */
static int tolua_get_object_kind_extra2(lua_State* tolua_S)
{
  object_kind* self = (object_kind*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'extra2'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->extra2);
 return 1;
}

/* set function: extra2 of class  object_kind */
static int tolua_set_object_kind_extra2(lua_State* tolua_S)
{
  object_kind* self = (object_kind*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'extra2'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->extra2 = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: extra3 of class  object_kind */
static int tolua_get_object_kind_extra3(lua_State* tolua_S)
{
  object_kind* self = (object_kind*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'extra3'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->extra3);
 return 1;
}

/* set function: extra3 of class  object_kind */
static int tolua_set_object_kind_extra3(lua_State* tolua_S)
{
  object_kind* self = (object_kind*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'extra3'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->extra3 = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: extra4 of class  object_kind */
static int tolua_get_object_kind_extra4(lua_State* tolua_S)
{
  object_kind* self = (object_kind*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'extra4'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->extra4);
 return 1;
}

/* set function: extra4 of class  object_kind */
static int tolua_set_object_kind_extra4(lua_State* tolua_S)
{
  object_kind* self = (object_kind*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'extra4'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->extra4 = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: extra5 of class  object_kind */
static int tolua_get_object_kind_extra5(lua_State* tolua_S)
{
  object_kind* self = (object_kind*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'extra5'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->extra5);
 return 1;
}

/* set function: extra5 of class  object_kind */
static int tolua_set_object_kind_extra5(lua_State* tolua_S)
{
  object_kind* self = (object_kind*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'extra5'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->extra5 = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: reflect of class  object_kind */
static int tolua_get_object_kind_reflect(lua_State* tolua_S)
{
  object_kind* self = (object_kind*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'reflect'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->reflect);
 return 1;
}

/* set function: reflect of class  object_kind */
static int tolua_set_object_kind_reflect(lua_State* tolua_S)
{
  object_kind* self = (object_kind*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'reflect'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->reflect = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: spell of class  object_kind */
static int tolua_get_types_object_kind_spell(lua_State* tolua_S)
{
 int tolua_index;
  object_kind* self;
 lua_pushstring(tolua_S,".self");
 lua_rawget(tolua_S,1);
 self = (object_kind*)  lua_touserdata(tolua_S,-1);
#ifndef TOLUA_RELEASE
 {
 tolua_Error tolua_err;
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in array indexing.",&tolua_err);
 }
#endif
 tolua_index = (int)tolua_tonumber(tolua_S,2,0)-1;
#ifndef TOLUA_RELEASE
 if (tolua_index<0 || tolua_index>=20)
 tolua_error(tolua_S,"array indexing out of range.",NULL);
#endif
 tolua_pushusertype(tolua_S,(void*)&self->spell[tolua_index],"monster_spell");
 return 1;
}

/* set function: spell of class  object_kind */
static int tolua_set_types_object_kind_spell(lua_State* tolua_S)
{
 int tolua_index;
  object_kind* self;
 lua_pushstring(tolua_S,".self");
 lua_rawget(tolua_S,1);
 self = (object_kind*)  lua_touserdata(tolua_S,-1);
#ifndef TOLUA_RELEASE
 {
 tolua_Error tolua_err;
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in array indexing.",&tolua_err);
 }
#endif
 tolua_index = (int)tolua_tonumber(tolua_S,2,0)-1;
#ifndef TOLUA_RELEASE
 if (tolua_index<0 || tolua_index>=20)
 tolua_error(tolua_S,"array indexing out of range.",NULL);
#endif
  self->spell[tolua_index] = *((monster_spell*)  tolua_tousertype(tolua_S,3,0));
 return 0;
}

/* get function: name of class  artifact_type */
static int tolua_get_artifact_type_name(lua_State* tolua_S)
{
  artifact_type* self = (artifact_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'name'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->name);
 return 1;
}

/* set function: name of class  artifact_type */
static int tolua_set_artifact_type_name(lua_State* tolua_S)
{
  artifact_type* self = (artifact_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'name'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->name = ((u32b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: text of class  artifact_type */
static int tolua_get_artifact_type_text(lua_State* tolua_S)
{
  artifact_type* self = (artifact_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'text'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->text);
 return 1;
}

/* set function: text of class  artifact_type */
static int tolua_set_artifact_type_text(lua_State* tolua_S)
{
  artifact_type* self = (artifact_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'text'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->text = ((u32b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: tval of class  artifact_type */
static int tolua_get_artifact_type_tval(lua_State* tolua_S)
{
  artifact_type* self = (artifact_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'tval'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->tval);
 return 1;
}

/* set function: tval of class  artifact_type */
static int tolua_set_artifact_type_tval(lua_State* tolua_S)
{
  artifact_type* self = (artifact_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'tval'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->tval = ((byte)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: sval of class  artifact_type */
static int tolua_get_artifact_type_sval(lua_State* tolua_S)
{
  artifact_type* self = (artifact_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'sval'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->sval);
 return 1;
}

/* set function: sval of class  artifact_type */
static int tolua_set_artifact_type_sval(lua_State* tolua_S)
{
  artifact_type* self = (artifact_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'sval'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->sval = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: pval of class  artifact_type */
static int tolua_get_artifact_type_pval(lua_State* tolua_S)
{
  artifact_type* self = (artifact_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'pval'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->pval);
 return 1;
}

/* set function: pval of class  artifact_type */
static int tolua_set_artifact_type_pval(lua_State* tolua_S)
{
  artifact_type* self = (artifact_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'pval'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->pval = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: to_h of class  artifact_type */
static int tolua_get_artifact_type_to_h(lua_State* tolua_S)
{
  artifact_type* self = (artifact_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'to_h'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->to_h);
 return 1;
}

/* set function: to_h of class  artifact_type */
static int tolua_set_artifact_type_to_h(lua_State* tolua_S)
{
  artifact_type* self = (artifact_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'to_h'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->to_h = ((s32b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: to_d of class  artifact_type */
static int tolua_get_artifact_type_to_d(lua_State* tolua_S)
{
  artifact_type* self = (artifact_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'to_d'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->to_d);
 return 1;
}

/* set function: to_d of class  artifact_type */
static int tolua_set_artifact_type_to_d(lua_State* tolua_S)
{
  artifact_type* self = (artifact_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'to_d'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->to_d = ((s32b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: to_a of class  artifact_type */
static int tolua_get_artifact_type_to_a(lua_State* tolua_S)
{
  artifact_type* self = (artifact_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'to_a'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->to_a);
 return 1;
}

/* set function: to_a of class  artifact_type */
static int tolua_set_artifact_type_to_a(lua_State* tolua_S)
{
  artifact_type* self = (artifact_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'to_a'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->to_a = ((s32b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: ac of class  artifact_type */
static int tolua_get_artifact_type_ac(lua_State* tolua_S)
{
  artifact_type* self = (artifact_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'ac'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->ac);
 return 1;
}

/* set function: ac of class  artifact_type */
static int tolua_set_artifact_type_ac(lua_State* tolua_S)
{
  artifact_type* self = (artifact_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'ac'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->ac = ((s32b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: dd of class  artifact_type */
static int tolua_get_artifact_type_dd(lua_State* tolua_S)
{
  artifact_type* self = (artifact_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'dd'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->dd);
 return 1;
}

/* set function: dd of class  artifact_type */
static int tolua_set_artifact_type_dd(lua_State* tolua_S)
{
  artifact_type* self = (artifact_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'dd'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->dd = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: ds of class  artifact_type */
static int tolua_get_artifact_type_ds(lua_State* tolua_S)
{
  artifact_type* self = (artifact_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'ds'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->ds);
 return 1;
}

/* set function: ds of class  artifact_type */
static int tolua_set_artifact_type_ds(lua_State* tolua_S)
{
  artifact_type* self = (artifact_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'ds'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->ds = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: weight of class  artifact_type */
static int tolua_get_artifact_type_weight(lua_State* tolua_S)
{
  artifact_type* self = (artifact_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'weight'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->weight);
 return 1;
}

/* set function: weight of class  artifact_type */
static int tolua_set_artifact_type_weight(lua_State* tolua_S)
{
  artifact_type* self = (artifact_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'weight'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->weight = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: cost of class  artifact_type */
static int tolua_get_artifact_type_cost(lua_State* tolua_S)
{
  artifact_type* self = (artifact_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'cost'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->cost);
 return 1;
}

/* set function: cost of class  artifact_type */
static int tolua_set_artifact_type_cost(lua_State* tolua_S)
{
  artifact_type* self = (artifact_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'cost'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->cost = ((s32b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: flags1 of class  artifact_type */
static int tolua_get_artifact_type_flags1(lua_State* tolua_S)
{
  artifact_type* self = (artifact_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'flags1'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->flags1);
 return 1;
}

/* set function: flags1 of class  artifact_type */
static int tolua_set_artifact_type_flags1(lua_State* tolua_S)
{
  artifact_type* self = (artifact_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'flags1'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->flags1 = ((u32b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: flags2 of class  artifact_type */
static int tolua_get_artifact_type_flags2(lua_State* tolua_S)
{
  artifact_type* self = (artifact_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'flags2'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->flags2);
 return 1;
}

/* set function: flags2 of class  artifact_type */
static int tolua_set_artifact_type_flags2(lua_State* tolua_S)
{
  artifact_type* self = (artifact_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'flags2'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->flags2 = ((u32b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: flags3 of class  artifact_type */
static int tolua_get_artifact_type_flags3(lua_State* tolua_S)
{
  artifact_type* self = (artifact_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'flags3'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->flags3);
 return 1;
}

/* set function: flags3 of class  artifact_type */
static int tolua_set_artifact_type_flags3(lua_State* tolua_S)
{
  artifact_type* self = (artifact_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'flags3'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->flags3 = ((u32b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: flags4 of class  artifact_type */
static int tolua_get_artifact_type_flags4(lua_State* tolua_S)
{
  artifact_type* self = (artifact_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'flags4'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->flags4);
 return 1;
}

/* set function: flags4 of class  artifact_type */
static int tolua_set_artifact_type_flags4(lua_State* tolua_S)
{
  artifact_type* self = (artifact_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'flags4'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->flags4 = ((u32b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: level of class  artifact_type */
static int tolua_get_artifact_type_level(lua_State* tolua_S)
{
  artifact_type* self = (artifact_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'level'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->level);
 return 1;
}

/* set function: level of class  artifact_type */
static int tolua_set_artifact_type_level(lua_State* tolua_S)
{
  artifact_type* self = (artifact_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'level'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->level = ((byte)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: rarity of class  artifact_type */
static int tolua_get_artifact_type_rarity(lua_State* tolua_S)
{
  artifact_type* self = (artifact_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'rarity'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->rarity);
 return 1;
}

/* set function: rarity of class  artifact_type */
static int tolua_set_artifact_type_rarity(lua_State* tolua_S)
{
  artifact_type* self = (artifact_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'rarity'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->rarity = ((byte)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: cur_num of class  artifact_type */
static int tolua_get_artifact_type_cur_num(lua_State* tolua_S)
{
  artifact_type* self = (artifact_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'cur_num'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->cur_num);
 return 1;
}

/* set function: cur_num of class  artifact_type */
static int tolua_set_artifact_type_cur_num(lua_State* tolua_S)
{
  artifact_type* self = (artifact_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'cur_num'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->cur_num = ((byte)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: max_num of class  artifact_type */
static int tolua_get_artifact_type_max_num(lua_State* tolua_S)
{
  artifact_type* self = (artifact_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'max_num'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->max_num);
 return 1;
}

/* set function: max_num of class  artifact_type */
static int tolua_set_artifact_type_max_num(lua_State* tolua_S)
{
  artifact_type* self = (artifact_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'max_num'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->max_num = ((byte)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: brandtype of class  artifact_type */
static int tolua_get_artifact_type_brandtype(lua_State* tolua_S)
{
  artifact_type* self = (artifact_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'brandtype'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->brandtype);
 return 1;
}

/* set function: brandtype of class  artifact_type */
static int tolua_set_artifact_type_brandtype(lua_State* tolua_S)
{
  artifact_type* self = (artifact_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'brandtype'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->brandtype = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: branddam of class  artifact_type */
static int tolua_get_artifact_type_branddam(lua_State* tolua_S)
{
  artifact_type* self = (artifact_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'branddam'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->branddam);
 return 1;
}

/* set function: branddam of class  artifact_type */
static int tolua_set_artifact_type_branddam(lua_State* tolua_S)
{
  artifact_type* self = (artifact_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'branddam'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->branddam = ((s32b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: brandrad of class  artifact_type */
static int tolua_get_artifact_type_brandrad(lua_State* tolua_S)
{
  artifact_type* self = (artifact_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'brandrad'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->brandrad);
 return 1;
}

/* set function: brandrad of class  artifact_type */
static int tolua_set_artifact_type_brandrad(lua_State* tolua_S)
{
  artifact_type* self = (artifact_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'brandrad'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->brandrad = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: resistances of class  artifact_type */
static int tolua_get_types_artifact_type_resistances(lua_State* tolua_S)
{
 int tolua_index;
  artifact_type* self;
 lua_pushstring(tolua_S,".self");
 lua_rawget(tolua_S,1);
 self = (artifact_type*)  lua_touserdata(tolua_S,-1);
#ifndef TOLUA_RELEASE
 {
 tolua_Error tolua_err;
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in array indexing.",&tolua_err);
 }
#endif
 tolua_index = (int)tolua_tonumber(tolua_S,2,0)-1;
#ifndef TOLUA_RELEASE
 if (tolua_index<0 || tolua_index>=MAX_RESIST)
 tolua_error(tolua_S,"array indexing out of range.",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->resistances[tolua_index]);
 return 1;
}

/* set function: resistances of class  artifact_type */
static int tolua_set_types_artifact_type_resistances(lua_State* tolua_S)
{
 int tolua_index;
  artifact_type* self;
 lua_pushstring(tolua_S,".self");
 lua_rawget(tolua_S,1);
 self = (artifact_type*)  lua_touserdata(tolua_S,-1);
#ifndef TOLUA_RELEASE
 {
 tolua_Error tolua_err;
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in array indexing.",&tolua_err);
 }
#endif
 tolua_index = (int)tolua_tonumber(tolua_S,2,0)-1;
#ifndef TOLUA_RELEASE
 if (tolua_index<0 || tolua_index>=MAX_RESIST)
 tolua_error(tolua_S,"array indexing out of range.",NULL);
#endif
  self->resistances[tolua_index] = ((s16b)  tolua_tonumber(tolua_S,3,0));
 return 0;
}

/* get function: itemtype of class  artifact_type */
static int tolua_get_artifact_type_itemtype(lua_State* tolua_S)
{
  artifact_type* self = (artifact_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'itemtype'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->itemtype);
 return 1;
}

/* set function: itemtype of class  artifact_type */
static int tolua_set_artifact_type_itemtype(lua_State* tolua_S)
{
  artifact_type* self = (artifact_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'itemtype'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->itemtype = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: itemskill of class  artifact_type */
static int tolua_get_artifact_type_itemskill(lua_State* tolua_S)
{
  artifact_type* self = (artifact_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'itemskill'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->itemskill);
 return 1;
}

/* set function: itemskill of class  artifact_type */
static int tolua_set_artifact_type_itemskill(lua_State* tolua_S)
{
  artifact_type* self = (artifact_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'itemskill'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->itemskill = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: statsbonus of class  artifact_type */
static int tolua_get_types_artifact_type_statsbonus(lua_State* tolua_S)
{
 int tolua_index;
  artifact_type* self;
 lua_pushstring(tolua_S,".self");
 lua_rawget(tolua_S,1);
 self = (artifact_type*)  lua_touserdata(tolua_S,-1);
#ifndef TOLUA_RELEASE
 {
 tolua_Error tolua_err;
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in array indexing.",&tolua_err);
 }
#endif
 tolua_index = (int)tolua_tonumber(tolua_S,2,0)-1;
#ifndef TOLUA_RELEASE
 if (tolua_index<0 || tolua_index>=6)
 tolua_error(tolua_S,"array indexing out of range.",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->statsbonus[tolua_index]);
 return 1;
}

/* set function: statsbonus of class  artifact_type */
static int tolua_set_types_artifact_type_statsbonus(lua_State* tolua_S)
{
 int tolua_index;
  artifact_type* self;
 lua_pushstring(tolua_S,".self");
 lua_rawget(tolua_S,1);
 self = (artifact_type*)  lua_touserdata(tolua_S,-1);
#ifndef TOLUA_RELEASE
 {
 tolua_Error tolua_err;
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in array indexing.",&tolua_err);
 }
#endif
 tolua_index = (int)tolua_tonumber(tolua_S,2,0)-1;
#ifndef TOLUA_RELEASE
 if (tolua_index<0 || tolua_index>=6)
 tolua_error(tolua_S,"array indexing out of range.",NULL);
#endif
  self->statsbonus[tolua_index] = ((s16b)  tolua_tonumber(tolua_S,3,0));
 return 0;
}

/* get function: skillsbonus of class  artifact_type */
static int tolua_get_types_artifact_type_skillsbonus(lua_State* tolua_S)
{
 int tolua_index;
  artifact_type* self;
 lua_pushstring(tolua_S,".self");
 lua_rawget(tolua_S,1);
 self = (artifact_type*)  lua_touserdata(tolua_S,-1);
#ifndef TOLUA_RELEASE
 {
 tolua_Error tolua_err;
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in array indexing.",&tolua_err);
 }
#endif
 tolua_index = (int)tolua_tonumber(tolua_S,2,0)-1;
#ifndef TOLUA_RELEASE
 if (tolua_index<0 || tolua_index>=SKILL_MAX)
 tolua_error(tolua_S,"array indexing out of range.",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->skillsbonus[tolua_index]);
 return 1;
}

/* set function: skillsbonus of class  artifact_type */
static int tolua_set_types_artifact_type_skillsbonus(lua_State* tolua_S)
{
 int tolua_index;
  artifact_type* self;
 lua_pushstring(tolua_S,".self");
 lua_rawget(tolua_S,1);
 self = (artifact_type*)  lua_touserdata(tolua_S,-1);
#ifndef TOLUA_RELEASE
 {
 tolua_Error tolua_err;
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in array indexing.",&tolua_err);
 }
#endif
 tolua_index = (int)tolua_tonumber(tolua_S,2,0)-1;
#ifndef TOLUA_RELEASE
 if (tolua_index<0 || tolua_index>=SKILL_MAX)
 tolua_error(tolua_S,"array indexing out of range.",NULL);
#endif
  self->skillsbonus[tolua_index] = ((s16b)  tolua_tonumber(tolua_S,3,0));
 return 0;
}

/* get function: extrablows of class  artifact_type */
static int tolua_get_artifact_type_extrablows(lua_State* tolua_S)
{
  artifact_type* self = (artifact_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'extrablows'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->extrablows);
 return 1;
}

/* set function: extrablows of class  artifact_type */
static int tolua_set_artifact_type_extrablows(lua_State* tolua_S)
{
  artifact_type* self = (artifact_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'extrablows'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->extrablows = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: extrashots of class  artifact_type */
static int tolua_get_artifact_type_extrashots(lua_State* tolua_S)
{
  artifact_type* self = (artifact_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'extrashots'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->extrashots);
 return 1;
}

/* set function: extrashots of class  artifact_type */
static int tolua_set_artifact_type_extrashots(lua_State* tolua_S)
{
  artifact_type* self = (artifact_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'extrashots'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->extrashots = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: speedbonus of class  artifact_type */
static int tolua_get_artifact_type_speedbonus(lua_State* tolua_S)
{
  artifact_type* self = (artifact_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'speedbonus'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->speedbonus);
 return 1;
}

/* set function: speedbonus of class  artifact_type */
static int tolua_set_artifact_type_speedbonus(lua_State* tolua_S)
{
  artifact_type* self = (artifact_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'speedbonus'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->speedbonus = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: lifebonus of class  artifact_type */
static int tolua_get_artifact_type_lifebonus(lua_State* tolua_S)
{
  artifact_type* self = (artifact_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'lifebonus'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->lifebonus);
 return 1;
}

/* set function: lifebonus of class  artifact_type */
static int tolua_set_artifact_type_lifebonus(lua_State* tolua_S)
{
  artifact_type* self = (artifact_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'lifebonus'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->lifebonus = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: manabonus of class  artifact_type */
static int tolua_get_artifact_type_manabonus(lua_State* tolua_S)
{
  artifact_type* self = (artifact_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'manabonus'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->manabonus);
 return 1;
}

/* set function: manabonus of class  artifact_type */
static int tolua_set_artifact_type_manabonus(lua_State* tolua_S)
{
  artifact_type* self = (artifact_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'manabonus'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->manabonus = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: infravision of class  artifact_type */
static int tolua_get_artifact_type_infravision(lua_State* tolua_S)
{
  artifact_type* self = (artifact_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'infravision'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->infravision);
 return 1;
}

/* set function: infravision of class  artifact_type */
static int tolua_set_artifact_type_infravision(lua_State* tolua_S)
{
  artifact_type* self = (artifact_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'infravision'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->infravision = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: spellbonus of class  artifact_type */
static int tolua_get_artifact_type_spellbonus(lua_State* tolua_S)
{
  artifact_type* self = (artifact_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'spellbonus'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->spellbonus);
 return 1;
}

/* set function: spellbonus of class  artifact_type */
static int tolua_set_artifact_type_spellbonus(lua_State* tolua_S)
{
  artifact_type* self = (artifact_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'spellbonus'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->spellbonus = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: invisibility of class  artifact_type */
static int tolua_get_artifact_type_invisibility(lua_State* tolua_S)
{
  artifact_type* self = (artifact_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'invisibility'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->invisibility);
 return 1;
}

/* set function: invisibility of class  artifact_type */
static int tolua_set_artifact_type_invisibility(lua_State* tolua_S)
{
  artifact_type* self = (artifact_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'invisibility'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->invisibility = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: light of class  artifact_type */
static int tolua_get_artifact_type_light(lua_State* tolua_S)
{
  artifact_type* self = (artifact_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'light'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->light);
 return 1;
}

/* set function: light of class  artifact_type */
static int tolua_set_artifact_type_light(lua_State* tolua_S)
{
  artifact_type* self = (artifact_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'light'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->light = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: extra1 of class  artifact_type */
static int tolua_get_artifact_type_extra1(lua_State* tolua_S)
{
  artifact_type* self = (artifact_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'extra1'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->extra1);
 return 1;
}

/* set function: extra1 of class  artifact_type */
static int tolua_set_artifact_type_extra1(lua_State* tolua_S)
{
  artifact_type* self = (artifact_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'extra1'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->extra1 = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: extra2 of class  artifact_type */
static int tolua_get_artifact_type_extra2(lua_State* tolua_S)
{
  artifact_type* self = (artifact_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'extra2'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->extra2);
 return 1;
}

/* set function: extra2 of class  artifact_type */
static int tolua_set_artifact_type_extra2(lua_State* tolua_S)
{
  artifact_type* self = (artifact_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'extra2'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->extra2 = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: extra3 of class  artifact_type */
static int tolua_get_artifact_type_extra3(lua_State* tolua_S)
{
  artifact_type* self = (artifact_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'extra3'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->extra3);
 return 1;
}

/* set function: extra3 of class  artifact_type */
static int tolua_set_artifact_type_extra3(lua_State* tolua_S)
{
  artifact_type* self = (artifact_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'extra3'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->extra3 = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: extra4 of class  artifact_type */
static int tolua_get_artifact_type_extra4(lua_State* tolua_S)
{
  artifact_type* self = (artifact_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'extra4'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->extra4);
 return 1;
}

/* set function: extra4 of class  artifact_type */
static int tolua_set_artifact_type_extra4(lua_State* tolua_S)
{
  artifact_type* self = (artifact_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'extra4'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->extra4 = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: extra5 of class  artifact_type */
static int tolua_get_artifact_type_extra5(lua_State* tolua_S)
{
  artifact_type* self = (artifact_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'extra5'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->extra5);
 return 1;
}

/* set function: extra5 of class  artifact_type */
static int tolua_set_artifact_type_extra5(lua_State* tolua_S)
{
  artifact_type* self = (artifact_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'extra5'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->extra5 = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: reflect of class  artifact_type */
static int tolua_get_artifact_type_reflect(lua_State* tolua_S)
{
  artifact_type* self = (artifact_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'reflect'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->reflect);
 return 1;
}

/* set function: reflect of class  artifact_type */
static int tolua_set_artifact_type_reflect(lua_State* tolua_S)
{
  artifact_type* self = (artifact_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'reflect'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->reflect = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: spell of class  artifact_type */
static int tolua_get_types_artifact_type_spell(lua_State* tolua_S)
{
 int tolua_index;
  artifact_type* self;
 lua_pushstring(tolua_S,".self");
 lua_rawget(tolua_S,1);
 self = (artifact_type*)  lua_touserdata(tolua_S,-1);
#ifndef TOLUA_RELEASE
 {
 tolua_Error tolua_err;
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in array indexing.",&tolua_err);
 }
#endif
 tolua_index = (int)tolua_tonumber(tolua_S,2,0)-1;
#ifndef TOLUA_RELEASE
 if (tolua_index<0 || tolua_index>=20)
 tolua_error(tolua_S,"array indexing out of range.",NULL);
#endif
 tolua_pushusertype(tolua_S,(void*)&self->spell[tolua_index],"monster_spell");
 return 1;
}

/* set function: spell of class  artifact_type */
static int tolua_set_types_artifact_type_spell(lua_State* tolua_S)
{
 int tolua_index;
  artifact_type* self;
 lua_pushstring(tolua_S,".self");
 lua_rawget(tolua_S,1);
 self = (artifact_type*)  lua_touserdata(tolua_S,-1);
#ifndef TOLUA_RELEASE
 {
 tolua_Error tolua_err;
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in array indexing.",&tolua_err);
 }
#endif
 tolua_index = (int)tolua_tonumber(tolua_S,2,0)-1;
#ifndef TOLUA_RELEASE
 if (tolua_index<0 || tolua_index>=20)
 tolua_error(tolua_S,"array indexing out of range.",NULL);
#endif
  self->spell[tolua_index] = *((monster_spell*)  tolua_tousertype(tolua_S,3,0));
 return 0;
}

/* get function: name of class  ego_item_type */
static int tolua_get_ego_item_type_name(lua_State* tolua_S)
{
  ego_item_type* self = (ego_item_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'name'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->name);
 return 1;
}

/* set function: name of class  ego_item_type */
static int tolua_set_ego_item_type_name(lua_State* tolua_S)
{
  ego_item_type* self = (ego_item_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'name'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->name = ((u32b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: text of class  ego_item_type */
static int tolua_get_ego_item_type_text(lua_State* tolua_S)
{
  ego_item_type* self = (ego_item_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'text'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->text);
 return 1;
}

/* set function: text of class  ego_item_type */
static int tolua_set_ego_item_type_text(lua_State* tolua_S)
{
  ego_item_type* self = (ego_item_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'text'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->text = ((u32b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: slot of class  ego_item_type */
static int tolua_get_ego_item_type_slot(lua_State* tolua_S)
{
  ego_item_type* self = (ego_item_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'slot'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->slot);
 return 1;
}

/* set function: slot of class  ego_item_type */
static int tolua_set_ego_item_type_slot(lua_State* tolua_S)
{
  ego_item_type* self = (ego_item_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'slot'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->slot = ((byte)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: rating of class  ego_item_type */
static int tolua_get_ego_item_type_rating(lua_State* tolua_S)
{
  ego_item_type* self = (ego_item_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'rating'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->rating);
 return 1;
}

/* set function: rating of class  ego_item_type */
static int tolua_set_ego_item_type_rating(lua_State* tolua_S)
{
  ego_item_type* self = (ego_item_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'rating'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->rating = ((byte)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: level of class  ego_item_type */
static int tolua_get_ego_item_type_level(lua_State* tolua_S)
{
  ego_item_type* self = (ego_item_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'level'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->level);
 return 1;
}

/* set function: level of class  ego_item_type */
static int tolua_set_ego_item_type_level(lua_State* tolua_S)
{
  ego_item_type* self = (ego_item_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'level'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->level = ((byte)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: rarity of class  ego_item_type */
static int tolua_get_ego_item_type_rarity(lua_State* tolua_S)
{
  ego_item_type* self = (ego_item_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'rarity'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->rarity);
 return 1;
}

/* set function: rarity of class  ego_item_type */
static int tolua_set_ego_item_type_rarity(lua_State* tolua_S)
{
  ego_item_type* self = (ego_item_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'rarity'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->rarity = ((byte)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: max_to_h of class  ego_item_type */
static int tolua_get_ego_item_type_max_to_h(lua_State* tolua_S)
{
  ego_item_type* self = (ego_item_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'max_to_h'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->max_to_h);
 return 1;
}

/* set function: max_to_h of class  ego_item_type */
static int tolua_set_ego_item_type_max_to_h(lua_State* tolua_S)
{
  ego_item_type* self = (ego_item_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'max_to_h'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->max_to_h = ((byte)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: max_to_d of class  ego_item_type */
static int tolua_get_ego_item_type_max_to_d(lua_State* tolua_S)
{
  ego_item_type* self = (ego_item_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'max_to_d'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->max_to_d);
 return 1;
}

/* set function: max_to_d of class  ego_item_type */
static int tolua_set_ego_item_type_max_to_d(lua_State* tolua_S)
{
  ego_item_type* self = (ego_item_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'max_to_d'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->max_to_d = ((byte)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: max_to_a of class  ego_item_type */
static int tolua_get_ego_item_type_max_to_a(lua_State* tolua_S)
{
  ego_item_type* self = (ego_item_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'max_to_a'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->max_to_a);
 return 1;
}

/* set function: max_to_a of class  ego_item_type */
static int tolua_set_ego_item_type_max_to_a(lua_State* tolua_S)
{
  ego_item_type* self = (ego_item_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'max_to_a'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->max_to_a = ((byte)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: max_pval of class  ego_item_type */
static int tolua_get_ego_item_type_max_pval(lua_State* tolua_S)
{
  ego_item_type* self = (ego_item_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'max_pval'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->max_pval);
 return 1;
}

/* set function: max_pval of class  ego_item_type */
static int tolua_set_ego_item_type_max_pval(lua_State* tolua_S)
{
  ego_item_type* self = (ego_item_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'max_pval'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->max_pval = ((byte)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: cost of class  ego_item_type */
static int tolua_get_ego_item_type_cost(lua_State* tolua_S)
{
  ego_item_type* self = (ego_item_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'cost'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->cost);
 return 1;
}

/* set function: cost of class  ego_item_type */
static int tolua_set_ego_item_type_cost(lua_State* tolua_S)
{
  ego_item_type* self = (ego_item_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'cost'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->cost = ((s32b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: flags1 of class  ego_item_type */
static int tolua_get_ego_item_type_flags1(lua_State* tolua_S)
{
  ego_item_type* self = (ego_item_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'flags1'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->flags1);
 return 1;
}

/* set function: flags1 of class  ego_item_type */
static int tolua_set_ego_item_type_flags1(lua_State* tolua_S)
{
  ego_item_type* self = (ego_item_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'flags1'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->flags1 = ((u32b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: flags2 of class  ego_item_type */
static int tolua_get_ego_item_type_flags2(lua_State* tolua_S)
{
  ego_item_type* self = (ego_item_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'flags2'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->flags2);
 return 1;
}

/* set function: flags2 of class  ego_item_type */
static int tolua_set_ego_item_type_flags2(lua_State* tolua_S)
{
  ego_item_type* self = (ego_item_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'flags2'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->flags2 = ((u32b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: flags3 of class  ego_item_type */
static int tolua_get_ego_item_type_flags3(lua_State* tolua_S)
{
  ego_item_type* self = (ego_item_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'flags3'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->flags3);
 return 1;
}

/* set function: flags3 of class  ego_item_type */
static int tolua_set_ego_item_type_flags3(lua_State* tolua_S)
{
  ego_item_type* self = (ego_item_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'flags3'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->flags3 = ((u32b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: flags4 of class  ego_item_type */
static int tolua_get_ego_item_type_flags4(lua_State* tolua_S)
{
  ego_item_type* self = (ego_item_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'flags4'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->flags4);
 return 1;
}

/* set function: flags4 of class  ego_item_type */
static int tolua_set_ego_item_type_flags4(lua_State* tolua_S)
{
  ego_item_type* self = (ego_item_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'flags4'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->flags4 = ((u32b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: name of class  monster_race */
static int tolua_get_monster_race_name(lua_State* tolua_S)
{
  monster_race* self = (monster_race*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'name'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->name);
 return 1;
}

/* set function: name of class  monster_race */
static int tolua_set_monster_race_name(lua_State* tolua_S)
{
  monster_race* self = (monster_race*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'name'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->name = ((u32b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: text of class  monster_race */
static int tolua_get_monster_race_text(lua_State* tolua_S)
{
  monster_race* self = (monster_race*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'text'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->text);
 return 1;
}

/* set function: text of class  monster_race */
static int tolua_set_monster_race_text(lua_State* tolua_S)
{
  monster_race* self = (monster_race*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'text'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->text = ((u32b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: name_char of class  monster_race */
static int tolua_get_monster_race_name_char(lua_State* tolua_S)
{
  monster_race* self = (monster_race*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'name_char'",NULL);
#endif
 tolua_pushstring(tolua_S,(const char*)self->name_char);
 return 1;
}

/* set function: name_char of class  monster_race */
static int tolua_set_monster_race_name_char(lua_State* tolua_S)
{
  monster_race* self = (monster_race*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'name_char'",NULL);
 if (!tolua_isstring(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
 strncpy(self->name_char,tolua_tostring(tolua_S,2,0),200-1);
 return 0;
}

/* get function: hdice of class  monster_race */
static int tolua_get_monster_race_hdice(lua_State* tolua_S)
{
  monster_race* self = (monster_race*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'hdice'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->hdice);
 return 1;
}

/* set function: hdice of class  monster_race */
static int tolua_set_monster_race_hdice(lua_State* tolua_S)
{
  monster_race* self = (monster_race*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'hdice'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->hdice = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: hside of class  monster_race */
static int tolua_get_monster_race_hside(lua_State* tolua_S)
{
  monster_race* self = (monster_race*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'hside'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->hside);
 return 1;
}

/* set function: hside of class  monster_race */
static int tolua_set_monster_race_hside(lua_State* tolua_S)
{
  monster_race* self = (monster_race*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'hside'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->hside = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: ac of class  monster_race */
static int tolua_get_monster_race_ac(lua_State* tolua_S)
{
  monster_race* self = (monster_race*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'ac'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->ac);
 return 1;
}

/* set function: ac of class  monster_race */
static int tolua_set_monster_race_ac(lua_State* tolua_S)
{
  monster_race* self = (monster_race*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'ac'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->ac = ((s32b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: sleep of class  monster_race */
static int tolua_get_monster_race_sleep(lua_State* tolua_S)
{
  monster_race* self = (monster_race*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'sleep'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->sleep);
 return 1;
}

/* set function: sleep of class  monster_race */
static int tolua_set_monster_race_sleep(lua_State* tolua_S)
{
  monster_race* self = (monster_race*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'sleep'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->sleep = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: aaf of class  monster_race */
static int tolua_get_monster_race_aaf(lua_State* tolua_S)
{
  monster_race* self = (monster_race*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'aaf'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->aaf);
 return 1;
}

/* set function: aaf of class  monster_race */
static int tolua_set_monster_race_aaf(lua_State* tolua_S)
{
  monster_race* self = (monster_race*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'aaf'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->aaf = ((byte)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: speed of class  monster_race */
static int tolua_get_monster_race_speed(lua_State* tolua_S)
{
  monster_race* self = (monster_race*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'speed'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->speed);
 return 1;
}

/* set function: speed of class  monster_race */
static int tolua_set_monster_race_speed(lua_State* tolua_S)
{
  monster_race* self = (monster_race*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'speed'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->speed = ((byte)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: mexp of class  monster_race */
static int tolua_get_monster_race_mexp(lua_State* tolua_S)
{
  monster_race* self = (monster_race*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'mexp'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->mexp);
 return 1;
}

/* set function: mexp of class  monster_race */
static int tolua_set_monster_race_mexp(lua_State* tolua_S)
{
  monster_race* self = (monster_race*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'mexp'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->mexp = ((s32b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: weight of class  monster_race */
static int tolua_get_monster_race_weight(lua_State* tolua_S)
{
  monster_race* self = (monster_race*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'weight'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->weight);
 return 1;
}

/* set function: weight of class  monster_race */
static int tolua_set_monster_race_weight(lua_State* tolua_S)
{
  monster_race* self = (monster_race*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'weight'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->weight = ((s32b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: freq_inate of class  monster_race */
static int tolua_get_monster_race_freq_inate(lua_State* tolua_S)
{
  monster_race* self = (monster_race*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'freq_inate'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->freq_inate);
 return 1;
}

/* set function: freq_inate of class  monster_race */
static int tolua_set_monster_race_freq_inate(lua_State* tolua_S)
{
  monster_race* self = (monster_race*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'freq_inate'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->freq_inate = ((byte)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: freq_spell of class  monster_race */
static int tolua_get_monster_race_freq_spell(lua_State* tolua_S)
{
  monster_race* self = (monster_race*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'freq_spell'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->freq_spell);
 return 1;
}

/* set function: freq_spell of class  monster_race */
static int tolua_set_monster_race_freq_spell(lua_State* tolua_S)
{
  monster_race* self = (monster_race*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'freq_spell'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->freq_spell = ((byte)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: flags1 of class  monster_race */
static int tolua_get_monster_race_flags1(lua_State* tolua_S)
{
  monster_race* self = (monster_race*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'flags1'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->flags1);
 return 1;
}

/* set function: flags1 of class  monster_race */
static int tolua_set_monster_race_flags1(lua_State* tolua_S)
{
  monster_race* self = (monster_race*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'flags1'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->flags1 = ((u32b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: flags2 of class  monster_race */
static int tolua_get_monster_race_flags2(lua_State* tolua_S)
{
  monster_race* self = (monster_race*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'flags2'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->flags2);
 return 1;
}

/* set function: flags2 of class  monster_race */
static int tolua_set_monster_race_flags2(lua_State* tolua_S)
{
  monster_race* self = (monster_race*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'flags2'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->flags2 = ((u32b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: flags3 of class  monster_race */
static int tolua_get_monster_race_flags3(lua_State* tolua_S)
{
  monster_race* self = (monster_race*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'flags3'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->flags3);
 return 1;
}

/* set function: flags3 of class  monster_race */
static int tolua_set_monster_race_flags3(lua_State* tolua_S)
{
  monster_race* self = (monster_race*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'flags3'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->flags3 = ((u32b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: flags4 of class  monster_race */
static int tolua_get_monster_race_flags4(lua_State* tolua_S)
{
  monster_race* self = (monster_race*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'flags4'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->flags4);
 return 1;
}

/* set function: flags4 of class  monster_race */
static int tolua_set_monster_race_flags4(lua_State* tolua_S)
{
  monster_race* self = (monster_race*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'flags4'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->flags4 = ((u32b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: flags5 of class  monster_race */
static int tolua_get_monster_race_flags5(lua_State* tolua_S)
{
  monster_race* self = (monster_race*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'flags5'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->flags5);
 return 1;
}

/* set function: flags5 of class  monster_race */
static int tolua_set_monster_race_flags5(lua_State* tolua_S)
{
  monster_race* self = (monster_race*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'flags5'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->flags5 = ((u32b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: flags6 of class  monster_race */
static int tolua_get_monster_race_flags6(lua_State* tolua_S)
{
  monster_race* self = (monster_race*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'flags6'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->flags6);
 return 1;
}

/* set function: flags6 of class  monster_race */
static int tolua_set_monster_race_flags6(lua_State* tolua_S)
{
  monster_race* self = (monster_race*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'flags6'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->flags6 = ((u32b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: flags7 of class  monster_race */
static int tolua_get_monster_race_flags7(lua_State* tolua_S)
{
  monster_race* self = (monster_race*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'flags7'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->flags7);
 return 1;
}

/* set function: flags7 of class  monster_race */
static int tolua_set_monster_race_flags7(lua_State* tolua_S)
{
  monster_race* self = (monster_race*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'flags7'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->flags7 = ((u32b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: flags8 of class  monster_race */
static int tolua_get_monster_race_flags8(lua_State* tolua_S)
{
  monster_race* self = (monster_race*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'flags8'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->flags8);
 return 1;
}

/* set function: flags8 of class  monster_race */
static int tolua_set_monster_race_flags8(lua_State* tolua_S)
{
  monster_race* self = (monster_race*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'flags8'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->flags8 = ((u32b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: flags9 of class  monster_race */
static int tolua_get_monster_race_flags9(lua_State* tolua_S)
{
  monster_race* self = (monster_race*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'flags9'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->flags9);
 return 1;
}

/* set function: flags9 of class  monster_race */
static int tolua_set_monster_race_flags9(lua_State* tolua_S)
{
  monster_race* self = (monster_race*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'flags9'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->flags9 = ((u32b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: blow of class  monster_race */
static int tolua_get_types_monster_race_blow(lua_State* tolua_S)
{
 int tolua_index;
  monster_race* self;
 lua_pushstring(tolua_S,".self");
 lua_rawget(tolua_S,1);
 self = (monster_race*)  lua_touserdata(tolua_S,-1);
#ifndef TOLUA_RELEASE
 {
 tolua_Error tolua_err;
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in array indexing.",&tolua_err);
 }
#endif
 tolua_index = (int)tolua_tonumber(tolua_S,2,0)-1;
#ifndef TOLUA_RELEASE
 if (tolua_index<0 || tolua_index>=4)
 tolua_error(tolua_S,"array indexing out of range.",NULL);
#endif
 tolua_pushusertype(tolua_S,(void*)&self->blow[tolua_index],"monster_blow");
 return 1;
}

/* set function: blow of class  monster_race */
static int tolua_set_types_monster_race_blow(lua_State* tolua_S)
{
 int tolua_index;
  monster_race* self;
 lua_pushstring(tolua_S,".self");
 lua_rawget(tolua_S,1);
 self = (monster_race*)  lua_touserdata(tolua_S,-1);
#ifndef TOLUA_RELEASE
 {
 tolua_Error tolua_err;
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in array indexing.",&tolua_err);
 }
#endif
 tolua_index = (int)tolua_tonumber(tolua_S,2,0)-1;
#ifndef TOLUA_RELEASE
 if (tolua_index<0 || tolua_index>=4)
 tolua_error(tolua_S,"array indexing out of range.",NULL);
#endif
  self->blow[tolua_index] = *((monster_blow*)  tolua_tousertype(tolua_S,3,0));
 return 0;
}

/* get function: body_parts of class  monster_race */
static int tolua_get_types_monster_race_body_parts(lua_State* tolua_S)
{
 int tolua_index;
  monster_race* self;
 lua_pushstring(tolua_S,".self");
 lua_rawget(tolua_S,1);
 self = (monster_race*)  lua_touserdata(tolua_S,-1);
#ifndef TOLUA_RELEASE
 {
 tolua_Error tolua_err;
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in array indexing.",&tolua_err);
 }
#endif
 tolua_index = (int)tolua_tonumber(tolua_S,2,0)-1;
#ifndef TOLUA_RELEASE
 if (tolua_index<0 || tolua_index>=BODY_MAX)
 tolua_error(tolua_S,"array indexing out of range.",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->body_parts[tolua_index]);
 return 1;
}

/* set function: body_parts of class  monster_race */
static int tolua_set_types_monster_race_body_parts(lua_State* tolua_S)
{
 int tolua_index;
  monster_race* self;
 lua_pushstring(tolua_S,".self");
 lua_rawget(tolua_S,1);
 self = (monster_race*)  lua_touserdata(tolua_S,-1);
#ifndef TOLUA_RELEASE
 {
 tolua_Error tolua_err;
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in array indexing.",&tolua_err);
 }
#endif
 tolua_index = (int)tolua_tonumber(tolua_S,2,0)-1;
#ifndef TOLUA_RELEASE
 if (tolua_index<0 || tolua_index>=BODY_MAX)
 tolua_error(tolua_S,"array indexing out of range.",NULL);
#endif
  self->body_parts[tolua_index] = ((byte)  tolua_tonumber(tolua_S,3,0));
 return 0;
}

/* get function: level of class  monster_race */
static int tolua_get_monster_race_level(lua_State* tolua_S)
{
  monster_race* self = (monster_race*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'level'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->level);
 return 1;
}

/* set function: level of class  monster_race */
static int tolua_set_monster_race_level(lua_State* tolua_S)
{
  monster_race* self = (monster_race*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'level'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->level = ((byte)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: rarity of class  monster_race */
static int tolua_get_monster_race_rarity(lua_State* tolua_S)
{
  monster_race* self = (monster_race*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'rarity'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->rarity);
 return 1;
}

/* set function: rarity of class  monster_race */
static int tolua_set_monster_race_rarity(lua_State* tolua_S)
{
  monster_race* self = (monster_race*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'rarity'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->rarity = ((byte)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: d_attr of class  monster_race */
static int tolua_get_monster_race_d_attr(lua_State* tolua_S)
{
  monster_race* self = (monster_race*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'd_attr'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->d_attr);
 return 1;
}

/* set function: d_attr of class  monster_race */
static int tolua_set_monster_race_d_attr(lua_State* tolua_S)
{
  monster_race* self = (monster_race*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'd_attr'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->d_attr = ((byte)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: d_char of class  monster_race */
static int tolua_get_monster_race_d_char(lua_State* tolua_S)
{
  monster_race* self = (monster_race*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'd_char'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->d_char);
 return 1;
}

/* set function: d_char of class  monster_race */
static int tolua_set_monster_race_d_char(lua_State* tolua_S)
{
  monster_race* self = (monster_race*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'd_char'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->d_char = ((char)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: x_attr of class  monster_race */
static int tolua_get_monster_race_x_attr(lua_State* tolua_S)
{
  monster_race* self = (monster_race*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'x_attr'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->x_attr);
 return 1;
}

/* set function: x_attr of class  monster_race */
static int tolua_set_monster_race_x_attr(lua_State* tolua_S)
{
  monster_race* self = (monster_race*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'x_attr'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->x_attr = ((byte)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: x_char of class  monster_race */
static int tolua_get_monster_race_x_char(lua_State* tolua_S)
{
  monster_race* self = (monster_race*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'x_char'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->x_char);
 return 1;
}

/* set function: x_char of class  monster_race */
static int tolua_set_monster_race_x_char(lua_State* tolua_S)
{
  monster_race* self = (monster_race*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'x_char'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->x_char = ((char)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: max_num of class  monster_race */
static int tolua_get_monster_race_max_num(lua_State* tolua_S)
{
  monster_race* self = (monster_race*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'max_num'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->max_num);
 return 1;
}

/* set function: max_num of class  monster_race */
static int tolua_set_monster_race_max_num(lua_State* tolua_S)
{
  monster_race* self = (monster_race*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'max_num'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->max_num = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: cur_num of class  monster_race */
static int tolua_get_monster_race_cur_num(lua_State* tolua_S)
{
  monster_race* self = (monster_race*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'cur_num'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->cur_num);
 return 1;
}

/* set function: cur_num of class  monster_race */
static int tolua_set_monster_race_cur_num(lua_State* tolua_S)
{
  monster_race* self = (monster_race*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'cur_num'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->cur_num = ((byte)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: r_sights of class  monster_race */
static int tolua_get_monster_race_r_sights(lua_State* tolua_S)
{
  monster_race* self = (monster_race*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'r_sights'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->r_sights);
 return 1;
}

/* set function: r_sights of class  monster_race */
static int tolua_set_monster_race_r_sights(lua_State* tolua_S)
{
  monster_race* self = (monster_race*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'r_sights'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->r_sights = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: r_deaths of class  monster_race */
static int tolua_get_monster_race_r_deaths(lua_State* tolua_S)
{
  monster_race* self = (monster_race*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'r_deaths'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->r_deaths);
 return 1;
}

/* set function: r_deaths of class  monster_race */
static int tolua_set_monster_race_r_deaths(lua_State* tolua_S)
{
  monster_race* self = (monster_race*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'r_deaths'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->r_deaths = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: r_pkills of class  monster_race */
static int tolua_get_monster_race_r_pkills(lua_State* tolua_S)
{
  monster_race* self = (monster_race*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'r_pkills'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->r_pkills);
 return 1;
}

/* set function: r_pkills of class  monster_race */
static int tolua_set_monster_race_r_pkills(lua_State* tolua_S)
{
  monster_race* self = (monster_race*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'r_pkills'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->r_pkills = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: r_tkills of class  monster_race */
static int tolua_get_monster_race_r_tkills(lua_State* tolua_S)
{
  monster_race* self = (monster_race*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'r_tkills'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->r_tkills);
 return 1;
}

/* set function: r_tkills of class  monster_race */
static int tolua_set_monster_race_r_tkills(lua_State* tolua_S)
{
  monster_race* self = (monster_race*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'r_tkills'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->r_tkills = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: r_wake of class  monster_race */
static int tolua_get_monster_race_r_wake(lua_State* tolua_S)
{
  monster_race* self = (monster_race*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'r_wake'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->r_wake);
 return 1;
}

/* set function: r_wake of class  monster_race */
static int tolua_set_monster_race_r_wake(lua_State* tolua_S)
{
  monster_race* self = (monster_race*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'r_wake'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->r_wake = ((byte)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: r_ignore of class  monster_race */
static int tolua_get_monster_race_r_ignore(lua_State* tolua_S)
{
  monster_race* self = (monster_race*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'r_ignore'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->r_ignore);
 return 1;
}

/* set function: r_ignore of class  monster_race */
static int tolua_set_monster_race_r_ignore(lua_State* tolua_S)
{
  monster_race* self = (monster_race*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'r_ignore'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->r_ignore = ((byte)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: r_xtra1 of class  monster_race */
static int tolua_get_monster_race_r_xtra1(lua_State* tolua_S)
{
  monster_race* self = (monster_race*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'r_xtra1'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->r_xtra1);
 return 1;
}

/* set function: r_xtra1 of class  monster_race */
static int tolua_set_monster_race_r_xtra1(lua_State* tolua_S)
{
  monster_race* self = (monster_race*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'r_xtra1'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->r_xtra1 = ((byte)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: r_xtra2 of class  monster_race */
static int tolua_get_monster_race_r_xtra2(lua_State* tolua_S)
{
  monster_race* self = (monster_race*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'r_xtra2'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->r_xtra2);
 return 1;
}

/* set function: r_xtra2 of class  monster_race */
static int tolua_set_monster_race_r_xtra2(lua_State* tolua_S)
{
  monster_race* self = (monster_race*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'r_xtra2'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->r_xtra2 = ((byte)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: r_drop_gold of class  monster_race */
static int tolua_get_monster_race_r_drop_gold(lua_State* tolua_S)
{
  monster_race* self = (monster_race*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'r_drop_gold'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->r_drop_gold);
 return 1;
}

/* set function: r_drop_gold of class  monster_race */
static int tolua_set_monster_race_r_drop_gold(lua_State* tolua_S)
{
  monster_race* self = (monster_race*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'r_drop_gold'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->r_drop_gold = ((byte)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: r_drop_item of class  monster_race */
static int tolua_get_monster_race_r_drop_item(lua_State* tolua_S)
{
  monster_race* self = (monster_race*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'r_drop_item'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->r_drop_item);
 return 1;
}

/* set function: r_drop_item of class  monster_race */
static int tolua_set_monster_race_r_drop_item(lua_State* tolua_S)
{
  monster_race* self = (monster_race*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'r_drop_item'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->r_drop_item = ((byte)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: r_cast_inate of class  monster_race */
static int tolua_get_monster_race_r_cast_inate(lua_State* tolua_S)
{
  monster_race* self = (monster_race*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'r_cast_inate'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->r_cast_inate);
 return 1;
}

/* set function: r_cast_inate of class  monster_race */
static int tolua_set_monster_race_r_cast_inate(lua_State* tolua_S)
{
  monster_race* self = (monster_race*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'r_cast_inate'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->r_cast_inate = ((byte)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: r_cast_spell of class  monster_race */
static int tolua_get_monster_race_r_cast_spell(lua_State* tolua_S)
{
  monster_race* self = (monster_race*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'r_cast_spell'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->r_cast_spell);
 return 1;
}

/* set function: r_cast_spell of class  monster_race */
static int tolua_set_monster_race_r_cast_spell(lua_State* tolua_S)
{
  monster_race* self = (monster_race*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'r_cast_spell'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->r_cast_spell = ((byte)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: r_blows of class  monster_race */
static int tolua_get_types_monster_race_r_blows(lua_State* tolua_S)
{
 int tolua_index;
  monster_race* self;
 lua_pushstring(tolua_S,".self");
 lua_rawget(tolua_S,1);
 self = (monster_race*)  lua_touserdata(tolua_S,-1);
#ifndef TOLUA_RELEASE
 {
 tolua_Error tolua_err;
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in array indexing.",&tolua_err);
 }
#endif
 tolua_index = (int)tolua_tonumber(tolua_S,2,0)-1;
#ifndef TOLUA_RELEASE
 if (tolua_index<0 || tolua_index>=20)
 tolua_error(tolua_S,"array indexing out of range.",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->r_blows[tolua_index]);
 return 1;
}

/* set function: r_blows of class  monster_race */
static int tolua_set_types_monster_race_r_blows(lua_State* tolua_S)
{
 int tolua_index;
  monster_race* self;
 lua_pushstring(tolua_S,".self");
 lua_rawget(tolua_S,1);
 self = (monster_race*)  lua_touserdata(tolua_S,-1);
#ifndef TOLUA_RELEASE
 {
 tolua_Error tolua_err;
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in array indexing.",&tolua_err);
 }
#endif
 tolua_index = (int)tolua_tonumber(tolua_S,2,0)-1;
#ifndef TOLUA_RELEASE
 if (tolua_index<0 || tolua_index>=20)
 tolua_error(tolua_S,"array indexing out of range.",NULL);
#endif
  self->r_blows[tolua_index] = ((byte)  tolua_tonumber(tolua_S,3,0));
 return 0;
}

/* get function: r_resist of class  monster_race */
static int tolua_get_types_monster_race_r_resist(lua_State* tolua_S)
{
 int tolua_index;
  monster_race* self;
 lua_pushstring(tolua_S,".self");
 lua_rawget(tolua_S,1);
 self = (monster_race*)  lua_touserdata(tolua_S,-1);
#ifndef TOLUA_RELEASE
 {
 tolua_Error tolua_err;
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in array indexing.",&tolua_err);
 }
#endif
 tolua_index = (int)tolua_tonumber(tolua_S,2,0)-1;
#ifndef TOLUA_RELEASE
 if (tolua_index<0 || tolua_index>=MAX_RESIST)
 tolua_error(tolua_S,"array indexing out of range.",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->r_resist[tolua_index]);
 return 1;
}

/* set function: r_resist of class  monster_race */
static int tolua_set_types_monster_race_r_resist(lua_State* tolua_S)
{
 int tolua_index;
  monster_race* self;
 lua_pushstring(tolua_S,".self");
 lua_rawget(tolua_S,1);
 self = (monster_race*)  lua_touserdata(tolua_S,-1);
#ifndef TOLUA_RELEASE
 {
 tolua_Error tolua_err;
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in array indexing.",&tolua_err);
 }
#endif
 tolua_index = (int)tolua_tonumber(tolua_S,2,0)-1;
#ifndef TOLUA_RELEASE
 if (tolua_index<0 || tolua_index>=MAX_RESIST)
 tolua_error(tolua_S,"array indexing out of range.",NULL);
#endif
  self->r_resist[tolua_index] = ((byte)  tolua_tonumber(tolua_S,3,0));
 return 0;
}

/* get function: r_spells of class  monster_race */
static int tolua_get_types_monster_race_r_spells(lua_State* tolua_S)
{
 int tolua_index;
  monster_race* self;
 lua_pushstring(tolua_S,".self");
 lua_rawget(tolua_S,1);
 self = (monster_race*)  lua_touserdata(tolua_S,-1);
#ifndef TOLUA_RELEASE
 {
 tolua_Error tolua_err;
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in array indexing.",&tolua_err);
 }
#endif
 tolua_index = (int)tolua_tonumber(tolua_S,2,0)-1;
#ifndef TOLUA_RELEASE
 if (tolua_index<0 || tolua_index>=20)
 tolua_error(tolua_S,"array indexing out of range.",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->r_spells[tolua_index]);
 return 1;
}

/* set function: r_spells of class  monster_race */
static int tolua_set_types_monster_race_r_spells(lua_State* tolua_S)
{
 int tolua_index;
  monster_race* self;
 lua_pushstring(tolua_S,".self");
 lua_rawget(tolua_S,1);
 self = (monster_race*)  lua_touserdata(tolua_S,-1);
#ifndef TOLUA_RELEASE
 {
 tolua_Error tolua_err;
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in array indexing.",&tolua_err);
 }
#endif
 tolua_index = (int)tolua_tonumber(tolua_S,2,0)-1;
#ifndef TOLUA_RELEASE
 if (tolua_index<0 || tolua_index>=20)
 tolua_error(tolua_S,"array indexing out of range.",NULL);
#endif
  self->r_spells[tolua_index] = ((byte)  tolua_tonumber(tolua_S,3,0));
 return 0;
}

/* get function: r_flags1 of class  monster_race */
static int tolua_get_monster_race_r_flags1(lua_State* tolua_S)
{
  monster_race* self = (monster_race*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'r_flags1'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->r_flags1);
 return 1;
}

/* set function: r_flags1 of class  monster_race */
static int tolua_set_monster_race_r_flags1(lua_State* tolua_S)
{
  monster_race* self = (monster_race*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'r_flags1'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->r_flags1 = ((u32b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: r_flags2 of class  monster_race */
static int tolua_get_monster_race_r_flags2(lua_State* tolua_S)
{
  monster_race* self = (monster_race*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'r_flags2'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->r_flags2);
 return 1;
}

/* set function: r_flags2 of class  monster_race */
static int tolua_set_monster_race_r_flags2(lua_State* tolua_S)
{
  monster_race* self = (monster_race*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'r_flags2'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->r_flags2 = ((u32b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: r_flags3 of class  monster_race */
static int tolua_get_monster_race_r_flags3(lua_State* tolua_S)
{
  monster_race* self = (monster_race*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'r_flags3'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->r_flags3);
 return 1;
}

/* set function: r_flags3 of class  monster_race */
static int tolua_set_monster_race_r_flags3(lua_State* tolua_S)
{
  monster_race* self = (monster_race*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'r_flags3'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->r_flags3 = ((u32b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: r_flags4 of class  monster_race */
static int tolua_get_monster_race_r_flags4(lua_State* tolua_S)
{
  monster_race* self = (monster_race*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'r_flags4'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->r_flags4);
 return 1;
}

/* set function: r_flags4 of class  monster_race */
static int tolua_set_monster_race_r_flags4(lua_State* tolua_S)
{
  monster_race* self = (monster_race*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'r_flags4'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->r_flags4 = ((u32b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: r_flags5 of class  monster_race */
static int tolua_get_monster_race_r_flags5(lua_State* tolua_S)
{
  monster_race* self = (monster_race*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'r_flags5'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->r_flags5);
 return 1;
}

/* set function: r_flags5 of class  monster_race */
static int tolua_set_monster_race_r_flags5(lua_State* tolua_S)
{
  monster_race* self = (monster_race*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'r_flags5'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->r_flags5 = ((u32b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: r_flags6 of class  monster_race */
static int tolua_get_monster_race_r_flags6(lua_State* tolua_S)
{
  monster_race* self = (monster_race*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'r_flags6'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->r_flags6);
 return 1;
}

/* set function: r_flags6 of class  monster_race */
static int tolua_set_monster_race_r_flags6(lua_State* tolua_S)
{
  monster_race* self = (monster_race*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'r_flags6'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->r_flags6 = ((u32b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: r_flags7 of class  monster_race */
static int tolua_get_monster_race_r_flags7(lua_State* tolua_S)
{
  monster_race* self = (monster_race*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'r_flags7'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->r_flags7);
 return 1;
}

/* set function: r_flags7 of class  monster_race */
static int tolua_set_monster_race_r_flags7(lua_State* tolua_S)
{
  monster_race* self = (monster_race*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'r_flags7'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->r_flags7 = ((u32b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: r_flags8 of class  monster_race */
static int tolua_get_monster_race_r_flags8(lua_State* tolua_S)
{
  monster_race* self = (monster_race*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'r_flags8'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->r_flags8);
 return 1;
}

/* set function: r_flags8 of class  monster_race */
static int tolua_set_monster_race_r_flags8(lua_State* tolua_S)
{
  monster_race* self = (monster_race*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'r_flags8'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->r_flags8 = ((u32b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: r_flags9 of class  monster_race */
static int tolua_get_monster_race_r_flags9(lua_State* tolua_S)
{
  monster_race* self = (monster_race*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'r_flags9'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->r_flags9);
 return 1;
}

/* set function: r_flags9 of class  monster_race */
static int tolua_set_monster_race_r_flags9(lua_State* tolua_S)
{
  monster_race* self = (monster_race*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'r_flags9'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->r_flags9 = ((u32b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: on_saved of class  monster_race */
static int tolua_get_monster_race_on_saved(lua_State* tolua_S)
{
  monster_race* self = (monster_race*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'on_saved'",NULL);
#endif
 tolua_pushboolean(tolua_S,(bool)self->on_saved);
 return 1;
}

/* set function: on_saved of class  monster_race */
static int tolua_set_monster_race_on_saved(lua_State* tolua_S)
{
  monster_race* self = (monster_race*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'on_saved'",NULL);
 if (!tolua_isboolean(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->on_saved = ((bool)  tolua_toboolean(tolua_S,2,0));
 return 0;
}

/* get function: str of class  monster_race */
static int tolua_get_monster_race_str(lua_State* tolua_S)
{
  monster_race* self = (monster_race*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'str'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->str);
 return 1;
}

/* set function: str of class  monster_race */
static int tolua_set_monster_race_str(lua_State* tolua_S)
{
  monster_race* self = (monster_race*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'str'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->str = ((s32b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: dex of class  monster_race */
static int tolua_get_monster_race_dex(lua_State* tolua_S)
{
  monster_race* self = (monster_race*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'dex'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->dex);
 return 1;
}

/* set function: dex of class  monster_race */
static int tolua_set_monster_race_dex(lua_State* tolua_S)
{
  monster_race* self = (monster_race*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'dex'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->dex = ((s32b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: mind of class  monster_race */
static int tolua_get_monster_race_mind(lua_State* tolua_S)
{
  monster_race* self = (monster_race*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'mind'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->mind);
 return 1;
}

/* set function: mind of class  monster_race */
static int tolua_set_monster_race_mind(lua_State* tolua_S)
{
  monster_race* self = (monster_race*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'mind'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->mind = ((s32b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: skill_attack of class  monster_race */
static int tolua_get_monster_race_skill_attack(lua_State* tolua_S)
{
  monster_race* self = (monster_race*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'skill_attack'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->skill_attack);
 return 1;
}

/* set function: skill_attack of class  monster_race */
static int tolua_set_monster_race_skill_attack(lua_State* tolua_S)
{
  monster_race* self = (monster_race*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'skill_attack'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->skill_attack = ((s32b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: skill_ranged of class  monster_race */
static int tolua_get_monster_race_skill_ranged(lua_State* tolua_S)
{
  monster_race* self = (monster_race*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'skill_ranged'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->skill_ranged);
 return 1;
}

/* set function: skill_ranged of class  monster_race */
static int tolua_set_monster_race_skill_ranged(lua_State* tolua_S)
{
  monster_race* self = (monster_race*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'skill_ranged'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->skill_ranged = ((s32b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: skill_magic of class  monster_race */
static int tolua_get_monster_race_skill_magic(lua_State* tolua_S)
{
  monster_race* self = (monster_race*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'skill_magic'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->skill_magic);
 return 1;
}

/* set function: skill_magic of class  monster_race */
static int tolua_set_monster_race_skill_magic(lua_State* tolua_S)
{
  monster_race* self = (monster_race*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'skill_magic'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->skill_magic = ((s32b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: countertype of class  monster_race */
static int tolua_get_monster_race_countertype(lua_State* tolua_S)
{
  monster_race* self = (monster_race*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'countertype'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->countertype);
 return 1;
}

/* set function: countertype of class  monster_race */
static int tolua_set_monster_race_countertype(lua_State* tolua_S)
{
  monster_race* self = (monster_race*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'countertype'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->countertype = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: counterchance of class  monster_race */
static int tolua_get_monster_race_counterchance(lua_State* tolua_S)
{
  monster_race* self = (monster_race*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'counterchance'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->counterchance);
 return 1;
}

/* set function: counterchance of class  monster_race */
static int tolua_set_monster_race_counterchance(lua_State* tolua_S)
{
  monster_race* self = (monster_race*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'counterchance'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->counterchance = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: resistances of class  monster_race */
static int tolua_get_types_monster_race_resistances(lua_State* tolua_S)
{
 int tolua_index;
  monster_race* self;
 lua_pushstring(tolua_S,".self");
 lua_rawget(tolua_S,1);
 self = (monster_race*)  lua_touserdata(tolua_S,-1);
#ifndef TOLUA_RELEASE
 {
 tolua_Error tolua_err;
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in array indexing.",&tolua_err);
 }
#endif
 tolua_index = (int)tolua_tonumber(tolua_S,2,0)-1;
#ifndef TOLUA_RELEASE
 if (tolua_index<0 || tolua_index>=MAX_RESIST)
 tolua_error(tolua_S,"array indexing out of range.",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->resistances[tolua_index]);
 return 1;
}

/* set function: resistances of class  monster_race */
static int tolua_set_types_monster_race_resistances(lua_State* tolua_S)
{
 int tolua_index;
  monster_race* self;
 lua_pushstring(tolua_S,".self");
 lua_rawget(tolua_S,1);
 self = (monster_race*)  lua_touserdata(tolua_S,-1);
#ifndef TOLUA_RELEASE
 {
 tolua_Error tolua_err;
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in array indexing.",&tolua_err);
 }
#endif
 tolua_index = (int)tolua_tonumber(tolua_S,2,0)-1;
#ifndef TOLUA_RELEASE
 if (tolua_index<0 || tolua_index>=MAX_RESIST)
 tolua_error(tolua_S,"array indexing out of range.",NULL);
#endif
  self->resistances[tolua_index] = ((s16b)  tolua_tonumber(tolua_S,3,0));
 return 0;
}

/* get function: spellchance of class  monster_race */
static int tolua_get_monster_race_spellchance(lua_State* tolua_S)
{
  monster_race* self = (monster_race*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'spellchance'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->spellchance);
 return 1;
}

/* set function: spellchance of class  monster_race */
static int tolua_set_monster_race_spellchance(lua_State* tolua_S)
{
  monster_race* self = (monster_race*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'spellchance'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->spellchance = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: attacks of class  monster_race */
static int tolua_get_monster_race_attacks(lua_State* tolua_S)
{
  monster_race* self = (monster_race*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'attacks'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->attacks);
 return 1;
}

/* set function: attacks of class  monster_race */
static int tolua_set_monster_race_attacks(lua_State* tolua_S)
{
  monster_race* self = (monster_race*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'attacks'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->attacks = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: attack of class  monster_race */
static int tolua_get_types_monster_race_attack(lua_State* tolua_S)
{
 int tolua_index;
  monster_race* self;
 lua_pushstring(tolua_S,".self");
 lua_rawget(tolua_S,1);
 self = (monster_race*)  lua_touserdata(tolua_S,-1);
#ifndef TOLUA_RELEASE
 {
 tolua_Error tolua_err;
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in array indexing.",&tolua_err);
 }
#endif
 tolua_index = (int)tolua_tonumber(tolua_S,2,0)-1;
#ifndef TOLUA_RELEASE
 if (tolua_index<0 || tolua_index>=20)
 tolua_error(tolua_S,"array indexing out of range.",NULL);
#endif
 tolua_pushusertype(tolua_S,(void*)&self->attack[tolua_index],"monster_attack");
 return 1;
}

/* set function: attack of class  monster_race */
static int tolua_set_types_monster_race_attack(lua_State* tolua_S)
{
 int tolua_index;
  monster_race* self;
 lua_pushstring(tolua_S,".self");
 lua_rawget(tolua_S,1);
 self = (monster_race*)  lua_touserdata(tolua_S,-1);
#ifndef TOLUA_RELEASE
 {
 tolua_Error tolua_err;
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in array indexing.",&tolua_err);
 }
#endif
 tolua_index = (int)tolua_tonumber(tolua_S,2,0)-1;
#ifndef TOLUA_RELEASE
 if (tolua_index<0 || tolua_index>=20)
 tolua_error(tolua_S,"array indexing out of range.",NULL);
#endif
  self->attack[tolua_index] = *((monster_attack*)  tolua_tousertype(tolua_S,3,0));
 return 0;
}

/* get function: spells of class  monster_race */
static int tolua_get_monster_race_spells(lua_State* tolua_S)
{
  monster_race* self = (monster_race*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'spells'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->spells);
 return 1;
}

/* set function: spells of class  monster_race */
static int tolua_set_monster_race_spells(lua_State* tolua_S)
{
  monster_race* self = (monster_race*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'spells'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->spells = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: spell of class  monster_race */
static int tolua_get_types_monster_race_spell(lua_State* tolua_S)
{
 int tolua_index;
  monster_race* self;
 lua_pushstring(tolua_S,".self");
 lua_rawget(tolua_S,1);
 self = (monster_race*)  lua_touserdata(tolua_S,-1);
#ifndef TOLUA_RELEASE
 {
 tolua_Error tolua_err;
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in array indexing.",&tolua_err);
 }
#endif
 tolua_index = (int)tolua_tonumber(tolua_S,2,0)-1;
#ifndef TOLUA_RELEASE
 if (tolua_index<0 || tolua_index>=20)
 tolua_error(tolua_S,"array indexing out of range.",NULL);
#endif
 tolua_pushusertype(tolua_S,(void*)&self->spell[tolua_index],"monster_spell");
 return 1;
}

/* set function: spell of class  monster_race */
static int tolua_set_types_monster_race_spell(lua_State* tolua_S)
{
 int tolua_index;
  monster_race* self;
 lua_pushstring(tolua_S,".self");
 lua_rawget(tolua_S,1);
 self = (monster_race*)  lua_touserdata(tolua_S,-1);
#ifndef TOLUA_RELEASE
 {
 tolua_Error tolua_err;
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in array indexing.",&tolua_err);
 }
#endif
 tolua_index = (int)tolua_tonumber(tolua_S,2,0)-1;
#ifndef TOLUA_RELEASE
 if (tolua_index<0 || tolua_index>=20)
 tolua_error(tolua_S,"array indexing out of range.",NULL);
#endif
  self->spell[tolua_index] = *((monster_spell*)  tolua_tousertype(tolua_S,3,0));
 return 0;
}

/* get function: treasuretval of class  monster_race */
static int tolua_get_monster_race_treasuretval(lua_State* tolua_S)
{
  monster_race* self = (monster_race*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'treasuretval'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->treasuretval);
 return 1;
}

/* set function: treasuretval of class  monster_race */
static int tolua_set_monster_race_treasuretval(lua_State* tolua_S)
{
  monster_race* self = (monster_race*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'treasuretval'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->treasuretval = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: treasuresval of class  monster_race */
static int tolua_get_monster_race_treasuresval(lua_State* tolua_S)
{
  monster_race* self = (monster_race*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'treasuresval'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->treasuresval);
 return 1;
}

/* set function: treasuresval of class  monster_race */
static int tolua_set_monster_race_treasuresval(lua_State* tolua_S)
{
  monster_race* self = (monster_race*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'treasuresval'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->treasuresval = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: treasurechance of class  monster_race */
static int tolua_get_monster_race_treasurechance(lua_State* tolua_S)
{
  monster_race* self = (monster_race*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'treasurechance'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->treasurechance);
 return 1;
}

/* set function: treasurechance of class  monster_race */
static int tolua_set_monster_race_treasurechance(lua_State* tolua_S)
{
  monster_race* self = (monster_race*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'treasurechance'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->treasurechance = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: treasuremagic of class  monster_race */
static int tolua_get_monster_race_treasuremagic(lua_State* tolua_S)
{
  monster_race* self = (monster_race*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'treasuremagic'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->treasuremagic);
 return 1;
}

/* set function: treasuremagic of class  monster_race */
static int tolua_set_monster_race_treasuremagic(lua_State* tolua_S)
{
  monster_race* self = (monster_race*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'treasuremagic'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->treasuremagic = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: event of class  monster_race */
static int tolua_get_monster_race_event(lua_State* tolua_S)
{
  monster_race* self = (monster_race*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'event'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->event);
 return 1;
}

/* set function: event of class  monster_race */
static int tolua_set_monster_race_event(lua_State* tolua_S)
{
  monster_race* self = (monster_race*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'event'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->event = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: extra1 of class  monster_race */
static int tolua_get_monster_race_extra1(lua_State* tolua_S)
{
  monster_race* self = (monster_race*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'extra1'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->extra1);
 return 1;
}

/* set function: extra1 of class  monster_race */
static int tolua_set_monster_race_extra1(lua_State* tolua_S)
{
  monster_race* self = (monster_race*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'extra1'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->extra1 = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: extra2 of class  monster_race */
static int tolua_get_monster_race_extra2(lua_State* tolua_S)
{
  monster_race* self = (monster_race*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'extra2'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->extra2);
 return 1;
}

/* set function: extra2 of class  monster_race */
static int tolua_set_monster_race_extra2(lua_State* tolua_S)
{
  monster_race* self = (monster_race*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'extra2'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->extra2 = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: fixedlevel of class  monster_race */
static int tolua_get_monster_race_fixedlevel(lua_State* tolua_S)
{
  monster_race* self = (monster_race*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'fixedlevel'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->fixedlevel);
 return 1;
}

/* set function: fixedlevel of class  monster_race */
static int tolua_set_monster_race_fixedlevel(lua_State* tolua_S)
{
  monster_race* self = (monster_race*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'fixedlevel'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->fixedlevel = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: townnum of class  monster_race */
static int tolua_get_monster_race_townnum(lua_State* tolua_S)
{
  monster_race* self = (monster_race*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'townnum'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->townnum);
 return 1;
}

/* set function: townnum of class  monster_race */
static int tolua_set_monster_race_townnum(lua_State* tolua_S)
{
  monster_race* self = (monster_race*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'townnum'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->townnum = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: dunnum of class  monster_race */
static int tolua_get_monster_race_dunnum(lua_State* tolua_S)
{
  monster_race* self = (monster_race*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'dunnum'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->dunnum);
 return 1;
}

/* set function: dunnum of class  monster_race */
static int tolua_set_monster_race_dunnum(lua_State* tolua_S)
{
  monster_race* self = (monster_race*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'dunnum'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->dunnum = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: lives of class  monster_race */
static int tolua_get_monster_race_lives(lua_State* tolua_S)
{
  monster_race* self = (monster_race*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'lives'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->lives);
 return 1;
}

/* set function: lives of class  monster_race */
static int tolua_set_monster_race_lives(lua_State* tolua_S)
{
  monster_race* self = (monster_race*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'lives'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->lives = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: cursed of class  monster_race */
static int tolua_get_monster_race_cursed(lua_State* tolua_S)
{
  monster_race* self = (monster_race*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'cursed'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->cursed);
 return 1;
}

/* set function: cursed of class  monster_race */
static int tolua_set_monster_race_cursed(lua_State* tolua_S)
{
  monster_race* self = (monster_race*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'cursed'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->cursed = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: event_before_melee of class  monster_race */
static int tolua_get_monster_race_event_before_melee(lua_State* tolua_S)
{
  monster_race* self = (monster_race*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'event_before_melee'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->event_before_melee);
 return 1;
}

/* set function: event_before_melee of class  monster_race */
static int tolua_set_monster_race_event_before_melee(lua_State* tolua_S)
{
  monster_race* self = (monster_race*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'event_before_melee'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->event_before_melee = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: event_after_melee of class  monster_race */
static int tolua_get_monster_race_event_after_melee(lua_State* tolua_S)
{
  monster_race* self = (monster_race*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'event_after_melee'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->event_after_melee);
 return 1;
}

/* set function: event_after_melee of class  monster_race */
static int tolua_set_monster_race_event_after_melee(lua_State* tolua_S)
{
  monster_race* self = (monster_race*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'event_after_melee'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->event_after_melee = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: event_before_ranged of class  monster_race */
static int tolua_get_monster_race_event_before_ranged(lua_State* tolua_S)
{
  monster_race* self = (monster_race*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'event_before_ranged'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->event_before_ranged);
 return 1;
}

/* set function: event_before_ranged of class  monster_race */
static int tolua_set_monster_race_event_before_ranged(lua_State* tolua_S)
{
  monster_race* self = (monster_race*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'event_before_ranged'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->event_before_ranged = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: event_after_ranged of class  monster_race */
static int tolua_get_monster_race_event_after_ranged(lua_State* tolua_S)
{
  monster_race* self = (monster_race*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'event_after_ranged'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->event_after_ranged);
 return 1;
}

/* set function: event_after_ranged of class  monster_race */
static int tolua_set_monster_race_event_after_ranged(lua_State* tolua_S)
{
  monster_race* self = (monster_race*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'event_after_ranged'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->event_after_ranged = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: event_before_magic of class  monster_race */
static int tolua_get_monster_race_event_before_magic(lua_State* tolua_S)
{
  monster_race* self = (monster_race*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'event_before_magic'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->event_before_magic);
 return 1;
}

/* set function: event_before_magic of class  monster_race */
static int tolua_set_monster_race_event_before_magic(lua_State* tolua_S)
{
  monster_race* self = (monster_race*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'event_before_magic'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->event_before_magic = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: event_after_magic of class  monster_race */
static int tolua_get_monster_race_event_after_magic(lua_State* tolua_S)
{
  monster_race* self = (monster_race*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'event_after_magic'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->event_after_magic);
 return 1;
}

/* set function: event_after_magic of class  monster_race */
static int tolua_set_monster_race_event_after_magic(lua_State* tolua_S)
{
  monster_race* self = (monster_race*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'event_after_magic'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->event_after_magic = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: event_before_move of class  monster_race */
static int tolua_get_monster_race_event_before_move(lua_State* tolua_S)
{
  monster_race* self = (monster_race*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'event_before_move'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->event_before_move);
 return 1;
}

/* set function: event_before_move of class  monster_race */
static int tolua_set_monster_race_event_before_move(lua_State* tolua_S)
{
  monster_race* self = (monster_race*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'event_before_move'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->event_before_move = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: event_after_move of class  monster_race */
static int tolua_get_monster_race_event_after_move(lua_State* tolua_S)
{
  monster_race* self = (monster_race*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'event_after_move'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->event_after_move);
 return 1;
}

/* set function: event_after_move of class  monster_race */
static int tolua_set_monster_race_event_after_move(lua_State* tolua_S)
{
  monster_race* self = (monster_race*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'event_after_move'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->event_after_move = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: event_passive of class  monster_race */
static int tolua_get_monster_race_event_passive(lua_State* tolua_S)
{
  monster_race* self = (monster_race*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'event_passive'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->event_passive);
 return 1;
}

/* set function: event_passive of class  monster_race */
static int tolua_set_monster_race_event_passive(lua_State* tolua_S)
{
  monster_race* self = (monster_race*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'event_passive'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->event_passive = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: event_take_damages of class  monster_race */
static int tolua_get_monster_race_event_take_damages(lua_State* tolua_S)
{
  monster_race* self = (monster_race*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'event_take_damages'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->event_take_damages);
 return 1;
}

/* set function: event_take_damages of class  monster_race */
static int tolua_set_monster_race_event_take_damages(lua_State* tolua_S)
{
  monster_race* self = (monster_race*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'event_take_damages'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->event_take_damages = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: event_death of class  monster_race */
static int tolua_get_monster_race_event_death(lua_State* tolua_S)
{
  monster_race* self = (monster_race*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'event_death'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->event_death);
 return 1;
}

/* set function: event_death of class  monster_race */
static int tolua_set_monster_race_event_death(lua_State* tolua_S)
{
  monster_race* self = (monster_race*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'event_death'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->event_death = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: event_spawn of class  monster_race */
static int tolua_get_monster_race_event_spawn(lua_State* tolua_S)
{
  monster_race* self = (monster_race*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'event_spawn'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->event_spawn);
 return 1;
}

/* set function: event_spawn of class  monster_race */
static int tolua_set_monster_race_event_spawn(lua_State* tolua_S)
{
  monster_race* self = (monster_race*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'event_spawn'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->event_spawn = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: event_misc of class  monster_race */
static int tolua_get_monster_race_event_misc(lua_State* tolua_S)
{
  monster_race* self = (monster_race*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'event_misc'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->event_misc);
 return 1;
}

/* set function: event_misc of class  monster_race */
static int tolua_set_monster_race_event_misc(lua_State* tolua_S)
{
  monster_race* self = (monster_race*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'event_misc'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->event_misc = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: name of class  vault_type */
static int tolua_get_vault_type_name(lua_State* tolua_S)
{
  vault_type* self = (vault_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'name'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->name);
 return 1;
}

/* set function: name of class  vault_type */
static int tolua_set_vault_type_name(lua_State* tolua_S)
{
  vault_type* self = (vault_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'name'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->name = ((u32b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: text of class  vault_type */
static int tolua_get_vault_type_text(lua_State* tolua_S)
{
  vault_type* self = (vault_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'text'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->text);
 return 1;
}

/* set function: text of class  vault_type */
static int tolua_set_vault_type_text(lua_State* tolua_S)
{
  vault_type* self = (vault_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'text'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->text = ((u32b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: typ of class  vault_type */
static int tolua_get_vault_type_typ(lua_State* tolua_S)
{
  vault_type* self = (vault_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'typ'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->typ);
 return 1;
}

/* set function: typ of class  vault_type */
static int tolua_set_vault_type_typ(lua_State* tolua_S)
{
  vault_type* self = (vault_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'typ'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->typ = ((byte)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: rat of class  vault_type */
static int tolua_get_vault_type_rat(lua_State* tolua_S)
{
  vault_type* self = (vault_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'rat'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->rat);
 return 1;
}

/* set function: rat of class  vault_type */
static int tolua_set_vault_type_rat(lua_State* tolua_S)
{
  vault_type* self = (vault_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'rat'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->rat = ((byte)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: hgt of class  vault_type */
static int tolua_get_vault_type_hgt(lua_State* tolua_S)
{
  vault_type* self = (vault_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'hgt'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->hgt);
 return 1;
}

/* set function: hgt of class  vault_type */
static int tolua_set_vault_type_hgt(lua_State* tolua_S)
{
  vault_type* self = (vault_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'hgt'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->hgt = ((byte)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: wid of class  vault_type */
static int tolua_get_vault_type_wid(lua_State* tolua_S)
{
  vault_type* self = (vault_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'wid'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->wid);
 return 1;
}

/* set function: wid of class  vault_type */
static int tolua_set_vault_type_wid(lua_State* tolua_S)
{
  vault_type* self = (vault_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'wid'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->wid = ((byte)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: lvl of class  vault_type */
static int tolua_get_vault_type_lvl(lua_State* tolua_S)
{
  vault_type* self = (vault_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'lvl'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->lvl);
 return 1;
}

/* set function: lvl of class  vault_type */
static int tolua_set_vault_type_lvl(lua_State* tolua_S)
{
  vault_type* self = (vault_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'lvl'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->lvl = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: dun_type of class  vault_type */
static int tolua_get_vault_type_dun_type(lua_State* tolua_S)
{
  vault_type* self = (vault_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'dun_type'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->dun_type);
 return 1;
}

/* set function: dun_type of class  vault_type */
static int tolua_set_vault_type_dun_type(lua_State* tolua_S)
{
  vault_type* self = (vault_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'dun_type'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->dun_type = ((byte)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: mon of class  vault_type */
static int tolua_get_types_vault_type_mon(lua_State* tolua_S)
{
 int tolua_index;
  vault_type* self;
 lua_pushstring(tolua_S,".self");
 lua_rawget(tolua_S,1);
 self = (vault_type*)  lua_touserdata(tolua_S,-1);
#ifndef TOLUA_RELEASE
 {
 tolua_Error tolua_err;
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in array indexing.",&tolua_err);
 }
#endif
 tolua_index = (int)tolua_tonumber(tolua_S,2,0)-1;
#ifndef TOLUA_RELEASE
 if (tolua_index<0 || tolua_index>=10)
 tolua_error(tolua_S,"array indexing out of range.",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->mon[tolua_index]);
 return 1;
}

/* set function: mon of class  vault_type */
static int tolua_set_types_vault_type_mon(lua_State* tolua_S)
{
 int tolua_index;
  vault_type* self;
 lua_pushstring(tolua_S,".self");
 lua_rawget(tolua_S,1);
 self = (vault_type*)  lua_touserdata(tolua_S,-1);
#ifndef TOLUA_RELEASE
 {
 tolua_Error tolua_err;
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in array indexing.",&tolua_err);
 }
#endif
 tolua_index = (int)tolua_tonumber(tolua_S,2,0)-1;
#ifndef TOLUA_RELEASE
 if (tolua_index<0 || tolua_index>=10)
 tolua_error(tolua_S,"array indexing out of range.",NULL);
#endif
  self->mon[tolua_index] = ((s16b)  tolua_tonumber(tolua_S,3,0));
 return 0;
}

/* get function: item of class  vault_type */
static int tolua_get_types_vault_type_item(lua_State* tolua_S)
{
 int tolua_index;
  vault_type* self;
 lua_pushstring(tolua_S,".self");
 lua_rawget(tolua_S,1);
 self = (vault_type*)  lua_touserdata(tolua_S,-1);
#ifndef TOLUA_RELEASE
 {
 tolua_Error tolua_err;
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in array indexing.",&tolua_err);
 }
#endif
 tolua_index = (int)tolua_tonumber(tolua_S,2,0)-1;
#ifndef TOLUA_RELEASE
 if (tolua_index<0 || tolua_index>=3)
 tolua_error(tolua_S,"array indexing out of range.",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->item[tolua_index]);
 return 1;
}

/* set function: item of class  vault_type */
static int tolua_set_types_vault_type_item(lua_State* tolua_S)
{
 int tolua_index;
  vault_type* self;
 lua_pushstring(tolua_S,".self");
 lua_rawget(tolua_S,1);
 self = (vault_type*)  lua_touserdata(tolua_S,-1);
#ifndef TOLUA_RELEASE
 {
 tolua_Error tolua_err;
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in array indexing.",&tolua_err);
 }
#endif
 tolua_index = (int)tolua_tonumber(tolua_S,2,0)-1;
#ifndef TOLUA_RELEASE
 if (tolua_index<0 || tolua_index>=3)
 tolua_error(tolua_S,"array indexing out of range.",NULL);
#endif
  self->item[tolua_index] = ((int)  tolua_tonumber(tolua_S,3,0));
 return 0;
}

/* get function: probability of class  trap_type */
static int tolua_get_trap_type_probability(lua_State* tolua_S)
{
  trap_type* self = (trap_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'probability'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->probability);
 return 1;
}

/* set function: probability of class  trap_type */
static int tolua_set_trap_type_probability(lua_State* tolua_S)
{
  trap_type* self = (trap_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'probability'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->probability = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: another of class  trap_type */
static int tolua_get_trap_type_another(lua_State* tolua_S)
{
  trap_type* self = (trap_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'another'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->another);
 return 1;
}

/* set function: another of class  trap_type */
static int tolua_set_trap_type_another(lua_State* tolua_S)
{
  trap_type* self = (trap_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'another'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->another = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: p1valinc of class  trap_type */
static int tolua_get_trap_type_p1valinc(lua_State* tolua_S)
{
  trap_type* self = (trap_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'p1valinc'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->p1valinc);
 return 1;
}

/* set function: p1valinc of class  trap_type */
static int tolua_set_trap_type_p1valinc(lua_State* tolua_S)
{
  trap_type* self = (trap_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'p1valinc'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->p1valinc = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: difficulty of class  trap_type */
static int tolua_get_trap_type_difficulty(lua_State* tolua_S)
{
  trap_type* self = (trap_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'difficulty'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->difficulty);
 return 1;
}

/* set function: difficulty of class  trap_type */
static int tolua_set_trap_type_difficulty(lua_State* tolua_S)
{
  trap_type* self = (trap_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'difficulty'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->difficulty = ((byte)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: minlevel of class  trap_type */
static int tolua_get_trap_type_minlevel(lua_State* tolua_S)
{
  trap_type* self = (trap_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'minlevel'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->minlevel);
 return 1;
}

/* set function: minlevel of class  trap_type */
static int tolua_set_trap_type_minlevel(lua_State* tolua_S)
{
  trap_type* self = (trap_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'minlevel'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->minlevel = ((byte)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: color of class  trap_type */
static int tolua_get_trap_type_color(lua_State* tolua_S)
{
  trap_type* self = (trap_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'color'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->color);
 return 1;
}

/* set function: color of class  trap_type */
static int tolua_set_trap_type_color(lua_State* tolua_S)
{
  trap_type* self = (trap_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'color'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->color = ((byte)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: flags of class  trap_type */
static int tolua_get_trap_type_flags(lua_State* tolua_S)
{
  trap_type* self = (trap_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'flags'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->flags);
 return 1;
}

/* set function: flags of class  trap_type */
static int tolua_set_trap_type_flags(lua_State* tolua_S)
{
  trap_type* self = (trap_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'flags'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->flags = ((u32b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: ident of class  trap_type */
static int tolua_get_trap_type_ident(lua_State* tolua_S)
{
  trap_type* self = (trap_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'ident'",NULL);
#endif
 tolua_pushboolean(tolua_S,(bool)self->ident);
 return 1;
}

/* set function: ident of class  trap_type */
static int tolua_set_trap_type_ident(lua_State* tolua_S)
{
  trap_type* self = (trap_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'ident'",NULL);
 if (!tolua_isboolean(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->ident = ((bool)  tolua_toboolean(tolua_S,2,0));
 return 0;
}

/* get function: known of class  trap_type */
static int tolua_get_trap_type_known(lua_State* tolua_S)
{
  trap_type* self = (trap_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'known'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->known);
 return 1;
}

/* set function: known of class  trap_type */
static int tolua_set_trap_type_known(lua_State* tolua_S)
{
  trap_type* self = (trap_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'known'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->known = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: name of class  trap_type */
static int tolua_get_trap_type_name(lua_State* tolua_S)
{
  trap_type* self = (trap_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'name'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->name);
 return 1;
}

/* set function: name of class  trap_type */
static int tolua_set_trap_type_name(lua_State* tolua_S)
{
  trap_type* self = (trap_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'name'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->name = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: dd of class  trap_type */
static int tolua_get_trap_type_dd(lua_State* tolua_S)
{
  trap_type* self = (trap_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'dd'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->dd);
 return 1;
}

/* set function: dd of class  trap_type */
static int tolua_set_trap_type_dd(lua_State* tolua_S)
{
  trap_type* self = (trap_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'dd'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->dd = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: ds of class  trap_type */
static int tolua_get_trap_type_ds(lua_State* tolua_S)
{
  trap_type* self = (trap_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'ds'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->ds);
 return 1;
}

/* set function: ds of class  trap_type */
static int tolua_set_trap_type_ds(lua_State* tolua_S)
{
  trap_type* self = (trap_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'ds'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->ds = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: text of class  trap_type */
static int tolua_get_trap_type_text(lua_State* tolua_S)
{
  trap_type* self = (trap_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'text'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->text);
 return 1;
}

/* set function: text of class  trap_type */
static int tolua_set_trap_type_text(lua_State* tolua_S)
{
  trap_type* self = (trap_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'text'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->text = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: info of class  cave_type */
static int tolua_get_cave_type_info(lua_State* tolua_S)
{
  cave_type* self = (cave_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'info'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->info);
 return 1;
}

/* set function: info of class  cave_type */
static int tolua_set_cave_type_info(lua_State* tolua_S)
{
  cave_type* self = (cave_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'info'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->info = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: feat of class  cave_type */
static int tolua_get_cave_type_feat(lua_State* tolua_S)
{
  cave_type* self = (cave_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'feat'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->feat);
 return 1;
}

/* set function: feat of class  cave_type */
static int tolua_set_cave_type_feat(lua_State* tolua_S)
{
  cave_type* self = (cave_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'feat'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->feat = ((byte)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: o_idx of class  cave_type */
static int tolua_get_cave_type_o_idx(lua_State* tolua_S)
{
  cave_type* self = (cave_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'o_idx'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->o_idx);
 return 1;
}

/* set function: o_idx of class  cave_type */
static int tolua_set_cave_type_o_idx(lua_State* tolua_S)
{
  cave_type* self = (cave_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'o_idx'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->o_idx = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: m_idx of class  cave_type */
static int tolua_get_cave_type_m_idx(lua_State* tolua_S)
{
  cave_type* self = (cave_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'm_idx'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->m_idx);
 return 1;
}

/* set function: m_idx of class  cave_type */
static int tolua_set_cave_type_m_idx(lua_State* tolua_S)
{
  cave_type* self = (cave_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'm_idx'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->m_idx = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: t_idx of class  cave_type */
static int tolua_get_cave_type_t_idx(lua_State* tolua_S)
{
  cave_type* self = (cave_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 't_idx'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->t_idx);
 return 1;
}

/* set function: t_idx of class  cave_type */
static int tolua_set_cave_type_t_idx(lua_State* tolua_S)
{
  cave_type* self = (cave_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 't_idx'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->t_idx = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: special of class  cave_type */
static int tolua_get_cave_type_special(lua_State* tolua_S)
{
  cave_type* self = (cave_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'special'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->special);
 return 1;
}

/* set function: special of class  cave_type */
static int tolua_set_cave_type_special(lua_State* tolua_S)
{
  cave_type* self = (cave_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'special'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->special = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: inscription of class  cave_type */
static int tolua_get_cave_type_inscription(lua_State* tolua_S)
{
  cave_type* self = (cave_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'inscription'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->inscription);
 return 1;
}

/* set function: inscription of class  cave_type */
static int tolua_set_cave_type_inscription(lua_State* tolua_S)
{
  cave_type* self = (cave_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'inscription'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->inscription = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: mana of class  cave_type */
static int tolua_get_cave_type_mana(lua_State* tolua_S)
{
  cave_type* self = (cave_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'mana'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->mana);
 return 1;
}

/* set function: mana of class  cave_type */
static int tolua_set_cave_type_mana(lua_State* tolua_S)
{
  cave_type* self = (cave_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'mana'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->mana = ((byte)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: mimic of class  cave_type */
static int tolua_get_cave_type_mimic(lua_State* tolua_S)
{
  cave_type* self = (cave_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'mimic'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->mimic);
 return 1;
}

/* set function: mimic of class  cave_type */
static int tolua_set_cave_type_mimic(lua_State* tolua_S)
{
  cave_type* self = (cave_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'mimic'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->mimic = ((byte)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: cost of class  cave_type */
static int tolua_get_cave_type_cost(lua_State* tolua_S)
{
  cave_type* self = (cave_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'cost'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->cost);
 return 1;
}

/* set function: cost of class  cave_type */
static int tolua_set_cave_type_cost(lua_State* tolua_S)
{
  cave_type* self = (cave_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'cost'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->cost = ((byte)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: when of class  cave_type */
static int tolua_get_cave_type_when(lua_State* tolua_S)
{
  cave_type* self = (cave_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'when'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->when);
 return 1;
}

/* set function: when of class  cave_type */
static int tolua_set_cave_type_when(lua_State* tolua_S)
{
  cave_type* self = (cave_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'when'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->when = ((byte)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: field_damage of class  cave_type */
static int tolua_get_cave_type_field_damage(lua_State* tolua_S)
{
  cave_type* self = (cave_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'field_damage'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->field_damage);
 return 1;
}

/* set function: field_damage of class  cave_type */
static int tolua_set_cave_type_field_damage(lua_State* tolua_S)
{
  cave_type* self = (cave_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'field_damage'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->field_damage = ((s32b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: event of class  cave_type */
static int tolua_get_cave_type_event(lua_State* tolua_S)
{
  cave_type* self = (cave_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'event'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->event);
 return 1;
}

/* set function: event of class  cave_type */
static int tolua_set_cave_type_event(lua_State* tolua_S)
{
  cave_type* self = (cave_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'event'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->event = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: eventtype of class  cave_type */
static int tolua_get_cave_type_eventtype(lua_State* tolua_S)
{
  cave_type* self = (cave_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'eventtype'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->eventtype);
 return 1;
}

/* set function: eventtype of class  cave_type */
static int tolua_set_cave_type_eventtype(lua_State* tolua_S)
{
  cave_type* self = (cave_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'eventtype'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->eventtype = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: eventextra of class  cave_type */
static int tolua_get_cave_type_eventextra(lua_State* tolua_S)
{
  cave_type* self = (cave_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'eventextra'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->eventextra);
 return 1;
}

/* set function: eventextra of class  cave_type */
static int tolua_set_cave_type_eventextra(lua_State* tolua_S)
{
  cave_type* self = (cave_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'eventextra'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->eventextra = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: eventextra2 of class  cave_type */
static int tolua_get_cave_type_eventextra2(lua_State* tolua_S)
{
  cave_type* self = (cave_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'eventextra2'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->eventextra2);
 return 1;
}

/* set function: eventextra2 of class  cave_type */
static int tolua_set_cave_type_eventextra2(lua_State* tolua_S)
{
  cave_type* self = (cave_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'eventextra2'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->eventextra2 = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: eventcond of class  cave_type */
static int tolua_get_cave_type_eventcond(lua_State* tolua_S)
{
  cave_type* self = (cave_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'eventcond'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->eventcond);
 return 1;
}

/* set function: eventcond of class  cave_type */
static int tolua_set_cave_type_eventcond(lua_State* tolua_S)
{
  cave_type* self = (cave_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'eventcond'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->eventcond = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: eventcondval of class  cave_type */
static int tolua_get_cave_type_eventcondval(lua_State* tolua_S)
{
  cave_type* self = (cave_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'eventcondval'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->eventcondval);
 return 1;
}

/* set function: eventcondval of class  cave_type */
static int tolua_set_cave_type_eventcondval(lua_State* tolua_S)
{
  cave_type* self = (cave_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'eventcondval'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->eventcondval = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: eventset of class  cave_type */
static int tolua_get_cave_type_eventset(lua_State* tolua_S)
{
  cave_type* self = (cave_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'eventset'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->eventset);
 return 1;
}

/* set function: eventset of class  cave_type */
static int tolua_set_cave_type_eventset(lua_State* tolua_S)
{
  cave_type* self = (cave_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'eventset'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->eventset = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: eventsetval of class  cave_type */
static int tolua_get_cave_type_eventsetval(lua_State* tolua_S)
{
  cave_type* self = (cave_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'eventsetval'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->eventsetval);
 return 1;
}

/* set function: eventsetval of class  cave_type */
static int tolua_set_cave_type_eventsetval(lua_State* tolua_S)
{
  cave_type* self = (cave_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'eventsetval'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->eventsetval = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: script of class  cave_type */
static int tolua_get_cave_type_script(lua_State* tolua_S)
{
  cave_type* self = (cave_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'script'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->script);
 return 1;
}

/* set function: script of class  cave_type */
static int tolua_set_cave_type_script(lua_State* tolua_S)
{
  cave_type* self = (cave_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'script'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->script = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: owner of class  cave_type */
static int tolua_get_cave_type_owner(lua_State* tolua_S)
{
  cave_type* self = (cave_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'owner'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->owner);
 return 1;
}

/* set function: owner of class  cave_type */
static int tolua_set_cave_type_owner(lua_State* tolua_S)
{
  cave_type* self = (cave_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'owner'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->owner = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: script_name of class  cave_type */
static int tolua_get_cave_type_script_name(lua_State* tolua_S)
{
  cave_type* self = (cave_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'script_name'",NULL);
#endif
 tolua_pushstring(tolua_S,(const char*)self->script_name);
 return 1;
}

/* set function: script_name of class  cave_type */
static int tolua_set_cave_type_script_name(lua_State* tolua_S)
{
  cave_type* self = (cave_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'script_name'",NULL);
 if (!tolua_isstring(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
 strncpy(self->script_name,tolua_tostring(tolua_S,2,0),120-1);
 return 0;
}

/* get function: k_idx of class  object_type */
static int tolua_get_object_type_k_idx(lua_State* tolua_S)
{
  object_type* self = (object_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'k_idx'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->k_idx);
 return 1;
}

/* set function: k_idx of class  object_type */
static int tolua_set_object_type_k_idx(lua_State* tolua_S)
{
  object_type* self = (object_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'k_idx'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->k_idx = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: iy of class  object_type */
static int tolua_get_object_type_iy(lua_State* tolua_S)
{
  object_type* self = (object_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'iy'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->iy);
 return 1;
}

/* set function: iy of class  object_type */
static int tolua_set_object_type_iy(lua_State* tolua_S)
{
  object_type* self = (object_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'iy'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->iy = ((byte)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: ix of class  object_type */
static int tolua_get_object_type_ix(lua_State* tolua_S)
{
  object_type* self = (object_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'ix'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->ix);
 return 1;
}

/* set function: ix of class  object_type */
static int tolua_set_object_type_ix(lua_State* tolua_S)
{
  object_type* self = (object_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'ix'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->ix = ((byte)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: tval of class  object_type */
static int tolua_get_object_type_tval(lua_State* tolua_S)
{
  object_type* self = (object_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'tval'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->tval);
 return 1;
}

/* set function: tval of class  object_type */
static int tolua_set_object_type_tval(lua_State* tolua_S)
{
  object_type* self = (object_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'tval'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->tval = ((byte)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: sval of class  object_type */
static int tolua_get_object_type_sval(lua_State* tolua_S)
{
  object_type* self = (object_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'sval'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->sval);
 return 1;
}

/* set function: sval of class  object_type */
static int tolua_set_object_type_sval(lua_State* tolua_S)
{
  object_type* self = (object_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'sval'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->sval = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: pval of class  object_type */
static int tolua_get_object_type_pval(lua_State* tolua_S)
{
  object_type* self = (object_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'pval'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->pval);
 return 1;
}

/* set function: pval of class  object_type */
static int tolua_set_object_type_pval(lua_State* tolua_S)
{
  object_type* self = (object_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'pval'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->pval = ((s32b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: pval2 of class  object_type */
static int tolua_get_object_type_pval2(lua_State* tolua_S)
{
  object_type* self = (object_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'pval2'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->pval2);
 return 1;
}

/* set function: pval2 of class  object_type */
static int tolua_set_object_type_pval2(lua_State* tolua_S)
{
  object_type* self = (object_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'pval2'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->pval2 = ((s32b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: pval3 of class  object_type */
static int tolua_get_object_type_pval3(lua_State* tolua_S)
{
  object_type* self = (object_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'pval3'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->pval3);
 return 1;
}

/* set function: pval3 of class  object_type */
static int tolua_set_object_type_pval3(lua_State* tolua_S)
{
  object_type* self = (object_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'pval3'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->pval3 = ((s32b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: brandtype of class  object_type */
static int tolua_get_object_type_brandtype(lua_State* tolua_S)
{
  object_type* self = (object_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'brandtype'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->brandtype);
 return 1;
}

/* set function: brandtype of class  object_type */
static int tolua_set_object_type_brandtype(lua_State* tolua_S)
{
  object_type* self = (object_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'brandtype'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->brandtype = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: branddam of class  object_type */
static int tolua_get_object_type_branddam(lua_State* tolua_S)
{
  object_type* self = (object_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'branddam'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->branddam);
 return 1;
}

/* set function: branddam of class  object_type */
static int tolua_set_object_type_branddam(lua_State* tolua_S)
{
  object_type* self = (object_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'branddam'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->branddam = ((s32b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: brandrad of class  object_type */
static int tolua_get_object_type_brandrad(lua_State* tolua_S)
{
  object_type* self = (object_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'brandrad'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->brandrad);
 return 1;
}

/* set function: brandrad of class  object_type */
static int tolua_set_object_type_brandrad(lua_State* tolua_S)
{
  object_type* self = (object_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'brandrad'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->brandrad = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: resistances of class  object_type */
static int tolua_get_types_object_type_resistances(lua_State* tolua_S)
{
 int tolua_index;
  object_type* self;
 lua_pushstring(tolua_S,".self");
 lua_rawget(tolua_S,1);
 self = (object_type*)  lua_touserdata(tolua_S,-1);
#ifndef TOLUA_RELEASE
 {
 tolua_Error tolua_err;
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in array indexing.",&tolua_err);
 }
#endif
 tolua_index = (int)tolua_tonumber(tolua_S,2,0)-1;
#ifndef TOLUA_RELEASE
 if (tolua_index<0 || tolua_index>=MAX_RESIST)
 tolua_error(tolua_S,"array indexing out of range.",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->resistances[tolua_index]);
 return 1;
}

/* set function: resistances of class  object_type */
static int tolua_set_types_object_type_resistances(lua_State* tolua_S)
{
 int tolua_index;
  object_type* self;
 lua_pushstring(tolua_S,".self");
 lua_rawget(tolua_S,1);
 self = (object_type*)  lua_touserdata(tolua_S,-1);
#ifndef TOLUA_RELEASE
 {
 tolua_Error tolua_err;
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in array indexing.",&tolua_err);
 }
#endif
 tolua_index = (int)tolua_tonumber(tolua_S,2,0)-1;
#ifndef TOLUA_RELEASE
 if (tolua_index<0 || tolua_index>=MAX_RESIST)
 tolua_error(tolua_S,"array indexing out of range.",NULL);
#endif
  self->resistances[tolua_index] = ((s16b)  tolua_tonumber(tolua_S,3,0));
 return 0;
}

/* get function: itemtype of class  object_type */
static int tolua_get_object_type_itemtype(lua_State* tolua_S)
{
  object_type* self = (object_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'itemtype'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->itemtype);
 return 1;
}

/* set function: itemtype of class  object_type */
static int tolua_set_object_type_itemtype(lua_State* tolua_S)
{
  object_type* self = (object_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'itemtype'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->itemtype = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: itemskill of class  object_type */
static int tolua_get_object_type_itemskill(lua_State* tolua_S)
{
  object_type* self = (object_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'itemskill'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->itemskill);
 return 1;
}

/* set function: itemskill of class  object_type */
static int tolua_set_object_type_itemskill(lua_State* tolua_S)
{
  object_type* self = (object_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'itemskill'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->itemskill = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: statsbonus of class  object_type */
static int tolua_get_types_object_type_statsbonus(lua_State* tolua_S)
{
 int tolua_index;
  object_type* self;
 lua_pushstring(tolua_S,".self");
 lua_rawget(tolua_S,1);
 self = (object_type*)  lua_touserdata(tolua_S,-1);
#ifndef TOLUA_RELEASE
 {
 tolua_Error tolua_err;
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in array indexing.",&tolua_err);
 }
#endif
 tolua_index = (int)tolua_tonumber(tolua_S,2,0)-1;
#ifndef TOLUA_RELEASE
 if (tolua_index<0 || tolua_index>=6)
 tolua_error(tolua_S,"array indexing out of range.",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->statsbonus[tolua_index]);
 return 1;
}

/* set function: statsbonus of class  object_type */
static int tolua_set_types_object_type_statsbonus(lua_State* tolua_S)
{
 int tolua_index;
  object_type* self;
 lua_pushstring(tolua_S,".self");
 lua_rawget(tolua_S,1);
 self = (object_type*)  lua_touserdata(tolua_S,-1);
#ifndef TOLUA_RELEASE
 {
 tolua_Error tolua_err;
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in array indexing.",&tolua_err);
 }
#endif
 tolua_index = (int)tolua_tonumber(tolua_S,2,0)-1;
#ifndef TOLUA_RELEASE
 if (tolua_index<0 || tolua_index>=6)
 tolua_error(tolua_S,"array indexing out of range.",NULL);
#endif
  self->statsbonus[tolua_index] = ((s16b)  tolua_tonumber(tolua_S,3,0));
 return 0;
}

/* get function: skillsbonus of class  object_type */
static int tolua_get_types_object_type_skillsbonus(lua_State* tolua_S)
{
 int tolua_index;
  object_type* self;
 lua_pushstring(tolua_S,".self");
 lua_rawget(tolua_S,1);
 self = (object_type*)  lua_touserdata(tolua_S,-1);
#ifndef TOLUA_RELEASE
 {
 tolua_Error tolua_err;
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in array indexing.",&tolua_err);
 }
#endif
 tolua_index = (int)tolua_tonumber(tolua_S,2,0)-1;
#ifndef TOLUA_RELEASE
 if (tolua_index<0 || tolua_index>=SKILL_MAX)
 tolua_error(tolua_S,"array indexing out of range.",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->skillsbonus[tolua_index]);
 return 1;
}

/* set function: skillsbonus of class  object_type */
static int tolua_set_types_object_type_skillsbonus(lua_State* tolua_S)
{
 int tolua_index;
  object_type* self;
 lua_pushstring(tolua_S,".self");
 lua_rawget(tolua_S,1);
 self = (object_type*)  lua_touserdata(tolua_S,-1);
#ifndef TOLUA_RELEASE
 {
 tolua_Error tolua_err;
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in array indexing.",&tolua_err);
 }
#endif
 tolua_index = (int)tolua_tonumber(tolua_S,2,0)-1;
#ifndef TOLUA_RELEASE
 if (tolua_index<0 || tolua_index>=SKILL_MAX)
 tolua_error(tolua_S,"array indexing out of range.",NULL);
#endif
  self->skillsbonus[tolua_index] = ((s16b)  tolua_tonumber(tolua_S,3,0));
 return 0;
}

/* get function: extrablows of class  object_type */
static int tolua_get_object_type_extrablows(lua_State* tolua_S)
{
  object_type* self = (object_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'extrablows'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->extrablows);
 return 1;
}

/* set function: extrablows of class  object_type */
static int tolua_set_object_type_extrablows(lua_State* tolua_S)
{
  object_type* self = (object_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'extrablows'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->extrablows = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: extrashots of class  object_type */
static int tolua_get_object_type_extrashots(lua_State* tolua_S)
{
  object_type* self = (object_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'extrashots'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->extrashots);
 return 1;
}

/* set function: extrashots of class  object_type */
static int tolua_set_object_type_extrashots(lua_State* tolua_S)
{
  object_type* self = (object_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'extrashots'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->extrashots = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: speedbonus of class  object_type */
static int tolua_get_object_type_speedbonus(lua_State* tolua_S)
{
  object_type* self = (object_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'speedbonus'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->speedbonus);
 return 1;
}

/* set function: speedbonus of class  object_type */
static int tolua_set_object_type_speedbonus(lua_State* tolua_S)
{
  object_type* self = (object_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'speedbonus'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->speedbonus = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: lifebonus of class  object_type */
static int tolua_get_object_type_lifebonus(lua_State* tolua_S)
{
  object_type* self = (object_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'lifebonus'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->lifebonus);
 return 1;
}

/* set function: lifebonus of class  object_type */
static int tolua_set_object_type_lifebonus(lua_State* tolua_S)
{
  object_type* self = (object_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'lifebonus'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->lifebonus = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: manabonus of class  object_type */
static int tolua_get_object_type_manabonus(lua_State* tolua_S)
{
  object_type* self = (object_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'manabonus'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->manabonus);
 return 1;
}

/* set function: manabonus of class  object_type */
static int tolua_set_object_type_manabonus(lua_State* tolua_S)
{
  object_type* self = (object_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'manabonus'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->manabonus = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: infravision of class  object_type */
static int tolua_get_object_type_infravision(lua_State* tolua_S)
{
  object_type* self = (object_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'infravision'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->infravision);
 return 1;
}

/* set function: infravision of class  object_type */
static int tolua_set_object_type_infravision(lua_State* tolua_S)
{
  object_type* self = (object_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'infravision'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->infravision = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: spellbonus of class  object_type */
static int tolua_get_object_type_spellbonus(lua_State* tolua_S)
{
  object_type* self = (object_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'spellbonus'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->spellbonus);
 return 1;
}

/* set function: spellbonus of class  object_type */
static int tolua_set_object_type_spellbonus(lua_State* tolua_S)
{
  object_type* self = (object_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'spellbonus'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->spellbonus = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: invisibility of class  object_type */
static int tolua_get_object_type_invisibility(lua_State* tolua_S)
{
  object_type* self = (object_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'invisibility'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->invisibility);
 return 1;
}

/* set function: invisibility of class  object_type */
static int tolua_set_object_type_invisibility(lua_State* tolua_S)
{
  object_type* self = (object_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'invisibility'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->invisibility = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: light of class  object_type */
static int tolua_get_object_type_light(lua_State* tolua_S)
{
  object_type* self = (object_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'light'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->light);
 return 1;
}

/* set function: light of class  object_type */
static int tolua_set_object_type_light(lua_State* tolua_S)
{
  object_type* self = (object_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'light'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->light = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: extra1 of class  object_type */
static int tolua_get_object_type_extra1(lua_State* tolua_S)
{
  object_type* self = (object_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'extra1'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->extra1);
 return 1;
}

/* set function: extra1 of class  object_type */
static int tolua_set_object_type_extra1(lua_State* tolua_S)
{
  object_type* self = (object_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'extra1'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->extra1 = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: extra2 of class  object_type */
static int tolua_get_object_type_extra2(lua_State* tolua_S)
{
  object_type* self = (object_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'extra2'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->extra2);
 return 1;
}

/* set function: extra2 of class  object_type */
static int tolua_set_object_type_extra2(lua_State* tolua_S)
{
  object_type* self = (object_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'extra2'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->extra2 = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: extra3 of class  object_type */
static int tolua_get_object_type_extra3(lua_State* tolua_S)
{
  object_type* self = (object_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'extra3'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->extra3);
 return 1;
}

/* set function: extra3 of class  object_type */
static int tolua_set_object_type_extra3(lua_State* tolua_S)
{
  object_type* self = (object_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'extra3'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->extra3 = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: extra4 of class  object_type */
static int tolua_get_object_type_extra4(lua_State* tolua_S)
{
  object_type* self = (object_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'extra4'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->extra4);
 return 1;
}

/* set function: extra4 of class  object_type */
static int tolua_set_object_type_extra4(lua_State* tolua_S)
{
  object_type* self = (object_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'extra4'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->extra4 = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: extra5 of class  object_type */
static int tolua_get_object_type_extra5(lua_State* tolua_S)
{
  object_type* self = (object_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'extra5'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->extra5);
 return 1;
}

/* set function: extra5 of class  object_type */
static int tolua_set_object_type_extra5(lua_State* tolua_S)
{
  object_type* self = (object_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'extra5'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->extra5 = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: reflect of class  object_type */
static int tolua_get_object_type_reflect(lua_State* tolua_S)
{
  object_type* self = (object_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'reflect'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->reflect);
 return 1;
}

/* set function: reflect of class  object_type */
static int tolua_set_object_type_reflect(lua_State* tolua_S)
{
  object_type* self = (object_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'reflect'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->reflect = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: cursed of class  object_type */
static int tolua_get_object_type_cursed(lua_State* tolua_S)
{
  object_type* self = (object_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'cursed'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->cursed);
 return 1;
}

/* set function: cursed of class  object_type */
static int tolua_set_object_type_cursed(lua_State* tolua_S)
{
  object_type* self = (object_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'cursed'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->cursed = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: tweakpoints of class  object_type */
static int tolua_get_object_type_tweakpoints(lua_State* tolua_S)
{
  object_type* self = (object_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'tweakpoints'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->tweakpoints);
 return 1;
}

/* set function: tweakpoints of class  object_type */
static int tolua_set_object_type_tweakpoints(lua_State* tolua_S)
{
  object_type* self = (object_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'tweakpoints'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->tweakpoints = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: disabled of class  object_type */
static int tolua_get_object_type_disabled(lua_State* tolua_S)
{
  object_type* self = (object_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'disabled'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->disabled);
 return 1;
}

/* set function: disabled of class  object_type */
static int tolua_set_object_type_disabled(lua_State* tolua_S)
{
  object_type* self = (object_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'disabled'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->disabled = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: discount of class  object_type */
static int tolua_get_object_type_discount(lua_State* tolua_S)
{
  object_type* self = (object_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'discount'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->discount);
 return 1;
}

/* set function: discount of class  object_type */
static int tolua_set_object_type_discount(lua_State* tolua_S)
{
  object_type* self = (object_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'discount'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->discount = ((byte)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: number of class  object_type */
static int tolua_get_object_type_number(lua_State* tolua_S)
{
  object_type* self = (object_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'number'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->number);
 return 1;
}

/* set function: number of class  object_type */
static int tolua_set_object_type_number(lua_State* tolua_S)
{
  object_type* self = (object_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'number'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->number = ((byte)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: weight of class  object_type */
static int tolua_get_object_type_weight(lua_State* tolua_S)
{
  object_type* self = (object_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'weight'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->weight);
 return 1;
}

/* set function: weight of class  object_type */
static int tolua_set_object_type_weight(lua_State* tolua_S)
{
  object_type* self = (object_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'weight'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->weight = ((s32b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: level of class  object_type */
static int tolua_get_object_type_level(lua_State* tolua_S)
{
  object_type* self = (object_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'level'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->level);
 return 1;
}

/* set function: level of class  object_type */
static int tolua_set_object_type_level(lua_State* tolua_S)
{
  object_type* self = (object_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'level'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->level = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: kills of class  object_type */
static int tolua_get_object_type_kills(lua_State* tolua_S)
{
  object_type* self = (object_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'kills'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->kills);
 return 1;
}

/* set function: kills of class  object_type */
static int tolua_set_object_type_kills(lua_State* tolua_S)
{
  object_type* self = (object_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'kills'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->kills = ((s32b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: name1 of class  object_type */
static int tolua_get_object_type_name1(lua_State* tolua_S)
{
  object_type* self = (object_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'name1'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->name1);
 return 1;
}

/* set function: name1 of class  object_type */
static int tolua_set_object_type_name1(lua_State* tolua_S)
{
  object_type* self = (object_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'name1'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->name1 = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: name2 of class  object_type */
static int tolua_get_object_type_name2(lua_State* tolua_S)
{
  object_type* self = (object_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'name2'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->name2);
 return 1;
}

/* set function: name2 of class  object_type */
static int tolua_set_object_type_name2(lua_State* tolua_S)
{
  object_type* self = (object_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'name2'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->name2 = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: xtra1 of class  object_type */
static int tolua_get_object_type_xtra1(lua_State* tolua_S)
{
  object_type* self = (object_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'xtra1'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->xtra1);
 return 1;
}

/* set function: xtra1 of class  object_type */
static int tolua_set_object_type_xtra1(lua_State* tolua_S)
{
  object_type* self = (object_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'xtra1'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->xtra1 = ((s32b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: xtra2 of class  object_type */
static int tolua_get_object_type_xtra2(lua_State* tolua_S)
{
  object_type* self = (object_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'xtra2'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->xtra2);
 return 1;
}

/* set function: xtra2 of class  object_type */
static int tolua_set_object_type_xtra2(lua_State* tolua_S)
{
  object_type* self = (object_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'xtra2'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->xtra2 = ((byte)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: to_h of class  object_type */
static int tolua_get_object_type_to_h(lua_State* tolua_S)
{
  object_type* self = (object_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'to_h'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->to_h);
 return 1;
}

/* set function: to_h of class  object_type */
static int tolua_set_object_type_to_h(lua_State* tolua_S)
{
  object_type* self = (object_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'to_h'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->to_h = ((s32b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: to_d of class  object_type */
static int tolua_get_object_type_to_d(lua_State* tolua_S)
{
  object_type* self = (object_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'to_d'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->to_d);
 return 1;
}

/* set function: to_d of class  object_type */
static int tolua_set_object_type_to_d(lua_State* tolua_S)
{
  object_type* self = (object_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'to_d'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->to_d = ((s32b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: to_a of class  object_type */
static int tolua_get_object_type_to_a(lua_State* tolua_S)
{
  object_type* self = (object_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'to_a'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->to_a);
 return 1;
}

/* set function: to_a of class  object_type */
static int tolua_set_object_type_to_a(lua_State* tolua_S)
{
  object_type* self = (object_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'to_a'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->to_a = ((s32b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: ac of class  object_type */
static int tolua_get_object_type_ac(lua_State* tolua_S)
{
  object_type* self = (object_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'ac'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->ac);
 return 1;
}

/* set function: ac of class  object_type */
static int tolua_set_object_type_ac(lua_State* tolua_S)
{
  object_type* self = (object_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'ac'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->ac = ((s32b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: dd of class  object_type */
static int tolua_get_object_type_dd(lua_State* tolua_S)
{
  object_type* self = (object_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'dd'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->dd);
 return 1;
}

/* set function: dd of class  object_type */
static int tolua_set_object_type_dd(lua_State* tolua_S)
{
  object_type* self = (object_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'dd'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->dd = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: ds of class  object_type */
static int tolua_get_object_type_ds(lua_State* tolua_S)
{
  object_type* self = (object_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'ds'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->ds);
 return 1;
}

/* set function: ds of class  object_type */
static int tolua_set_object_type_ds(lua_State* tolua_S)
{
  object_type* self = (object_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'ds'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->ds = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: timeout of class  object_type */
static int tolua_get_object_type_timeout(lua_State* tolua_S)
{
  object_type* self = (object_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'timeout'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->timeout);
 return 1;
}

/* set function: timeout of class  object_type */
static int tolua_set_object_type_timeout(lua_State* tolua_S)
{
  object_type* self = (object_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'timeout'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->timeout = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: ident of class  object_type */
static int tolua_get_object_type_ident(lua_State* tolua_S)
{
  object_type* self = (object_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'ident'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->ident);
 return 1;
}

/* set function: ident of class  object_type */
static int tolua_set_object_type_ident(lua_State* tolua_S)
{
  object_type* self = (object_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'ident'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->ident = ((byte)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: marked of class  object_type */
static int tolua_get_object_type_marked(lua_State* tolua_S)
{
  object_type* self = (object_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'marked'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->marked);
 return 1;
}

/* set function: marked of class  object_type */
static int tolua_set_object_type_marked(lua_State* tolua_S)
{
  object_type* self = (object_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'marked'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->marked = ((byte)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: note of class  object_type */
static int tolua_get_object_type_note(lua_State* tolua_S)
{
  object_type* self = (object_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'note'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->note);
 return 1;
}

/* set function: note of class  object_type */
static int tolua_set_object_type_note(lua_State* tolua_S)
{
  object_type* self = (object_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'note'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->note = ((u16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: art_name of class  object_type */
static int tolua_get_object_type_art_name(lua_State* tolua_S)
{
  object_type* self = (object_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'art_name'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->art_name);
 return 1;
}

/* set function: art_name of class  object_type */
static int tolua_set_object_type_art_name(lua_State* tolua_S)
{
  object_type* self = (object_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'art_name'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->art_name = ((u16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: art_flags1 of class  object_type */
static int tolua_get_object_type_art_flags1(lua_State* tolua_S)
{
  object_type* self = (object_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'art_flags1'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->art_flags1);
 return 1;
}

/* set function: art_flags1 of class  object_type */
static int tolua_set_object_type_art_flags1(lua_State* tolua_S)
{
  object_type* self = (object_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'art_flags1'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->art_flags1 = ((u32b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: art_flags2 of class  object_type */
static int tolua_get_object_type_art_flags2(lua_State* tolua_S)
{
  object_type* self = (object_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'art_flags2'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->art_flags2);
 return 1;
}

/* set function: art_flags2 of class  object_type */
static int tolua_set_object_type_art_flags2(lua_State* tolua_S)
{
  object_type* self = (object_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'art_flags2'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->art_flags2 = ((u32b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: art_flags3 of class  object_type */
static int tolua_get_object_type_art_flags3(lua_State* tolua_S)
{
  object_type* self = (object_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'art_flags3'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->art_flags3);
 return 1;
}

/* set function: art_flags3 of class  object_type */
static int tolua_set_object_type_art_flags3(lua_State* tolua_S)
{
  object_type* self = (object_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'art_flags3'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->art_flags3 = ((u32b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: art_flags4 of class  object_type */
static int tolua_get_object_type_art_flags4(lua_State* tolua_S)
{
  object_type* self = (object_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'art_flags4'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->art_flags4);
 return 1;
}

/* set function: art_flags4 of class  object_type */
static int tolua_set_object_type_art_flags4(lua_State* tolua_S)
{
  object_type* self = (object_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'art_flags4'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->art_flags4 = ((u32b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: next_o_idx of class  object_type */
static int tolua_get_object_type_next_o_idx(lua_State* tolua_S)
{
  object_type* self = (object_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'next_o_idx'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->next_o_idx);
 return 1;
}

/* set function: next_o_idx of class  object_type */
static int tolua_set_object_type_next_o_idx(lua_State* tolua_S)
{
  object_type* self = (object_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'next_o_idx'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->next_o_idx = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: held_m_idx of class  object_type */
static int tolua_get_object_type_held_m_idx(lua_State* tolua_S)
{
  object_type* self = (object_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'held_m_idx'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->held_m_idx);
 return 1;
}

/* set function: held_m_idx of class  object_type */
static int tolua_set_object_type_held_m_idx(lua_State* tolua_S)
{
  object_type* self = (object_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'held_m_idx'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->held_m_idx = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: spell of class  object_type */
static int tolua_get_types_object_type_spell(lua_State* tolua_S)
{
 int tolua_index;
  object_type* self;
 lua_pushstring(tolua_S,".self");
 lua_rawget(tolua_S,1);
 self = (object_type*)  lua_touserdata(tolua_S,-1);
#ifndef TOLUA_RELEASE
 {
 tolua_Error tolua_err;
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in array indexing.",&tolua_err);
 }
#endif
 tolua_index = (int)tolua_tonumber(tolua_S,2,0)-1;
#ifndef TOLUA_RELEASE
 if (tolua_index<0 || tolua_index>=20)
 tolua_error(tolua_S,"array indexing out of range.",NULL);
#endif
 tolua_pushusertype(tolua_S,(void*)&self->spell[tolua_index],"monster_spell");
 return 1;
}

/* set function: spell of class  object_type */
static int tolua_set_types_object_type_spell(lua_State* tolua_S)
{
 int tolua_index;
  object_type* self;
 lua_pushstring(tolua_S,".self");
 lua_rawget(tolua_S,1);
 self = (object_type*)  lua_touserdata(tolua_S,-1);
#ifndef TOLUA_RELEASE
 {
 tolua_Error tolua_err;
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in array indexing.",&tolua_err);
 }
#endif
 tolua_index = (int)tolua_tonumber(tolua_S,2,0)-1;
#ifndef TOLUA_RELEASE
 if (tolua_index<0 || tolua_index>=20)
 tolua_error(tolua_S,"array indexing out of range.",NULL);
#endif
  self->spell[tolua_index] = *((monster_spell*)  tolua_tousertype(tolua_S,3,0));
 return 0;
}

/* get function: r_idx of class  monster_type */
static int tolua_get_monster_type_r_idx(lua_State* tolua_S)
{
  monster_type* self = (monster_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'r_idx'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->r_idx);
 return 1;
}

/* set function: r_idx of class  monster_type */
static int tolua_set_monster_type_r_idx(lua_State* tolua_S)
{
  monster_type* self = (monster_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'r_idx'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->r_idx = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: fy of class  monster_type */
static int tolua_get_monster_type_fy(lua_State* tolua_S)
{
  monster_type* self = (monster_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'fy'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->fy);
 return 1;
}

/* set function: fy of class  monster_type */
static int tolua_set_monster_type_fy(lua_State* tolua_S)
{
  monster_type* self = (monster_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'fy'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->fy = ((byte)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: fx of class  monster_type */
static int tolua_get_monster_type_fx(lua_State* tolua_S)
{
  monster_type* self = (monster_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'fx'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->fx);
 return 1;
}

/* set function: fx of class  monster_type */
static int tolua_set_monster_type_fx(lua_State* tolua_S)
{
  monster_type* self = (monster_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'fx'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->fx = ((byte)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: hp of class  monster_type */
static int tolua_get_monster_type_hp(lua_State* tolua_S)
{
  monster_type* self = (monster_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'hp'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->hp);
 return 1;
}

/* set function: hp of class  monster_type */
static int tolua_set_monster_type_hp(lua_State* tolua_S)
{
  monster_type* self = (monster_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'hp'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->hp = ((s32b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: maxhp of class  monster_type */
static int tolua_get_monster_type_maxhp(lua_State* tolua_S)
{
  monster_type* self = (monster_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'maxhp'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->maxhp);
 return 1;
}

/* set function: maxhp of class  monster_type */
static int tolua_set_monster_type_maxhp(lua_State* tolua_S)
{
  monster_type* self = (monster_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'maxhp'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->maxhp = ((s32b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: csleep of class  monster_type */
static int tolua_get_monster_type_csleep(lua_State* tolua_S)
{
  monster_type* self = (monster_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'csleep'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->csleep);
 return 1;
}

/* set function: csleep of class  monster_type */
static int tolua_set_monster_type_csleep(lua_State* tolua_S)
{
  monster_type* self = (monster_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'csleep'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->csleep = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: mspeed of class  monster_type */
static int tolua_get_monster_type_mspeed(lua_State* tolua_S)
{
  monster_type* self = (monster_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'mspeed'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->mspeed);
 return 1;
}

/* set function: mspeed of class  monster_type */
static int tolua_set_monster_type_mspeed(lua_State* tolua_S)
{
  monster_type* self = (monster_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'mspeed'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->mspeed = ((byte)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: energy of class  monster_type */
static int tolua_get_monster_type_energy(lua_State* tolua_S)
{
  monster_type* self = (monster_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'energy'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->energy);
 return 1;
}

/* set function: energy of class  monster_type */
static int tolua_set_monster_type_energy(lua_State* tolua_S)
{
  monster_type* self = (monster_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'energy'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->energy = ((byte)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: stunned of class  monster_type */
static int tolua_get_monster_type_stunned(lua_State* tolua_S)
{
  monster_type* self = (monster_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'stunned'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->stunned);
 return 1;
}

/* set function: stunned of class  monster_type */
static int tolua_set_monster_type_stunned(lua_State* tolua_S)
{
  monster_type* self = (monster_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'stunned'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->stunned = ((byte)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: confused of class  monster_type */
static int tolua_get_monster_type_confused(lua_State* tolua_S)
{
  monster_type* self = (monster_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'confused'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->confused);
 return 1;
}

/* set function: confused of class  monster_type */
static int tolua_set_monster_type_confused(lua_State* tolua_S)
{
  monster_type* self = (monster_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'confused'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->confused = ((byte)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: monfear of class  monster_type */
static int tolua_get_monster_type_monfear(lua_State* tolua_S)
{
  monster_type* self = (monster_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'monfear'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->monfear);
 return 1;
}

/* set function: monfear of class  monster_type */
static int tolua_set_monster_type_monfear(lua_State* tolua_S)
{
  monster_type* self = (monster_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'monfear'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->monfear = ((byte)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: cdis of class  monster_type */
static int tolua_get_monster_type_cdis(lua_State* tolua_S)
{
  monster_type* self = (monster_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'cdis'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->cdis);
 return 1;
}

/* set function: cdis of class  monster_type */
static int tolua_set_monster_type_cdis(lua_State* tolua_S)
{
  monster_type* self = (monster_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'cdis'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->cdis = ((byte)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: mflag of class  monster_type */
static int tolua_get_monster_type_mflag(lua_State* tolua_S)
{
  monster_type* self = (monster_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'mflag'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->mflag);
 return 1;
}

/* set function: mflag of class  monster_type */
static int tolua_set_monster_type_mflag(lua_State* tolua_S)
{
  monster_type* self = (monster_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'mflag'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->mflag = ((byte)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: ml of class  monster_type */
static int tolua_get_monster_type_ml(lua_State* tolua_S)
{
  monster_type* self = (monster_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'ml'",NULL);
#endif
 tolua_pushboolean(tolua_S,(bool)self->ml);
 return 1;
}

/* set function: ml of class  monster_type */
static int tolua_set_monster_type_ml(lua_State* tolua_S)
{
  monster_type* self = (monster_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'ml'",NULL);
 if (!tolua_isboolean(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->ml = ((bool)  tolua_toboolean(tolua_S,2,0));
 return 0;
}

/* get function: hold_o_idx of class  monster_type */
static int tolua_get_monster_type_hold_o_idx(lua_State* tolua_S)
{
  monster_type* self = (monster_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'hold_o_idx'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->hold_o_idx);
 return 1;
}

/* set function: hold_o_idx of class  monster_type */
static int tolua_set_monster_type_hold_o_idx(lua_State* tolua_S)
{
  monster_type* self = (monster_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'hold_o_idx'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->hold_o_idx = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: smart of class  monster_type */
static int tolua_get_monster_type_smart(lua_State* tolua_S)
{
  monster_type* self = (monster_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'smart'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->smart);
 return 1;
}

/* set function: smart of class  monster_type */
static int tolua_set_monster_type_smart(lua_State* tolua_S)
{
  monster_type* self = (monster_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'smart'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->smart = ((u32b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: imprinted of class  monster_type */
static int tolua_get_monster_type_imprinted(lua_State* tolua_S)
{
  monster_type* self = (monster_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'imprinted'",NULL);
#endif
 tolua_pushboolean(tolua_S,(bool)self->imprinted);
 return 1;
}

/* set function: imprinted of class  monster_type */
static int tolua_set_monster_type_imprinted(lua_State* tolua_S)
{
  monster_type* self = (monster_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'imprinted'",NULL);
 if (!tolua_isboolean(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->imprinted = ((bool)  tolua_toboolean(tolua_S,2,0));
 return 0;
}

/* get function: level of class  monster_type */
static int tolua_get_monster_type_level(lua_State* tolua_S)
{
  monster_type* self = (monster_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'level'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->level);
 return 1;
}

/* set function: level of class  monster_type */
static int tolua_set_monster_type_level(lua_State* tolua_S)
{
  monster_type* self = (monster_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'level'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->level = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: angered_pet of class  monster_type */
static int tolua_get_monster_type_angered_pet(lua_State* tolua_S)
{
  monster_type* self = (monster_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'angered_pet'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->angered_pet);
 return 1;
}

/* set function: angered_pet of class  monster_type */
static int tolua_set_monster_type_angered_pet(lua_State* tolua_S)
{
  monster_type* self = (monster_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'angered_pet'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->angered_pet = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: boss of class  monster_type */
static int tolua_get_monster_type_boss(lua_State* tolua_S)
{
  monster_type* self = (monster_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'boss'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->boss);
 return 1;
}

/* set function: boss of class  monster_type */
static int tolua_set_monster_type_boss(lua_State* tolua_S)
{
  monster_type* self = (monster_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'boss'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->boss = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: abilities of class  monster_type */
static int tolua_get_monster_type_abilities(lua_State* tolua_S)
{
  monster_type* self = (monster_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'abilities'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->abilities);
 return 1;
}

/* set function: abilities of class  monster_type */
static int tolua_set_monster_type_abilities(lua_State* tolua_S)
{
  monster_type* self = (monster_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'abilities'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->abilities = ((u32b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: friend of class  monster_type */
static int tolua_get_monster_type_friend(lua_State* tolua_S)
{
  monster_type* self = (monster_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'friend'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->friend);
 return 1;
}

/* set function: friend of class  monster_type */
static int tolua_set_monster_type_friend(lua_State* tolua_S)
{
  monster_type* self = (monster_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'friend'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->friend = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: hitrate of class  monster_type */
static int tolua_get_monster_type_hitrate(lua_State* tolua_S)
{
  monster_type* self = (monster_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'hitrate'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->hitrate);
 return 1;
}

/* set function: hitrate of class  monster_type */
static int tolua_set_monster_type_hitrate(lua_State* tolua_S)
{
  monster_type* self = (monster_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'hitrate'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->hitrate = ((s32b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: defense of class  monster_type */
static int tolua_get_monster_type_defense(lua_State* tolua_S)
{
  monster_type* self = (monster_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'defense'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->defense);
 return 1;
}

/* set function: defense of class  monster_type */
static int tolua_set_monster_type_defense(lua_State* tolua_S)
{
  monster_type* self = (monster_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'defense'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->defense = ((s32b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: animated of class  monster_type */
static int tolua_get_monster_type_animated(lua_State* tolua_S)
{
  monster_type* self = (monster_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'animated'",NULL);
#endif
 tolua_pushboolean(tolua_S,(bool)self->animated);
 return 1;
}

/* set function: animated of class  monster_type */
static int tolua_set_monster_type_animated(lua_State* tolua_S)
{
  monster_type* self = (monster_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'animated'",NULL);
 if (!tolua_isboolean(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->animated = ((bool)  tolua_toboolean(tolua_S,2,0));
 return 0;
}

/* get function: animdam_d of class  monster_type */
static int tolua_get_monster_type_animdam_d(lua_State* tolua_S)
{
  monster_type* self = (monster_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'animdam_d'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->animdam_d);
 return 1;
}

/* set function: animdam_d of class  monster_type */
static int tolua_set_monster_type_animdam_d(lua_State* tolua_S)
{
  monster_type* self = (monster_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'animdam_d'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->animdam_d = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: animdam_s of class  monster_type */
static int tolua_get_monster_type_animdam_s(lua_State* tolua_S)
{
  monster_type* self = (monster_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'animdam_s'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->animdam_s);
 return 1;
}

/* set function: animdam_s of class  monster_type */
static int tolua_set_monster_type_animdam_s(lua_State* tolua_S)
{
  monster_type* self = (monster_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'animdam_s'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->animdam_s = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: seallight of class  monster_type */
static int tolua_get_monster_type_seallight(lua_State* tolua_S)
{
  monster_type* self = (monster_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'seallight'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->seallight);
 return 1;
}

/* set function: seallight of class  monster_type */
static int tolua_set_monster_type_seallight(lua_State* tolua_S)
{
  monster_type* self = (monster_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'seallight'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->seallight = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: str of class  monster_type */
static int tolua_get_monster_type_str(lua_State* tolua_S)
{
  monster_type* self = (monster_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'str'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->str);
 return 1;
}

/* set function: str of class  monster_type */
static int tolua_set_monster_type_str(lua_State* tolua_S)
{
  monster_type* self = (monster_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'str'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->str = ((s32b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: dex of class  monster_type */
static int tolua_get_monster_type_dex(lua_State* tolua_S)
{
  monster_type* self = (monster_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'dex'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->dex);
 return 1;
}

/* set function: dex of class  monster_type */
static int tolua_set_monster_type_dex(lua_State* tolua_S)
{
  monster_type* self = (monster_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'dex'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->dex = ((s32b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: mind of class  monster_type */
static int tolua_get_monster_type_mind(lua_State* tolua_S)
{
  monster_type* self = (monster_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'mind'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->mind);
 return 1;
}

/* set function: mind of class  monster_type */
static int tolua_set_monster_type_mind(lua_State* tolua_S)
{
  monster_type* self = (monster_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'mind'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->mind = ((s32b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: skill_attack of class  monster_type */
static int tolua_get_monster_type_skill_attack(lua_State* tolua_S)
{
  monster_type* self = (monster_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'skill_attack'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->skill_attack);
 return 1;
}

/* set function: skill_attack of class  monster_type */
static int tolua_set_monster_type_skill_attack(lua_State* tolua_S)
{
  monster_type* self = (monster_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'skill_attack'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->skill_attack = ((s32b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: skill_ranged of class  monster_type */
static int tolua_get_monster_type_skill_ranged(lua_State* tolua_S)
{
  monster_type* self = (monster_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'skill_ranged'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->skill_ranged);
 return 1;
}

/* set function: skill_ranged of class  monster_type */
static int tolua_set_monster_type_skill_ranged(lua_State* tolua_S)
{
  monster_type* self = (monster_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'skill_ranged'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->skill_ranged = ((s32b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: skill_magic of class  monster_type */
static int tolua_get_monster_type_skill_magic(lua_State* tolua_S)
{
  monster_type* self = (monster_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'skill_magic'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->skill_magic);
 return 1;
}

/* set function: skill_magic of class  monster_type */
static int tolua_set_monster_type_skill_magic(lua_State* tolua_S)
{
  monster_type* self = (monster_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'skill_magic'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->skill_magic = ((s32b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: mana of class  monster_type */
static int tolua_get_monster_type_mana(lua_State* tolua_S)
{
  monster_type* self = (monster_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'mana'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->mana);
 return 1;
}

/* set function: mana of class  monster_type */
static int tolua_set_monster_type_mana(lua_State* tolua_S)
{
  monster_type* self = (monster_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'mana'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->mana = ((s32b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: hasted of class  monster_type */
static int tolua_get_monster_type_hasted(lua_State* tolua_S)
{
  monster_type* self = (monster_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'hasted'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->hasted);
 return 1;
}

/* set function: hasted of class  monster_type */
static int tolua_set_monster_type_hasted(lua_State* tolua_S)
{
  monster_type* self = (monster_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'hasted'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->hasted = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: boosted of class  monster_type */
static int tolua_get_monster_type_boosted(lua_State* tolua_S)
{
  monster_type* self = (monster_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'boosted'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->boosted);
 return 1;
}

/* set function: boosted of class  monster_type */
static int tolua_set_monster_type_boosted(lua_State* tolua_S)
{
  monster_type* self = (monster_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'boosted'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->boosted = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: spoke of class  monster_type */
static int tolua_get_monster_type_spoke(lua_State* tolua_S)
{
  monster_type* self = (monster_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'spoke'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->spoke);
 return 1;
}

/* set function: spoke of class  monster_type */
static int tolua_set_monster_type_spoke(lua_State* tolua_S)
{
  monster_type* self = (monster_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'spoke'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->spoke = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: lives of class  monster_type */
static int tolua_get_monster_type_lives(lua_State* tolua_S)
{
  monster_type* self = (monster_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'lives'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->lives);
 return 1;
}

/* set function: lives of class  monster_type */
static int tolua_set_monster_type_lives(lua_State* tolua_S)
{
  monster_type* self = (monster_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'lives'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->lives = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: summoned of class  monster_type */
static int tolua_get_monster_type_summoned(lua_State* tolua_S)
{
  monster_type* self = (monster_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'summoned'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->summoned);
 return 1;
}

/* set function: summoned of class  monster_type */
static int tolua_set_monster_type_summoned(lua_State* tolua_S)
{
  monster_type* self = (monster_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'summoned'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->summoned = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: no_experience of class  monster_type */
static int tolua_get_monster_type_no_experience(lua_State* tolua_S)
{
  monster_type* self = (monster_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'no_experience'",NULL);
#endif
 tolua_pushboolean(tolua_S,(bool)self->no_experience);
 return 1;
}

/* set function: no_experience of class  monster_type */
static int tolua_set_monster_type_no_experience(lua_State* tolua_S)
{
  monster_type* self = (monster_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'no_experience'",NULL);
 if (!tolua_isboolean(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->no_experience = ((bool)  tolua_toboolean(tolua_S,2,0));
 return 0;
}

/* get function: index of class  alloc_entry */
static int tolua_get_alloc_entry_index(lua_State* tolua_S)
{
  alloc_entry* self = (alloc_entry*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'index'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->index);
 return 1;
}

/* set function: index of class  alloc_entry */
static int tolua_set_alloc_entry_index(lua_State* tolua_S)
{
  alloc_entry* self = (alloc_entry*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'index'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->index = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: level of class  alloc_entry */
static int tolua_get_alloc_entry_level(lua_State* tolua_S)
{
  alloc_entry* self = (alloc_entry*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'level'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->level);
 return 1;
}

/* set function: level of class  alloc_entry */
static int tolua_set_alloc_entry_level(lua_State* tolua_S)
{
  alloc_entry* self = (alloc_entry*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'level'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->level = ((byte)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: prob1 of class  alloc_entry */
static int tolua_get_alloc_entry_prob1(lua_State* tolua_S)
{
  alloc_entry* self = (alloc_entry*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'prob1'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->prob1);
 return 1;
}

/* set function: prob1 of class  alloc_entry */
static int tolua_set_alloc_entry_prob1(lua_State* tolua_S)
{
  alloc_entry* self = (alloc_entry*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'prob1'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->prob1 = ((byte)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: prob2 of class  alloc_entry */
static int tolua_get_alloc_entry_prob2(lua_State* tolua_S)
{
  alloc_entry* self = (alloc_entry*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'prob2'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->prob2);
 return 1;
}

/* set function: prob2 of class  alloc_entry */
static int tolua_set_alloc_entry_prob2(lua_State* tolua_S)
{
  alloc_entry* self = (alloc_entry*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'prob2'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->prob2 = ((byte)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: prob3 of class  alloc_entry */
static int tolua_get_alloc_entry_prob3(lua_State* tolua_S)
{
  alloc_entry* self = (alloc_entry*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'prob3'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->prob3);
 return 1;
}

/* set function: prob3 of class  alloc_entry */
static int tolua_set_alloc_entry_prob3(lua_State* tolua_S)
{
  alloc_entry* self = (alloc_entry*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'prob3'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->prob3 = ((byte)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: total of class  alloc_entry */
static int tolua_get_alloc_entry_total(lua_State* tolua_S)
{
  alloc_entry* self = (alloc_entry*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'total'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->total);
 return 1;
}

/* set function: total of class  alloc_entry */
static int tolua_set_alloc_entry_total(lua_State* tolua_S)
{
  alloc_entry* self = (alloc_entry*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'total'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->total = ((u16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: o_var of class  option_type */
static int tolua_get_option_type_o_var(lua_State* tolua_S)
{
  option_type* self = (option_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'o_var'",NULL);
#endif
 tolua_pushboolean(tolua_S,(bool)self->o_var);
 return 1;
}

/* set function: o_var of class  option_type */
static int tolua_set_option_type_o_var(lua_State* tolua_S)
{
  option_type* self = (option_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'o_var'",NULL);
 if (!tolua_isboolean(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->o_var = ((bool)  tolua_toboolean(tolua_S,2,0));
 return 0;
}

/* get function: o_norm of class  option_type */
static int tolua_get_option_type_o_norm(lua_State* tolua_S)
{
  option_type* self = (option_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'o_norm'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->o_norm);
 return 1;
}

/* set function: o_norm of class  option_type */
static int tolua_set_option_type_o_norm(lua_State* tolua_S)
{
  option_type* self = (option_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'o_norm'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->o_norm = ((byte)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: o_page of class  option_type */
static int tolua_get_option_type_o_page(lua_State* tolua_S)
{
  option_type* self = (option_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'o_page'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->o_page);
 return 1;
}

/* set function: o_page of class  option_type */
static int tolua_set_option_type_o_page(lua_State* tolua_S)
{
  option_type* self = (option_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'o_page'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->o_page = ((byte)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: o_set of class  option_type */
static int tolua_get_option_type_o_set(lua_State* tolua_S)
{
  option_type* self = (option_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'o_set'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->o_set);
 return 1;
}

/* set function: o_set of class  option_type */
static int tolua_set_option_type_o_set(lua_State* tolua_S)
{
  option_type* self = (option_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'o_set'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->o_set = ((byte)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: o_bit of class  option_type */
static int tolua_get_option_type_o_bit(lua_State* tolua_S)
{
  option_type* self = (option_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'o_bit'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->o_bit);
 return 1;
}

/* set function: o_bit of class  option_type */
static int tolua_set_option_type_o_bit(lua_State* tolua_S)
{
  option_type* self = (option_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'o_bit'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->o_bit = ((byte)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: o_text of class  option_type */
static int tolua_get_option_type_o_text(lua_State* tolua_S)
{
  option_type* self = (option_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'o_text'",NULL);
#endif
 tolua_pushstring(tolua_S,(const char*)self->o_text);
 return 1;
}

/* set function: o_text of class  option_type */
static int tolua_set_option_type_o_text(lua_State* tolua_S)
{
  option_type* self = (option_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'o_text'",NULL);
 if (!tolua_isstring(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->o_text = ((cptr)  tolua_tostring(tolua_S,2,0));
 return 0;
}

/* get function: o_desc of class  option_type */
static int tolua_get_option_type_o_desc(lua_State* tolua_S)
{
  option_type* self = (option_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'o_desc'",NULL);
#endif
 tolua_pushstring(tolua_S,(const char*)self->o_desc);
 return 1;
}

/* set function: o_desc of class  option_type */
static int tolua_set_option_type_o_desc(lua_State* tolua_S)
{
  option_type* self = (option_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'o_desc'",NULL);
 if (!tolua_isstring(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->o_desc = ((cptr)  tolua_tostring(tolua_S,2,0));
 return 0;
}

/* get function: owner_name of class  owner_type */
static int tolua_get_owner_type_owner_name(lua_State* tolua_S)
{
  owner_type* self = (owner_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'owner_name'",NULL);
#endif
 tolua_pushstring(tolua_S,(const char*)self->owner_name);
 return 1;
}

/* set function: owner_name of class  owner_type */
static int tolua_set_owner_type_owner_name(lua_State* tolua_S)
{
  owner_type* self = (owner_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'owner_name'",NULL);
 if (!tolua_isstring(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->owner_name = ((cptr)  tolua_tostring(tolua_S,2,0));
 return 0;
}

/* get function: max_cost of class  owner_type */
static int tolua_get_owner_type_max_cost(lua_State* tolua_S)
{
  owner_type* self = (owner_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'max_cost'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->max_cost);
 return 1;
}

/* set function: max_cost of class  owner_type */
static int tolua_set_owner_type_max_cost(lua_State* tolua_S)
{
  owner_type* self = (owner_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'max_cost'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->max_cost = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: max_inflate of class  owner_type */
static int tolua_get_owner_type_max_inflate(lua_State* tolua_S)
{
  owner_type* self = (owner_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'max_inflate'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->max_inflate);
 return 1;
}

/* set function: max_inflate of class  owner_type */
static int tolua_set_owner_type_max_inflate(lua_State* tolua_S)
{
  owner_type* self = (owner_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'max_inflate'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->max_inflate = ((byte)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: min_inflate of class  owner_type */
static int tolua_get_owner_type_min_inflate(lua_State* tolua_S)
{
  owner_type* self = (owner_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'min_inflate'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->min_inflate);
 return 1;
}

/* set function: min_inflate of class  owner_type */
static int tolua_set_owner_type_min_inflate(lua_State* tolua_S)
{
  owner_type* self = (owner_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'min_inflate'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->min_inflate = ((byte)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: haggle_per of class  owner_type */
static int tolua_get_owner_type_haggle_per(lua_State* tolua_S)
{
  owner_type* self = (owner_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'haggle_per'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->haggle_per);
 return 1;
}

/* set function: haggle_per of class  owner_type */
static int tolua_set_owner_type_haggle_per(lua_State* tolua_S)
{
  owner_type* self = (owner_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'haggle_per'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->haggle_per = ((byte)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: insult_max of class  owner_type */
static int tolua_get_owner_type_insult_max(lua_State* tolua_S)
{
  owner_type* self = (owner_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'insult_max'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->insult_max);
 return 1;
}

/* set function: insult_max of class  owner_type */
static int tolua_set_owner_type_insult_max(lua_State* tolua_S)
{
  owner_type* self = (owner_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'insult_max'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->insult_max = ((byte)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: owner_race of class  owner_type */
static int tolua_get_owner_type_owner_race(lua_State* tolua_S)
{
  owner_type* self = (owner_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'owner_race'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->owner_race);
 return 1;
}

/* set function: owner_race of class  owner_type */
static int tolua_set_owner_type_owner_race(lua_State* tolua_S)
{
  owner_type* self = (owner_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'owner_race'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->owner_race = ((byte)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: type of class  store_type */
static int tolua_get_store_type_type(lua_State* tolua_S)
{
  store_type* self = (store_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'type'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->type);
 return 1;
}

/* set function: type of class  store_type */
static int tolua_set_store_type_type(lua_State* tolua_S)
{
  store_type* self = (store_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'type'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->type = ((byte)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: owner of class  store_type */
static int tolua_get_store_type_owner(lua_State* tolua_S)
{
  store_type* self = (store_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'owner'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->owner);
 return 1;
}

/* set function: owner of class  store_type */
static int tolua_set_store_type_owner(lua_State* tolua_S)
{
  store_type* self = (store_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'owner'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->owner = ((byte)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: extra of class  store_type */
static int tolua_get_store_type_extra(lua_State* tolua_S)
{
  store_type* self = (store_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'extra'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->extra);
 return 1;
}

/* set function: extra of class  store_type */
static int tolua_set_store_type_extra(lua_State* tolua_S)
{
  store_type* self = (store_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'extra'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->extra = ((byte)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: insult_cur of class  store_type */
static int tolua_get_store_type_insult_cur(lua_State* tolua_S)
{
  store_type* self = (store_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'insult_cur'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->insult_cur);
 return 1;
}

/* set function: insult_cur of class  store_type */
static int tolua_set_store_type_insult_cur(lua_State* tolua_S)
{
  store_type* self = (store_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'insult_cur'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->insult_cur = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: good_buy of class  store_type */
static int tolua_get_store_type_good_buy(lua_State* tolua_S)
{
  store_type* self = (store_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'good_buy'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->good_buy);
 return 1;
}

/* set function: good_buy of class  store_type */
static int tolua_set_store_type_good_buy(lua_State* tolua_S)
{
  store_type* self = (store_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'good_buy'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->good_buy = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: bad_buy of class  store_type */
static int tolua_get_store_type_bad_buy(lua_State* tolua_S)
{
  store_type* self = (store_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'bad_buy'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->bad_buy);
 return 1;
}

/* set function: bad_buy of class  store_type */
static int tolua_set_store_type_bad_buy(lua_State* tolua_S)
{
  store_type* self = (store_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'bad_buy'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->bad_buy = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: store_open of class  store_type */
static int tolua_get_store_type_store_open(lua_State* tolua_S)
{
  store_type* self = (store_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'store_open'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->store_open);
 return 1;
}

/* set function: store_open of class  store_type */
static int tolua_set_store_type_store_open(lua_State* tolua_S)
{
  store_type* self = (store_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'store_open'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->store_open = ((s32b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: last_visit of class  store_type */
static int tolua_get_store_type_last_visit(lua_State* tolua_S)
{
  store_type* self = (store_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'last_visit'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->last_visit);
 return 1;
}

/* set function: last_visit of class  store_type */
static int tolua_set_store_type_last_visit(lua_State* tolua_S)
{
  store_type* self = (store_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'last_visit'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->last_visit = ((s32b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: table_num of class  store_type */
static int tolua_get_store_type_table_num(lua_State* tolua_S)
{
  store_type* self = (store_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'table_num'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->table_num);
 return 1;
}

/* set function: table_num of class  store_type */
static int tolua_set_store_type_table_num(lua_State* tolua_S)
{
  store_type* self = (store_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'table_num'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->table_num = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: table_size of class  store_type */
static int tolua_get_store_type_table_size(lua_State* tolua_S)
{
  store_type* self = (store_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'table_size'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->table_size);
 return 1;
}

/* set function: table_size of class  store_type */
static int tolua_set_store_type_table_size(lua_State* tolua_S)
{
  store_type* self = (store_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'table_size'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->table_size = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: table of class  store_type */
static int tolua_get_store_type_table(lua_State* tolua_S)
{
  store_type* self = (store_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'table'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->table);
 return 1;
}

/* set function: table of class  store_type */
static int tolua_set_store_type_table(lua_State* tolua_S)
{
  store_type* self = (store_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'table'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->table = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: stock_num of class  store_type */
static int tolua_get_store_type_stock_num(lua_State* tolua_S)
{
  store_type* self = (store_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'stock_num'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->stock_num);
 return 1;
}

/* set function: stock_num of class  store_type */
static int tolua_set_store_type_stock_num(lua_State* tolua_S)
{
  store_type* self = (store_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'stock_num'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->stock_num = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: stock_size of class  store_type */
static int tolua_get_store_type_stock_size(lua_State* tolua_S)
{
  store_type* self = (store_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'stock_size'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->stock_size);
 return 1;
}

/* set function: stock_size of class  store_type */
static int tolua_set_store_type_stock_size(lua_State* tolua_S)
{
  store_type* self = (store_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'stock_size'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->stock_size = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: stock of class  store_type */
static int tolua_get_store_type_stock_ptr(lua_State* tolua_S)
{
  store_type* self = (store_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'stock'",NULL);
#endif
 tolua_pushusertype(tolua_S,(void*)self->stock,"object_type");
 return 1;
}

/* set function: stock of class  store_type */
static int tolua_set_store_type_stock_ptr(lua_State* tolua_S)
{
  store_type* self = (store_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'stock'",NULL);
 if (!tolua_isusertype(tolua_S,2,"object_type",0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->stock = ((object_type*)  tolua_tousertype(tolua_S,2,0));
 return 0;
}

/* get function: title of class  player_sex */
static int tolua_get_player_sex_title(lua_State* tolua_S)
{
  player_sex* self = (player_sex*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'title'",NULL);
#endif
 tolua_pushstring(tolua_S,(const char*)self->title);
 return 1;
}

/* set function: title of class  player_sex */
static int tolua_set_player_sex_title(lua_State* tolua_S)
{
  player_sex* self = (player_sex*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'title'",NULL);
 if (!tolua_isstring(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->title = ((cptr)  tolua_tostring(tolua_S,2,0));
 return 0;
}

/* get function: winner of class  player_sex */
static int tolua_get_player_sex_winner(lua_State* tolua_S)
{
  player_sex* self = (player_sex*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'winner'",NULL);
#endif
 tolua_pushstring(tolua_S,(const char*)self->winner);
 return 1;
}

/* set function: winner of class  player_sex */
static int tolua_set_player_sex_winner(lua_State* tolua_S)
{
  player_sex* self = (player_sex*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'winner'",NULL);
 if (!tolua_isstring(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->winner = ((cptr)  tolua_tostring(tolua_S,2,0));
 return 0;
}

/* get function: title of class  player_race */
static int tolua_get_player_race_title(lua_State* tolua_S)
{
  player_race* self = (player_race*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'title'",NULL);
#endif
 tolua_pushstring(tolua_S,(const char*)self->title);
 return 1;
}

/* set function: title of class  player_race */
static int tolua_set_player_race_title(lua_State* tolua_S)
{
  player_race* self = (player_race*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'title'",NULL);
 if (!tolua_isstring(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->title = ((cptr)  tolua_tostring(tolua_S,2,0));
 return 0;
}

/* get function: r_adj of class  player_race */
static int tolua_get_types_player_race_r_adj(lua_State* tolua_S)
{
 int tolua_index;
  player_race* self;
 lua_pushstring(tolua_S,".self");
 lua_rawget(tolua_S,1);
 self = (player_race*)  lua_touserdata(tolua_S,-1);
#ifndef TOLUA_RELEASE
 {
 tolua_Error tolua_err;
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in array indexing.",&tolua_err);
 }
#endif
 tolua_index = (int)tolua_tonumber(tolua_S,2,0)-1;
#ifndef TOLUA_RELEASE
 if (tolua_index<0 || tolua_index>=6)
 tolua_error(tolua_S,"array indexing out of range.",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->r_adj[tolua_index]);
 return 1;
}

/* set function: r_adj of class  player_race */
static int tolua_set_types_player_race_r_adj(lua_State* tolua_S)
{
 int tolua_index;
  player_race* self;
 lua_pushstring(tolua_S,".self");
 lua_rawget(tolua_S,1);
 self = (player_race*)  lua_touserdata(tolua_S,-1);
#ifndef TOLUA_RELEASE
 {
 tolua_Error tolua_err;
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in array indexing.",&tolua_err);
 }
#endif
 tolua_index = (int)tolua_tonumber(tolua_S,2,0)-1;
#ifndef TOLUA_RELEASE
 if (tolua_index<0 || tolua_index>=6)
 tolua_error(tolua_S,"array indexing out of range.",NULL);
#endif
  self->r_adj[tolua_index] = ((s16b)  tolua_tonumber(tolua_S,3,0));
 return 0;
}

/* get function: r_dis of class  player_race */
static int tolua_get_player_race_r_dis(lua_State* tolua_S)
{
  player_race* self = (player_race*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'r_dis'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->r_dis);
 return 1;
}

/* set function: r_dis of class  player_race */
static int tolua_set_player_race_r_dis(lua_State* tolua_S)
{
  player_race* self = (player_race*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'r_dis'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->r_dis = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: r_dev of class  player_race */
static int tolua_get_player_race_r_dev(lua_State* tolua_S)
{
  player_race* self = (player_race*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'r_dev'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->r_dev);
 return 1;
}

/* set function: r_dev of class  player_race */
static int tolua_set_player_race_r_dev(lua_State* tolua_S)
{
  player_race* self = (player_race*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'r_dev'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->r_dev = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: r_sav of class  player_race */
static int tolua_get_player_race_r_sav(lua_State* tolua_S)
{
  player_race* self = (player_race*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'r_sav'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->r_sav);
 return 1;
}

/* set function: r_sav of class  player_race */
static int tolua_set_player_race_r_sav(lua_State* tolua_S)
{
  player_race* self = (player_race*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'r_sav'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->r_sav = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: r_stl of class  player_race */
static int tolua_get_player_race_r_stl(lua_State* tolua_S)
{
  player_race* self = (player_race*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'r_stl'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->r_stl);
 return 1;
}

/* set function: r_stl of class  player_race */
static int tolua_set_player_race_r_stl(lua_State* tolua_S)
{
  player_race* self = (player_race*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'r_stl'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->r_stl = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: r_srh of class  player_race */
static int tolua_get_player_race_r_srh(lua_State* tolua_S)
{
  player_race* self = (player_race*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'r_srh'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->r_srh);
 return 1;
}

/* set function: r_srh of class  player_race */
static int tolua_set_player_race_r_srh(lua_State* tolua_S)
{
  player_race* self = (player_race*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'r_srh'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->r_srh = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: r_fos of class  player_race */
static int tolua_get_player_race_r_fos(lua_State* tolua_S)
{
  player_race* self = (player_race*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'r_fos'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->r_fos);
 return 1;
}

/* set function: r_fos of class  player_race */
static int tolua_set_player_race_r_fos(lua_State* tolua_S)
{
  player_race* self = (player_race*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'r_fos'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->r_fos = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: r_thn of class  player_race */
static int tolua_get_player_race_r_thn(lua_State* tolua_S)
{
  player_race* self = (player_race*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'r_thn'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->r_thn);
 return 1;
}

/* set function: r_thn of class  player_race */
static int tolua_set_player_race_r_thn(lua_State* tolua_S)
{
  player_race* self = (player_race*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'r_thn'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->r_thn = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: r_thb of class  player_race */
static int tolua_get_player_race_r_thb(lua_State* tolua_S)
{
  player_race* self = (player_race*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'r_thb'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->r_thb);
 return 1;
}

/* set function: r_thb of class  player_race */
static int tolua_set_player_race_r_thb(lua_State* tolua_S)
{
  player_race* self = (player_race*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'r_thb'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->r_thb = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: r_mhp of class  player_race */
static int tolua_get_player_race_r_mhp(lua_State* tolua_S)
{
  player_race* self = (player_race*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'r_mhp'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->r_mhp);
 return 1;
}

/* set function: r_mhp of class  player_race */
static int tolua_set_player_race_r_mhp(lua_State* tolua_S)
{
  player_race* self = (player_race*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'r_mhp'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->r_mhp = ((byte)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: r_exp of class  player_race */
static int tolua_get_player_race_r_exp(lua_State* tolua_S)
{
  player_race* self = (player_race*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'r_exp'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->r_exp);
 return 1;
}

/* set function: r_exp of class  player_race */
static int tolua_set_player_race_r_exp(lua_State* tolua_S)
{
  player_race* self = (player_race*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'r_exp'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->r_exp = ((u16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: b_age of class  player_race */
static int tolua_get_player_race_b_age(lua_State* tolua_S)
{
  player_race* self = (player_race*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'b_age'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->b_age);
 return 1;
}

/* set function: b_age of class  player_race */
static int tolua_set_player_race_b_age(lua_State* tolua_S)
{
  player_race* self = (player_race*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'b_age'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->b_age = ((byte)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: m_age of class  player_race */
static int tolua_get_player_race_m_age(lua_State* tolua_S)
{
  player_race* self = (player_race*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'm_age'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->m_age);
 return 1;
}

/* set function: m_age of class  player_race */
static int tolua_set_player_race_m_age(lua_State* tolua_S)
{
  player_race* self = (player_race*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'm_age'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->m_age = ((byte)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: m_b_ht of class  player_race */
static int tolua_get_player_race_m_b_ht(lua_State* tolua_S)
{
  player_race* self = (player_race*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'm_b_ht'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->m_b_ht);
 return 1;
}

/* set function: m_b_ht of class  player_race */
static int tolua_set_player_race_m_b_ht(lua_State* tolua_S)
{
  player_race* self = (player_race*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'm_b_ht'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->m_b_ht = ((byte)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: m_m_ht of class  player_race */
static int tolua_get_player_race_m_m_ht(lua_State* tolua_S)
{
  player_race* self = (player_race*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'm_m_ht'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->m_m_ht);
 return 1;
}

/* set function: m_m_ht of class  player_race */
static int tolua_set_player_race_m_m_ht(lua_State* tolua_S)
{
  player_race* self = (player_race*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'm_m_ht'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->m_m_ht = ((byte)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: m_b_wt of class  player_race */
static int tolua_get_player_race_m_b_wt(lua_State* tolua_S)
{
  player_race* self = (player_race*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'm_b_wt'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->m_b_wt);
 return 1;
}

/* set function: m_b_wt of class  player_race */
static int tolua_set_player_race_m_b_wt(lua_State* tolua_S)
{
  player_race* self = (player_race*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'm_b_wt'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->m_b_wt = ((byte)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: m_m_wt of class  player_race */
static int tolua_get_player_race_m_m_wt(lua_State* tolua_S)
{
  player_race* self = (player_race*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'm_m_wt'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->m_m_wt);
 return 1;
}

/* set function: m_m_wt of class  player_race */
static int tolua_set_player_race_m_m_wt(lua_State* tolua_S)
{
  player_race* self = (player_race*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'm_m_wt'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->m_m_wt = ((byte)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: f_b_ht of class  player_race */
static int tolua_get_player_race_f_b_ht(lua_State* tolua_S)
{
  player_race* self = (player_race*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'f_b_ht'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->f_b_ht);
 return 1;
}

/* set function: f_b_ht of class  player_race */
static int tolua_set_player_race_f_b_ht(lua_State* tolua_S)
{
  player_race* self = (player_race*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'f_b_ht'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->f_b_ht = ((byte)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: f_m_ht of class  player_race */
static int tolua_get_player_race_f_m_ht(lua_State* tolua_S)
{
  player_race* self = (player_race*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'f_m_ht'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->f_m_ht);
 return 1;
}

/* set function: f_m_ht of class  player_race */
static int tolua_set_player_race_f_m_ht(lua_State* tolua_S)
{
  player_race* self = (player_race*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'f_m_ht'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->f_m_ht = ((byte)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: f_b_wt of class  player_race */
static int tolua_get_player_race_f_b_wt(lua_State* tolua_S)
{
  player_race* self = (player_race*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'f_b_wt'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->f_b_wt);
 return 1;
}

/* set function: f_b_wt of class  player_race */
static int tolua_set_player_race_f_b_wt(lua_State* tolua_S)
{
  player_race* self = (player_race*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'f_b_wt'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->f_b_wt = ((byte)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: f_m_wt of class  player_race */
static int tolua_get_player_race_f_m_wt(lua_State* tolua_S)
{
  player_race* self = (player_race*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'f_m_wt'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->f_m_wt);
 return 1;
}

/* set function: f_m_wt of class  player_race */
static int tolua_set_player_race_f_m_wt(lua_State* tolua_S)
{
  player_race* self = (player_race*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'f_m_wt'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->f_m_wt = ((byte)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: infra of class  player_race */
static int tolua_get_player_race_infra(lua_State* tolua_S)
{
  player_race* self = (player_race*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'infra'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->infra);
 return 1;
}

/* set function: infra of class  player_race */
static int tolua_set_player_race_infra(lua_State* tolua_S)
{
  player_race* self = (player_race*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'infra'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->infra = ((byte)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: choice of class  player_race */
static int tolua_get_player_race_choice(lua_State* tolua_S)
{
  player_race* self = (player_race*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'choice'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->choice);
 return 1;
}

/* set function: choice of class  player_race */
static int tolua_set_player_race_choice(lua_State* tolua_S)
{
  player_race* self = (player_race*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'choice'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->choice = ((u32b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: title of class  player_class */
static int tolua_get_player_class_title(lua_State* tolua_S)
{
  player_class* self = (player_class*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'title'",NULL);
#endif
 tolua_pushstring(tolua_S,(const char*)self->title);
 return 1;
}

/* set function: title of class  player_class */
static int tolua_set_player_class_title(lua_State* tolua_S)
{
  player_class* self = (player_class*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'title'",NULL);
 if (!tolua_isstring(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->title = ((cptr)  tolua_tostring(tolua_S,2,0));
 return 0;
}

/* get function: c_adj of class  player_class */
static int tolua_get_types_player_class_c_adj(lua_State* tolua_S)
{
 int tolua_index;
  player_class* self;
 lua_pushstring(tolua_S,".self");
 lua_rawget(tolua_S,1);
 self = (player_class*)  lua_touserdata(tolua_S,-1);
#ifndef TOLUA_RELEASE
 {
 tolua_Error tolua_err;
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in array indexing.",&tolua_err);
 }
#endif
 tolua_index = (int)tolua_tonumber(tolua_S,2,0)-1;
#ifndef TOLUA_RELEASE
 if (tolua_index<0 || tolua_index>=6)
 tolua_error(tolua_S,"array indexing out of range.",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->c_adj[tolua_index]);
 return 1;
}

/* set function: c_adj of class  player_class */
static int tolua_set_types_player_class_c_adj(lua_State* tolua_S)
{
 int tolua_index;
  player_class* self;
 lua_pushstring(tolua_S,".self");
 lua_rawget(tolua_S,1);
 self = (player_class*)  lua_touserdata(tolua_S,-1);
#ifndef TOLUA_RELEASE
 {
 tolua_Error tolua_err;
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in array indexing.",&tolua_err);
 }
#endif
 tolua_index = (int)tolua_tonumber(tolua_S,2,0)-1;
#ifndef TOLUA_RELEASE
 if (tolua_index<0 || tolua_index>=6)
 tolua_error(tolua_S,"array indexing out of range.",NULL);
#endif
  self->c_adj[tolua_index] = ((s16b)  tolua_tonumber(tolua_S,3,0));
 return 0;
}

/* get function: c_dis of class  player_class */
static int tolua_get_player_class_c_dis(lua_State* tolua_S)
{
  player_class* self = (player_class*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'c_dis'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->c_dis);
 return 1;
}

/* set function: c_dis of class  player_class */
static int tolua_set_player_class_c_dis(lua_State* tolua_S)
{
  player_class* self = (player_class*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'c_dis'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->c_dis = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: c_dev of class  player_class */
static int tolua_get_player_class_c_dev(lua_State* tolua_S)
{
  player_class* self = (player_class*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'c_dev'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->c_dev);
 return 1;
}

/* set function: c_dev of class  player_class */
static int tolua_set_player_class_c_dev(lua_State* tolua_S)
{
  player_class* self = (player_class*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'c_dev'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->c_dev = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: c_sav of class  player_class */
static int tolua_get_player_class_c_sav(lua_State* tolua_S)
{
  player_class* self = (player_class*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'c_sav'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->c_sav);
 return 1;
}

/* set function: c_sav of class  player_class */
static int tolua_set_player_class_c_sav(lua_State* tolua_S)
{
  player_class* self = (player_class*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'c_sav'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->c_sav = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: c_stl of class  player_class */
static int tolua_get_player_class_c_stl(lua_State* tolua_S)
{
  player_class* self = (player_class*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'c_stl'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->c_stl);
 return 1;
}

/* set function: c_stl of class  player_class */
static int tolua_set_player_class_c_stl(lua_State* tolua_S)
{
  player_class* self = (player_class*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'c_stl'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->c_stl = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: c_srh of class  player_class */
static int tolua_get_player_class_c_srh(lua_State* tolua_S)
{
  player_class* self = (player_class*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'c_srh'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->c_srh);
 return 1;
}

/* set function: c_srh of class  player_class */
static int tolua_set_player_class_c_srh(lua_State* tolua_S)
{
  player_class* self = (player_class*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'c_srh'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->c_srh = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: c_fos of class  player_class */
static int tolua_get_player_class_c_fos(lua_State* tolua_S)
{
  player_class* self = (player_class*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'c_fos'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->c_fos);
 return 1;
}

/* set function: c_fos of class  player_class */
static int tolua_set_player_class_c_fos(lua_State* tolua_S)
{
  player_class* self = (player_class*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'c_fos'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->c_fos = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: c_thn of class  player_class */
static int tolua_get_player_class_c_thn(lua_State* tolua_S)
{
  player_class* self = (player_class*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'c_thn'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->c_thn);
 return 1;
}

/* set function: c_thn of class  player_class */
static int tolua_set_player_class_c_thn(lua_State* tolua_S)
{
  player_class* self = (player_class*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'c_thn'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->c_thn = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: c_thb of class  player_class */
static int tolua_get_player_class_c_thb(lua_State* tolua_S)
{
  player_class* self = (player_class*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'c_thb'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->c_thb);
 return 1;
}

/* set function: c_thb of class  player_class */
static int tolua_set_player_class_c_thb(lua_State* tolua_S)
{
  player_class* self = (player_class*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'c_thb'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->c_thb = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: x_dis of class  player_class */
static int tolua_get_player_class_x_dis(lua_State* tolua_S)
{
  player_class* self = (player_class*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'x_dis'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->x_dis);
 return 1;
}

/* set function: x_dis of class  player_class */
static int tolua_set_player_class_x_dis(lua_State* tolua_S)
{
  player_class* self = (player_class*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'x_dis'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->x_dis = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: x_dev of class  player_class */
static int tolua_get_player_class_x_dev(lua_State* tolua_S)
{
  player_class* self = (player_class*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'x_dev'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->x_dev);
 return 1;
}

/* set function: x_dev of class  player_class */
static int tolua_set_player_class_x_dev(lua_State* tolua_S)
{
  player_class* self = (player_class*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'x_dev'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->x_dev = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: x_sav of class  player_class */
static int tolua_get_player_class_x_sav(lua_State* tolua_S)
{
  player_class* self = (player_class*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'x_sav'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->x_sav);
 return 1;
}

/* set function: x_sav of class  player_class */
static int tolua_set_player_class_x_sav(lua_State* tolua_S)
{
  player_class* self = (player_class*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'x_sav'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->x_sav = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: x_stl of class  player_class */
static int tolua_get_player_class_x_stl(lua_State* tolua_S)
{
  player_class* self = (player_class*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'x_stl'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->x_stl);
 return 1;
}

/* set function: x_stl of class  player_class */
static int tolua_set_player_class_x_stl(lua_State* tolua_S)
{
  player_class* self = (player_class*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'x_stl'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->x_stl = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: x_srh of class  player_class */
static int tolua_get_player_class_x_srh(lua_State* tolua_S)
{
  player_class* self = (player_class*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'x_srh'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->x_srh);
 return 1;
}

/* set function: x_srh of class  player_class */
static int tolua_set_player_class_x_srh(lua_State* tolua_S)
{
  player_class* self = (player_class*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'x_srh'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->x_srh = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: x_fos of class  player_class */
static int tolua_get_player_class_x_fos(lua_State* tolua_S)
{
  player_class* self = (player_class*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'x_fos'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->x_fos);
 return 1;
}

/* set function: x_fos of class  player_class */
static int tolua_set_player_class_x_fos(lua_State* tolua_S)
{
  player_class* self = (player_class*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'x_fos'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->x_fos = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: x_thn of class  player_class */
static int tolua_get_player_class_x_thn(lua_State* tolua_S)
{
  player_class* self = (player_class*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'x_thn'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->x_thn);
 return 1;
}

/* set function: x_thn of class  player_class */
static int tolua_set_player_class_x_thn(lua_State* tolua_S)
{
  player_class* self = (player_class*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'x_thn'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->x_thn = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: x_thb of class  player_class */
static int tolua_get_player_class_x_thb(lua_State* tolua_S)
{
  player_class* self = (player_class*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'x_thb'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->x_thb);
 return 1;
}

/* set function: x_thb of class  player_class */
static int tolua_set_player_class_x_thb(lua_State* tolua_S)
{
  player_class* self = (player_class*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'x_thb'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->x_thb = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: c_mhp of class  player_class */
static int tolua_get_player_class_c_mhp(lua_State* tolua_S)
{
  player_class* self = (player_class*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'c_mhp'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->c_mhp);
 return 1;
}

/* set function: c_mhp of class  player_class */
static int tolua_set_player_class_c_mhp(lua_State* tolua_S)
{
  player_class* self = (player_class*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'c_mhp'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->c_mhp = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: c_exp of class  player_class */
static int tolua_get_player_class_c_exp(lua_State* tolua_S)
{
  player_class* self = (player_class*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'c_exp'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->c_exp);
 return 1;
}

/* set function: c_exp of class  player_class */
static int tolua_set_player_class_c_exp(lua_State* tolua_S)
{
  player_class* self = (player_class*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'c_exp'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->c_exp = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: oldpy of class  player_type */
static int tolua_get_player_type_oldpy(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'oldpy'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->oldpy);
 return 1;
}

/* set function: oldpy of class  player_type */
static int tolua_set_player_type_oldpy(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'oldpy'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->oldpy = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: oldpx of class  player_type */
static int tolua_get_player_type_oldpx(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'oldpx'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->oldpx);
 return 1;
}

/* set function: oldpx of class  player_type */
static int tolua_set_player_type_oldpx(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'oldpx'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->oldpx = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: psex of class  player_type */
static int tolua_get_player_type_psex(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'psex'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->psex);
 return 1;
}

/* set function: psex of class  player_type */
static int tolua_set_player_type_psex(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'psex'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->psex = ((byte)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: prace of class  player_type */
static int tolua_get_player_type_prace(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'prace'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->prace);
 return 1;
}

/* set function: prace of class  player_type */
static int tolua_set_player_type_prace(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'prace'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->prace = ((byte)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: pclass of class  player_type */
static int tolua_get_player_type_pclass(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'pclass'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->pclass);
 return 1;
}

/* set function: pclass of class  player_type */
static int tolua_set_player_type_pclass(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'pclass'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->pclass = ((byte)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: oops of class  player_type */
static int tolua_get_player_type_oops(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'oops'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->oops);
 return 1;
}

/* set function: oops of class  player_type */
static int tolua_set_player_type_oops(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'oops'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->oops = ((byte)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: hitdie of class  player_type */
static int tolua_get_player_type_hitdie(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'hitdie'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->hitdie);
 return 1;
}

/* set function: hitdie of class  player_type */
static int tolua_set_player_type_hitdie(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'hitdie'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->hitdie = ((byte)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: expfact of class  player_type */
static int tolua_get_player_type_expfact(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'expfact'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->expfact);
 return 1;
}

/* set function: expfact of class  player_type */
static int tolua_set_player_type_expfact(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'expfact'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->expfact = ((u16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: maximize of class  player_type */
static int tolua_get_player_type_maximize(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'maximize'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->maximize);
 return 1;
}

/* set function: maximize of class  player_type */
static int tolua_set_player_type_maximize(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'maximize'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->maximize = ((byte)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: preserve of class  player_type */
static int tolua_get_player_type_preserve(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'preserve'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->preserve);
 return 1;
}

/* set function: preserve of class  player_type */
static int tolua_set_player_type_preserve(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'preserve'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->preserve = ((byte)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: special of class  player_type */
static int tolua_get_player_type_special(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'special'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->special);
 return 1;
}

/* set function: special of class  player_type */
static int tolua_set_player_type_special(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'special'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->special = ((byte)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: allow_one_death of class  player_type */
static int tolua_get_player_type_allow_one_death(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'allow_one_death'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->allow_one_death);
 return 1;
}

/* set function: allow_one_death of class  player_type */
static int tolua_set_player_type_allow_one_death(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'allow_one_death'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->allow_one_death = ((byte)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: age of class  player_type */
static int tolua_get_player_type_age(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'age'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->age);
 return 1;
}

/* set function: age of class  player_type */
static int tolua_set_player_type_age(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'age'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->age = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: ht of class  player_type */
static int tolua_get_player_type_ht(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'ht'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->ht);
 return 1;
}

/* set function: ht of class  player_type */
static int tolua_set_player_type_ht(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'ht'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->ht = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: wt of class  player_type */
static int tolua_get_player_type_wt(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'wt'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->wt);
 return 1;
}

/* set function: wt of class  player_type */
static int tolua_set_player_type_wt(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'wt'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->wt = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: sc of class  player_type */
static int tolua_get_player_type_sc(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'sc'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->sc);
 return 1;
}

/* set function: sc of class  player_type */
static int tolua_set_player_type_sc(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'sc'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->sc = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: au of class  player_type */
static int tolua_get_player_type_au(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'au'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->au);
 return 1;
}

/* set function: au of class  player_type */
static int tolua_set_player_type_au(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'au'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->au = ((s32b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: max_exp of class  player_type */
static int tolua_get_player_type_max_exp(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'max_exp'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->max_exp);
 return 1;
}

/* set function: max_exp of class  player_type */
static int tolua_set_player_type_max_exp(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'max_exp'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->max_exp = ((s32b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: exp of class  player_type */
static int tolua_get_player_type_exp(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'exp'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->exp);
 return 1;
}

/* set function: exp of class  player_type */
static int tolua_set_player_type_exp(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'exp'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->exp = ((s32b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: exp_frac of class  player_type */
static int tolua_get_player_type_exp_frac(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'exp_frac'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->exp_frac);
 return 1;
}

/* set function: exp_frac of class  player_type */
static int tolua_set_player_type_exp_frac(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'exp_frac'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->exp_frac = ((u16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: lev of class  player_type */
static int tolua_get_player_type_lev(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'lev'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->lev);
 return 1;
}

/* set function: lev of class  player_type */
static int tolua_set_player_type_lev(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'lev'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->lev = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: town_num of class  player_type */
static int tolua_get_player_type_town_num(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'town_num'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->town_num);
 return 1;
}

/* set function: town_num of class  player_type */
static int tolua_set_player_type_town_num(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'town_num'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->town_num = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: town_name of class  player_type */
static int tolua_get_player_type_town_name(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'town_name'",NULL);
#endif
 tolua_pushstring(tolua_S,(const char*)self->town_name);
 return 1;
}

/* set function: town_name of class  player_type */
static int tolua_set_player_type_town_name(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'town_name'",NULL);
 if (!tolua_isstring(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
 strncpy(self->town_name,tolua_tostring(tolua_S,2,0),80-1);
 return 0;
}

/* get function: inside_quest of class  player_type */
static int tolua_get_player_type_inside_quest(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'inside_quest'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->inside_quest);
 return 1;
}

/* set function: inside_quest of class  player_type */
static int tolua_set_player_type_inside_quest(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'inside_quest'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->inside_quest = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: quest_name of class  player_type */
static int tolua_get_player_type_quest_name(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'quest_name'",NULL);
#endif
 tolua_pushstring(tolua_S,(const char*)self->quest_name);
 return 1;
}

/* set function: quest_name of class  player_type */
static int tolua_set_player_type_quest_name(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'quest_name'",NULL);
 if (!tolua_isstring(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
 strncpy(self->quest_name,tolua_tostring(tolua_S,2,0),80-1);
 return 0;
}

/* get function: death_dialog of class  player_type */
static int tolua_get_player_type_death_dialog(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'death_dialog'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->death_dialog);
 return 1;
}

/* set function: death_dialog of class  player_type */
static int tolua_set_player_type_death_dialog(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'death_dialog'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->death_dialog = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: eventdeath of class  player_type */
static int tolua_get_player_type_eventdeath(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'eventdeath'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->eventdeath);
 return 1;
}

/* set function: eventdeath of class  player_type */
static int tolua_set_player_type_eventdeath(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'eventdeath'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->eventdeath = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: eventdeathset of class  player_type */
static int tolua_get_player_type_eventdeathset(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'eventdeathset'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->eventdeathset);
 return 1;
}

/* set function: eventdeathset of class  player_type */
static int tolua_set_player_type_eventdeathset(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'eventdeathset'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->eventdeathset = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: wild_x of class  player_type */
static int tolua_get_player_type_wild_x(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'wild_x'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->wild_x);
 return 1;
}

/* set function: wild_x of class  player_type */
static int tolua_set_player_type_wild_x(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'wild_x'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->wild_x = ((s32b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: wild_y of class  player_type */
static int tolua_get_player_type_wild_y(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'wild_y'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->wild_y);
 return 1;
}

/* set function: wild_y of class  player_type */
static int tolua_set_player_type_wild_y(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'wild_y'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->wild_y = ((s32b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: wild_mode of class  player_type */
static int tolua_get_player_type_wild_mode(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'wild_mode'",NULL);
#endif
 tolua_pushboolean(tolua_S,(bool)self->wild_mode);
 return 1;
}

/* set function: wild_mode of class  player_type */
static int tolua_set_player_type_wild_mode(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'wild_mode'",NULL);
 if (!tolua_isboolean(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->wild_mode = ((bool)  tolua_toboolean(tolua_S,2,0));
 return 0;
}

/* get function: mhp of class  player_type */
static int tolua_get_player_type_mhp(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'mhp'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->mhp);
 return 1;
}

/* set function: mhp of class  player_type */
static int tolua_set_player_type_mhp(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'mhp'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->mhp = ((s32b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: chp of class  player_type */
static int tolua_get_player_type_chp(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'chp'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->chp);
 return 1;
}

/* set function: chp of class  player_type */
static int tolua_set_player_type_chp(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'chp'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->chp = ((s32b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: chp_frac of class  player_type */
static int tolua_get_player_type_chp_frac(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'chp_frac'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->chp_frac);
 return 1;
}

/* set function: chp_frac of class  player_type */
static int tolua_set_player_type_chp_frac(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'chp_frac'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->chp_frac = ((u16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: msp of class  player_type */
static int tolua_get_player_type_msp(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'msp'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->msp);
 return 1;
}

/* set function: msp of class  player_type */
static int tolua_set_player_type_msp(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'msp'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->msp = ((s32b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: csp of class  player_type */
static int tolua_get_player_type_csp(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'csp'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->csp);
 return 1;
}

/* set function: csp of class  player_type */
static int tolua_set_player_type_csp(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'csp'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->csp = ((s32b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: csp_frac of class  player_type */
static int tolua_get_player_type_csp_frac(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'csp_frac'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->csp_frac);
 return 1;
}

/* set function: csp_frac of class  player_type */
static int tolua_set_player_type_csp_frac(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'csp_frac'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->csp_frac = ((u16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: max_plv of class  player_type */
static int tolua_get_player_type_max_plv(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'max_plv'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->max_plv);
 return 1;
}

/* set function: max_plv of class  player_type */
static int tolua_set_player_type_max_plv(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'max_plv'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->max_plv = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: stat_max of class  player_type */
static int tolua_get_types_player_type_stat_max(lua_State* tolua_S)
{
 int tolua_index;
  player_type* self;
 lua_pushstring(tolua_S,".self");
 lua_rawget(tolua_S,1);
 self = (player_type*)  lua_touserdata(tolua_S,-1);
#ifndef TOLUA_RELEASE
 {
 tolua_Error tolua_err;
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in array indexing.",&tolua_err);
 }
#endif
 tolua_index = (int)tolua_tonumber(tolua_S,2,0)-1;
#ifndef TOLUA_RELEASE
 if (tolua_index<0 || tolua_index>=6)
 tolua_error(tolua_S,"array indexing out of range.",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->stat_max[tolua_index]);
 return 1;
}

/* set function: stat_max of class  player_type */
static int tolua_set_types_player_type_stat_max(lua_State* tolua_S)
{
 int tolua_index;
  player_type* self;
 lua_pushstring(tolua_S,".self");
 lua_rawget(tolua_S,1);
 self = (player_type*)  lua_touserdata(tolua_S,-1);
#ifndef TOLUA_RELEASE
 {
 tolua_Error tolua_err;
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in array indexing.",&tolua_err);
 }
#endif
 tolua_index = (int)tolua_tonumber(tolua_S,2,0)-1;
#ifndef TOLUA_RELEASE
 if (tolua_index<0 || tolua_index>=6)
 tolua_error(tolua_S,"array indexing out of range.",NULL);
#endif
  self->stat_max[tolua_index] = ((s16b)  tolua_tonumber(tolua_S,3,0));
 return 0;
}

/* get function: stat_cur of class  player_type */
static int tolua_get_types_player_type_stat_cur(lua_State* tolua_S)
{
 int tolua_index;
  player_type* self;
 lua_pushstring(tolua_S,".self");
 lua_rawget(tolua_S,1);
 self = (player_type*)  lua_touserdata(tolua_S,-1);
#ifndef TOLUA_RELEASE
 {
 tolua_Error tolua_err;
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in array indexing.",&tolua_err);
 }
#endif
 tolua_index = (int)tolua_tonumber(tolua_S,2,0)-1;
#ifndef TOLUA_RELEASE
 if (tolua_index<0 || tolua_index>=6)
 tolua_error(tolua_S,"array indexing out of range.",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->stat_cur[tolua_index]);
 return 1;
}

/* set function: stat_cur of class  player_type */
static int tolua_set_types_player_type_stat_cur(lua_State* tolua_S)
{
 int tolua_index;
  player_type* self;
 lua_pushstring(tolua_S,".self");
 lua_rawget(tolua_S,1);
 self = (player_type*)  lua_touserdata(tolua_S,-1);
#ifndef TOLUA_RELEASE
 {
 tolua_Error tolua_err;
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in array indexing.",&tolua_err);
 }
#endif
 tolua_index = (int)tolua_tonumber(tolua_S,2,0)-1;
#ifndef TOLUA_RELEASE
 if (tolua_index<0 || tolua_index>=6)
 tolua_error(tolua_S,"array indexing out of range.",NULL);
#endif
  self->stat_cur[tolua_index] = ((s16b)  tolua_tonumber(tolua_S,3,0));
 return 0;
}

/* get function: fast of class  player_type */
static int tolua_get_player_type_fast(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'fast'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->fast);
 return 1;
}

/* set function: fast of class  player_type */
static int tolua_set_player_type_fast(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'fast'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->fast = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: slow of class  player_type */
static int tolua_get_player_type_slow(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'slow'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->slow);
 return 1;
}

/* set function: slow of class  player_type */
static int tolua_set_player_type_slow(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'slow'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->slow = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: blind of class  player_type */
static int tolua_get_player_type_blind(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'blind'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->blind);
 return 1;
}

/* set function: blind of class  player_type */
static int tolua_set_player_type_blind(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'blind'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->blind = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: paralyzed of class  player_type */
static int tolua_get_player_type_paralyzed(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'paralyzed'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->paralyzed);
 return 1;
}

/* set function: paralyzed of class  player_type */
static int tolua_set_player_type_paralyzed(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'paralyzed'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->paralyzed = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: confused of class  player_type */
static int tolua_get_player_type_confused(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'confused'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->confused);
 return 1;
}

/* set function: confused of class  player_type */
static int tolua_set_player_type_confused(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'confused'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->confused = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: afraid of class  player_type */
static int tolua_get_player_type_afraid(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'afraid'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->afraid);
 return 1;
}

/* set function: afraid of class  player_type */
static int tolua_set_player_type_afraid(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'afraid'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->afraid = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: image of class  player_type */
static int tolua_get_player_type_image(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'image'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->image);
 return 1;
}

/* set function: image of class  player_type */
static int tolua_set_player_type_image(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'image'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->image = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: poisoned of class  player_type */
static int tolua_get_player_type_poisoned(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'poisoned'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->poisoned);
 return 1;
}

/* set function: poisoned of class  player_type */
static int tolua_set_player_type_poisoned(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'poisoned'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->poisoned = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: cut of class  player_type */
static int tolua_get_player_type_cut(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'cut'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->cut);
 return 1;
}

/* set function: cut of class  player_type */
static int tolua_set_player_type_cut(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'cut'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->cut = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: stun of class  player_type */
static int tolua_get_player_type_stun(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'stun'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->stun);
 return 1;
}

/* set function: stun of class  player_type */
static int tolua_set_player_type_stun(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'stun'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->stun = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: hero of class  player_type */
static int tolua_get_player_type_hero(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'hero'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->hero);
 return 1;
}

/* set function: hero of class  player_type */
static int tolua_set_player_type_hero(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'hero'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->hero = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: shero of class  player_type */
static int tolua_get_player_type_shero(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'shero'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->shero);
 return 1;
}

/* set function: shero of class  player_type */
static int tolua_set_player_type_shero(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'shero'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->shero = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: shield of class  player_type */
static int tolua_get_player_type_shield(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'shield'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->shield);
 return 1;
}

/* set function: shield of class  player_type */
static int tolua_set_player_type_shield(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'shield'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->shield = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: shield_power of class  player_type */
static int tolua_get_player_type_shield_power(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'shield_power'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->shield_power);
 return 1;
}

/* set function: shield_power of class  player_type */
static int tolua_set_player_type_shield_power(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'shield_power'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->shield_power = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: blessed of class  player_type */
static int tolua_get_player_type_blessed(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'blessed'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->blessed);
 return 1;
}

/* set function: blessed of class  player_type */
static int tolua_set_player_type_blessed(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'blessed'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->blessed = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: tim_invis of class  player_type */
static int tolua_get_player_type_tim_invis(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'tim_invis'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->tim_invis);
 return 1;
}

/* set function: tim_invis of class  player_type */
static int tolua_set_player_type_tim_invis(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'tim_invis'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->tim_invis = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: tim_infra of class  player_type */
static int tolua_get_player_type_tim_infra(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'tim_infra'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->tim_infra);
 return 1;
}

/* set function: tim_infra of class  player_type */
static int tolua_set_player_type_tim_infra(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'tim_infra'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->tim_infra = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: tim_esp of class  player_type */
static int tolua_get_player_type_tim_esp(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'tim_esp'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->tim_esp);
 return 1;
}

/* set function: tim_esp of class  player_type */
static int tolua_set_player_type_tim_esp(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'tim_esp'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->tim_esp = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: wraith_form of class  player_type */
static int tolua_get_player_type_wraith_form(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'wraith_form'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->wraith_form);
 return 1;
}

/* set function: wraith_form of class  player_type */
static int tolua_set_player_type_wraith_form(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'wraith_form'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->wraith_form = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: tim_ffall of class  player_type */
static int tolua_get_player_type_tim_ffall(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'tim_ffall'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->tim_ffall);
 return 1;
}

/* set function: tim_ffall of class  player_type */
static int tolua_set_player_type_tim_ffall(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'tim_ffall'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->tim_ffall = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: tim_invisible of class  player_type */
static int tolua_get_player_type_tim_invisible(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'tim_invisible'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->tim_invisible);
 return 1;
}

/* set function: tim_invisible of class  player_type */
static int tolua_set_player_type_tim_invisible(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'tim_invisible'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->tim_invisible = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: tim_inv_pow of class  player_type */
static int tolua_get_player_type_tim_inv_pow(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'tim_inv_pow'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->tim_inv_pow);
 return 1;
}

/* set function: tim_inv_pow of class  player_type */
static int tolua_set_player_type_tim_inv_pow(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'tim_inv_pow'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->tim_inv_pow = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: muta1 of class  player_type */
static int tolua_get_player_type_muta1(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'muta1'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->muta1);
 return 1;
}

/* set function: muta1 of class  player_type */
static int tolua_set_player_type_muta1(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'muta1'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->muta1 = ((u32b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: muta2 of class  player_type */
static int tolua_get_player_type_muta2(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'muta2'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->muta2);
 return 1;
}

/* set function: muta2 of class  player_type */
static int tolua_set_player_type_muta2(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'muta2'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->muta2 = ((u32b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: muta3 of class  player_type */
static int tolua_get_player_type_muta3(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'muta3'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->muta3);
 return 1;
}

/* set function: muta3 of class  player_type */
static int tolua_set_player_type_muta3(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'muta3'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->muta3 = ((u32b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: recall_dungeon of class  player_type */
static int tolua_get_player_type_recall_dungeon(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'recall_dungeon'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->recall_dungeon);
 return 1;
}

/* set function: recall_dungeon of class  player_type */
static int tolua_set_player_type_recall_dungeon(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'recall_dungeon'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->recall_dungeon = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: word_recall of class  player_type */
static int tolua_get_player_type_word_recall(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'word_recall'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->word_recall);
 return 1;
}

/* set function: word_recall of class  player_type */
static int tolua_set_player_type_word_recall(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'word_recall'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->word_recall = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: energy of class  player_type */
static int tolua_get_player_type_energy(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'energy'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->energy);
 return 1;
}

/* set function: energy of class  player_type */
static int tolua_set_player_type_energy(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'energy'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->energy = ((s32b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: food of class  player_type */
static int tolua_get_player_type_food(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'food'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->food);
 return 1;
}

/* set function: food of class  player_type */
static int tolua_set_player_type_food(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'food'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->food = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: confusing of class  player_type */
static int tolua_get_player_type_confusing(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'confusing'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->confusing);
 return 1;
}

/* set function: confusing of class  player_type */
static int tolua_set_player_type_confusing(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'confusing'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->confusing = ((byte)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: searching of class  player_type */
static int tolua_get_player_type_searching(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'searching'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->searching);
 return 1;
}

/* set function: searching of class  player_type */
static int tolua_set_player_type_searching(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'searching'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->searching = ((byte)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: old_lite of class  player_type */
static int tolua_get_player_type_old_lite(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'old_lite'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->old_lite);
 return 1;
}

/* set function: old_lite of class  player_type */
static int tolua_set_player_type_old_lite(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'old_lite'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->old_lite = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: old_view of class  player_type */
static int tolua_get_player_type_old_view(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'old_view'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->old_view);
 return 1;
}

/* set function: old_view of class  player_type */
static int tolua_set_player_type_old_view(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'old_view'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->old_view = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: old_food_aux of class  player_type */
static int tolua_get_player_type_old_food_aux(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'old_food_aux'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->old_food_aux);
 return 1;
}

/* set function: old_food_aux of class  player_type */
static int tolua_set_player_type_old_food_aux(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'old_food_aux'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->old_food_aux = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: cur_lite of class  player_type */
static int tolua_get_player_type_cur_lite(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'cur_lite'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->cur_lite);
 return 1;
}

/* set function: cur_lite of class  player_type */
static int tolua_set_player_type_cur_lite(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'cur_lite'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->cur_lite = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: notice of class  player_type */
static int tolua_get_player_type_notice(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'notice'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->notice);
 return 1;
}

/* set function: notice of class  player_type */
static int tolua_set_player_type_notice(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'notice'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->notice = ((u32b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: update of class  player_type */
static int tolua_get_player_type_update(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'update'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->update);
 return 1;
}

/* set function: update of class  player_type */
static int tolua_set_player_type_update(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'update'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->update = ((u32b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: redraw of class  player_type */
static int tolua_get_player_type_redraw(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'redraw'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->redraw);
 return 1;
}

/* set function: redraw of class  player_type */
static int tolua_set_player_type_redraw(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'redraw'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->redraw = ((u32b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: window of class  player_type */
static int tolua_get_player_type_window(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'window'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->window);
 return 1;
}

/* set function: window of class  player_type */
static int tolua_set_player_type_window(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'window'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->window = ((u32b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: stat_use of class  player_type */
static int tolua_get_types_player_type_stat_use(lua_State* tolua_S)
{
 int tolua_index;
  player_type* self;
 lua_pushstring(tolua_S,".self");
 lua_rawget(tolua_S,1);
 self = (player_type*)  lua_touserdata(tolua_S,-1);
#ifndef TOLUA_RELEASE
 {
 tolua_Error tolua_err;
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in array indexing.",&tolua_err);
 }
#endif
 tolua_index = (int)tolua_tonumber(tolua_S,2,0)-1;
#ifndef TOLUA_RELEASE
 if (tolua_index<0 || tolua_index>=6)
 tolua_error(tolua_S,"array indexing out of range.",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->stat_use[tolua_index]);
 return 1;
}

/* set function: stat_use of class  player_type */
static int tolua_set_types_player_type_stat_use(lua_State* tolua_S)
{
 int tolua_index;
  player_type* self;
 lua_pushstring(tolua_S,".self");
 lua_rawget(tolua_S,1);
 self = (player_type*)  lua_touserdata(tolua_S,-1);
#ifndef TOLUA_RELEASE
 {
 tolua_Error tolua_err;
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in array indexing.",&tolua_err);
 }
#endif
 tolua_index = (int)tolua_tonumber(tolua_S,2,0)-1;
#ifndef TOLUA_RELEASE
 if (tolua_index<0 || tolua_index>=6)
 tolua_error(tolua_S,"array indexing out of range.",NULL);
#endif
  self->stat_use[tolua_index] = ((s16b)  tolua_tonumber(tolua_S,3,0));
 return 0;
}

/* get function: stat_top of class  player_type */
static int tolua_get_types_player_type_stat_top(lua_State* tolua_S)
{
 int tolua_index;
  player_type* self;
 lua_pushstring(tolua_S,".self");
 lua_rawget(tolua_S,1);
 self = (player_type*)  lua_touserdata(tolua_S,-1);
#ifndef TOLUA_RELEASE
 {
 tolua_Error tolua_err;
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in array indexing.",&tolua_err);
 }
#endif
 tolua_index = (int)tolua_tonumber(tolua_S,2,0)-1;
#ifndef TOLUA_RELEASE
 if (tolua_index<0 || tolua_index>=6)
 tolua_error(tolua_S,"array indexing out of range.",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->stat_top[tolua_index]);
 return 1;
}

/* set function: stat_top of class  player_type */
static int tolua_set_types_player_type_stat_top(lua_State* tolua_S)
{
 int tolua_index;
  player_type* self;
 lua_pushstring(tolua_S,".self");
 lua_rawget(tolua_S,1);
 self = (player_type*)  lua_touserdata(tolua_S,-1);
#ifndef TOLUA_RELEASE
 {
 tolua_Error tolua_err;
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in array indexing.",&tolua_err);
 }
#endif
 tolua_index = (int)tolua_tonumber(tolua_S,2,0)-1;
#ifndef TOLUA_RELEASE
 if (tolua_index<0 || tolua_index>=6)
 tolua_error(tolua_S,"array indexing out of range.",NULL);
#endif
  self->stat_top[tolua_index] = ((s16b)  tolua_tonumber(tolua_S,3,0));
 return 0;
}

/* get function: stat_add of class  player_type */
static int tolua_get_types_player_type_stat_add(lua_State* tolua_S)
{
 int tolua_index;
  player_type* self;
 lua_pushstring(tolua_S,".self");
 lua_rawget(tolua_S,1);
 self = (player_type*)  lua_touserdata(tolua_S,-1);
#ifndef TOLUA_RELEASE
 {
 tolua_Error tolua_err;
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in array indexing.",&tolua_err);
 }
#endif
 tolua_index = (int)tolua_tonumber(tolua_S,2,0)-1;
#ifndef TOLUA_RELEASE
 if (tolua_index<0 || tolua_index>=6)
 tolua_error(tolua_S,"array indexing out of range.",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->stat_add[tolua_index]);
 return 1;
}

/* set function: stat_add of class  player_type */
static int tolua_set_types_player_type_stat_add(lua_State* tolua_S)
{
 int tolua_index;
  player_type* self;
 lua_pushstring(tolua_S,".self");
 lua_rawget(tolua_S,1);
 self = (player_type*)  lua_touserdata(tolua_S,-1);
#ifndef TOLUA_RELEASE
 {
 tolua_Error tolua_err;
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in array indexing.",&tolua_err);
 }
#endif
 tolua_index = (int)tolua_tonumber(tolua_S,2,0)-1;
#ifndef TOLUA_RELEASE
 if (tolua_index<0 || tolua_index>=6)
 tolua_error(tolua_S,"array indexing out of range.",NULL);
#endif
  self->stat_add[tolua_index] = ((s16b)  tolua_tonumber(tolua_S,3,0));
 return 0;
}

/* get function: stat_ind of class  player_type */
static int tolua_get_types_player_type_stat_ind(lua_State* tolua_S)
{
 int tolua_index;
  player_type* self;
 lua_pushstring(tolua_S,".self");
 lua_rawget(tolua_S,1);
 self = (player_type*)  lua_touserdata(tolua_S,-1);
#ifndef TOLUA_RELEASE
 {
 tolua_Error tolua_err;
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in array indexing.",&tolua_err);
 }
#endif
 tolua_index = (int)tolua_tonumber(tolua_S,2,0)-1;
#ifndef TOLUA_RELEASE
 if (tolua_index<0 || tolua_index>=6)
 tolua_error(tolua_S,"array indexing out of range.",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->stat_ind[tolua_index]);
 return 1;
}

/* set function: stat_ind of class  player_type */
static int tolua_set_types_player_type_stat_ind(lua_State* tolua_S)
{
 int tolua_index;
  player_type* self;
 lua_pushstring(tolua_S,".self");
 lua_rawget(tolua_S,1);
 self = (player_type*)  lua_touserdata(tolua_S,-1);
#ifndef TOLUA_RELEASE
 {
 tolua_Error tolua_err;
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in array indexing.",&tolua_err);
 }
#endif
 tolua_index = (int)tolua_tonumber(tolua_S,2,0)-1;
#ifndef TOLUA_RELEASE
 if (tolua_index<0 || tolua_index>=6)
 tolua_error(tolua_S,"array indexing out of range.",NULL);
#endif
  self->stat_ind[tolua_index] = ((s16b)  tolua_tonumber(tolua_S,3,0));
 return 0;
}

/* get function: stat_cnt of class  player_type */
static int tolua_get_types_player_type_stat_cnt(lua_State* tolua_S)
{
 int tolua_index;
  player_type* self;
 lua_pushstring(tolua_S,".self");
 lua_rawget(tolua_S,1);
 self = (player_type*)  lua_touserdata(tolua_S,-1);
#ifndef TOLUA_RELEASE
 {
 tolua_Error tolua_err;
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in array indexing.",&tolua_err);
 }
#endif
 tolua_index = (int)tolua_tonumber(tolua_S,2,0)-1;
#ifndef TOLUA_RELEASE
 if (tolua_index<0 || tolua_index>=6)
 tolua_error(tolua_S,"array indexing out of range.",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->stat_cnt[tolua_index]);
 return 1;
}

/* set function: stat_cnt of class  player_type */
static int tolua_set_types_player_type_stat_cnt(lua_State* tolua_S)
{
 int tolua_index;
  player_type* self;
 lua_pushstring(tolua_S,".self");
 lua_rawget(tolua_S,1);
 self = (player_type*)  lua_touserdata(tolua_S,-1);
#ifndef TOLUA_RELEASE
 {
 tolua_Error tolua_err;
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in array indexing.",&tolua_err);
 }
#endif
 tolua_index = (int)tolua_tonumber(tolua_S,2,0)-1;
#ifndef TOLUA_RELEASE
 if (tolua_index<0 || tolua_index>=6)
 tolua_error(tolua_S,"array indexing out of range.",NULL);
#endif
  self->stat_cnt[tolua_index] = ((s16b)  tolua_tonumber(tolua_S,3,0));
 return 0;
}

/* get function: stat_los of class  player_type */
static int tolua_get_types_player_type_stat_los(lua_State* tolua_S)
{
 int tolua_index;
  player_type* self;
 lua_pushstring(tolua_S,".self");
 lua_rawget(tolua_S,1);
 self = (player_type*)  lua_touserdata(tolua_S,-1);
#ifndef TOLUA_RELEASE
 {
 tolua_Error tolua_err;
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in array indexing.",&tolua_err);
 }
#endif
 tolua_index = (int)tolua_tonumber(tolua_S,2,0)-1;
#ifndef TOLUA_RELEASE
 if (tolua_index<0 || tolua_index>=6)
 tolua_error(tolua_S,"array indexing out of range.",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->stat_los[tolua_index]);
 return 1;
}

/* set function: stat_los of class  player_type */
static int tolua_set_types_player_type_stat_los(lua_State* tolua_S)
{
 int tolua_index;
  player_type* self;
 lua_pushstring(tolua_S,".self");
 lua_rawget(tolua_S,1);
 self = (player_type*)  lua_touserdata(tolua_S,-1);
#ifndef TOLUA_RELEASE
 {
 tolua_Error tolua_err;
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in array indexing.",&tolua_err);
 }
#endif
 tolua_index = (int)tolua_tonumber(tolua_S,2,0)-1;
#ifndef TOLUA_RELEASE
 if (tolua_index<0 || tolua_index>=6)
 tolua_error(tolua_S,"array indexing out of range.",NULL);
#endif
  self->stat_los[tolua_index] = ((s16b)  tolua_tonumber(tolua_S,3,0));
 return 0;
}

/* get function: stat_mut of class  player_type */
static int tolua_get_types_player_type_stat_mut(lua_State* tolua_S)
{
 int tolua_index;
  player_type* self;
 lua_pushstring(tolua_S,".self");
 lua_rawget(tolua_S,1);
 self = (player_type*)  lua_touserdata(tolua_S,-1);
#ifndef TOLUA_RELEASE
 {
 tolua_Error tolua_err;
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in array indexing.",&tolua_err);
 }
#endif
 tolua_index = (int)tolua_tonumber(tolua_S,2,0)-1;
#ifndef TOLUA_RELEASE
 if (tolua_index<0 || tolua_index>=6)
 tolua_error(tolua_S,"array indexing out of range.",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->stat_mut[tolua_index]);
 return 1;
}

/* set function: stat_mut of class  player_type */
static int tolua_set_types_player_type_stat_mut(lua_State* tolua_S)
{
 int tolua_index;
  player_type* self;
 lua_pushstring(tolua_S,".self");
 lua_rawget(tolua_S,1);
 self = (player_type*)  lua_touserdata(tolua_S,-1);
#ifndef TOLUA_RELEASE
 {
 tolua_Error tolua_err;
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in array indexing.",&tolua_err);
 }
#endif
 tolua_index = (int)tolua_tonumber(tolua_S,2,0)-1;
#ifndef TOLUA_RELEASE
 if (tolua_index<0 || tolua_index>=6)
 tolua_error(tolua_S,"array indexing out of range.",NULL);
#endif
  self->stat_mut[tolua_index] = ((s16b)  tolua_tonumber(tolua_S,3,0));
 return 0;
}

/* get function: resist_conf of class  player_type */
static int tolua_get_player_type_resist_conf(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'resist_conf'",NULL);
#endif
 tolua_pushboolean(tolua_S,(bool)self->resist_conf);
 return 1;
}

/* set function: resist_conf of class  player_type */
static int tolua_set_player_type_resist_conf(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'resist_conf'",NULL);
 if (!tolua_isboolean(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->resist_conf = ((bool)  tolua_toboolean(tolua_S,2,0));
 return 0;
}

/* get function: resist_blind of class  player_type */
static int tolua_get_player_type_resist_blind(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'resist_blind'",NULL);
#endif
 tolua_pushboolean(tolua_S,(bool)self->resist_blind);
 return 1;
}

/* set function: resist_blind of class  player_type */
static int tolua_set_player_type_resist_blind(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'resist_blind'",NULL);
 if (!tolua_isboolean(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->resist_blind = ((bool)  tolua_toboolean(tolua_S,2,0));
 return 0;
}

/* get function: resist_fear of class  player_type */
static int tolua_get_player_type_resist_fear(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'resist_fear'",NULL);
#endif
 tolua_pushboolean(tolua_S,(bool)self->resist_fear);
 return 1;
}

/* set function: resist_fear of class  player_type */
static int tolua_set_player_type_resist_fear(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'resist_fear'",NULL);
 if (!tolua_isboolean(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->resist_fear = ((bool)  tolua_toboolean(tolua_S,2,0));
 return 0;
}

/* get function: reflect of class  player_type */
static int tolua_get_player_type_reflect(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'reflect'",NULL);
#endif
 tolua_pushboolean(tolua_S,(bool)self->reflect);
 return 1;
}

/* set function: reflect of class  player_type */
static int tolua_set_player_type_reflect(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'reflect'",NULL);
 if (!tolua_isboolean(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->reflect = ((bool)  tolua_toboolean(tolua_S,2,0));
 return 0;
}

/* get function: sh_fire of class  player_type */
static int tolua_get_player_type_sh_fire(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'sh_fire'",NULL);
#endif
 tolua_pushboolean(tolua_S,(bool)self->sh_fire);
 return 1;
}

/* set function: sh_fire of class  player_type */
static int tolua_set_player_type_sh_fire(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'sh_fire'",NULL);
 if (!tolua_isboolean(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->sh_fire = ((bool)  tolua_toboolean(tolua_S,2,0));
 return 0;
}

/* get function: sh_elec of class  player_type */
static int tolua_get_player_type_sh_elec(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'sh_elec'",NULL);
#endif
 tolua_pushboolean(tolua_S,(bool)self->sh_elec);
 return 1;
}

/* set function: sh_elec of class  player_type */
static int tolua_set_player_type_sh_elec(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'sh_elec'",NULL);
 if (!tolua_isboolean(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->sh_elec = ((bool)  tolua_toboolean(tolua_S,2,0));
 return 0;
}

/* get function: sustain_str of class  player_type */
static int tolua_get_player_type_sustain_str(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'sustain_str'",NULL);
#endif
 tolua_pushboolean(tolua_S,(bool)self->sustain_str);
 return 1;
}

/* set function: sustain_str of class  player_type */
static int tolua_set_player_type_sustain_str(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'sustain_str'",NULL);
 if (!tolua_isboolean(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->sustain_str = ((bool)  tolua_toboolean(tolua_S,2,0));
 return 0;
}

/* get function: sustain_int of class  player_type */
static int tolua_get_player_type_sustain_int(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'sustain_int'",NULL);
#endif
 tolua_pushboolean(tolua_S,(bool)self->sustain_int);
 return 1;
}

/* set function: sustain_int of class  player_type */
static int tolua_set_player_type_sustain_int(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'sustain_int'",NULL);
 if (!tolua_isboolean(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->sustain_int = ((bool)  tolua_toboolean(tolua_S,2,0));
 return 0;
}

/* get function: sustain_wis of class  player_type */
static int tolua_get_player_type_sustain_wis(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'sustain_wis'",NULL);
#endif
 tolua_pushboolean(tolua_S,(bool)self->sustain_wis);
 return 1;
}

/* set function: sustain_wis of class  player_type */
static int tolua_set_player_type_sustain_wis(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'sustain_wis'",NULL);
 if (!tolua_isboolean(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->sustain_wis = ((bool)  tolua_toboolean(tolua_S,2,0));
 return 0;
}

/* get function: sustain_dex of class  player_type */
static int tolua_get_player_type_sustain_dex(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'sustain_dex'",NULL);
#endif
 tolua_pushboolean(tolua_S,(bool)self->sustain_dex);
 return 1;
}

/* set function: sustain_dex of class  player_type */
static int tolua_set_player_type_sustain_dex(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'sustain_dex'",NULL);
 if (!tolua_isboolean(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->sustain_dex = ((bool)  tolua_toboolean(tolua_S,2,0));
 return 0;
}

/* get function: sustain_con of class  player_type */
static int tolua_get_player_type_sustain_con(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'sustain_con'",NULL);
#endif
 tolua_pushboolean(tolua_S,(bool)self->sustain_con);
 return 1;
}

/* set function: sustain_con of class  player_type */
static int tolua_set_player_type_sustain_con(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'sustain_con'",NULL);
 if (!tolua_isboolean(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->sustain_con = ((bool)  tolua_toboolean(tolua_S,2,0));
 return 0;
}

/* get function: sustain_chr of class  player_type */
static int tolua_get_player_type_sustain_chr(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'sustain_chr'",NULL);
#endif
 tolua_pushboolean(tolua_S,(bool)self->sustain_chr);
 return 1;
}

/* set function: sustain_chr of class  player_type */
static int tolua_set_player_type_sustain_chr(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'sustain_chr'",NULL);
 if (!tolua_isboolean(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->sustain_chr = ((bool)  tolua_toboolean(tolua_S,2,0));
 return 0;
}

/* get function: aggravate of class  player_type */
static int tolua_get_player_type_aggravate(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'aggravate'",NULL);
#endif
 tolua_pushboolean(tolua_S,(bool)self->aggravate);
 return 1;
}

/* set function: aggravate of class  player_type */
static int tolua_set_player_type_aggravate(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'aggravate'",NULL);
 if (!tolua_isboolean(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->aggravate = ((bool)  tolua_toboolean(tolua_S,2,0));
 return 0;
}

/* get function: teleport of class  player_type */
static int tolua_get_player_type_teleport(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'teleport'",NULL);
#endif
 tolua_pushboolean(tolua_S,(bool)self->teleport);
 return 1;
}

/* set function: teleport of class  player_type */
static int tolua_set_player_type_teleport(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'teleport'",NULL);
 if (!tolua_isboolean(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->teleport = ((bool)  tolua_toboolean(tolua_S,2,0));
 return 0;
}

/* get function: exp_drain of class  player_type */
static int tolua_get_player_type_exp_drain(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'exp_drain'",NULL);
#endif
 tolua_pushboolean(tolua_S,(bool)self->exp_drain);
 return 1;
}

/* set function: exp_drain of class  player_type */
static int tolua_set_player_type_exp_drain(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'exp_drain'",NULL);
 if (!tolua_isboolean(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->exp_drain = ((bool)  tolua_toboolean(tolua_S,2,0));
 return 0;
}

/* get function: climb of class  player_type */
static int tolua_get_player_type_climb(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'climb'",NULL);
#endif
 tolua_pushboolean(tolua_S,(bool)self->climb);
 return 1;
}

/* set function: climb of class  player_type */
static int tolua_set_player_type_climb(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'climb'",NULL);
 if (!tolua_isboolean(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->climb = ((bool)  tolua_toboolean(tolua_S,2,0));
 return 0;
}

/* get function: fly of class  player_type */
static int tolua_get_player_type_fly(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'fly'",NULL);
#endif
 tolua_pushboolean(tolua_S,(bool)self->fly);
 return 1;
}

/* set function: fly of class  player_type */
static int tolua_set_player_type_fly(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'fly'",NULL);
 if (!tolua_isboolean(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->fly = ((bool)  tolua_toboolean(tolua_S,2,0));
 return 0;
}

/* get function: ffall of class  player_type */
static int tolua_get_player_type_ffall(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'ffall'",NULL);
#endif
 tolua_pushboolean(tolua_S,(bool)self->ffall);
 return 1;
}

/* set function: ffall of class  player_type */
static int tolua_set_player_type_ffall(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'ffall'",NULL);
 if (!tolua_isboolean(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->ffall = ((bool)  tolua_toboolean(tolua_S,2,0));
 return 0;
}

/* get function: lite of class  player_type */
static int tolua_get_player_type_lite(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'lite'",NULL);
#endif
 tolua_pushboolean(tolua_S,(bool)self->lite);
 return 1;
}

/* set function: lite of class  player_type */
static int tolua_set_player_type_lite(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'lite'",NULL);
 if (!tolua_isboolean(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->lite = ((bool)  tolua_toboolean(tolua_S,2,0));
 return 0;
}

/* get function: free_act of class  player_type */
static int tolua_get_player_type_free_act(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'free_act'",NULL);
#endif
 tolua_pushboolean(tolua_S,(bool)self->free_act);
 return 1;
}

/* set function: free_act of class  player_type */
static int tolua_set_player_type_free_act(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'free_act'",NULL);
 if (!tolua_isboolean(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->free_act = ((bool)  tolua_toboolean(tolua_S,2,0));
 return 0;
}

/* get function: see_inv of class  player_type */
static int tolua_get_player_type_see_inv(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'see_inv'",NULL);
#endif
 tolua_pushboolean(tolua_S,(bool)self->see_inv);
 return 1;
}

/* set function: see_inv of class  player_type */
static int tolua_set_player_type_see_inv(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'see_inv'",NULL);
 if (!tolua_isboolean(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->see_inv = ((bool)  tolua_toboolean(tolua_S,2,0));
 return 0;
}

/* get function: regenerate of class  player_type */
static int tolua_get_player_type_regenerate(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'regenerate'",NULL);
#endif
 tolua_pushboolean(tolua_S,(bool)self->regenerate);
 return 1;
}

/* set function: regenerate of class  player_type */
static int tolua_set_player_type_regenerate(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'regenerate'",NULL);
 if (!tolua_isboolean(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->regenerate = ((bool)  tolua_toboolean(tolua_S,2,0));
 return 0;
}

/* get function: hold_life of class  player_type */
static int tolua_get_player_type_hold_life(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'hold_life'",NULL);
#endif
 tolua_pushboolean(tolua_S,(bool)self->hold_life);
 return 1;
}

/* set function: hold_life of class  player_type */
static int tolua_set_player_type_hold_life(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'hold_life'",NULL);
 if (!tolua_isboolean(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->hold_life = ((bool)  tolua_toboolean(tolua_S,2,0));
 return 0;
}

/* get function: telepathy of class  player_type */
static int tolua_get_player_type_telepathy(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'telepathy'",NULL);
#endif
 tolua_pushboolean(tolua_S,(bool)self->telepathy);
 return 1;
}

/* set function: telepathy of class  player_type */
static int tolua_set_player_type_telepathy(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'telepathy'",NULL);
 if (!tolua_isboolean(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->telepathy = ((bool)  tolua_toboolean(tolua_S,2,0));
 return 0;
}

/* get function: slow_digest of class  player_type */
static int tolua_get_player_type_slow_digest(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'slow_digest'",NULL);
#endif
 tolua_pushboolean(tolua_S,(bool)self->slow_digest);
 return 1;
}

/* set function: slow_digest of class  player_type */
static int tolua_set_player_type_slow_digest(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'slow_digest'",NULL);
 if (!tolua_isboolean(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->slow_digest = ((bool)  tolua_toboolean(tolua_S,2,0));
 return 0;
}

/* get function: xtra_might of class  player_type */
static int tolua_get_player_type_xtra_might(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'xtra_might'",NULL);
#endif
 tolua_pushboolean(tolua_S,(bool)self->xtra_might);
 return 1;
}

/* set function: xtra_might of class  player_type */
static int tolua_set_player_type_xtra_might(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'xtra_might'",NULL);
 if (!tolua_isboolean(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->xtra_might = ((bool)  tolua_toboolean(tolua_S,2,0));
 return 0;
}

/* get function: invis of class  player_type */
static int tolua_get_player_type_invis(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'invis'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->invis);
 return 1;
}

/* set function: invis of class  player_type */
static int tolua_set_player_type_invis(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'invis'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->invis = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: dis_to_h of class  player_type */
static int tolua_get_player_type_dis_to_h(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'dis_to_h'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->dis_to_h);
 return 1;
}

/* set function: dis_to_h of class  player_type */
static int tolua_set_player_type_dis_to_h(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'dis_to_h'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->dis_to_h = ((s32b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: dis_to_d of class  player_type */
static int tolua_get_player_type_dis_to_d(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'dis_to_d'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->dis_to_d);
 return 1;
}

/* set function: dis_to_d of class  player_type */
static int tolua_set_player_type_dis_to_d(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'dis_to_d'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->dis_to_d = ((s32b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: dis_to_a of class  player_type */
static int tolua_get_player_type_dis_to_a(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'dis_to_a'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->dis_to_a);
 return 1;
}

/* set function: dis_to_a of class  player_type */
static int tolua_set_player_type_dis_to_a(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'dis_to_a'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->dis_to_a = ((s32b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: dis_ac of class  player_type */
static int tolua_get_player_type_dis_ac(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'dis_ac'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->dis_ac);
 return 1;
}

/* set function: dis_ac of class  player_type */
static int tolua_set_player_type_dis_ac(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'dis_ac'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->dis_ac = ((s32b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: to_m of class  player_type */
static int tolua_get_player_type_to_m(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'to_m'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->to_m);
 return 1;
}

/* set function: to_m of class  player_type */
static int tolua_set_player_type_to_m(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'to_m'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->to_m = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: to_s of class  player_type */
static int tolua_get_player_type_to_s(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'to_s'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->to_s);
 return 1;
}

/* set function: to_s of class  player_type */
static int tolua_set_player_type_to_s(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'to_s'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->to_s = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: to_h of class  player_type */
static int tolua_get_player_type_to_h(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'to_h'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->to_h);
 return 1;
}

/* set function: to_h of class  player_type */
static int tolua_set_player_type_to_h(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'to_h'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->to_h = ((s32b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: to_d of class  player_type */
static int tolua_get_player_type_to_d(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'to_d'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->to_d);
 return 1;
}

/* set function: to_d of class  player_type */
static int tolua_set_player_type_to_d(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'to_d'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->to_d = ((s32b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: to_a of class  player_type */
static int tolua_get_player_type_to_a(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'to_a'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->to_a);
 return 1;
}

/* set function: to_a of class  player_type */
static int tolua_set_player_type_to_a(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'to_a'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->to_a = ((s32b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: ac of class  player_type */
static int tolua_get_player_type_ac(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'ac'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->ac);
 return 1;
}

/* set function: ac of class  player_type */
static int tolua_set_player_type_ac(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'ac'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->ac = ((s32b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: see_infra of class  player_type */
static int tolua_get_player_type_see_infra(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'see_infra'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->see_infra);
 return 1;
}

/* set function: see_infra of class  player_type */
static int tolua_set_player_type_see_infra(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'see_infra'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->see_infra = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: num_blow of class  player_type */
static int tolua_get_player_type_num_blow(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'num_blow'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->num_blow);
 return 1;
}

/* set function: num_blow of class  player_type */
static int tolua_set_player_type_num_blow(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'num_blow'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->num_blow = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: num_fire of class  player_type */
static int tolua_get_player_type_num_fire(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'num_fire'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->num_fire);
 return 1;
}

/* set function: num_fire of class  player_type */
static int tolua_set_player_type_num_fire(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'num_fire'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->num_fire = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: tval_xtra of class  player_type */
static int tolua_get_player_type_tval_xtra(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'tval_xtra'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->tval_xtra);
 return 1;
}

/* set function: tval_xtra of class  player_type */
static int tolua_set_player_type_tval_xtra(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'tval_xtra'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->tval_xtra = ((byte)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: tval_ammo of class  player_type */
static int tolua_get_player_type_tval_ammo(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'tval_ammo'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->tval_ammo);
 return 1;
}

/* set function: tval_ammo of class  player_type */
static int tolua_set_player_type_tval_ammo(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'tval_ammo'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->tval_ammo = ((byte)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: pspeed of class  player_type */
static int tolua_get_player_type_pspeed(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'pspeed'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->pspeed);
 return 1;
}

/* set function: pspeed of class  player_type */
static int tolua_set_player_type_pspeed(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'pspeed'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->pspeed = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: pet_follow_distance of class  player_type */
static int tolua_get_player_type_pet_follow_distance(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'pet_follow_distance'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->pet_follow_distance);
 return 1;
}

/* set function: pet_follow_distance of class  player_type */
static int tolua_set_player_type_pet_follow_distance(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'pet_follow_distance'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->pet_follow_distance = ((byte)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: pet_open_doors of class  player_type */
static int tolua_get_player_type_pet_open_doors(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'pet_open_doors'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->pet_open_doors);
 return 1;
}

/* set function: pet_open_doors of class  player_type */
static int tolua_set_player_type_pet_open_doors(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'pet_open_doors'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->pet_open_doors = ((byte)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: pet_pickup_items of class  player_type */
static int tolua_get_player_type_pet_pickup_items(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'pet_pickup_items'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->pet_pickup_items);
 return 1;
}

/* set function: pet_pickup_items of class  player_type */
static int tolua_set_player_type_pet_pickup_items(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'pet_pickup_items'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->pet_pickup_items = ((byte)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: body_monster of class  player_type */
static int tolua_get_player_type_body_monster(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'body_monster'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->body_monster);
 return 1;
}

/* set function: body_monster of class  player_type */
static int tolua_set_player_type_body_monster(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'body_monster'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->body_monster = ((u16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: disembodied of class  player_type */
static int tolua_get_player_type_disembodied(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'disembodied'",NULL);
#endif
 tolua_pushboolean(tolua_S,(bool)self->disembodied);
 return 1;
}

/* set function: disembodied of class  player_type */
static int tolua_set_player_type_disembodied(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'disembodied'",NULL);
 if (!tolua_isboolean(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->disembodied = ((bool)  tolua_toboolean(tolua_S,2,0));
 return 0;
}

/* get function: body_parts of class  player_type */
static int tolua_get_types_player_type_body_parts(lua_State* tolua_S)
{
 int tolua_index;
  player_type* self;
 lua_pushstring(tolua_S,".self");
 lua_rawget(tolua_S,1);
 self = (player_type*)  lua_touserdata(tolua_S,-1);
#ifndef TOLUA_RELEASE
 {
 tolua_Error tolua_err;
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in array indexing.",&tolua_err);
 }
#endif
 tolua_index = (int)tolua_tonumber(tolua_S,2,0)-1;
#ifndef TOLUA_RELEASE
 if (tolua_index<0 || tolua_index>=41)
 tolua_error(tolua_S,"array indexing out of range.",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->body_parts[tolua_index]);
 return 1;
}

/* set function: body_parts of class  player_type */
static int tolua_set_types_player_type_body_parts(lua_State* tolua_S)
{
 int tolua_index;
  player_type* self;
 lua_pushstring(tolua_S,".self");
 lua_rawget(tolua_S,1);
 self = (player_type*)  lua_touserdata(tolua_S,-1);
#ifndef TOLUA_RELEASE
 {
 tolua_Error tolua_err;
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in array indexing.",&tolua_err);
 }
#endif
 tolua_index = (int)tolua_tonumber(tolua_S,2,0)-1;
#ifndef TOLUA_RELEASE
 if (tolua_index<0 || tolua_index>=41)
 tolua_error(tolua_S,"array indexing out of range.",NULL);
#endif
  self->body_parts[tolua_index] = ((byte)  tolua_tonumber(tolua_S,3,0));
 return 0;
}

/* get function: leaving of class  player_type */
static int tolua_get_player_type_leaving(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'leaving'",NULL);
#endif
 tolua_pushboolean(tolua_S,(bool)self->leaving);
 return 1;
}

/* set function: leaving of class  player_type */
static int tolua_set_player_type_leaving(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'leaving'",NULL);
 if (!tolua_isboolean(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->leaving = ((bool)  tolua_toboolean(tolua_S,2,0));
 return 0;
}

/* get function: ability_points of class  player_type */
static int tolua_get_player_type_ability_points(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'ability_points'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->ability_points);
 return 1;
}

/* set function: ability_points of class  player_type */
static int tolua_set_player_type_ability_points(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'ability_points'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->ability_points = ((s32b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: memorized of class  player_type */
static int tolua_get_player_type_memorized(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'memorized'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->memorized);
 return 1;
}

/* set function: memorized of class  player_type */
static int tolua_set_player_type_memorized(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'memorized'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->memorized = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: elemlord of class  player_type */
static int tolua_get_player_type_elemlord(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'elemlord'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->elemlord);
 return 1;
}

/* set function: elemlord of class  player_type */
static int tolua_set_player_type_elemlord(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'elemlord'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->elemlord = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: statpoints of class  player_type */
static int tolua_get_player_type_statpoints(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'statpoints'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->statpoints);
 return 1;
}

/* set function: statpoints of class  player_type */
static int tolua_set_player_type_statpoints(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'statpoints'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->statpoints = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: skillpoints of class  player_type */
static int tolua_get_player_type_skillpoints(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'skillpoints'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->skillpoints);
 return 1;
}

/* set function: skillpoints of class  player_type */
static int tolua_set_player_type_skillpoints(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'skillpoints'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->skillpoints = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: skill_base of class  player_type */
static int tolua_get_types_player_type_skill_base(lua_State* tolua_S)
{
 int tolua_index;
  player_type* self;
 lua_pushstring(tolua_S,".self");
 lua_rawget(tolua_S,1);
 self = (player_type*)  lua_touserdata(tolua_S,-1);
#ifndef TOLUA_RELEASE
 {
 tolua_Error tolua_err;
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in array indexing.",&tolua_err);
 }
#endif
 tolua_index = (int)tolua_tonumber(tolua_S,2,0)-1;
#ifndef TOLUA_RELEASE
 if (tolua_index<0 || tolua_index>=SKILL_MAX)
 tolua_error(tolua_S,"array indexing out of range.",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->skill_base[tolua_index]);
 return 1;
}

/* set function: skill_base of class  player_type */
static int tolua_set_types_player_type_skill_base(lua_State* tolua_S)
{
 int tolua_index;
  player_type* self;
 lua_pushstring(tolua_S,".self");
 lua_rawget(tolua_S,1);
 self = (player_type*)  lua_touserdata(tolua_S,-1);
#ifndef TOLUA_RELEASE
 {
 tolua_Error tolua_err;
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in array indexing.",&tolua_err);
 }
#endif
 tolua_index = (int)tolua_tonumber(tolua_S,2,0)-1;
#ifndef TOLUA_RELEASE
 if (tolua_index<0 || tolua_index>=SKILL_MAX)
 tolua_error(tolua_S,"array indexing out of range.",NULL);
#endif
  self->skill_base[tolua_index] = ((s16b)  tolua_tonumber(tolua_S,3,0));
 return 0;
}

/* get function: skill_bonus of class  player_type */
static int tolua_get_types_player_type_skill_bonus(lua_State* tolua_S)
{
 int tolua_index;
  player_type* self;
 lua_pushstring(tolua_S,".self");
 lua_rawget(tolua_S,1);
 self = (player_type*)  lua_touserdata(tolua_S,-1);
#ifndef TOLUA_RELEASE
 {
 tolua_Error tolua_err;
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in array indexing.",&tolua_err);
 }
#endif
 tolua_index = (int)tolua_tonumber(tolua_S,2,0)-1;
#ifndef TOLUA_RELEASE
 if (tolua_index<0 || tolua_index>=SKILL_MAX)
 tolua_error(tolua_S,"array indexing out of range.",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->skill_bonus[tolua_index]);
 return 1;
}

/* set function: skill_bonus of class  player_type */
static int tolua_set_types_player_type_skill_bonus(lua_State* tolua_S)
{
 int tolua_index;
  player_type* self;
 lua_pushstring(tolua_S,".self");
 lua_rawget(tolua_S,1);
 self = (player_type*)  lua_touserdata(tolua_S,-1);
#ifndef TOLUA_RELEASE
 {
 tolua_Error tolua_err;
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in array indexing.",&tolua_err);
 }
#endif
 tolua_index = (int)tolua_tonumber(tolua_S,2,0)-1;
#ifndef TOLUA_RELEASE
 if (tolua_index<0 || tolua_index>=SKILL_MAX)
 tolua_error(tolua_S,"array indexing out of range.",NULL);
#endif
  self->skill_bonus[tolua_index] = ((s16b)  tolua_tonumber(tolua_S,3,0));
 return 0;
}

/* get function: skill of class  player_type */
static int tolua_get_types_player_type_skill(lua_State* tolua_S)
{
 int tolua_index;
  player_type* self;
 lua_pushstring(tolua_S,".self");
 lua_rawget(tolua_S,1);
 self = (player_type*)  lua_touserdata(tolua_S,-1);
#ifndef TOLUA_RELEASE
 {
 tolua_Error tolua_err;
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in array indexing.",&tolua_err);
 }
#endif
 tolua_index = (int)tolua_tonumber(tolua_S,2,0)-1;
#ifndef TOLUA_RELEASE
 if (tolua_index<0 || tolua_index>=SKILL_MAX)
 tolua_error(tolua_S,"array indexing out of range.",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->skill[tolua_index]);
 return 1;
}

/* set function: skill of class  player_type */
static int tolua_set_types_player_type_skill(lua_State* tolua_S)
{
 int tolua_index;
  player_type* self;
 lua_pushstring(tolua_S,".self");
 lua_rawget(tolua_S,1);
 self = (player_type*)  lua_touserdata(tolua_S,-1);
#ifndef TOLUA_RELEASE
 {
 tolua_Error tolua_err;
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in array indexing.",&tolua_err);
 }
#endif
 tolua_index = (int)tolua_tonumber(tolua_S,2,0)-1;
#ifndef TOLUA_RELEASE
 if (tolua_index<0 || tolua_index>=SKILL_MAX)
 tolua_error(tolua_S,"array indexing out of range.",NULL);
#endif
  self->skill[tolua_index] = ((s16b)  tolua_tonumber(tolua_S,3,0));
 return 0;
}

/* get function: str_boost of class  player_type */
static int tolua_get_player_type_str_boost(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'str_boost'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->str_boost);
 return 1;
}

/* set function: str_boost of class  player_type */
static int tolua_set_player_type_str_boost(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'str_boost'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->str_boost = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: str_boost_dur of class  player_type */
static int tolua_get_player_type_str_boost_dur(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'str_boost_dur'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->str_boost_dur);
 return 1;
}

/* set function: str_boost_dur of class  player_type */
static int tolua_set_player_type_str_boost_dur(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'str_boost_dur'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->str_boost_dur = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: int_boost of class  player_type */
static int tolua_get_player_type_int_boost(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'int_boost'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->int_boost);
 return 1;
}

/* set function: int_boost of class  player_type */
static int tolua_set_player_type_int_boost(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'int_boost'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->int_boost = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: int_boost_dur of class  player_type */
static int tolua_get_player_type_int_boost_dur(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'int_boost_dur'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->int_boost_dur);
 return 1;
}

/* set function: int_boost_dur of class  player_type */
static int tolua_set_player_type_int_boost_dur(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'int_boost_dur'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->int_boost_dur = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: wis_boost of class  player_type */
static int tolua_get_player_type_wis_boost(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'wis_boost'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->wis_boost);
 return 1;
}

/* set function: wis_boost of class  player_type */
static int tolua_set_player_type_wis_boost(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'wis_boost'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->wis_boost = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: wis_boost_dur of class  player_type */
static int tolua_get_player_type_wis_boost_dur(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'wis_boost_dur'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->wis_boost_dur);
 return 1;
}

/* set function: wis_boost_dur of class  player_type */
static int tolua_set_player_type_wis_boost_dur(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'wis_boost_dur'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->wis_boost_dur = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: dex_boost of class  player_type */
static int tolua_get_player_type_dex_boost(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'dex_boost'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->dex_boost);
 return 1;
}

/* set function: dex_boost of class  player_type */
static int tolua_set_player_type_dex_boost(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'dex_boost'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->dex_boost = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: dex_boost_dur of class  player_type */
static int tolua_get_player_type_dex_boost_dur(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'dex_boost_dur'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->dex_boost_dur);
 return 1;
}

/* set function: dex_boost_dur of class  player_type */
static int tolua_set_player_type_dex_boost_dur(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'dex_boost_dur'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->dex_boost_dur = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: con_boost of class  player_type */
static int tolua_get_player_type_con_boost(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'con_boost'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->con_boost);
 return 1;
}

/* set function: con_boost of class  player_type */
static int tolua_set_player_type_con_boost(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'con_boost'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->con_boost = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: con_boost_dur of class  player_type */
static int tolua_get_player_type_con_boost_dur(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'con_boost_dur'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->con_boost_dur);
 return 1;
}

/* set function: con_boost_dur of class  player_type */
static int tolua_set_player_type_con_boost_dur(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'con_boost_dur'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->con_boost_dur = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: chr_boost of class  player_type */
static int tolua_get_player_type_chr_boost(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'chr_boost'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->chr_boost);
 return 1;
}

/* set function: chr_boost of class  player_type */
static int tolua_set_player_type_chr_boost(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'chr_boost'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->chr_boost = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: chr_boost_dur of class  player_type */
static int tolua_get_player_type_chr_boost_dur(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'chr_boost_dur'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->chr_boost_dur);
 return 1;
}

/* set function: chr_boost_dur of class  player_type */
static int tolua_set_player_type_chr_boost_dur(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'chr_boost_dur'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->chr_boost_dur = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: pres of class  player_type */
static int tolua_get_player_type_pres(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'pres'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->pres);
 return 1;
}

/* set function: pres of class  player_type */
static int tolua_set_player_type_pres(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'pres'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->pres = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: pres_dur of class  player_type */
static int tolua_get_player_type_pres_dur(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'pres_dur'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->pres_dur);
 return 1;
}

/* set function: pres_dur of class  player_type */
static int tolua_set_player_type_pres_dur(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'pres_dur'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->pres_dur = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: mres of class  player_type */
static int tolua_get_player_type_mres(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'mres'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->mres);
 return 1;
}

/* set function: mres of class  player_type */
static int tolua_set_player_type_mres(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'mres'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->mres = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: mres_dur of class  player_type */
static int tolua_get_player_type_mres_dur(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'mres_dur'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->mres_dur);
 return 1;
}

/* set function: mres_dur of class  player_type */
static int tolua_set_player_type_mres_dur(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'mres_dur'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->mres_dur = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: ac_boost of class  player_type */
static int tolua_get_player_type_ac_boost(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'ac_boost'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->ac_boost);
 return 1;
}

/* set function: ac_boost of class  player_type */
static int tolua_set_player_type_ac_boost(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'ac_boost'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->ac_boost = ((s32b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: ac_boost_dur of class  player_type */
static int tolua_get_player_type_ac_boost_dur(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'ac_boost_dur'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->ac_boost_dur);
 return 1;
}

/* set function: ac_boost_dur of class  player_type */
static int tolua_set_player_type_ac_boost_dur(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'ac_boost_dur'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->ac_boost_dur = ((s32b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: elem_shield of class  player_type */
static int tolua_get_player_type_elem_shield(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'elem_shield'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->elem_shield);
 return 1;
}

/* set function: elem_shield of class  player_type */
static int tolua_set_player_type_elem_shield(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'elem_shield'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->elem_shield = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: elemental of class  player_type */
static int tolua_get_player_type_elemental(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'elemental'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->elemental);
 return 1;
}

/* set function: elemental of class  player_type */
static int tolua_set_player_type_elemental(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'elemental'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->elemental = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: elemental_effects of class  player_type */
static int tolua_get_player_type_elemental_effects(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'elemental_effects'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->elemental_effects);
 return 1;
}

/* set function: elemental_effects of class  player_type */
static int tolua_set_player_type_elemental_effects(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'elemental_effects'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->elemental_effects = ((u32b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: alteration of class  player_type */
static int tolua_get_player_type_alteration(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'alteration'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->alteration);
 return 1;
}

/* set function: alteration of class  player_type */
static int tolua_set_player_type_alteration(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'alteration'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->alteration = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: alteration_effects of class  player_type */
static int tolua_get_player_type_alteration_effects(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'alteration_effects'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->alteration_effects);
 return 1;
}

/* set function: alteration_effects of class  player_type */
static int tolua_set_player_type_alteration_effects(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'alteration_effects'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->alteration_effects = ((u32b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: healing of class  player_type */
static int tolua_get_player_type_healing(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'healing'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->healing);
 return 1;
}

/* set function: healing of class  player_type */
static int tolua_set_player_type_healing(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'healing'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->healing = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: healing_effects of class  player_type */
static int tolua_get_player_type_healing_effects(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'healing_effects'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->healing_effects);
 return 1;
}

/* set function: healing_effects of class  player_type */
static int tolua_set_player_type_healing_effects(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'healing_effects'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->healing_effects = ((u32b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: conjuration of class  player_type */
static int tolua_get_player_type_conjuration(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'conjuration'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->conjuration);
 return 1;
}

/* set function: conjuration of class  player_type */
static int tolua_set_player_type_conjuration(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'conjuration'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->conjuration = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: conjuration_effects of class  player_type */
static int tolua_get_player_type_conjuration_effects(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'conjuration_effects'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->conjuration_effects);
 return 1;
}

/* set function: conjuration_effects of class  player_type */
static int tolua_set_player_type_conjuration_effects(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'conjuration_effects'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->conjuration_effects = ((u32b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: divination of class  player_type */
static int tolua_get_player_type_divination(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'divination'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->divination);
 return 1;
}

/* set function: divination of class  player_type */
static int tolua_set_player_type_divination(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'divination'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->divination = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: divination_effects of class  player_type */
static int tolua_get_player_type_divination_effects(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'divination_effects'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->divination_effects);
 return 1;
}

/* set function: divination_effects of class  player_type */
static int tolua_set_player_type_divination_effects(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'divination_effects'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->divination_effects = ((u32b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: class_level of class  player_type */
static int tolua_get_types_player_type_class_level(lua_State* tolua_S)
{
 int tolua_index;
  player_type* self;
 lua_pushstring(tolua_S,".self");
 lua_rawget(tolua_S,1);
 self = (player_type*)  lua_touserdata(tolua_S,-1);
#ifndef TOLUA_RELEASE
 {
 tolua_Error tolua_err;
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in array indexing.",&tolua_err);
 }
#endif
 tolua_index = (int)tolua_tonumber(tolua_S,2,0)-1;
#ifndef TOLUA_RELEASE
 if (tolua_index<0 || tolua_index>=MAX_CLASS)
 tolua_error(tolua_S,"array indexing out of range.",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->class_level[tolua_index]);
 return 1;
}

/* set function: class_level of class  player_type */
static int tolua_set_types_player_type_class_level(lua_State* tolua_S)
{
 int tolua_index;
  player_type* self;
 lua_pushstring(tolua_S,".self");
 lua_rawget(tolua_S,1);
 self = (player_type*)  lua_touserdata(tolua_S,-1);
#ifndef TOLUA_RELEASE
 {
 tolua_Error tolua_err;
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in array indexing.",&tolua_err);
 }
#endif
 tolua_index = (int)tolua_tonumber(tolua_S,2,0)-1;
#ifndef TOLUA_RELEASE
 if (tolua_index<0 || tolua_index>=MAX_CLASS)
 tolua_error(tolua_S,"array indexing out of range.",NULL);
#endif
  self->class_level[tolua_index] = ((s16b)  tolua_tonumber(tolua_S,3,0));
 return 0;
}

/* get function: class_kills of class  player_type */
static int tolua_get_types_player_type_class_kills(lua_State* tolua_S)
{
 int tolua_index;
  player_type* self;
 lua_pushstring(tolua_S,".self");
 lua_rawget(tolua_S,1);
 self = (player_type*)  lua_touserdata(tolua_S,-1);
#ifndef TOLUA_RELEASE
 {
 tolua_Error tolua_err;
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in array indexing.",&tolua_err);
 }
#endif
 tolua_index = (int)tolua_tonumber(tolua_S,2,0)-1;
#ifndef TOLUA_RELEASE
 if (tolua_index<0 || tolua_index>=MAX_CLASS)
 tolua_error(tolua_S,"array indexing out of range.",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->class_kills[tolua_index]);
 return 1;
}

/* set function: class_kills of class  player_type */
static int tolua_set_types_player_type_class_kills(lua_State* tolua_S)
{
 int tolua_index;
  player_type* self;
 lua_pushstring(tolua_S,".self");
 lua_rawget(tolua_S,1);
 self = (player_type*)  lua_touserdata(tolua_S,-1);
#ifndef TOLUA_RELEASE
 {
 tolua_Error tolua_err;
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in array indexing.",&tolua_err);
 }
#endif
 tolua_index = (int)tolua_tonumber(tolua_S,2,0)-1;
#ifndef TOLUA_RELEASE
 if (tolua_index<0 || tolua_index>=MAX_CLASS)
 tolua_error(tolua_S,"array indexing out of range.",NULL);
#endif
  self->class_kills[tolua_index] = ((s16b)  tolua_tonumber(tolua_S,3,0));
 return 0;
}

/* get function: abilities of class  player_type */
static int tolua_get_types_player_type_abilities(lua_State* tolua_S)
{
 int tolua_index;
  player_type* self;
 lua_pushstring(tolua_S,".self");
 lua_rawget(tolua_S,1);
 self = (player_type*)  lua_touserdata(tolua_S,-1);
#ifndef TOLUA_RELEASE
 {
 tolua_Error tolua_err;
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in array indexing.",&tolua_err);
 }
#endif
 tolua_index = (int)tolua_tonumber(tolua_S,2,0)-1;
#ifndef TOLUA_RELEASE
 if (tolua_index<0 || tolua_index>=MAX_ABILITIES)
 tolua_error(tolua_S,"array indexing out of range.",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->abilities[tolua_index]);
 return 1;
}

/* set function: abilities of class  player_type */
static int tolua_set_types_player_type_abilities(lua_State* tolua_S)
{
 int tolua_index;
  player_type* self;
 lua_pushstring(tolua_S,".self");
 lua_rawget(tolua_S,1);
 self = (player_type*)  lua_touserdata(tolua_S,-1);
#ifndef TOLUA_RELEASE
 {
 tolua_Error tolua_err;
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in array indexing.",&tolua_err);
 }
#endif
 tolua_index = (int)tolua_tonumber(tolua_S,2,0)-1;
#ifndef TOLUA_RELEASE
 if (tolua_index<0 || tolua_index>=MAX_ABILITIES)
 tolua_error(tolua_S,"array indexing out of range.",NULL);
#endif
  self->abilities[tolua_index] = ((s16b)  tolua_tonumber(tolua_S,3,0));
 return 0;
}

/* get function: num_abilities of class  player_type */
static int tolua_get_player_type_num_abilities(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'num_abilities'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->num_abilities);
 return 1;
}

/* set function: num_abilities of class  player_type */
static int tolua_set_player_type_num_abilities(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'num_abilities'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->num_abilities = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: abilities_powers of class  player_type */
static int tolua_get_types_player_type_abilities_powers(lua_State* tolua_S)
{
 int tolua_index;
  player_type* self;
 lua_pushstring(tolua_S,".self");
 lua_rawget(tolua_S,1);
 self = (player_type*)  lua_touserdata(tolua_S,-1);
#ifndef TOLUA_RELEASE
 {
 tolua_Error tolua_err;
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in array indexing.",&tolua_err);
 }
#endif
 tolua_index = (int)tolua_tonumber(tolua_S,2,0)-1;
#ifndef TOLUA_RELEASE
 if (tolua_index<0 || tolua_index>=36)
 tolua_error(tolua_S,"array indexing out of range.",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->abilities_powers[tolua_index]);
 return 1;
}

/* set function: abilities_powers of class  player_type */
static int tolua_set_types_player_type_abilities_powers(lua_State* tolua_S)
{
 int tolua_index;
  player_type* self;
 lua_pushstring(tolua_S,".self");
 lua_rawget(tolua_S,1);
 self = (player_type*)  lua_touserdata(tolua_S,-1);
#ifndef TOLUA_RELEASE
 {
 tolua_Error tolua_err;
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in array indexing.",&tolua_err);
 }
#endif
 tolua_index = (int)tolua_tonumber(tolua_S,2,0)-1;
#ifndef TOLUA_RELEASE
 if (tolua_index<0 || tolua_index>=36)
 tolua_error(tolua_S,"array indexing out of range.",NULL);
#endif
  self->abilities_powers[tolua_index] = ((s16b)  tolua_tonumber(tolua_S,3,0));
 return 0;
}

/* get function: abilities_monster_attacks of class  player_type */
static int tolua_get_types_player_type_abilities_monster_attacks(lua_State* tolua_S)
{
 int tolua_index;
  player_type* self;
 lua_pushstring(tolua_S,".self");
 lua_rawget(tolua_S,1);
 self = (player_type*)  lua_touserdata(tolua_S,-1);
#ifndef TOLUA_RELEASE
 {
 tolua_Error tolua_err;
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in array indexing.",&tolua_err);
 }
#endif
 tolua_index = (int)tolua_tonumber(tolua_S,2,0)-1;
#ifndef TOLUA_RELEASE
 if (tolua_index<0 || tolua_index>=20)
 tolua_error(tolua_S,"array indexing out of range.",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->abilities_monster_attacks[tolua_index]);
 return 1;
}

/* set function: abilities_monster_attacks of class  player_type */
static int tolua_set_types_player_type_abilities_monster_attacks(lua_State* tolua_S)
{
 int tolua_index;
  player_type* self;
 lua_pushstring(tolua_S,".self");
 lua_rawget(tolua_S,1);
 self = (player_type*)  lua_touserdata(tolua_S,-1);
#ifndef TOLUA_RELEASE
 {
 tolua_Error tolua_err;
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in array indexing.",&tolua_err);
 }
#endif
 tolua_index = (int)tolua_tonumber(tolua_S,2,0)-1;
#ifndef TOLUA_RELEASE
 if (tolua_index<0 || tolua_index>=20)
 tolua_error(tolua_S,"array indexing out of range.",NULL);
#endif
  self->abilities_monster_attacks[tolua_index] = ((s16b)  tolua_tonumber(tolua_S,3,0));
 return 0;
}

/* get function: abilities_monster_spells of class  player_type */
static int tolua_get_types_player_type_abilities_monster_spells(lua_State* tolua_S)
{
 int tolua_index;
  player_type* self;
 lua_pushstring(tolua_S,".self");
 lua_rawget(tolua_S,1);
 self = (player_type*)  lua_touserdata(tolua_S,-1);
#ifndef TOLUA_RELEASE
 {
 tolua_Error tolua_err;
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in array indexing.",&tolua_err);
 }
#endif
 tolua_index = (int)tolua_tonumber(tolua_S,2,0)-1;
#ifndef TOLUA_RELEASE
 if (tolua_index<0 || tolua_index>=20)
 tolua_error(tolua_S,"array indexing out of range.",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->abilities_monster_spells[tolua_index]);
 return 1;
}

/* set function: abilities_monster_spells of class  player_type */
static int tolua_set_types_player_type_abilities_monster_spells(lua_State* tolua_S)
{
 int tolua_index;
  player_type* self;
 lua_pushstring(tolua_S,".self");
 lua_rawget(tolua_S,1);
 self = (player_type*)  lua_touserdata(tolua_S,-1);
#ifndef TOLUA_RELEASE
 {
 tolua_Error tolua_err;
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in array indexing.",&tolua_err);
 }
#endif
 tolua_index = (int)tolua_tonumber(tolua_S,2,0)-1;
#ifndef TOLUA_RELEASE
 if (tolua_index<0 || tolua_index>=20)
 tolua_error(tolua_S,"array indexing out of range.",NULL);
#endif
  self->abilities_monster_spells[tolua_index] = ((s16b)  tolua_tonumber(tolua_S,3,0));
 return 0;
}

/* get function: boss_abilities of class  player_type */
static int tolua_get_player_type_boss_abilities(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'boss_abilities'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->boss_abilities);
 return 1;
}

/* set function: boss_abilities of class  player_type */
static int tolua_set_player_type_boss_abilities(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'boss_abilities'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->boss_abilities = ((u32b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: magic_mode of class  player_type */
static int tolua_get_player_type_magic_mode(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'magic_mode'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->magic_mode);
 return 1;
}

/* set function: magic_mode of class  player_type */
static int tolua_set_player_type_magic_mode(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'magic_mode'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->magic_mode = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: auraon of class  player_type */
static int tolua_get_player_type_auraon(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'auraon'",NULL);
#endif
 tolua_pushboolean(tolua_S,(bool)self->auraon);
 return 1;
}

/* set function: auraon of class  player_type */
static int tolua_set_player_type_auraon(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'auraon'",NULL);
 if (!tolua_isboolean(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->auraon = ((bool)  tolua_toboolean(tolua_S,2,0));
 return 0;
}

/* get function: deathcount of class  player_type */
static int tolua_get_player_type_deathcount(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'deathcount'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->deathcount);
 return 1;
}

/* set function: deathcount of class  player_type */
static int tolua_set_player_type_deathcount(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'deathcount'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->deathcount = ((s32b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: guardconfuse of class  player_type */
static int tolua_get_player_type_guardconfuse(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'guardconfuse'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->guardconfuse);
 return 1;
}

/* set function: guardconfuse of class  player_type */
static int tolua_set_player_type_guardconfuse(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'guardconfuse'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->guardconfuse = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: learning of class  player_type */
static int tolua_get_player_type_learning(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'learning'",NULL);
#endif
 tolua_pushboolean(tolua_S,(bool)self->learning);
 return 1;
}

/* set function: learning of class  player_type */
static int tolua_set_player_type_learning(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'learning'",NULL);
 if (!tolua_isboolean(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->learning = ((bool)  tolua_toboolean(tolua_S,2,0));
 return 0;
}

/* get function: startx of class  player_type */
static int tolua_get_player_type_startx(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'startx'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->startx);
 return 1;
}

/* set function: startx of class  player_type */
static int tolua_set_player_type_startx(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'startx'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->startx = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: starty of class  player_type */
static int tolua_get_player_type_starty(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'starty'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->starty);
 return 1;
}

/* set function: starty of class  player_type */
static int tolua_set_player_type_starty(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'starty'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->starty = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: events of class  player_type */
static int tolua_get_types_player_type_events(lua_State* tolua_S)
{
 int tolua_index;
  player_type* self;
 lua_pushstring(tolua_S,".self");
 lua_rawget(tolua_S,1);
 self = (player_type*)  lua_touserdata(tolua_S,-1);
#ifndef TOLUA_RELEASE
 {
 tolua_Error tolua_err;
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in array indexing.",&tolua_err);
 }
#endif
 tolua_index = (int)tolua_tonumber(tolua_S,2,0)-1;
#ifndef TOLUA_RELEASE
 if (tolua_index<0 || tolua_index>=30000)
 tolua_error(tolua_S,"array indexing out of range.",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->events[tolua_index]);
 return 1;
}

/* set function: events of class  player_type */
static int tolua_set_types_player_type_events(lua_State* tolua_S)
{
 int tolua_index;
  player_type* self;
 lua_pushstring(tolua_S,".self");
 lua_rawget(tolua_S,1);
 self = (player_type*)  lua_touserdata(tolua_S,-1);
#ifndef TOLUA_RELEASE
 {
 tolua_Error tolua_err;
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in array indexing.",&tolua_err);
 }
#endif
 tolua_index = (int)tolua_tonumber(tolua_S,2,0)-1;
#ifndef TOLUA_RELEASE
 if (tolua_index<0 || tolua_index>=30000)
 tolua_error(tolua_S,"array indexing out of range.",NULL);
#endif
  self->events[tolua_index] = ((s16b)  tolua_tonumber(tolua_S,3,0));
 return 0;
}

/* get function: resistances of class  player_type */
static int tolua_get_types_player_type_resistances(lua_State* tolua_S)
{
 int tolua_index;
  player_type* self;
 lua_pushstring(tolua_S,".self");
 lua_rawget(tolua_S,1);
 self = (player_type*)  lua_touserdata(tolua_S,-1);
#ifndef TOLUA_RELEASE
 {
 tolua_Error tolua_err;
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in array indexing.",&tolua_err);
 }
#endif
 tolua_index = (int)tolua_tonumber(tolua_S,2,0)-1;
#ifndef TOLUA_RELEASE
 if (tolua_index<0 || tolua_index>=MAX_RESIST)
 tolua_error(tolua_S,"array indexing out of range.",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->resistances[tolua_index]);
 return 1;
}

/* set function: resistances of class  player_type */
static int tolua_set_types_player_type_resistances(lua_State* tolua_S)
{
 int tolua_index;
  player_type* self;
 lua_pushstring(tolua_S,".self");
 lua_rawget(tolua_S,1);
 self = (player_type*)  lua_touserdata(tolua_S,-1);
#ifndef TOLUA_RELEASE
 {
 tolua_Error tolua_err;
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in array indexing.",&tolua_err);
 }
#endif
 tolua_index = (int)tolua_tonumber(tolua_S,2,0)-1;
#ifndef TOLUA_RELEASE
 if (tolua_index<0 || tolua_index>=MAX_RESIST)
 tolua_error(tolua_S,"array indexing out of range.",NULL);
#endif
  self->resistances[tolua_index] = ((s16b)  tolua_tonumber(tolua_S,3,0));
 return 0;
}

/* get function: cur_wid of class  player_type */
static int tolua_get_player_type_cur_wid(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'cur_wid'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->cur_wid);
 return 1;
}

/* set function: cur_wid of class  player_type */
static int tolua_set_player_type_cur_wid(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'cur_wid'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->cur_wid = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: cur_hgt of class  player_type */
static int tolua_get_player_type_cur_hgt(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'cur_hgt'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->cur_hgt);
 return 1;
}

/* set function: cur_hgt of class  player_type */
static int tolua_set_player_type_cur_hgt(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'cur_hgt'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->cur_hgt = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: wild_startx of class  player_type */
static int tolua_get_player_type_wild_startx(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'wild_startx'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->wild_startx);
 return 1;
}

/* set function: wild_startx of class  player_type */
static int tolua_set_player_type_wild_startx(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'wild_startx'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->wild_startx = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: wild_starty of class  player_type */
static int tolua_get_player_type_wild_starty(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'wild_starty'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->wild_starty);
 return 1;
}

/* set function: wild_starty of class  player_type */
static int tolua_set_player_type_wild_starty(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'wild_starty'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->wild_starty = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: questx of class  player_type */
static int tolua_get_player_type_questx(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'questx'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->questx);
 return 1;
}

/* set function: questx of class  player_type */
static int tolua_set_player_type_questx(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'questx'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->questx = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: questy of class  player_type */
static int tolua_get_player_type_questy(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'questy'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->questy);
 return 1;
}

/* set function: questy of class  player_type */
static int tolua_set_player_type_questy(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'questy'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->questy = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: powerattack of class  player_type */
static int tolua_get_player_type_powerattack(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'powerattack'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->powerattack);
 return 1;
}

/* set function: powerattack of class  player_type */
static int tolua_set_player_type_powerattack(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'powerattack'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->powerattack = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: powerlevel of class  player_type */
static int tolua_get_player_type_powerlevel(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'powerlevel'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->powerlevel);
 return 1;
}

/* set function: powerlevel of class  player_type */
static int tolua_set_player_type_powerlevel(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'powerlevel'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->powerlevel = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: num_blow2 of class  player_type */
static int tolua_get_player_type_num_blow2(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'num_blow2'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->num_blow2);
 return 1;
}

/* set function: num_blow2 of class  player_type */
static int tolua_set_player_type_num_blow2(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'num_blow2'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->num_blow2 = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: num_fire2 of class  player_type */
static int tolua_get_player_type_num_fire2(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'num_fire2'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->num_fire2);
 return 1;
}

/* set function: num_fire2 of class  player_type */
static int tolua_set_player_type_num_fire2(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'num_fire2'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->num_fire2 = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: dualwield of class  player_type */
static int tolua_get_player_type_dualwield(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'dualwield'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->dualwield);
 return 1;
}

/* set function: dualwield of class  player_type */
static int tolua_set_player_type_dualwield(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'dualwield'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->dualwield = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: alignment of class  player_type */
static int tolua_get_player_type_alignment(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'alignment'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->alignment);
 return 1;
}

/* set function: alignment of class  player_type */
static int tolua_set_player_type_alignment(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'alignment'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->alignment = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: cursed of class  player_type */
static int tolua_get_player_type_cursed(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'cursed'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->cursed);
 return 1;
}

/* set function: cursed of class  player_type */
static int tolua_set_player_type_cursed(lua_State* tolua_S)
{
  player_type* self = (player_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'cursed'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->cursed = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: towns of class  player_type */
static int tolua_get_types_player_type_towns(lua_State* tolua_S)
{
 int tolua_index;
  player_type* self;
 lua_pushstring(tolua_S,".self");
 lua_rawget(tolua_S,1);
 self = (player_type*)  lua_touserdata(tolua_S,-1);
#ifndef TOLUA_RELEASE
 {
 tolua_Error tolua_err;
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in array indexing.",&tolua_err);
 }
#endif
 tolua_index = (int)tolua_tonumber(tolua_S,2,0)-1;
#ifndef TOLUA_RELEASE
 if (tolua_index<0 || tolua_index>=30000)
 tolua_error(tolua_S,"array indexing out of range.",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->towns[tolua_index]);
 return 1;
}

/* set function: towns of class  player_type */
static int tolua_set_types_player_type_towns(lua_State* tolua_S)
{
 int tolua_index;
  player_type* self;
 lua_pushstring(tolua_S,".self");
 lua_rawget(tolua_S,1);
 self = (player_type*)  lua_touserdata(tolua_S,-1);
#ifndef TOLUA_RELEASE
 {
 tolua_Error tolua_err;
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in array indexing.",&tolua_err);
 }
#endif
 tolua_index = (int)tolua_tonumber(tolua_S,2,0)-1;
#ifndef TOLUA_RELEASE
 if (tolua_index<0 || tolua_index>=30000)
 tolua_error(tolua_S,"array indexing out of range.",NULL);
#endif
  self->towns[tolua_index] = ((s16b)  tolua_tonumber(tolua_S,3,0));
 return 0;
}

/* get function: north of class  border_type */
static int tolua_get_types_border_type_north(lua_State* tolua_S)
{
 int tolua_index;
  border_type* self;
 lua_pushstring(tolua_S,".self");
 lua_rawget(tolua_S,1);
 self = (border_type*)  lua_touserdata(tolua_S,-1);
#ifndef TOLUA_RELEASE
 {
 tolua_Error tolua_err;
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in array indexing.",&tolua_err);
 }
#endif
 tolua_index = (int)tolua_tonumber(tolua_S,2,0)-1;
#ifndef TOLUA_RELEASE
 if (tolua_index<0 || tolua_index>=MAX_WID)
 tolua_error(tolua_S,"array indexing out of range.",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->north[tolua_index]);
 return 1;
}

/* set function: north of class  border_type */
static int tolua_set_types_border_type_north(lua_State* tolua_S)
{
 int tolua_index;
  border_type* self;
 lua_pushstring(tolua_S,".self");
 lua_rawget(tolua_S,1);
 self = (border_type*)  lua_touserdata(tolua_S,-1);
#ifndef TOLUA_RELEASE
 {
 tolua_Error tolua_err;
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in array indexing.",&tolua_err);
 }
#endif
 tolua_index = (int)tolua_tonumber(tolua_S,2,0)-1;
#ifndef TOLUA_RELEASE
 if (tolua_index<0 || tolua_index>=MAX_WID)
 tolua_error(tolua_S,"array indexing out of range.",NULL);
#endif
  self->north[tolua_index] = ((byte)  tolua_tonumber(tolua_S,3,0));
 return 0;
}

/* get function: south of class  border_type */
static int tolua_get_types_border_type_south(lua_State* tolua_S)
{
 int tolua_index;
  border_type* self;
 lua_pushstring(tolua_S,".self");
 lua_rawget(tolua_S,1);
 self = (border_type*)  lua_touserdata(tolua_S,-1);
#ifndef TOLUA_RELEASE
 {
 tolua_Error tolua_err;
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in array indexing.",&tolua_err);
 }
#endif
 tolua_index = (int)tolua_tonumber(tolua_S,2,0)-1;
#ifndef TOLUA_RELEASE
 if (tolua_index<0 || tolua_index>=MAX_WID)
 tolua_error(tolua_S,"array indexing out of range.",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->south[tolua_index]);
 return 1;
}

/* set function: south of class  border_type */
static int tolua_set_types_border_type_south(lua_State* tolua_S)
{
 int tolua_index;
  border_type* self;
 lua_pushstring(tolua_S,".self");
 lua_rawget(tolua_S,1);
 self = (border_type*)  lua_touserdata(tolua_S,-1);
#ifndef TOLUA_RELEASE
 {
 tolua_Error tolua_err;
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in array indexing.",&tolua_err);
 }
#endif
 tolua_index = (int)tolua_tonumber(tolua_S,2,0)-1;
#ifndef TOLUA_RELEASE
 if (tolua_index<0 || tolua_index>=MAX_WID)
 tolua_error(tolua_S,"array indexing out of range.",NULL);
#endif
  self->south[tolua_index] = ((byte)  tolua_tonumber(tolua_S,3,0));
 return 0;
}

/* get function: east of class  border_type */
static int tolua_get_types_border_type_east(lua_State* tolua_S)
{
 int tolua_index;
  border_type* self;
 lua_pushstring(tolua_S,".self");
 lua_rawget(tolua_S,1);
 self = (border_type*)  lua_touserdata(tolua_S,-1);
#ifndef TOLUA_RELEASE
 {
 tolua_Error tolua_err;
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in array indexing.",&tolua_err);
 }
#endif
 tolua_index = (int)tolua_tonumber(tolua_S,2,0)-1;
#ifndef TOLUA_RELEASE
 if (tolua_index<0 || tolua_index>=MAX_HGT)
 tolua_error(tolua_S,"array indexing out of range.",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->east[tolua_index]);
 return 1;
}

/* set function: east of class  border_type */
static int tolua_set_types_border_type_east(lua_State* tolua_S)
{
 int tolua_index;
  border_type* self;
 lua_pushstring(tolua_S,".self");
 lua_rawget(tolua_S,1);
 self = (border_type*)  lua_touserdata(tolua_S,-1);
#ifndef TOLUA_RELEASE
 {
 tolua_Error tolua_err;
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in array indexing.",&tolua_err);
 }
#endif
 tolua_index = (int)tolua_tonumber(tolua_S,2,0)-1;
#ifndef TOLUA_RELEASE
 if (tolua_index<0 || tolua_index>=MAX_HGT)
 tolua_error(tolua_S,"array indexing out of range.",NULL);
#endif
  self->east[tolua_index] = ((byte)  tolua_tonumber(tolua_S,3,0));
 return 0;
}

/* get function: west of class  border_type */
static int tolua_get_types_border_type_west(lua_State* tolua_S)
{
 int tolua_index;
  border_type* self;
 lua_pushstring(tolua_S,".self");
 lua_rawget(tolua_S,1);
 self = (border_type*)  lua_touserdata(tolua_S,-1);
#ifndef TOLUA_RELEASE
 {
 tolua_Error tolua_err;
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in array indexing.",&tolua_err);
 }
#endif
 tolua_index = (int)tolua_tonumber(tolua_S,2,0)-1;
#ifndef TOLUA_RELEASE
 if (tolua_index<0 || tolua_index>=MAX_HGT)
 tolua_error(tolua_S,"array indexing out of range.",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->west[tolua_index]);
 return 1;
}

/* set function: west of class  border_type */
static int tolua_set_types_border_type_west(lua_State* tolua_S)
{
 int tolua_index;
  border_type* self;
 lua_pushstring(tolua_S,".self");
 lua_rawget(tolua_S,1);
 self = (border_type*)  lua_touserdata(tolua_S,-1);
#ifndef TOLUA_RELEASE
 {
 tolua_Error tolua_err;
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in array indexing.",&tolua_err);
 }
#endif
 tolua_index = (int)tolua_tonumber(tolua_S,2,0)-1;
#ifndef TOLUA_RELEASE
 if (tolua_index<0 || tolua_index>=MAX_HGT)
 tolua_error(tolua_S,"array indexing out of range.",NULL);
#endif
  self->west[tolua_index] = ((byte)  tolua_tonumber(tolua_S,3,0));
 return 0;
}

/* get function: north_west of class  border_type */
static int tolua_get_border_type_north_west(lua_State* tolua_S)
{
  border_type* self = (border_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'north_west'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->north_west);
 return 1;
}

/* set function: north_west of class  border_type */
static int tolua_set_border_type_north_west(lua_State* tolua_S)
{
  border_type* self = (border_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'north_west'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->north_west = ((byte)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: north_east of class  border_type */
static int tolua_get_border_type_north_east(lua_State* tolua_S)
{
  border_type* self = (border_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'north_east'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->north_east);
 return 1;
}

/* set function: north_east of class  border_type */
static int tolua_set_border_type_north_east(lua_State* tolua_S)
{
  border_type* self = (border_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'north_east'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->north_east = ((byte)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: south_west of class  border_type */
static int tolua_get_border_type_south_west(lua_State* tolua_S)
{
  border_type* self = (border_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'south_west'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->south_west);
 return 1;
}

/* set function: south_west of class  border_type */
static int tolua_set_border_type_south_west(lua_State* tolua_S)
{
  border_type* self = (border_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'south_west'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->south_west = ((byte)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: south_east of class  border_type */
static int tolua_get_border_type_south_east(lua_State* tolua_S)
{
  border_type* self = (border_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'south_east'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->south_east);
 return 1;
}

/* set function: south_east of class  border_type */
static int tolua_set_border_type_south_east(lua_State* tolua_S)
{
  border_type* self = (border_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'south_east'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->south_east = ((byte)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: name of class  dungeon_info_type */
static int tolua_get_dungeon_info_type_name(lua_State* tolua_S)
{
  dungeon_info_type* self = (dungeon_info_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'name'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->name);
 return 1;
}

/* set function: name of class  dungeon_info_type */
static int tolua_set_dungeon_info_type_name(lua_State* tolua_S)
{
  dungeon_info_type* self = (dungeon_info_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'name'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->name = ((u32b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: text of class  dungeon_info_type */
static int tolua_get_dungeon_info_type_text(lua_State* tolua_S)
{
  dungeon_info_type* self = (dungeon_info_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'text'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->text);
 return 1;
}

/* set function: text of class  dungeon_info_type */
static int tolua_set_dungeon_info_type_text(lua_State* tolua_S)
{
  dungeon_info_type* self = (dungeon_info_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'text'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->text = ((u32b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: floor1 of class  dungeon_info_type */
static int tolua_get_dungeon_info_type_floor1(lua_State* tolua_S)
{
  dungeon_info_type* self = (dungeon_info_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'floor1'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->floor1);
 return 1;
}

/* set function: floor1 of class  dungeon_info_type */
static int tolua_set_dungeon_info_type_floor1(lua_State* tolua_S)
{
  dungeon_info_type* self = (dungeon_info_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'floor1'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->floor1 = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: floor_percent1 of class  dungeon_info_type */
static int tolua_get_dungeon_info_type_floor_percent1(lua_State* tolua_S)
{
  dungeon_info_type* self = (dungeon_info_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'floor_percent1'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->floor_percent1);
 return 1;
}

/* set function: floor_percent1 of class  dungeon_info_type */
static int tolua_set_dungeon_info_type_floor_percent1(lua_State* tolua_S)
{
  dungeon_info_type* self = (dungeon_info_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'floor_percent1'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->floor_percent1 = ((byte)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: floor2 of class  dungeon_info_type */
static int tolua_get_dungeon_info_type_floor2(lua_State* tolua_S)
{
  dungeon_info_type* self = (dungeon_info_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'floor2'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->floor2);
 return 1;
}

/* set function: floor2 of class  dungeon_info_type */
static int tolua_set_dungeon_info_type_floor2(lua_State* tolua_S)
{
  dungeon_info_type* self = (dungeon_info_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'floor2'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->floor2 = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: floor_percent2 of class  dungeon_info_type */
static int tolua_get_dungeon_info_type_floor_percent2(lua_State* tolua_S)
{
  dungeon_info_type* self = (dungeon_info_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'floor_percent2'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->floor_percent2);
 return 1;
}

/* set function: floor_percent2 of class  dungeon_info_type */
static int tolua_set_dungeon_info_type_floor_percent2(lua_State* tolua_S)
{
  dungeon_info_type* self = (dungeon_info_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'floor_percent2'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->floor_percent2 = ((byte)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: floor3 of class  dungeon_info_type */
static int tolua_get_dungeon_info_type_floor3(lua_State* tolua_S)
{
  dungeon_info_type* self = (dungeon_info_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'floor3'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->floor3);
 return 1;
}

/* set function: floor3 of class  dungeon_info_type */
static int tolua_set_dungeon_info_type_floor3(lua_State* tolua_S)
{
  dungeon_info_type* self = (dungeon_info_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'floor3'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->floor3 = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: floor_percent3 of class  dungeon_info_type */
static int tolua_get_dungeon_info_type_floor_percent3(lua_State* tolua_S)
{
  dungeon_info_type* self = (dungeon_info_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'floor_percent3'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->floor_percent3);
 return 1;
}

/* set function: floor_percent3 of class  dungeon_info_type */
static int tolua_set_dungeon_info_type_floor_percent3(lua_State* tolua_S)
{
  dungeon_info_type* self = (dungeon_info_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'floor_percent3'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->floor_percent3 = ((byte)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: outer_wall of class  dungeon_info_type */
static int tolua_get_dungeon_info_type_outer_wall(lua_State* tolua_S)
{
  dungeon_info_type* self = (dungeon_info_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'outer_wall'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->outer_wall);
 return 1;
}

/* set function: outer_wall of class  dungeon_info_type */
static int tolua_set_dungeon_info_type_outer_wall(lua_State* tolua_S)
{
  dungeon_info_type* self = (dungeon_info_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'outer_wall'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->outer_wall = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: inner_wall of class  dungeon_info_type */
static int tolua_get_dungeon_info_type_inner_wall(lua_State* tolua_S)
{
  dungeon_info_type* self = (dungeon_info_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'inner_wall'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->inner_wall);
 return 1;
}

/* set function: inner_wall of class  dungeon_info_type */
static int tolua_set_dungeon_info_type_inner_wall(lua_State* tolua_S)
{
  dungeon_info_type* self = (dungeon_info_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'inner_wall'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->inner_wall = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: fill_type1 of class  dungeon_info_type */
static int tolua_get_dungeon_info_type_fill_type1(lua_State* tolua_S)
{
  dungeon_info_type* self = (dungeon_info_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'fill_type1'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->fill_type1);
 return 1;
}

/* set function: fill_type1 of class  dungeon_info_type */
static int tolua_set_dungeon_info_type_fill_type1(lua_State* tolua_S)
{
  dungeon_info_type* self = (dungeon_info_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'fill_type1'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->fill_type1 = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: fill_percent1 of class  dungeon_info_type */
static int tolua_get_dungeon_info_type_fill_percent1(lua_State* tolua_S)
{
  dungeon_info_type* self = (dungeon_info_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'fill_percent1'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->fill_percent1);
 return 1;
}

/* set function: fill_percent1 of class  dungeon_info_type */
static int tolua_set_dungeon_info_type_fill_percent1(lua_State* tolua_S)
{
  dungeon_info_type* self = (dungeon_info_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'fill_percent1'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->fill_percent1 = ((byte)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: fill_type2 of class  dungeon_info_type */
static int tolua_get_dungeon_info_type_fill_type2(lua_State* tolua_S)
{
  dungeon_info_type* self = (dungeon_info_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'fill_type2'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->fill_type2);
 return 1;
}

/* set function: fill_type2 of class  dungeon_info_type */
static int tolua_set_dungeon_info_type_fill_type2(lua_State* tolua_S)
{
  dungeon_info_type* self = (dungeon_info_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'fill_type2'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->fill_type2 = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: fill_percent2 of class  dungeon_info_type */
static int tolua_get_dungeon_info_type_fill_percent2(lua_State* tolua_S)
{
  dungeon_info_type* self = (dungeon_info_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'fill_percent2'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->fill_percent2);
 return 1;
}

/* set function: fill_percent2 of class  dungeon_info_type */
static int tolua_set_dungeon_info_type_fill_percent2(lua_State* tolua_S)
{
  dungeon_info_type* self = (dungeon_info_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'fill_percent2'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->fill_percent2 = ((byte)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: fill_type3 of class  dungeon_info_type */
static int tolua_get_dungeon_info_type_fill_type3(lua_State* tolua_S)
{
  dungeon_info_type* self = (dungeon_info_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'fill_type3'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->fill_type3);
 return 1;
}

/* set function: fill_type3 of class  dungeon_info_type */
static int tolua_set_dungeon_info_type_fill_type3(lua_State* tolua_S)
{
  dungeon_info_type* self = (dungeon_info_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'fill_type3'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->fill_type3 = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: fill_percent3 of class  dungeon_info_type */
static int tolua_get_dungeon_info_type_fill_percent3(lua_State* tolua_S)
{
  dungeon_info_type* self = (dungeon_info_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'fill_percent3'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->fill_percent3);
 return 1;
}

/* set function: fill_percent3 of class  dungeon_info_type */
static int tolua_set_dungeon_info_type_fill_percent3(lua_State* tolua_S)
{
  dungeon_info_type* self = (dungeon_info_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'fill_percent3'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->fill_percent3 = ((byte)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: mindepth of class  dungeon_info_type */
static int tolua_get_dungeon_info_type_mindepth(lua_State* tolua_S)
{
  dungeon_info_type* self = (dungeon_info_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'mindepth'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->mindepth);
 return 1;
}

/* set function: mindepth of class  dungeon_info_type */
static int tolua_set_dungeon_info_type_mindepth(lua_State* tolua_S)
{
  dungeon_info_type* self = (dungeon_info_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'mindepth'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->mindepth = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: maxdepth of class  dungeon_info_type */
static int tolua_get_dungeon_info_type_maxdepth(lua_State* tolua_S)
{
  dungeon_info_type* self = (dungeon_info_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'maxdepth'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->maxdepth);
 return 1;
}

/* set function: maxdepth of class  dungeon_info_type */
static int tolua_set_dungeon_info_type_maxdepth(lua_State* tolua_S)
{
  dungeon_info_type* self = (dungeon_info_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'maxdepth'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->maxdepth = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: principal of class  dungeon_info_type */
static int tolua_get_dungeon_info_type_principal(lua_State* tolua_S)
{
  dungeon_info_type* self = (dungeon_info_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'principal'",NULL);
#endif
 tolua_pushboolean(tolua_S,(bool)self->principal);
 return 1;
}

/* set function: principal of class  dungeon_info_type */
static int tolua_set_dungeon_info_type_principal(lua_State* tolua_S)
{
  dungeon_info_type* self = (dungeon_info_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'principal'",NULL);
 if (!tolua_isboolean(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->principal = ((bool)  tolua_toboolean(tolua_S,2,0));
 return 0;
}

/* get function: next of class  dungeon_info_type */
static int tolua_get_dungeon_info_type_next(lua_State* tolua_S)
{
  dungeon_info_type* self = (dungeon_info_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'next'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->next);
 return 1;
}

/* set function: next of class  dungeon_info_type */
static int tolua_set_dungeon_info_type_next(lua_State* tolua_S)
{
  dungeon_info_type* self = (dungeon_info_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'next'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->next = ((byte)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: min_plev of class  dungeon_info_type */
static int tolua_get_dungeon_info_type_min_plev(lua_State* tolua_S)
{
  dungeon_info_type* self = (dungeon_info_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'min_plev'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->min_plev);
 return 1;
}

/* set function: min_plev of class  dungeon_info_type */
static int tolua_set_dungeon_info_type_min_plev(lua_State* tolua_S)
{
  dungeon_info_type* self = (dungeon_info_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'min_plev'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->min_plev = ((byte)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: mode of class  dungeon_info_type */
static int tolua_get_dungeon_info_type_mode(lua_State* tolua_S)
{
  dungeon_info_type* self = (dungeon_info_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'mode'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->mode);
 return 1;
}

/* set function: mode of class  dungeon_info_type */
static int tolua_set_dungeon_info_type_mode(lua_State* tolua_S)
{
  dungeon_info_type* self = (dungeon_info_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'mode'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->mode = ((byte)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: min_m_alloc_level of class  dungeon_info_type */
static int tolua_get_dungeon_info_type_min_m_alloc_level(lua_State* tolua_S)
{
  dungeon_info_type* self = (dungeon_info_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'min_m_alloc_level'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->min_m_alloc_level);
 return 1;
}

/* set function: min_m_alloc_level of class  dungeon_info_type */
static int tolua_set_dungeon_info_type_min_m_alloc_level(lua_State* tolua_S)
{
  dungeon_info_type* self = (dungeon_info_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'min_m_alloc_level'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->min_m_alloc_level = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: max_m_alloc_chance of class  dungeon_info_type */
static int tolua_get_dungeon_info_type_max_m_alloc_chance(lua_State* tolua_S)
{
  dungeon_info_type* self = (dungeon_info_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'max_m_alloc_chance'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->max_m_alloc_chance);
 return 1;
}

/* set function: max_m_alloc_chance of class  dungeon_info_type */
static int tolua_set_dungeon_info_type_max_m_alloc_chance(lua_State* tolua_S)
{
  dungeon_info_type* self = (dungeon_info_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'max_m_alloc_chance'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->max_m_alloc_chance = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: flags1 of class  dungeon_info_type */
static int tolua_get_dungeon_info_type_flags1(lua_State* tolua_S)
{
  dungeon_info_type* self = (dungeon_info_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'flags1'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->flags1);
 return 1;
}

/* set function: flags1 of class  dungeon_info_type */
static int tolua_set_dungeon_info_type_flags1(lua_State* tolua_S)
{
  dungeon_info_type* self = (dungeon_info_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'flags1'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->flags1 = ((s32b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: mflags1 of class  dungeon_info_type */
static int tolua_get_dungeon_info_type_mflags1(lua_State* tolua_S)
{
  dungeon_info_type* self = (dungeon_info_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'mflags1'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->mflags1);
 return 1;
}

/* set function: mflags1 of class  dungeon_info_type */
static int tolua_set_dungeon_info_type_mflags1(lua_State* tolua_S)
{
  dungeon_info_type* self = (dungeon_info_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'mflags1'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->mflags1 = ((s32b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: mflags2 of class  dungeon_info_type */
static int tolua_get_dungeon_info_type_mflags2(lua_State* tolua_S)
{
  dungeon_info_type* self = (dungeon_info_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'mflags2'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->mflags2);
 return 1;
}

/* set function: mflags2 of class  dungeon_info_type */
static int tolua_set_dungeon_info_type_mflags2(lua_State* tolua_S)
{
  dungeon_info_type* self = (dungeon_info_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'mflags2'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->mflags2 = ((s32b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: mflags3 of class  dungeon_info_type */
static int tolua_get_dungeon_info_type_mflags3(lua_State* tolua_S)
{
  dungeon_info_type* self = (dungeon_info_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'mflags3'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->mflags3);
 return 1;
}

/* set function: mflags3 of class  dungeon_info_type */
static int tolua_set_dungeon_info_type_mflags3(lua_State* tolua_S)
{
  dungeon_info_type* self = (dungeon_info_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'mflags3'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->mflags3 = ((s32b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: mflags4 of class  dungeon_info_type */
static int tolua_get_dungeon_info_type_mflags4(lua_State* tolua_S)
{
  dungeon_info_type* self = (dungeon_info_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'mflags4'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->mflags4);
 return 1;
}

/* set function: mflags4 of class  dungeon_info_type */
static int tolua_set_dungeon_info_type_mflags4(lua_State* tolua_S)
{
  dungeon_info_type* self = (dungeon_info_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'mflags4'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->mflags4 = ((s32b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: mflags5 of class  dungeon_info_type */
static int tolua_get_dungeon_info_type_mflags5(lua_State* tolua_S)
{
  dungeon_info_type* self = (dungeon_info_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'mflags5'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->mflags5);
 return 1;
}

/* set function: mflags5 of class  dungeon_info_type */
static int tolua_set_dungeon_info_type_mflags5(lua_State* tolua_S)
{
  dungeon_info_type* self = (dungeon_info_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'mflags5'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->mflags5 = ((s32b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: mflags6 of class  dungeon_info_type */
static int tolua_get_dungeon_info_type_mflags6(lua_State* tolua_S)
{
  dungeon_info_type* self = (dungeon_info_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'mflags6'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->mflags6);
 return 1;
}

/* set function: mflags6 of class  dungeon_info_type */
static int tolua_set_dungeon_info_type_mflags6(lua_State* tolua_S)
{
  dungeon_info_type* self = (dungeon_info_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'mflags6'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->mflags6 = ((s32b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: mflags7 of class  dungeon_info_type */
static int tolua_get_dungeon_info_type_mflags7(lua_State* tolua_S)
{
  dungeon_info_type* self = (dungeon_info_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'mflags7'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->mflags7);
 return 1;
}

/* set function: mflags7 of class  dungeon_info_type */
static int tolua_set_dungeon_info_type_mflags7(lua_State* tolua_S)
{
  dungeon_info_type* self = (dungeon_info_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'mflags7'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->mflags7 = ((s32b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: mflags8 of class  dungeon_info_type */
static int tolua_get_dungeon_info_type_mflags8(lua_State* tolua_S)
{
  dungeon_info_type* self = (dungeon_info_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'mflags8'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->mflags8);
 return 1;
}

/* set function: mflags8 of class  dungeon_info_type */
static int tolua_set_dungeon_info_type_mflags8(lua_State* tolua_S)
{
  dungeon_info_type* self = (dungeon_info_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'mflags8'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->mflags8 = ((s32b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: mflags9 of class  dungeon_info_type */
static int tolua_get_dungeon_info_type_mflags9(lua_State* tolua_S)
{
  dungeon_info_type* self = (dungeon_info_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'mflags9'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->mflags9);
 return 1;
}

/* set function: mflags9 of class  dungeon_info_type */
static int tolua_set_dungeon_info_type_mflags9(lua_State* tolua_S)
{
  dungeon_info_type* self = (dungeon_info_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'mflags9'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->mflags9 = ((s32b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: r_char of class  dungeon_info_type */
static int tolua_get_dungeon_info_type_r_char(lua_State* tolua_S)
{
  dungeon_info_type* self = (dungeon_info_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'r_char'",NULL);
#endif
 tolua_pushstring(tolua_S,(const char*)self->r_char);
 return 1;
}

/* set function: r_char of class  dungeon_info_type */
static int tolua_set_dungeon_info_type_r_char(lua_State* tolua_S)
{
  dungeon_info_type* self = (dungeon_info_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'r_char'",NULL);
 if (!tolua_isstring(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
 strncpy(self->r_char,tolua_tostring(tolua_S,2,0),5-1);
 return 0;
}

/* get function: final_artifact of class  dungeon_info_type */
static int tolua_get_dungeon_info_type_final_artifact(lua_State* tolua_S)
{
  dungeon_info_type* self = (dungeon_info_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'final_artifact'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->final_artifact);
 return 1;
}

/* set function: final_artifact of class  dungeon_info_type */
static int tolua_set_dungeon_info_type_final_artifact(lua_State* tolua_S)
{
  dungeon_info_type* self = (dungeon_info_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'final_artifact'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->final_artifact = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: final_guardian of class  dungeon_info_type */
static int tolua_get_dungeon_info_type_final_guardian(lua_State* tolua_S)
{
  dungeon_info_type* self = (dungeon_info_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'final_guardian'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->final_guardian);
 return 1;
}

/* set function: final_guardian of class  dungeon_info_type */
static int tolua_set_dungeon_info_type_final_guardian(lua_State* tolua_S)
{
  dungeon_info_type* self = (dungeon_info_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'final_guardian'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->final_guardian = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: special_percent of class  dungeon_info_type */
static int tolua_get_dungeon_info_type_special_percent(lua_State* tolua_S)
{
  dungeon_info_type* self = (dungeon_info_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'special_percent'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->special_percent);
 return 1;
}

/* set function: special_percent of class  dungeon_info_type */
static int tolua_set_dungeon_info_type_special_percent(lua_State* tolua_S)
{
  dungeon_info_type* self = (dungeon_info_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'special_percent'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->special_percent = ((byte)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: quest of class  dungeon_info_type */
static int tolua_get_dungeon_info_type_quest(lua_State* tolua_S)
{
  dungeon_info_type* self = (dungeon_info_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'quest'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->quest);
 return 1;
}

/* set function: quest of class  dungeon_info_type */
static int tolua_set_dungeon_info_type_quest(lua_State* tolua_S)
{
  dungeon_info_type* self = (dungeon_info_type*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'quest'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->quest = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: name of class  monster_magics */
static int tolua_get_monster_magics_name(lua_State* tolua_S)
{
  monster_magics* self = (monster_magics*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'name'",NULL);
#endif
 tolua_pushstring(tolua_S,(const char*)self->name);
 return 1;
}

/* set function: name of class  monster_magics */
static int tolua_set_monster_magics_name(lua_State* tolua_S)
{
  monster_magics* self = (monster_magics*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'name'",NULL);
 if (!tolua_isstring(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
 strncpy(self->name,tolua_tostring(tolua_S,2,0),30-1);
 return 0;
}

/* get function: act of class  monster_magics */
static int tolua_get_monster_magics_act(lua_State* tolua_S)
{
  monster_magics* self = (monster_magics*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'act'",NULL);
#endif
 tolua_pushstring(tolua_S,(const char*)self->act);
 return 1;
}

/* set function: act of class  monster_magics */
static int tolua_set_monster_magics_act(lua_State* tolua_S)
{
  monster_magics* self = (monster_magics*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'act'",NULL);
 if (!tolua_isstring(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
 strncpy(self->act,tolua_tostring(tolua_S,2,0),30-1);
 return 0;
}

/* get function: type of class  monster_magics */
static int tolua_get_monster_magics_type(lua_State* tolua_S)
{
  monster_magics* self = (monster_magics*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'type'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->type);
 return 1;
}

/* set function: type of class  monster_magics */
static int tolua_set_monster_magics_type(lua_State* tolua_S)
{
  monster_magics* self = (monster_magics*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'type'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->type = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: power of class  monster_magics */
static int tolua_get_monster_magics_power(lua_State* tolua_S)
{
  monster_magics* self = (monster_magics*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'power'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->power);
 return 1;
}

/* set function: power of class  monster_magics */
static int tolua_set_monster_magics_power(lua_State* tolua_S)
{
  monster_magics* self = (monster_magics*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'power'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->power = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: special1 of class  monster_magics */
static int tolua_get_monster_magics_special1(lua_State* tolua_S)
{
  monster_magics* self = (monster_magics*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'special1'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->special1);
 return 1;
}

/* set function: special1 of class  monster_magics */
static int tolua_set_monster_magics_special1(lua_State* tolua_S)
{
  monster_magics* self = (monster_magics*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'special1'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->special1 = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: special2 of class  monster_magics */
static int tolua_get_monster_magics_special2(lua_State* tolua_S)
{
  monster_magics* self = (monster_magics*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'special2'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->special2);
 return 1;
}

/* set function: special2 of class  monster_magics */
static int tolua_set_monster_magics_special2(lua_State* tolua_S)
{
  monster_magics* self = (monster_magics*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'special2'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->special2 = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: special3 of class  monster_magics */
static int tolua_get_monster_magics_special3(lua_State* tolua_S)
{
  monster_magics* self = (monster_magics*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'special3'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->special3);
 return 1;
}

/* set function: special3 of class  monster_magics */
static int tolua_set_monster_magics_special3(lua_State* tolua_S)
{
  monster_magics* self = (monster_magics*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'special3'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->special3 = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: summchar of class  monster_magics */
static int tolua_get_monster_magics_summchar(lua_State* tolua_S)
{
  monster_magics* self = (monster_magics*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'summchar'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->summchar);
 return 1;
}

/* set function: summchar of class  monster_magics */
static int tolua_set_monster_magics_summchar(lua_State* tolua_S)
{
  monster_magics* self = (monster_magics*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'summchar'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->summchar = ((char)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: cost of class  monster_magics */
static int tolua_get_monster_magics_cost(lua_State* tolua_S)
{
  monster_magics* self = (monster_magics*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'cost'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->cost);
 return 1;
}

/* set function: cost of class  monster_magics */
static int tolua_set_monster_magics_cost(lua_State* tolua_S)
{
  monster_magics* self = (monster_magics*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'cost'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->cost = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: name of class  dialog_answers */
static int tolua_get_dialog_answers_name(lua_State* tolua_S)
{
  dialog_answers* self = (dialog_answers*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'name'",NULL);
#endif
 tolua_pushstring(tolua_S,(const char*)self->name);
 return 1;
}

/* set function: name of class  dialog_answers */
static int tolua_set_dialog_answers_name(lua_State* tolua_S)
{
  dialog_answers* self = (dialog_answers*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'name'",NULL);
 if (!tolua_isstring(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
 strncpy(self->name,tolua_tostring(tolua_S,2,0),80-1);
 return 0;
}

/* get function: ctype of class  dialog_answers */
static int tolua_get_dialog_answers_ctype(lua_State* tolua_S)
{
  dialog_answers* self = (dialog_answers*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'ctype'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->ctype);
 return 1;
}

/* set function: ctype of class  dialog_answers */
static int tolua_set_dialog_answers_ctype(lua_State* tolua_S)
{
  dialog_answers* self = (dialog_answers*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'ctype'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->ctype = ((s32b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: cparam1 of class  dialog_answers */
static int tolua_get_dialog_answers_cparam1(lua_State* tolua_S)
{
  dialog_answers* self = (dialog_answers*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'cparam1'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->cparam1);
 return 1;
}

/* set function: cparam1 of class  dialog_answers */
static int tolua_set_dialog_answers_cparam1(lua_State* tolua_S)
{
  dialog_answers* self = (dialog_answers*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'cparam1'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->cparam1 = ((s32b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: cparam2 of class  dialog_answers */
static int tolua_get_dialog_answers_cparam2(lua_State* tolua_S)
{
  dialog_answers* self = (dialog_answers*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'cparam2'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->cparam2);
 return 1;
}

/* set function: cparam2 of class  dialog_answers */
static int tolua_set_dialog_answers_cparam2(lua_State* tolua_S)
{
  dialog_answers* self = (dialog_answers*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'cparam2'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->cparam2 = ((s32b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: effect of class  dialog_answers */
static int tolua_get_dialog_answers_effect(lua_State* tolua_S)
{
  dialog_answers* self = (dialog_answers*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'effect'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->effect);
 return 1;
}

/* set function: effect of class  dialog_answers */
static int tolua_set_dialog_answers_effect(lua_State* tolua_S)
{
  dialog_answers* self = (dialog_answers*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'effect'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->effect = ((s32b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: eparam1 of class  dialog_answers */
static int tolua_get_dialog_answers_eparam1(lua_State* tolua_S)
{
  dialog_answers* self = (dialog_answers*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'eparam1'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->eparam1);
 return 1;
}

/* set function: eparam1 of class  dialog_answers */
static int tolua_set_dialog_answers_eparam1(lua_State* tolua_S)
{
  dialog_answers* self = (dialog_answers*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'eparam1'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->eparam1 = ((s32b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: eparam2 of class  dialog_answers */
static int tolua_get_dialog_answers_eparam2(lua_State* tolua_S)
{
  dialog_answers* self = (dialog_answers*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'eparam2'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->eparam2);
 return 1;
}

/* set function: eparam2 of class  dialog_answers */
static int tolua_set_dialog_answers_eparam2(lua_State* tolua_S)
{
  dialog_answers* self = (dialog_answers*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'eparam2'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->eparam2 = ((s32b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: eparam3 of class  dialog_answers */
static int tolua_get_dialog_answers_eparam3(lua_State* tolua_S)
{
  dialog_answers* self = (dialog_answers*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'eparam3'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->eparam3);
 return 1;
}

/* set function: eparam3 of class  dialog_answers */
static int tolua_set_dialog_answers_eparam3(lua_State* tolua_S)
{
  dialog_answers* self = (dialog_answers*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'eparam3'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->eparam3 = ((s32b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: eparam4 of class  dialog_answers */
static int tolua_get_dialog_answers_eparam4(lua_State* tolua_S)
{
  dialog_answers* self = (dialog_answers*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'eparam4'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->eparam4);
 return 1;
}

/* set function: eparam4 of class  dialog_answers */
static int tolua_set_dialog_answers_eparam4(lua_State* tolua_S)
{
  dialog_answers* self = (dialog_answers*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'eparam4'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->eparam4 = ((s32b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: eparam5 of class  dialog_answers */
static int tolua_get_dialog_answers_eparam5(lua_State* tolua_S)
{
  dialog_answers* self = (dialog_answers*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'eparam5'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->eparam5);
 return 1;
}

/* set function: eparam5 of class  dialog_answers */
static int tolua_set_dialog_answers_eparam5(lua_State* tolua_S)
{
  dialog_answers* self = (dialog_answers*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'eparam5'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->eparam5 = ((s32b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: valid of class  dialog_answers */
static int tolua_get_dialog_answers_valid(lua_State* tolua_S)
{
  dialog_answers* self = (dialog_answers*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'valid'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->valid);
 return 1;
}

/* set function: valid of class  dialog_answers */
static int tolua_set_dialog_answers_valid(lua_State* tolua_S)
{
  dialog_answers* self = (dialog_answers*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'valid'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->valid = ((s32b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: town of class  wild_info */
static int tolua_get_wild_info_town(lua_State* tolua_S)
{
  wild_info* self = (wild_info*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'town'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->town);
 return 1;
}

/* set function: town of class  wild_info */
static int tolua_set_wild_info_town(lua_State* tolua_S)
{
  wild_info* self = (wild_info*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'town'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->town = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: feat of class  wild_info */
static int tolua_get_wild_info_feat(lua_State* tolua_S)
{
  wild_info* self = (wild_info*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'feat'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->feat);
 return 1;
}

/* set function: feat of class  wild_info */
static int tolua_set_wild_info_feat(lua_State* tolua_S)
{
  wild_info* self = (wild_info*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'feat'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->feat = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: revive of class  wild_info */
static int tolua_get_wild_info_revive(lua_State* tolua_S)
{
  wild_info* self = (wild_info*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'revive'",NULL);
#endif
 tolua_pushboolean(tolua_S,(bool)self->revive);
 return 1;
}

/* set function: revive of class  wild_info */
static int tolua_set_wild_info_revive(lua_State* tolua_S)
{
  wild_info* self = (wild_info*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'revive'",NULL);
 if (!tolua_isboolean(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->revive = ((bool)  tolua_toboolean(tolua_S,2,0));
 return 0;
}

/* get function: name of class  class_def */
static int tolua_get_class_def_name(lua_State* tolua_S)
{
  class_def* self = (class_def*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'name'",NULL);
#endif
 tolua_pushstring(tolua_S,(const char*)self->name);
 return 1;
}

/* set function: name of class  class_def */
static int tolua_set_class_def_name(lua_State* tolua_S)
{
  class_def* self = (class_def*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'name'",NULL);
 if (!tolua_isstring(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
 strncpy(self->name,tolua_tostring(tolua_S,2,0),80-1);
 return 0;
}

/* get function: ranksm of class  class_def */
static int tolua_get_class_def_ranksm(lua_State* tolua_S)
{
  class_def* self = (class_def*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'ranksm'",NULL);
#endif
 tolua_pushstring(tolua_S,(const char*)self->ranksm);
 return 1;
}

/* set function: ranksm of class  class_def */
static int tolua_set_class_def_ranksm(lua_State* tolua_S)
{
  class_def* self = (class_def*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'ranksm'",NULL);
 if (!tolua_isstring(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
 strncpy(self->ranksm,tolua_tostring(tolua_S,2,0),10-1);
 return 0;
}

/* get function: ranksf of class  class_def */
static int tolua_get_class_def_ranksf(lua_State* tolua_S)
{
  class_def* self = (class_def*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'ranksf'",NULL);
#endif
 tolua_pushstring(tolua_S,(const char*)self->ranksf);
 return 1;
}

/* set function: ranksf of class  class_def */
static int tolua_set_class_def_ranksf(lua_State* tolua_S)
{
  class_def* self = (class_def*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'ranksf'",NULL);
 if (!tolua_isstring(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
 strncpy(self->ranksf,tolua_tostring(tolua_S,2,0),10-1);
 return 0;
}

/* get function: advanced of class  class_def */
static int tolua_get_class_def_advanced(lua_State* tolua_S)
{
  class_def* self = (class_def*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'advanced'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->advanced);
 return 1;
}

/* set function: advanced of class  class_def */
static int tolua_set_class_def_advanced(lua_State* tolua_S)
{
  class_def* self = (class_def*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'advanced'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->advanced = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: req_str of class  class_def */
static int tolua_get_class_def_req_str(lua_State* tolua_S)
{
  class_def* self = (class_def*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'req_str'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->req_str);
 return 1;
}

/* set function: req_str of class  class_def */
static int tolua_set_class_def_req_str(lua_State* tolua_S)
{
  class_def* self = (class_def*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'req_str'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->req_str = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: req_int of class  class_def */
static int tolua_get_class_def_req_int(lua_State* tolua_S)
{
  class_def* self = (class_def*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'req_int'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->req_int);
 return 1;
}

/* set function: req_int of class  class_def */
static int tolua_set_class_def_req_int(lua_State* tolua_S)
{
  class_def* self = (class_def*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'req_int'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->req_int = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: req_wis of class  class_def */
static int tolua_get_class_def_req_wis(lua_State* tolua_S)
{
  class_def* self = (class_def*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'req_wis'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->req_wis);
 return 1;
}

/* set function: req_wis of class  class_def */
static int tolua_set_class_def_req_wis(lua_State* tolua_S)
{
  class_def* self = (class_def*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'req_wis'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->req_wis = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: req_dex of class  class_def */
static int tolua_get_class_def_req_dex(lua_State* tolua_S)
{
  class_def* self = (class_def*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'req_dex'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->req_dex);
 return 1;
}

/* set function: req_dex of class  class_def */
static int tolua_set_class_def_req_dex(lua_State* tolua_S)
{
  class_def* self = (class_def*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'req_dex'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->req_dex = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: req_con of class  class_def */
static int tolua_get_class_def_req_con(lua_State* tolua_S)
{
  class_def* self = (class_def*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'req_con'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->req_con);
 return 1;
}

/* set function: req_con of class  class_def */
static int tolua_set_class_def_req_con(lua_State* tolua_S)
{
  class_def* self = (class_def*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'req_con'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->req_con = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: req_chr of class  class_def */
static int tolua_get_class_def_req_chr(lua_State* tolua_S)
{
  class_def* self = (class_def*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'req_chr'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->req_chr);
 return 1;
}

/* set function: req_chr of class  class_def */
static int tolua_set_class_def_req_chr(lua_State* tolua_S)
{
  class_def* self = (class_def*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'req_chr'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->req_chr = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: str_bonus of class  class_def */
static int tolua_get_class_def_str_bonus(lua_State* tolua_S)
{
  class_def* self = (class_def*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'str_bonus'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->str_bonus);
 return 1;
}

/* set function: str_bonus of class  class_def */
static int tolua_set_class_def_str_bonus(lua_State* tolua_S)
{
  class_def* self = (class_def*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'str_bonus'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->str_bonus = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: int_bonus of class  class_def */
static int tolua_get_class_def_int_bonus(lua_State* tolua_S)
{
  class_def* self = (class_def*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'int_bonus'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->int_bonus);
 return 1;
}

/* set function: int_bonus of class  class_def */
static int tolua_set_class_def_int_bonus(lua_State* tolua_S)
{
  class_def* self = (class_def*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'int_bonus'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->int_bonus = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: wis_bonus of class  class_def */
static int tolua_get_class_def_wis_bonus(lua_State* tolua_S)
{
  class_def* self = (class_def*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'wis_bonus'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->wis_bonus);
 return 1;
}

/* set function: wis_bonus of class  class_def */
static int tolua_set_class_def_wis_bonus(lua_State* tolua_S)
{
  class_def* self = (class_def*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'wis_bonus'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->wis_bonus = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: dex_bonus of class  class_def */
static int tolua_get_class_def_dex_bonus(lua_State* tolua_S)
{
  class_def* self = (class_def*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'dex_bonus'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->dex_bonus);
 return 1;
}

/* set function: dex_bonus of class  class_def */
static int tolua_set_class_def_dex_bonus(lua_State* tolua_S)
{
  class_def* self = (class_def*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'dex_bonus'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->dex_bonus = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: con_bonus of class  class_def */
static int tolua_get_class_def_con_bonus(lua_State* tolua_S)
{
  class_def* self = (class_def*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'con_bonus'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->con_bonus);
 return 1;
}

/* set function: con_bonus of class  class_def */
static int tolua_set_class_def_con_bonus(lua_State* tolua_S)
{
  class_def* self = (class_def*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'con_bonus'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->con_bonus = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: chr_bonus of class  class_def */
static int tolua_get_class_def_chr_bonus(lua_State* tolua_S)
{
  class_def* self = (class_def*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'chr_bonus'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->chr_bonus);
 return 1;
}

/* set function: chr_bonus of class  class_def */
static int tolua_set_class_def_chr_bonus(lua_State* tolua_S)
{
  class_def* self = (class_def*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'chr_bonus'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->chr_bonus = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: req_skills of class  class_def */
static int tolua_get_types_class_def_req_skills(lua_State* tolua_S)
{
 int tolua_index;
  class_def* self;
 lua_pushstring(tolua_S,".self");
 lua_rawget(tolua_S,1);
 self = (class_def*)  lua_touserdata(tolua_S,-1);
#ifndef TOLUA_RELEASE
 {
 tolua_Error tolua_err;
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in array indexing.",&tolua_err);
 }
#endif
 tolua_index = (int)tolua_tonumber(tolua_S,2,0)-1;
#ifndef TOLUA_RELEASE
 if (tolua_index<0 || tolua_index>=SKILL_MAX)
 tolua_error(tolua_S,"array indexing out of range.",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->req_skills[tolua_index]);
 return 1;
}

/* set function: req_skills of class  class_def */
static int tolua_set_types_class_def_req_skills(lua_State* tolua_S)
{
 int tolua_index;
  class_def* self;
 lua_pushstring(tolua_S,".self");
 lua_rawget(tolua_S,1);
 self = (class_def*)  lua_touserdata(tolua_S,-1);
#ifndef TOLUA_RELEASE
 {
 tolua_Error tolua_err;
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in array indexing.",&tolua_err);
 }
#endif
 tolua_index = (int)tolua_tonumber(tolua_S,2,0)-1;
#ifndef TOLUA_RELEASE
 if (tolua_index<0 || tolua_index>=SKILL_MAX)
 tolua_error(tolua_S,"array indexing out of range.",NULL);
#endif
  self->req_skills[tolua_index] = ((s16b)  tolua_tonumber(tolua_S,3,0));
 return 0;
}

/* get function: req_classes of class  class_def */
static int tolua_get_types_class_def_req_classes(lua_State* tolua_S)
{
 int tolua_index;
  class_def* self;
 lua_pushstring(tolua_S,".self");
 lua_rawget(tolua_S,1);
 self = (class_def*)  lua_touserdata(tolua_S,-1);
#ifndef TOLUA_RELEASE
 {
 tolua_Error tolua_err;
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in array indexing.",&tolua_err);
 }
#endif
 tolua_index = (int)tolua_tonumber(tolua_S,2,0)-1;
#ifndef TOLUA_RELEASE
 if (tolua_index<0 || tolua_index>=MAX_CLASS)
 tolua_error(tolua_S,"array indexing out of range.",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->req_classes[tolua_index]);
 return 1;
}

/* set function: req_classes of class  class_def */
static int tolua_set_types_class_def_req_classes(lua_State* tolua_S)
{
 int tolua_index;
  class_def* self;
 lua_pushstring(tolua_S,".self");
 lua_rawget(tolua_S,1);
 self = (class_def*)  lua_touserdata(tolua_S,-1);
#ifndef TOLUA_RELEASE
 {
 tolua_Error tolua_err;
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in array indexing.",&tolua_err);
 }
#endif
 tolua_index = (int)tolua_tonumber(tolua_S,2,0)-1;
#ifndef TOLUA_RELEASE
 if (tolua_index<0 || tolua_index>=MAX_CLASS)
 tolua_error(tolua_S,"array indexing out of range.",NULL);
#endif
  self->req_classes[tolua_index] = ((s16b)  tolua_tonumber(tolua_S,3,0));
 return 0;
}

/* get function: skills_bonus of class  class_def */
static int tolua_get_types_class_def_skills_bonus(lua_State* tolua_S)
{
 int tolua_index;
  class_def* self;
 lua_pushstring(tolua_S,".self");
 lua_rawget(tolua_S,1);
 self = (class_def*)  lua_touserdata(tolua_S,-1);
#ifndef TOLUA_RELEASE
 {
 tolua_Error tolua_err;
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in array indexing.",&tolua_err);
 }
#endif
 tolua_index = (int)tolua_tonumber(tolua_S,2,0)-1;
#ifndef TOLUA_RELEASE
 if (tolua_index<0 || tolua_index>=SKILL_MAX)
 tolua_error(tolua_S,"array indexing out of range.",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->skills_bonus[tolua_index]);
 return 1;
}

/* set function: skills_bonus of class  class_def */
static int tolua_set_types_class_def_skills_bonus(lua_State* tolua_S)
{
 int tolua_index;
  class_def* self;
 lua_pushstring(tolua_S,".self");
 lua_rawget(tolua_S,1);
 self = (class_def*)  lua_touserdata(tolua_S,-1);
#ifndef TOLUA_RELEASE
 {
 tolua_Error tolua_err;
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in array indexing.",&tolua_err);
 }
#endif
 tolua_index = (int)tolua_tonumber(tolua_S,2,0)-1;
#ifndef TOLUA_RELEASE
 if (tolua_index<0 || tolua_index>=SKILL_MAX)
 tolua_error(tolua_S,"array indexing out of range.",NULL);
#endif
  self->skills_bonus[tolua_index] = ((s16b)  tolua_tonumber(tolua_S,3,0));
 return 0;
}

/* get function: name of class  ability_def */
static int tolua_get_ability_def_name(lua_State* tolua_S)
{
  ability_def* self = (ability_def*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'name'",NULL);
#endif
 tolua_pushstring(tolua_S,(const char*)self->name);
 return 1;
}

/* set function: name of class  ability_def */
static int tolua_set_ability_def_name(lua_State* tolua_S)
{
  ability_def* self = (ability_def*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'name'",NULL);
 if (!tolua_isstring(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
 strncpy(self->name,tolua_tostring(tolua_S,2,0),80-1);
 return 0;
}

/* get function: abtype of class  ability_def */
static int tolua_get_ability_def_abtype(lua_State* tolua_S)
{
  ability_def* self = (ability_def*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'abtype'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->abtype);
 return 1;
}

/* set function: abtype of class  ability_def */
static int tolua_set_ability_def_abtype(lua_State* tolua_S)
{
  ability_def* self = (ability_def*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'abtype'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->abtype = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: hardcode of class  ability_def */
static int tolua_get_ability_def_hardcode(lua_State* tolua_S)
{
  ability_def* self = (ability_def*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'hardcode'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->hardcode);
 return 1;
}

/* set function: hardcode of class  ability_def */
static int tolua_set_ability_def_hardcode(lua_State* tolua_S)
{
  ability_def* self = (ability_def*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'hardcode'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->hardcode = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: powerid of class  ability_def */
static int tolua_get_ability_def_powerid(lua_State* tolua_S)
{
  ability_def* self = (ability_def*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'powerid'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->powerid);
 return 1;
}

/* set function: powerid of class  ability_def */
static int tolua_set_ability_def_powerid(lua_State* tolua_S)
{
  ability_def* self = (ability_def*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'powerid'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->powerid = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: combatfeat of class  ability_def */
static int tolua_get_ability_def_combatfeat(lua_State* tolua_S)
{
  ability_def* self = (ability_def*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'combatfeat'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->combatfeat);
 return 1;
}

/* set function: combatfeat of class  ability_def */
static int tolua_set_ability_def_combatfeat(lua_State* tolua_S)
{
  ability_def* self = (ability_def*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'combatfeat'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->combatfeat = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: skill of class  ability_def */
static int tolua_get_ability_def_skill(lua_State* tolua_S)
{
  ability_def* self = (ability_def*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'skill'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->skill);
 return 1;
}

/* set function: skill of class  ability_def */
static int tolua_set_ability_def_skill(lua_State* tolua_S)
{
  ability_def* self = (ability_def*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'skill'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->skill = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: reqskill of class  ability_def */
static int tolua_get_ability_def_reqskill(lua_State* tolua_S)
{
  ability_def* self = (ability_def*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'reqskill'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->reqskill);
 return 1;
}

/* set function: reqskill of class  ability_def */
static int tolua_set_ability_def_reqskill(lua_State* tolua_S)
{
  ability_def* self = (ability_def*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'reqskill'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->reqskill = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: name of class  resist_def */
static int tolua_get_resist_def_name(lua_State* tolua_S)
{
  resist_def* self = (resist_def*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'name'",NULL);
#endif
 tolua_pushstring(tolua_S,(const char*)self->name);
 return 1;
}

/* set function: name of class  resist_def */
static int tolua_set_resist_def_name(lua_State* tolua_S)
{
  resist_def* self = (resist_def*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'name'",NULL);
 if (!tolua_isstring(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
 strncpy(self->name,tolua_tostring(tolua_S,2,0),80-1);
 return 0;
}

/* get function: element of class  resist_def */
static int tolua_get_resist_def_element(lua_State* tolua_S)
{
  resist_def* self = (resist_def*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'element'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->element);
 return 1;
}

/* set function: element of class  resist_def */
static int tolua_set_resist_def_element(lua_State* tolua_S)
{
  resist_def* self = (resist_def*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'element'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->element = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: magicitem of class  resist_def */
static int tolua_get_resist_def_magicitem(lua_State* tolua_S)
{
  resist_def* self = (resist_def*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'magicitem'",NULL);
#endif
 tolua_pushboolean(tolua_S,(bool)self->magicitem);
 return 1;
}

/* set function: magicitem of class  resist_def */
static int tolua_set_resist_def_magicitem(lua_State* tolua_S)
{
  resist_def* self = (resist_def*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'magicitem'",NULL);
 if (!tolua_isboolean(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->magicitem = ((bool)  tolua_toboolean(tolua_S,2,0));
 return 0;
}

/* get function: created of class  vault_def */
static int tolua_get_vault_def_created(lua_State* tolua_S)
{
  vault_def* self = (vault_def*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'created'",NULL);
#endif
 tolua_pushboolean(tolua_S,(bool)self->created);
 return 1;
}

/* set function: created of class  vault_def */
static int tolua_set_vault_def_created(lua_State* tolua_S)
{
  vault_def* self = (vault_def*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'created'",NULL);
 if (!tolua_isboolean(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->created = ((bool)  tolua_toboolean(tolua_S,2,0));
 return 0;
}

/* get function: num of class  vault_def */
static int tolua_get_vault_def_num(lua_State* tolua_S)
{
  vault_def* self = (vault_def*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'num'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->num);
 return 1;
}

/* set function: num of class  vault_def */
static int tolua_set_vault_def_num(lua_State* tolua_S)
{
  vault_def* self = (vault_def*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'num'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->num = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: width of class  vault_def */
static int tolua_get_vault_def_width(lua_State* tolua_S)
{
  vault_def* self = (vault_def*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'width'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->width);
 return 1;
}

/* set function: width of class  vault_def */
static int tolua_set_vault_def_width(lua_State* tolua_S)
{
  vault_def* self = (vault_def*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'width'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->width = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: height of class  vault_def */
static int tolua_get_vault_def_height(lua_State* tolua_S)
{
  vault_def* self = (vault_def*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'height'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->height);
 return 1;
}

/* set function: height of class  vault_def */
static int tolua_set_vault_def_height(lua_State* tolua_S)
{
  vault_def* self = (vault_def*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'height'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->height = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: mindlv of class  vault_def */
static int tolua_get_vault_def_mindlv(lua_State* tolua_S)
{
  vault_def* self = (vault_def*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'mindlv'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->mindlv);
 return 1;
}

/* set function: mindlv of class  vault_def */
static int tolua_set_vault_def_mindlv(lua_State* tolua_S)
{
  vault_def* self = (vault_def*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'mindlv'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->mindlv = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: maxdlv of class  vault_def */
static int tolua_get_vault_def_maxdlv(lua_State* tolua_S)
{
  vault_def* self = (vault_def*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'maxdlv'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->maxdlv);
 return 1;
}

/* set function: maxdlv of class  vault_def */
static int tolua_set_vault_def_maxdlv(lua_State* tolua_S)
{
  vault_def* self = (vault_def*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'maxdlv'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->maxdlv = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: teleport of class  vault_def */
static int tolua_get_vault_def_teleport(lua_State* tolua_S)
{
  vault_def* self = (vault_def*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'teleport'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->teleport);
 return 1;
}

/* set function: teleport of class  vault_def */
static int tolua_set_vault_def_teleport(lua_State* tolua_S)
{
  vault_def* self = (vault_def*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'teleport'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->teleport = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: type of class  vault_def */
static int tolua_get_vault_def_type(lua_State* tolua_S)
{
  vault_def* self = (vault_def*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'type'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->type);
 return 1;
}

/* set function: type of class  vault_def */
static int tolua_set_vault_def_type(lua_State* tolua_S)
{
  vault_def* self = (vault_def*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'type'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->type = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: rarity of class  vault_def */
static int tolua_get_vault_def_rarity(lua_State* tolua_S)
{
  vault_def* self = (vault_def*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'rarity'",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)self->rarity);
 return 1;
}

/* set function: rarity of class  vault_def */
static int tolua_set_vault_def_rarity(lua_State* tolua_S)
{
  vault_def* self = (vault_def*)  tolua_tousertype(tolua_S,1,0);
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!self) tolua_error(tolua_S,"invalid 'self' in accessing variable 'rarity'",NULL);
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  self->rarity = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* Open function */
TOLUA_API int tolua_types_open (lua_State* tolua_S)
{
 tolua_open(tolua_S);
 tolua_reg_types(tolua_S);
 tolua_module(tolua_S,NULL,0);
 tolua_beginmodule(tolua_S,NULL);
 tolua_cclass(tolua_S,"header","header","",NULL);
 tolua_beginmodule(tolua_S,"header");
 tolua_variable(tolua_S,"v_major",tolua_get_header_v_major,tolua_set_header_v_major);
 tolua_variable(tolua_S,"v_minor",tolua_get_header_v_minor,tolua_set_header_v_minor);
 tolua_variable(tolua_S,"v_patch",tolua_get_header_v_patch,tolua_set_header_v_patch);
 tolua_variable(tolua_S,"v_extra",tolua_get_header_v_extra,tolua_set_header_v_extra);
 tolua_variable(tolua_S,"info_num",tolua_get_header_info_num,tolua_set_header_info_num);
 tolua_variable(tolua_S,"info_len",tolua_get_header_info_len,tolua_set_header_info_len);
 tolua_variable(tolua_S,"head_size",tolua_get_header_head_size,tolua_set_header_head_size);
 tolua_variable(tolua_S,"info_size",tolua_get_header_info_size,tolua_set_header_info_size);
 tolua_variable(tolua_S,"name_size",tolua_get_header_name_size,tolua_set_header_name_size);
 tolua_variable(tolua_S,"text_size",tolua_get_header_text_size,tolua_set_header_text_size);
 tolua_endmodule(tolua_S);
 tolua_cclass(tolua_S,"random_room","random_room","",NULL);
 tolua_beginmodule(tolua_S,"random_room");
 tolua_variable(tolua_S,"top",tolua_get_random_room_top,tolua_set_random_room_top);
 tolua_variable(tolua_S,"left",tolua_get_random_room_left,tolua_set_random_room_left);
 tolua_variable(tolua_S,"right",tolua_get_random_room_right,tolua_set_random_room_right);
 tolua_variable(tolua_S,"bottom",tolua_get_random_room_bottom,tolua_set_random_room_bottom);
 tolua_variable(tolua_S,"centerx",tolua_get_random_room_centerx,tolua_set_random_room_centerx);
 tolua_variable(tolua_S,"centery",tolua_get_random_room_centery,tolua_set_random_room_centery);
 tolua_array(tolua_S,"connectedto",tolua_get_types_random_room_connectedto,tolua_set_types_random_room_connectedto);
 tolua_array(tolua_S,"connectx",tolua_get_types_random_room_connectx,tolua_set_types_random_room_connectx);
 tolua_array(tolua_S,"connecty",tolua_get_types_random_room_connecty,tolua_set_types_random_room_connecty);
 tolua_array(tolua_S,"pickeddir",tolua_get_types_random_room_pickeddir,tolua_set_types_random_room_pickeddir);
 tolua_variable(tolua_S,"created",tolua_get_random_room_created,tolua_set_random_room_created);
 tolua_endmodule(tolua_S);
 tolua_cclass(tolua_S,"feature_type","feature_type","",NULL);
 tolua_beginmodule(tolua_S,"feature_type");
 tolua_variable(tolua_S,"name",tolua_get_feature_type_name,tolua_set_feature_type_name);
 tolua_variable(tolua_S,"text",tolua_get_feature_type_text,tolua_set_feature_type_text);
 tolua_variable(tolua_S,"mimic",tolua_get_feature_type_mimic,tolua_set_feature_type_mimic);
 tolua_variable(tolua_S,"flags1",tolua_get_feature_type_flags1,tolua_set_feature_type_flags1);
 tolua_variable(tolua_S,"extra",tolua_get_feature_type_extra,tolua_set_feature_type_extra);
 tolua_variable(tolua_S,"unused",tolua_get_feature_type_unused,tolua_set_feature_type_unused);
 tolua_variable(tolua_S,"d_attr",tolua_get_feature_type_d_attr,tolua_set_feature_type_d_attr);
 tolua_variable(tolua_S,"d_char",tolua_get_feature_type_d_char,tolua_set_feature_type_d_char);
 tolua_variable(tolua_S,"x_attr",tolua_get_feature_type_x_attr,tolua_set_feature_type_x_attr);
 tolua_variable(tolua_S,"x_char",tolua_get_feature_type_x_char,tolua_set_feature_type_x_char);
 tolua_endmodule(tolua_S);
#ifdef __cplusplus
 tolua_cclass(tolua_S,"monster_attack","monster_attack","",tolua_collect_monster_attack);
#else
 tolua_cclass(tolua_S,"monster_attack","monster_attack","",NULL);
#endif
 tolua_beginmodule(tolua_S,"monster_attack");
 tolua_variable(tolua_S,"name",tolua_get_monster_attack_name,tolua_set_monster_attack_name);
 tolua_variable(tolua_S,"act",tolua_get_monster_attack_act,tolua_set_monster_attack_act);
 tolua_variable(tolua_S,"type",tolua_get_monster_attack_type,tolua_set_monster_attack_type);
 tolua_variable(tolua_S,"effect",tolua_get_monster_attack_effect,tolua_set_monster_attack_effect);
 tolua_variable(tolua_S,"ddice",tolua_get_monster_attack_ddice,tolua_set_monster_attack_ddice);
 tolua_variable(tolua_S,"dside",tolua_get_monster_attack_dside,tolua_set_monster_attack_dside);
 tolua_variable(tolua_S,"element",tolua_get_monster_attack_element,tolua_set_monster_attack_element);
 tolua_variable(tolua_S,"special1",tolua_get_monster_attack_special1,tolua_set_monster_attack_special1);
 tolua_variable(tolua_S,"special2",tolua_get_monster_attack_special2,tolua_set_monster_attack_special2);
 tolua_endmodule(tolua_S);
#ifdef __cplusplus
 tolua_cclass(tolua_S,"monster_spell","monster_spell","",tolua_collect_monster_spell);
#else
 tolua_cclass(tolua_S,"monster_spell","monster_spell","",NULL);
#endif
 tolua_beginmodule(tolua_S,"monster_spell");
 tolua_variable(tolua_S,"name",tolua_get_monster_spell_name,tolua_set_monster_spell_name);
 tolua_variable(tolua_S,"act",tolua_get_monster_spell_act,tolua_set_monster_spell_act);
 tolua_variable(tolua_S,"type",tolua_get_monster_spell_type,tolua_set_monster_spell_type);
 tolua_variable(tolua_S,"power",tolua_get_monster_spell_power,tolua_set_monster_spell_power);
 tolua_variable(tolua_S,"special1",tolua_get_monster_spell_special1,tolua_set_monster_spell_special1);
 tolua_variable(tolua_S,"special2",tolua_get_monster_spell_special2,tolua_set_monster_spell_special2);
 tolua_variable(tolua_S,"special3",tolua_get_monster_spell_special3,tolua_set_monster_spell_special3);
 tolua_variable(tolua_S,"summchar",tolua_get_monster_spell_summchar,tolua_set_monster_spell_summchar);
 tolua_variable(tolua_S,"cost",tolua_get_monster_spell_cost,tolua_set_monster_spell_cost);
 tolua_endmodule(tolua_S);
 tolua_cclass(tolua_S,"magic_spells","magic_spells","",NULL);
 tolua_beginmodule(tolua_S,"magic_spells");
 tolua_variable(tolua_S,"name",tolua_get_magic_spells_name,tolua_set_magic_spells_name);
 tolua_array(tolua_S,"school",tolua_get_types_magic_spells_school,tolua_set_types_magic_spells_school);
 tolua_array(tolua_S,"effect",tolua_get_types_magic_spells_effect,tolua_set_types_magic_spells_effect);
 tolua_array(tolua_S,"shape",tolua_get_types_magic_spells_shape,tolua_set_types_magic_spells_shape);
 tolua_array(tolua_S,"power",tolua_get_types_magic_spells_power,tolua_set_types_magic_spells_power);
 tolua_array(tolua_S,"radius",tolua_get_types_magic_spells_radius,tolua_set_types_magic_spells_radius);
 tolua_array(tolua_S,"type",tolua_get_types_magic_spells_type,tolua_set_types_magic_spells_type);
 tolua_array(tolua_S,"manacost",tolua_get_types_magic_spells_manacost,tolua_set_types_magic_spells_manacost);
 tolua_variable(tolua_S,"schar1",tolua_get_magic_spells_schar1,tolua_set_magic_spells_schar1);
 tolua_variable(tolua_S,"schar2",tolua_get_magic_spells_schar2,tolua_set_magic_spells_schar2);
 tolua_variable(tolua_S,"schar3",tolua_get_magic_spells_schar3,tolua_set_magic_spells_schar3);
 tolua_variable(tolua_S,"schar4",tolua_get_magic_spells_schar4,tolua_set_magic_spells_schar4);
 tolua_variable(tolua_S,"schar5",tolua_get_magic_spells_schar5,tolua_set_magic_spells_schar5);
 tolua_variable(tolua_S,"sspeci1",tolua_get_magic_spells_sspeci1,tolua_set_magic_spells_sspeci1);
 tolua_variable(tolua_S,"sspeci2",tolua_get_magic_spells_sspeci2,tolua_set_magic_spells_sspeci2);
 tolua_variable(tolua_S,"sspeci3",tolua_get_magic_spells_sspeci3,tolua_set_magic_spells_sspeci3);
 tolua_variable(tolua_S,"sspeci4",tolua_get_magic_spells_sspeci4,tolua_set_magic_spells_sspeci4);
 tolua_variable(tolua_S,"sspeci5",tolua_get_magic_spells_sspeci5,tolua_set_magic_spells_sspeci5);
 tolua_variable(tolua_S,"finalcost",tolua_get_magic_spells_finalcost,tolua_set_magic_spells_finalcost);
 tolua_variable(tolua_S,"created",tolua_get_magic_spells_created,tolua_set_magic_spells_created);
 tolua_endmodule(tolua_S);
 tolua_cclass(tolua_S,"music_songs","music_songs","",NULL);
 tolua_beginmodule(tolua_S,"music_songs");
 tolua_variable(tolua_S,"name",tolua_get_music_songs_name,tolua_set_music_songs_name);
 tolua_variable(tolua_S,"type",tolua_get_music_songs_type,tolua_set_music_songs_type);
 tolua_variable(tolua_S,"power",tolua_get_music_songs_power,tolua_set_music_songs_power);
 tolua_variable(tolua_S,"element",tolua_get_music_songs_element,tolua_set_music_songs_element);
 tolua_variable(tolua_S,"radius",tolua_get_music_songs_radius,tolua_set_music_songs_radius);
 tolua_variable(tolua_S,"cost",tolua_get_music_songs_cost,tolua_set_music_songs_cost);
 tolua_variable(tolua_S,"created",tolua_get_music_songs_created,tolua_set_music_songs_created);
 tolua_endmodule(tolua_S);
 tolua_cclass(tolua_S,"object_kind","object_kind","",NULL);
 tolua_beginmodule(tolua_S,"object_kind");
 tolua_variable(tolua_S,"name",tolua_get_object_kind_name,tolua_set_object_kind_name);
 tolua_variable(tolua_S,"text",tolua_get_object_kind_text,tolua_set_object_kind_text);
 tolua_variable(tolua_S,"tval",tolua_get_object_kind_tval,tolua_set_object_kind_tval);
 tolua_variable(tolua_S,"sval",tolua_get_object_kind_sval,tolua_set_object_kind_sval);
 tolua_variable(tolua_S,"pval",tolua_get_object_kind_pval,tolua_set_object_kind_pval);
 tolua_variable(tolua_S,"to_h",tolua_get_object_kind_to_h,tolua_set_object_kind_to_h);
 tolua_variable(tolua_S,"to_d",tolua_get_object_kind_to_d,tolua_set_object_kind_to_d);
 tolua_variable(tolua_S,"to_a",tolua_get_object_kind_to_a,tolua_set_object_kind_to_a);
 tolua_variable(tolua_S,"ac",tolua_get_object_kind_ac,tolua_set_object_kind_ac);
 tolua_variable(tolua_S,"dd",tolua_get_object_kind_dd,tolua_set_object_kind_dd);
 tolua_variable(tolua_S,"ds",tolua_get_object_kind_ds,tolua_set_object_kind_ds);
 tolua_variable(tolua_S,"weight",tolua_get_object_kind_weight,tolua_set_object_kind_weight);
 tolua_variable(tolua_S,"cost",tolua_get_object_kind_cost,tolua_set_object_kind_cost);
 tolua_variable(tolua_S,"flags1",tolua_get_object_kind_flags1,tolua_set_object_kind_flags1);
 tolua_variable(tolua_S,"flags2",tolua_get_object_kind_flags2,tolua_set_object_kind_flags2);
 tolua_variable(tolua_S,"flags3",tolua_get_object_kind_flags3,tolua_set_object_kind_flags3);
 tolua_variable(tolua_S,"flags4",tolua_get_object_kind_flags4,tolua_set_object_kind_flags4);
 tolua_array(tolua_S,"locale",tolua_get_types_object_kind_locale,tolua_set_types_object_kind_locale);
 tolua_array(tolua_S,"chance",tolua_get_types_object_kind_chance,tolua_set_types_object_kind_chance);
 tolua_variable(tolua_S,"level",tolua_get_object_kind_level,tolua_set_object_kind_level);
 tolua_variable(tolua_S,"extra",tolua_get_object_kind_extra,tolua_set_object_kind_extra);
 tolua_variable(tolua_S,"d_attr",tolua_get_object_kind_d_attr,tolua_set_object_kind_d_attr);
 tolua_variable(tolua_S,"d_char",tolua_get_object_kind_d_char,tolua_set_object_kind_d_char);
 tolua_variable(tolua_S,"x_attr",tolua_get_object_kind_x_attr,tolua_set_object_kind_x_attr);
 tolua_variable(tolua_S,"x_char",tolua_get_object_kind_x_char,tolua_set_object_kind_x_char);
 tolua_variable(tolua_S,"flavor",tolua_get_object_kind_flavor,tolua_set_object_kind_flavor);
 tolua_variable(tolua_S,"easy_know",tolua_get_object_kind_easy_know,tolua_set_object_kind_easy_know);
 tolua_variable(tolua_S,"aware",tolua_get_object_kind_aware,tolua_set_object_kind_aware);
 tolua_variable(tolua_S,"tried",tolua_get_object_kind_tried,tolua_set_object_kind_tried);
 tolua_variable(tolua_S,"know",tolua_get_object_kind_know,tolua_set_object_kind_know);
 tolua_variable(tolua_S,"recipe1",tolua_get_object_kind_recipe1,tolua_set_object_kind_recipe1);
 tolua_variable(tolua_S,"recipe2",tolua_get_object_kind_recipe2,tolua_set_object_kind_recipe2);
 tolua_variable(tolua_S,"brandtype",tolua_get_object_kind_brandtype,tolua_set_object_kind_brandtype);
 tolua_variable(tolua_S,"branddam",tolua_get_object_kind_branddam,tolua_set_object_kind_branddam);
 tolua_variable(tolua_S,"brandrad",tolua_get_object_kind_brandrad,tolua_set_object_kind_brandrad);
 tolua_array(tolua_S,"resistances",tolua_get_types_object_kind_resistances,tolua_set_types_object_kind_resistances);
 tolua_variable(tolua_S,"itemtype",tolua_get_object_kind_itemtype,tolua_set_object_kind_itemtype);
 tolua_variable(tolua_S,"itemskill",tolua_get_object_kind_itemskill,tolua_set_object_kind_itemskill);
 tolua_array(tolua_S,"statsbonus",tolua_get_types_object_kind_statsbonus,tolua_set_types_object_kind_statsbonus);
 tolua_array(tolua_S,"skillsbonus",tolua_get_types_object_kind_skillsbonus,tolua_set_types_object_kind_skillsbonus);
 tolua_variable(tolua_S,"extrablows",tolua_get_object_kind_extrablows,tolua_set_object_kind_extrablows);
 tolua_variable(tolua_S,"extrashots",tolua_get_object_kind_extrashots,tolua_set_object_kind_extrashots);
 tolua_variable(tolua_S,"speedbonus",tolua_get_object_kind_speedbonus,tolua_set_object_kind_speedbonus);
 tolua_variable(tolua_S,"lifebonus",tolua_get_object_kind_lifebonus,tolua_set_object_kind_lifebonus);
 tolua_variable(tolua_S,"manabonus",tolua_get_object_kind_manabonus,tolua_set_object_kind_manabonus);
 tolua_variable(tolua_S,"infravision",tolua_get_object_kind_infravision,tolua_set_object_kind_infravision);
 tolua_variable(tolua_S,"spellbonus",tolua_get_object_kind_spellbonus,tolua_set_object_kind_spellbonus);
 tolua_variable(tolua_S,"invisibility",tolua_get_object_kind_invisibility,tolua_set_object_kind_invisibility);
 tolua_variable(tolua_S,"light",tolua_get_object_kind_light,tolua_set_object_kind_light);
 tolua_variable(tolua_S,"extra1",tolua_get_object_kind_extra1,tolua_set_object_kind_extra1);
 tolua_variable(tolua_S,"extra2",tolua_get_object_kind_extra2,tolua_set_object_kind_extra2);
 tolua_variable(tolua_S,"extra3",tolua_get_object_kind_extra3,tolua_set_object_kind_extra3);
 tolua_variable(tolua_S,"extra4",tolua_get_object_kind_extra4,tolua_set_object_kind_extra4);
 tolua_variable(tolua_S,"extra5",tolua_get_object_kind_extra5,tolua_set_object_kind_extra5);
 tolua_variable(tolua_S,"reflect",tolua_get_object_kind_reflect,tolua_set_object_kind_reflect);
 tolua_array(tolua_S,"spell",tolua_get_types_object_kind_spell,tolua_set_types_object_kind_spell);
 tolua_endmodule(tolua_S);
 tolua_cclass(tolua_S,"artifact_type","artifact_type","",NULL);
 tolua_beginmodule(tolua_S,"artifact_type");
 tolua_variable(tolua_S,"name",tolua_get_artifact_type_name,tolua_set_artifact_type_name);
 tolua_variable(tolua_S,"text",tolua_get_artifact_type_text,tolua_set_artifact_type_text);
 tolua_variable(tolua_S,"tval",tolua_get_artifact_type_tval,tolua_set_artifact_type_tval);
 tolua_variable(tolua_S,"sval",tolua_get_artifact_type_sval,tolua_set_artifact_type_sval);
 tolua_variable(tolua_S,"pval",tolua_get_artifact_type_pval,tolua_set_artifact_type_pval);
 tolua_variable(tolua_S,"to_h",tolua_get_artifact_type_to_h,tolua_set_artifact_type_to_h);
 tolua_variable(tolua_S,"to_d",tolua_get_artifact_type_to_d,tolua_set_artifact_type_to_d);
 tolua_variable(tolua_S,"to_a",tolua_get_artifact_type_to_a,tolua_set_artifact_type_to_a);
 tolua_variable(tolua_S,"ac",tolua_get_artifact_type_ac,tolua_set_artifact_type_ac);
 tolua_variable(tolua_S,"dd",tolua_get_artifact_type_dd,tolua_set_artifact_type_dd);
 tolua_variable(tolua_S,"ds",tolua_get_artifact_type_ds,tolua_set_artifact_type_ds);
 tolua_variable(tolua_S,"weight",tolua_get_artifact_type_weight,tolua_set_artifact_type_weight);
 tolua_variable(tolua_S,"cost",tolua_get_artifact_type_cost,tolua_set_artifact_type_cost);
 tolua_variable(tolua_S,"flags1",tolua_get_artifact_type_flags1,tolua_set_artifact_type_flags1);
 tolua_variable(tolua_S,"flags2",tolua_get_artifact_type_flags2,tolua_set_artifact_type_flags2);
 tolua_variable(tolua_S,"flags3",tolua_get_artifact_type_flags3,tolua_set_artifact_type_flags3);
 tolua_variable(tolua_S,"flags4",tolua_get_artifact_type_flags4,tolua_set_artifact_type_flags4);
 tolua_variable(tolua_S,"level",tolua_get_artifact_type_level,tolua_set_artifact_type_level);
 tolua_variable(tolua_S,"rarity",tolua_get_artifact_type_rarity,tolua_set_artifact_type_rarity);
 tolua_variable(tolua_S,"cur_num",tolua_get_artifact_type_cur_num,tolua_set_artifact_type_cur_num);
 tolua_variable(tolua_S,"max_num",tolua_get_artifact_type_max_num,tolua_set_artifact_type_max_num);
 tolua_variable(tolua_S,"brandtype",tolua_get_artifact_type_brandtype,tolua_set_artifact_type_brandtype);
 tolua_variable(tolua_S,"branddam",tolua_get_artifact_type_branddam,tolua_set_artifact_type_branddam);
 tolua_variable(tolua_S,"brandrad",tolua_get_artifact_type_brandrad,tolua_set_artifact_type_brandrad);
 tolua_array(tolua_S,"resistances",tolua_get_types_artifact_type_resistances,tolua_set_types_artifact_type_resistances);
 tolua_variable(tolua_S,"itemtype",tolua_get_artifact_type_itemtype,tolua_set_artifact_type_itemtype);
 tolua_variable(tolua_S,"itemskill",tolua_get_artifact_type_itemskill,tolua_set_artifact_type_itemskill);
 tolua_array(tolua_S,"statsbonus",tolua_get_types_artifact_type_statsbonus,tolua_set_types_artifact_type_statsbonus);
 tolua_array(tolua_S,"skillsbonus",tolua_get_types_artifact_type_skillsbonus,tolua_set_types_artifact_type_skillsbonus);
 tolua_variable(tolua_S,"extrablows",tolua_get_artifact_type_extrablows,tolua_set_artifact_type_extrablows);
 tolua_variable(tolua_S,"extrashots",tolua_get_artifact_type_extrashots,tolua_set_artifact_type_extrashots);
 tolua_variable(tolua_S,"speedbonus",tolua_get_artifact_type_speedbonus,tolua_set_artifact_type_speedbonus);
 tolua_variable(tolua_S,"lifebonus",tolua_get_artifact_type_lifebonus,tolua_set_artifact_type_lifebonus);
 tolua_variable(tolua_S,"manabonus",tolua_get_artifact_type_manabonus,tolua_set_artifact_type_manabonus);
 tolua_variable(tolua_S,"infravision",tolua_get_artifact_type_infravision,tolua_set_artifact_type_infravision);
 tolua_variable(tolua_S,"spellbonus",tolua_get_artifact_type_spellbonus,tolua_set_artifact_type_spellbonus);
 tolua_variable(tolua_S,"invisibility",tolua_get_artifact_type_invisibility,tolua_set_artifact_type_invisibility);
 tolua_variable(tolua_S,"light",tolua_get_artifact_type_light,tolua_set_artifact_type_light);
 tolua_variable(tolua_S,"extra1",tolua_get_artifact_type_extra1,tolua_set_artifact_type_extra1);
 tolua_variable(tolua_S,"extra2",tolua_get_artifact_type_extra2,tolua_set_artifact_type_extra2);
 tolua_variable(tolua_S,"extra3",tolua_get_artifact_type_extra3,tolua_set_artifact_type_extra3);
 tolua_variable(tolua_S,"extra4",tolua_get_artifact_type_extra4,tolua_set_artifact_type_extra4);
 tolua_variable(tolua_S,"extra5",tolua_get_artifact_type_extra5,tolua_set_artifact_type_extra5);
 tolua_variable(tolua_S,"reflect",tolua_get_artifact_type_reflect,tolua_set_artifact_type_reflect);
 tolua_array(tolua_S,"spell",tolua_get_types_artifact_type_spell,tolua_set_types_artifact_type_spell);
 tolua_endmodule(tolua_S);
 tolua_cclass(tolua_S,"ego_item_type","ego_item_type","",NULL);
 tolua_beginmodule(tolua_S,"ego_item_type");
 tolua_variable(tolua_S,"name",tolua_get_ego_item_type_name,tolua_set_ego_item_type_name);
 tolua_variable(tolua_S,"text",tolua_get_ego_item_type_text,tolua_set_ego_item_type_text);
 tolua_variable(tolua_S,"slot",tolua_get_ego_item_type_slot,tolua_set_ego_item_type_slot);
 tolua_variable(tolua_S,"rating",tolua_get_ego_item_type_rating,tolua_set_ego_item_type_rating);
 tolua_variable(tolua_S,"level",tolua_get_ego_item_type_level,tolua_set_ego_item_type_level);
 tolua_variable(tolua_S,"rarity",tolua_get_ego_item_type_rarity,tolua_set_ego_item_type_rarity);
 tolua_variable(tolua_S,"max_to_h",tolua_get_ego_item_type_max_to_h,tolua_set_ego_item_type_max_to_h);
 tolua_variable(tolua_S,"max_to_d",tolua_get_ego_item_type_max_to_d,tolua_set_ego_item_type_max_to_d);
 tolua_variable(tolua_S,"max_to_a",tolua_get_ego_item_type_max_to_a,tolua_set_ego_item_type_max_to_a);
 tolua_variable(tolua_S,"max_pval",tolua_get_ego_item_type_max_pval,tolua_set_ego_item_type_max_pval);
 tolua_variable(tolua_S,"cost",tolua_get_ego_item_type_cost,tolua_set_ego_item_type_cost);
 tolua_variable(tolua_S,"flags1",tolua_get_ego_item_type_flags1,tolua_set_ego_item_type_flags1);
 tolua_variable(tolua_S,"flags2",tolua_get_ego_item_type_flags2,tolua_set_ego_item_type_flags2);
 tolua_variable(tolua_S,"flags3",tolua_get_ego_item_type_flags3,tolua_set_ego_item_type_flags3);
 tolua_variable(tolua_S,"flags4",tolua_get_ego_item_type_flags4,tolua_set_ego_item_type_flags4);
 tolua_endmodule(tolua_S);
 tolua_cclass(tolua_S,"monster_race","monster_race","",NULL);
 tolua_beginmodule(tolua_S,"monster_race");
 tolua_variable(tolua_S,"name",tolua_get_monster_race_name,tolua_set_monster_race_name);
 tolua_variable(tolua_S,"text",tolua_get_monster_race_text,tolua_set_monster_race_text);
 tolua_variable(tolua_S,"name_char",tolua_get_monster_race_name_char,tolua_set_monster_race_name_char);
 tolua_variable(tolua_S,"hdice",tolua_get_monster_race_hdice,tolua_set_monster_race_hdice);
 tolua_variable(tolua_S,"hside",tolua_get_monster_race_hside,tolua_set_monster_race_hside);
 tolua_variable(tolua_S,"ac",tolua_get_monster_race_ac,tolua_set_monster_race_ac);
 tolua_variable(tolua_S,"sleep",tolua_get_monster_race_sleep,tolua_set_monster_race_sleep);
 tolua_variable(tolua_S,"aaf",tolua_get_monster_race_aaf,tolua_set_monster_race_aaf);
 tolua_variable(tolua_S,"speed",tolua_get_monster_race_speed,tolua_set_monster_race_speed);
 tolua_variable(tolua_S,"mexp",tolua_get_monster_race_mexp,tolua_set_monster_race_mexp);
 tolua_variable(tolua_S,"weight",tolua_get_monster_race_weight,tolua_set_monster_race_weight);
 tolua_variable(tolua_S,"freq_inate",tolua_get_monster_race_freq_inate,tolua_set_monster_race_freq_inate);
 tolua_variable(tolua_S,"freq_spell",tolua_get_monster_race_freq_spell,tolua_set_monster_race_freq_spell);
 tolua_variable(tolua_S,"flags1",tolua_get_monster_race_flags1,tolua_set_monster_race_flags1);
 tolua_variable(tolua_S,"flags2",tolua_get_monster_race_flags2,tolua_set_monster_race_flags2);
 tolua_variable(tolua_S,"flags3",tolua_get_monster_race_flags3,tolua_set_monster_race_flags3);
 tolua_variable(tolua_S,"flags4",tolua_get_monster_race_flags4,tolua_set_monster_race_flags4);
 tolua_variable(tolua_S,"flags5",tolua_get_monster_race_flags5,tolua_set_monster_race_flags5);
 tolua_variable(tolua_S,"flags6",tolua_get_monster_race_flags6,tolua_set_monster_race_flags6);
 tolua_variable(tolua_S,"flags7",tolua_get_monster_race_flags7,tolua_set_monster_race_flags7);
 tolua_variable(tolua_S,"flags8",tolua_get_monster_race_flags8,tolua_set_monster_race_flags8);
 tolua_variable(tolua_S,"flags9",tolua_get_monster_race_flags9,tolua_set_monster_race_flags9);
 tolua_array(tolua_S,"blow",tolua_get_types_monster_race_blow,tolua_set_types_monster_race_blow);
 tolua_array(tolua_S,"body_parts",tolua_get_types_monster_race_body_parts,tolua_set_types_monster_race_body_parts);
 tolua_variable(tolua_S,"level",tolua_get_monster_race_level,tolua_set_monster_race_level);
 tolua_variable(tolua_S,"rarity",tolua_get_monster_race_rarity,tolua_set_monster_race_rarity);
 tolua_variable(tolua_S,"d_attr",tolua_get_monster_race_d_attr,tolua_set_monster_race_d_attr);
 tolua_variable(tolua_S,"d_char",tolua_get_monster_race_d_char,tolua_set_monster_race_d_char);
 tolua_variable(tolua_S,"x_attr",tolua_get_monster_race_x_attr,tolua_set_monster_race_x_attr);
 tolua_variable(tolua_S,"x_char",tolua_get_monster_race_x_char,tolua_set_monster_race_x_char);
 tolua_variable(tolua_S,"max_num",tolua_get_monster_race_max_num,tolua_set_monster_race_max_num);
 tolua_variable(tolua_S,"cur_num",tolua_get_monster_race_cur_num,tolua_set_monster_race_cur_num);
 tolua_variable(tolua_S,"r_sights",tolua_get_monster_race_r_sights,tolua_set_monster_race_r_sights);
 tolua_variable(tolua_S,"r_deaths",tolua_get_monster_race_r_deaths,tolua_set_monster_race_r_deaths);
 tolua_variable(tolua_S,"r_pkills",tolua_get_monster_race_r_pkills,tolua_set_monster_race_r_pkills);
 tolua_variable(tolua_S,"r_tkills",tolua_get_monster_race_r_tkills,tolua_set_monster_race_r_tkills);
 tolua_variable(tolua_S,"r_wake",tolua_get_monster_race_r_wake,tolua_set_monster_race_r_wake);
 tolua_variable(tolua_S,"r_ignore",tolua_get_monster_race_r_ignore,tolua_set_monster_race_r_ignore);
 tolua_variable(tolua_S,"r_xtra1",tolua_get_monster_race_r_xtra1,tolua_set_monster_race_r_xtra1);
 tolua_variable(tolua_S,"r_xtra2",tolua_get_monster_race_r_xtra2,tolua_set_monster_race_r_xtra2);
 tolua_variable(tolua_S,"r_drop_gold",tolua_get_monster_race_r_drop_gold,tolua_set_monster_race_r_drop_gold);
 tolua_variable(tolua_S,"r_drop_item",tolua_get_monster_race_r_drop_item,tolua_set_monster_race_r_drop_item);
 tolua_variable(tolua_S,"r_cast_inate",tolua_get_monster_race_r_cast_inate,tolua_set_monster_race_r_cast_inate);
 tolua_variable(tolua_S,"r_cast_spell",tolua_get_monster_race_r_cast_spell,tolua_set_monster_race_r_cast_spell);
 tolua_array(tolua_S,"r_blows",tolua_get_types_monster_race_r_blows,tolua_set_types_monster_race_r_blows);
 tolua_array(tolua_S,"r_resist",tolua_get_types_monster_race_r_resist,tolua_set_types_monster_race_r_resist);
 tolua_array(tolua_S,"r_spells",tolua_get_types_monster_race_r_spells,tolua_set_types_monster_race_r_spells);
 tolua_variable(tolua_S,"r_flags1",tolua_get_monster_race_r_flags1,tolua_set_monster_race_r_flags1);
 tolua_variable(tolua_S,"r_flags2",tolua_get_monster_race_r_flags2,tolua_set_monster_race_r_flags2);
 tolua_variable(tolua_S,"r_flags3",tolua_get_monster_race_r_flags3,tolua_set_monster_race_r_flags3);
 tolua_variable(tolua_S,"r_flags4",tolua_get_monster_race_r_flags4,tolua_set_monster_race_r_flags4);
 tolua_variable(tolua_S,"r_flags5",tolua_get_monster_race_r_flags5,tolua_set_monster_race_r_flags5);
 tolua_variable(tolua_S,"r_flags6",tolua_get_monster_race_r_flags6,tolua_set_monster_race_r_flags6);
 tolua_variable(tolua_S,"r_flags7",tolua_get_monster_race_r_flags7,tolua_set_monster_race_r_flags7);
 tolua_variable(tolua_S,"r_flags8",tolua_get_monster_race_r_flags8,tolua_set_monster_race_r_flags8);
 tolua_variable(tolua_S,"r_flags9",tolua_get_monster_race_r_flags9,tolua_set_monster_race_r_flags9);
 tolua_variable(tolua_S,"on_saved",tolua_get_monster_race_on_saved,tolua_set_monster_race_on_saved);
 tolua_variable(tolua_S,"str",tolua_get_monster_race_str,tolua_set_monster_race_str);
 tolua_variable(tolua_S,"dex",tolua_get_monster_race_dex,tolua_set_monster_race_dex);
 tolua_variable(tolua_S,"mind",tolua_get_monster_race_mind,tolua_set_monster_race_mind);
 tolua_variable(tolua_S,"skill_attack",tolua_get_monster_race_skill_attack,tolua_set_monster_race_skill_attack);
 tolua_variable(tolua_S,"skill_ranged",tolua_get_monster_race_skill_ranged,tolua_set_monster_race_skill_ranged);
 tolua_variable(tolua_S,"skill_magic",tolua_get_monster_race_skill_magic,tolua_set_monster_race_skill_magic);
 tolua_variable(tolua_S,"countertype",tolua_get_monster_race_countertype,tolua_set_monster_race_countertype);
 tolua_variable(tolua_S,"counterchance",tolua_get_monster_race_counterchance,tolua_set_monster_race_counterchance);
 tolua_array(tolua_S,"resistances",tolua_get_types_monster_race_resistances,tolua_set_types_monster_race_resistances);
 tolua_variable(tolua_S,"spellchance",tolua_get_monster_race_spellchance,tolua_set_monster_race_spellchance);
 tolua_variable(tolua_S,"attacks",tolua_get_monster_race_attacks,tolua_set_monster_race_attacks);
 tolua_array(tolua_S,"attack",tolua_get_types_monster_race_attack,tolua_set_types_monster_race_attack);
 tolua_variable(tolua_S,"spells",tolua_get_monster_race_spells,tolua_set_monster_race_spells);
 tolua_array(tolua_S,"spell",tolua_get_types_monster_race_spell,tolua_set_types_monster_race_spell);
 tolua_variable(tolua_S,"treasuretval",tolua_get_monster_race_treasuretval,tolua_set_monster_race_treasuretval);
 tolua_variable(tolua_S,"treasuresval",tolua_get_monster_race_treasuresval,tolua_set_monster_race_treasuresval);
 tolua_variable(tolua_S,"treasurechance",tolua_get_monster_race_treasurechance,tolua_set_monster_race_treasurechance);
 tolua_variable(tolua_S,"treasuremagic",tolua_get_monster_race_treasuremagic,tolua_set_monster_race_treasuremagic);
 tolua_variable(tolua_S,"event",tolua_get_monster_race_event,tolua_set_monster_race_event);
 tolua_variable(tolua_S,"extra1",tolua_get_monster_race_extra1,tolua_set_monster_race_extra1);
 tolua_variable(tolua_S,"extra2",tolua_get_monster_race_extra2,tolua_set_monster_race_extra2);
 tolua_variable(tolua_S,"fixedlevel",tolua_get_monster_race_fixedlevel,tolua_set_monster_race_fixedlevel);
 tolua_variable(tolua_S,"townnum",tolua_get_monster_race_townnum,tolua_set_monster_race_townnum);
 tolua_variable(tolua_S,"dunnum",tolua_get_monster_race_dunnum,tolua_set_monster_race_dunnum);
 tolua_variable(tolua_S,"lives",tolua_get_monster_race_lives,tolua_set_monster_race_lives);
 tolua_variable(tolua_S,"cursed",tolua_get_monster_race_cursed,tolua_set_monster_race_cursed);
 tolua_variable(tolua_S,"event_before_melee",tolua_get_monster_race_event_before_melee,tolua_set_monster_race_event_before_melee);
 tolua_variable(tolua_S,"event_after_melee",tolua_get_monster_race_event_after_melee,tolua_set_monster_race_event_after_melee);
 tolua_variable(tolua_S,"event_before_ranged",tolua_get_monster_race_event_before_ranged,tolua_set_monster_race_event_before_ranged);
 tolua_variable(tolua_S,"event_after_ranged",tolua_get_monster_race_event_after_ranged,tolua_set_monster_race_event_after_ranged);
 tolua_variable(tolua_S,"event_before_magic",tolua_get_monster_race_event_before_magic,tolua_set_monster_race_event_before_magic);
 tolua_variable(tolua_S,"event_after_magic",tolua_get_monster_race_event_after_magic,tolua_set_monster_race_event_after_magic);
 tolua_variable(tolua_S,"event_before_move",tolua_get_monster_race_event_before_move,tolua_set_monster_race_event_before_move);
 tolua_variable(tolua_S,"event_after_move",tolua_get_monster_race_event_after_move,tolua_set_monster_race_event_after_move);
 tolua_variable(tolua_S,"event_passive",tolua_get_monster_race_event_passive,tolua_set_monster_race_event_passive);
 tolua_variable(tolua_S,"event_take_damages",tolua_get_monster_race_event_take_damages,tolua_set_monster_race_event_take_damages);
 tolua_variable(tolua_S,"event_death",tolua_get_monster_race_event_death,tolua_set_monster_race_event_death);
 tolua_variable(tolua_S,"event_spawn",tolua_get_monster_race_event_spawn,tolua_set_monster_race_event_spawn);
 tolua_variable(tolua_S,"event_misc",tolua_get_monster_race_event_misc,tolua_set_monster_race_event_misc);
 tolua_endmodule(tolua_S);
 tolua_cclass(tolua_S,"vault_type","vault_type","",NULL);
 tolua_beginmodule(tolua_S,"vault_type");
 tolua_variable(tolua_S,"name",tolua_get_vault_type_name,tolua_set_vault_type_name);
 tolua_variable(tolua_S,"text",tolua_get_vault_type_text,tolua_set_vault_type_text);
 tolua_variable(tolua_S,"typ",tolua_get_vault_type_typ,tolua_set_vault_type_typ);
 tolua_variable(tolua_S,"rat",tolua_get_vault_type_rat,tolua_set_vault_type_rat);
 tolua_variable(tolua_S,"hgt",tolua_get_vault_type_hgt,tolua_set_vault_type_hgt);
 tolua_variable(tolua_S,"wid",tolua_get_vault_type_wid,tolua_set_vault_type_wid);
 tolua_variable(tolua_S,"lvl",tolua_get_vault_type_lvl,tolua_set_vault_type_lvl);
 tolua_variable(tolua_S,"dun_type",tolua_get_vault_type_dun_type,tolua_set_vault_type_dun_type);
 tolua_array(tolua_S,"mon",tolua_get_types_vault_type_mon,tolua_set_types_vault_type_mon);
 tolua_array(tolua_S,"item",tolua_get_types_vault_type_item,tolua_set_types_vault_type_item);
 tolua_endmodule(tolua_S);
 tolua_cclass(tolua_S,"trap_type","trap_type","",NULL);
 tolua_beginmodule(tolua_S,"trap_type");
 tolua_variable(tolua_S,"probability",tolua_get_trap_type_probability,tolua_set_trap_type_probability);
 tolua_variable(tolua_S,"another",tolua_get_trap_type_another,tolua_set_trap_type_another);
 tolua_variable(tolua_S,"p1valinc",tolua_get_trap_type_p1valinc,tolua_set_trap_type_p1valinc);
 tolua_variable(tolua_S,"difficulty",tolua_get_trap_type_difficulty,tolua_set_trap_type_difficulty);
 tolua_variable(tolua_S,"minlevel",tolua_get_trap_type_minlevel,tolua_set_trap_type_minlevel);
 tolua_variable(tolua_S,"color",tolua_get_trap_type_color,tolua_set_trap_type_color);
 tolua_variable(tolua_S,"flags",tolua_get_trap_type_flags,tolua_set_trap_type_flags);
 tolua_variable(tolua_S,"ident",tolua_get_trap_type_ident,tolua_set_trap_type_ident);
 tolua_variable(tolua_S,"known",tolua_get_trap_type_known,tolua_set_trap_type_known);
 tolua_variable(tolua_S,"name",tolua_get_trap_type_name,tolua_set_trap_type_name);
 tolua_variable(tolua_S,"dd",tolua_get_trap_type_dd,tolua_set_trap_type_dd);
 tolua_variable(tolua_S,"ds",tolua_get_trap_type_ds,tolua_set_trap_type_ds);
 tolua_variable(tolua_S,"text",tolua_get_trap_type_text,tolua_set_trap_type_text);
 tolua_endmodule(tolua_S);
 tolua_cclass(tolua_S,"cave_type","cave_type","",NULL);
 tolua_beginmodule(tolua_S,"cave_type");
 tolua_variable(tolua_S,"info",tolua_get_cave_type_info,tolua_set_cave_type_info);
 tolua_variable(tolua_S,"feat",tolua_get_cave_type_feat,tolua_set_cave_type_feat);
 tolua_variable(tolua_S,"o_idx",tolua_get_cave_type_o_idx,tolua_set_cave_type_o_idx);
 tolua_variable(tolua_S,"m_idx",tolua_get_cave_type_m_idx,tolua_set_cave_type_m_idx);
 tolua_variable(tolua_S,"t_idx",tolua_get_cave_type_t_idx,tolua_set_cave_type_t_idx);
 tolua_variable(tolua_S,"special",tolua_get_cave_type_special,tolua_set_cave_type_special);
 tolua_variable(tolua_S,"inscription",tolua_get_cave_type_inscription,tolua_set_cave_type_inscription);
 tolua_variable(tolua_S,"mana",tolua_get_cave_type_mana,tolua_set_cave_type_mana);
 tolua_variable(tolua_S,"mimic",tolua_get_cave_type_mimic,tolua_set_cave_type_mimic);
 tolua_variable(tolua_S,"cost",tolua_get_cave_type_cost,tolua_set_cave_type_cost);
 tolua_variable(tolua_S,"when",tolua_get_cave_type_when,tolua_set_cave_type_when);
 tolua_variable(tolua_S,"field_damage",tolua_get_cave_type_field_damage,tolua_set_cave_type_field_damage);
 tolua_variable(tolua_S,"event",tolua_get_cave_type_event,tolua_set_cave_type_event);
 tolua_variable(tolua_S,"eventtype",tolua_get_cave_type_eventtype,tolua_set_cave_type_eventtype);
 tolua_variable(tolua_S,"eventextra",tolua_get_cave_type_eventextra,tolua_set_cave_type_eventextra);
 tolua_variable(tolua_S,"eventextra2",tolua_get_cave_type_eventextra2,tolua_set_cave_type_eventextra2);
 tolua_variable(tolua_S,"eventcond",tolua_get_cave_type_eventcond,tolua_set_cave_type_eventcond);
 tolua_variable(tolua_S,"eventcondval",tolua_get_cave_type_eventcondval,tolua_set_cave_type_eventcondval);
 tolua_variable(tolua_S,"eventset",tolua_get_cave_type_eventset,tolua_set_cave_type_eventset);
 tolua_variable(tolua_S,"eventsetval",tolua_get_cave_type_eventsetval,tolua_set_cave_type_eventsetval);
 tolua_variable(tolua_S,"script",tolua_get_cave_type_script,tolua_set_cave_type_script);
 tolua_variable(tolua_S,"owner",tolua_get_cave_type_owner,tolua_set_cave_type_owner);
 tolua_variable(tolua_S,"script_name",tolua_get_cave_type_script_name,tolua_set_cave_type_script_name);
 tolua_endmodule(tolua_S);
 tolua_cclass(tolua_S,"object_type","object_type","",NULL);
 tolua_beginmodule(tolua_S,"object_type");
 tolua_variable(tolua_S,"k_idx",tolua_get_object_type_k_idx,tolua_set_object_type_k_idx);
 tolua_variable(tolua_S,"iy",tolua_get_object_type_iy,tolua_set_object_type_iy);
 tolua_variable(tolua_S,"ix",tolua_get_object_type_ix,tolua_set_object_type_ix);
 tolua_variable(tolua_S,"tval",tolua_get_object_type_tval,tolua_set_object_type_tval);
 tolua_variable(tolua_S,"sval",tolua_get_object_type_sval,tolua_set_object_type_sval);
 tolua_variable(tolua_S,"pval",tolua_get_object_type_pval,tolua_set_object_type_pval);
 tolua_variable(tolua_S,"pval2",tolua_get_object_type_pval2,tolua_set_object_type_pval2);
 tolua_variable(tolua_S,"pval3",tolua_get_object_type_pval3,tolua_set_object_type_pval3);
 tolua_variable(tolua_S,"brandtype",tolua_get_object_type_brandtype,tolua_set_object_type_brandtype);
 tolua_variable(tolua_S,"branddam",tolua_get_object_type_branddam,tolua_set_object_type_branddam);
 tolua_variable(tolua_S,"brandrad",tolua_get_object_type_brandrad,tolua_set_object_type_brandrad);
 tolua_array(tolua_S,"resistances",tolua_get_types_object_type_resistances,tolua_set_types_object_type_resistances);
 tolua_variable(tolua_S,"itemtype",tolua_get_object_type_itemtype,tolua_set_object_type_itemtype);
 tolua_variable(tolua_S,"itemskill",tolua_get_object_type_itemskill,tolua_set_object_type_itemskill);
 tolua_array(tolua_S,"statsbonus",tolua_get_types_object_type_statsbonus,tolua_set_types_object_type_statsbonus);
 tolua_array(tolua_S,"skillsbonus",tolua_get_types_object_type_skillsbonus,tolua_set_types_object_type_skillsbonus);
 tolua_variable(tolua_S,"extrablows",tolua_get_object_type_extrablows,tolua_set_object_type_extrablows);
 tolua_variable(tolua_S,"extrashots",tolua_get_object_type_extrashots,tolua_set_object_type_extrashots);
 tolua_variable(tolua_S,"speedbonus",tolua_get_object_type_speedbonus,tolua_set_object_type_speedbonus);
 tolua_variable(tolua_S,"lifebonus",tolua_get_object_type_lifebonus,tolua_set_object_type_lifebonus);
 tolua_variable(tolua_S,"manabonus",tolua_get_object_type_manabonus,tolua_set_object_type_manabonus);
 tolua_variable(tolua_S,"infravision",tolua_get_object_type_infravision,tolua_set_object_type_infravision);
 tolua_variable(tolua_S,"spellbonus",tolua_get_object_type_spellbonus,tolua_set_object_type_spellbonus);
 tolua_variable(tolua_S,"invisibility",tolua_get_object_type_invisibility,tolua_set_object_type_invisibility);
 tolua_variable(tolua_S,"light",tolua_get_object_type_light,tolua_set_object_type_light);
 tolua_variable(tolua_S,"extra1",tolua_get_object_type_extra1,tolua_set_object_type_extra1);
 tolua_variable(tolua_S,"extra2",tolua_get_object_type_extra2,tolua_set_object_type_extra2);
 tolua_variable(tolua_S,"extra3",tolua_get_object_type_extra3,tolua_set_object_type_extra3);
 tolua_variable(tolua_S,"extra4",tolua_get_object_type_extra4,tolua_set_object_type_extra4);
 tolua_variable(tolua_S,"extra5",tolua_get_object_type_extra5,tolua_set_object_type_extra5);
 tolua_variable(tolua_S,"reflect",tolua_get_object_type_reflect,tolua_set_object_type_reflect);
 tolua_variable(tolua_S,"cursed",tolua_get_object_type_cursed,tolua_set_object_type_cursed);
 tolua_variable(tolua_S,"tweakpoints",tolua_get_object_type_tweakpoints,tolua_set_object_type_tweakpoints);
 tolua_variable(tolua_S,"disabled",tolua_get_object_type_disabled,tolua_set_object_type_disabled);
 tolua_variable(tolua_S,"discount",tolua_get_object_type_discount,tolua_set_object_type_discount);
 tolua_variable(tolua_S,"number",tolua_get_object_type_number,tolua_set_object_type_number);
 tolua_variable(tolua_S,"weight",tolua_get_object_type_weight,tolua_set_object_type_weight);
 tolua_variable(tolua_S,"level",tolua_get_object_type_level,tolua_set_object_type_level);
 tolua_variable(tolua_S,"kills",tolua_get_object_type_kills,tolua_set_object_type_kills);
 tolua_variable(tolua_S,"name1",tolua_get_object_type_name1,tolua_set_object_type_name1);
 tolua_variable(tolua_S,"name2",tolua_get_object_type_name2,tolua_set_object_type_name2);
 tolua_variable(tolua_S,"xtra1",tolua_get_object_type_xtra1,tolua_set_object_type_xtra1);
 tolua_variable(tolua_S,"xtra2",tolua_get_object_type_xtra2,tolua_set_object_type_xtra2);
 tolua_variable(tolua_S,"to_h",tolua_get_object_type_to_h,tolua_set_object_type_to_h);
 tolua_variable(tolua_S,"to_d",tolua_get_object_type_to_d,tolua_set_object_type_to_d);
 tolua_variable(tolua_S,"to_a",tolua_get_object_type_to_a,tolua_set_object_type_to_a);
 tolua_variable(tolua_S,"ac",tolua_get_object_type_ac,tolua_set_object_type_ac);
 tolua_variable(tolua_S,"dd",tolua_get_object_type_dd,tolua_set_object_type_dd);
 tolua_variable(tolua_S,"ds",tolua_get_object_type_ds,tolua_set_object_type_ds);
 tolua_variable(tolua_S,"timeout",tolua_get_object_type_timeout,tolua_set_object_type_timeout);
 tolua_variable(tolua_S,"ident",tolua_get_object_type_ident,tolua_set_object_type_ident);
 tolua_variable(tolua_S,"marked",tolua_get_object_type_marked,tolua_set_object_type_marked);
 tolua_variable(tolua_S,"note",tolua_get_object_type_note,tolua_set_object_type_note);
 tolua_variable(tolua_S,"art_name",tolua_get_object_type_art_name,tolua_set_object_type_art_name);
 tolua_variable(tolua_S,"art_flags1",tolua_get_object_type_art_flags1,tolua_set_object_type_art_flags1);
 tolua_variable(tolua_S,"art_flags2",tolua_get_object_type_art_flags2,tolua_set_object_type_art_flags2);
 tolua_variable(tolua_S,"art_flags3",tolua_get_object_type_art_flags3,tolua_set_object_type_art_flags3);
 tolua_variable(tolua_S,"art_flags4",tolua_get_object_type_art_flags4,tolua_set_object_type_art_flags4);
 tolua_variable(tolua_S,"next_o_idx",tolua_get_object_type_next_o_idx,tolua_set_object_type_next_o_idx);
 tolua_variable(tolua_S,"held_m_idx",tolua_get_object_type_held_m_idx,tolua_set_object_type_held_m_idx);
 tolua_array(tolua_S,"spell",tolua_get_types_object_type_spell,tolua_set_types_object_type_spell);
 tolua_endmodule(tolua_S);
 tolua_cclass(tolua_S,"monster_type","monster_type","",NULL);
 tolua_beginmodule(tolua_S,"monster_type");
 tolua_variable(tolua_S,"r_idx",tolua_get_monster_type_r_idx,tolua_set_monster_type_r_idx);
 tolua_variable(tolua_S,"fy",tolua_get_monster_type_fy,tolua_set_monster_type_fy);
 tolua_variable(tolua_S,"fx",tolua_get_monster_type_fx,tolua_set_monster_type_fx);
 tolua_variable(tolua_S,"hp",tolua_get_monster_type_hp,tolua_set_monster_type_hp);
 tolua_variable(tolua_S,"maxhp",tolua_get_monster_type_maxhp,tolua_set_monster_type_maxhp);
 tolua_variable(tolua_S,"csleep",tolua_get_monster_type_csleep,tolua_set_monster_type_csleep);
 tolua_variable(tolua_S,"mspeed",tolua_get_monster_type_mspeed,tolua_set_monster_type_mspeed);
 tolua_variable(tolua_S,"energy",tolua_get_monster_type_energy,tolua_set_monster_type_energy);
 tolua_variable(tolua_S,"stunned",tolua_get_monster_type_stunned,tolua_set_monster_type_stunned);
 tolua_variable(tolua_S,"confused",tolua_get_monster_type_confused,tolua_set_monster_type_confused);
 tolua_variable(tolua_S,"monfear",tolua_get_monster_type_monfear,tolua_set_monster_type_monfear);
 tolua_variable(tolua_S,"cdis",tolua_get_monster_type_cdis,tolua_set_monster_type_cdis);
 tolua_variable(tolua_S,"mflag",tolua_get_monster_type_mflag,tolua_set_monster_type_mflag);
 tolua_variable(tolua_S,"ml",tolua_get_monster_type_ml,tolua_set_monster_type_ml);
 tolua_variable(tolua_S,"hold_o_idx",tolua_get_monster_type_hold_o_idx,tolua_set_monster_type_hold_o_idx);
 tolua_variable(tolua_S,"smart",tolua_get_monster_type_smart,tolua_set_monster_type_smart);
 tolua_variable(tolua_S,"imprinted",tolua_get_monster_type_imprinted,tolua_set_monster_type_imprinted);
 tolua_variable(tolua_S,"level",tolua_get_monster_type_level,tolua_set_monster_type_level);
 tolua_variable(tolua_S,"angered_pet",tolua_get_monster_type_angered_pet,tolua_set_monster_type_angered_pet);
 tolua_variable(tolua_S,"boss",tolua_get_monster_type_boss,tolua_set_monster_type_boss);
 tolua_variable(tolua_S,"abilities",tolua_get_monster_type_abilities,tolua_set_monster_type_abilities);
 tolua_variable(tolua_S,"friend",tolua_get_monster_type_friend,tolua_set_monster_type_friend);
 tolua_variable(tolua_S,"hitrate",tolua_get_monster_type_hitrate,tolua_set_monster_type_hitrate);
 tolua_variable(tolua_S,"defense",tolua_get_monster_type_defense,tolua_set_monster_type_defense);
 tolua_variable(tolua_S,"animated",tolua_get_monster_type_animated,tolua_set_monster_type_animated);
 tolua_variable(tolua_S,"animdam_d",tolua_get_monster_type_animdam_d,tolua_set_monster_type_animdam_d);
 tolua_variable(tolua_S,"animdam_s",tolua_get_monster_type_animdam_s,tolua_set_monster_type_animdam_s);
 tolua_variable(tolua_S,"seallight",tolua_get_monster_type_seallight,tolua_set_monster_type_seallight);
 tolua_variable(tolua_S,"str",tolua_get_monster_type_str,tolua_set_monster_type_str);
 tolua_variable(tolua_S,"dex",tolua_get_monster_type_dex,tolua_set_monster_type_dex);
 tolua_variable(tolua_S,"mind",tolua_get_monster_type_mind,tolua_set_monster_type_mind);
 tolua_variable(tolua_S,"skill_attack",tolua_get_monster_type_skill_attack,tolua_set_monster_type_skill_attack);
 tolua_variable(tolua_S,"skill_ranged",tolua_get_monster_type_skill_ranged,tolua_set_monster_type_skill_ranged);
 tolua_variable(tolua_S,"skill_magic",tolua_get_monster_type_skill_magic,tolua_set_monster_type_skill_magic);
 tolua_variable(tolua_S,"mana",tolua_get_monster_type_mana,tolua_set_monster_type_mana);
 tolua_variable(tolua_S,"hasted",tolua_get_monster_type_hasted,tolua_set_monster_type_hasted);
 tolua_variable(tolua_S,"boosted",tolua_get_monster_type_boosted,tolua_set_monster_type_boosted);
 tolua_variable(tolua_S,"spoke",tolua_get_monster_type_spoke,tolua_set_monster_type_spoke);
 tolua_variable(tolua_S,"lives",tolua_get_monster_type_lives,tolua_set_monster_type_lives);
 tolua_variable(tolua_S,"summoned",tolua_get_monster_type_summoned,tolua_set_monster_type_summoned);
 tolua_variable(tolua_S,"no_experience",tolua_get_monster_type_no_experience,tolua_set_monster_type_no_experience);
 tolua_endmodule(tolua_S);
 tolua_cclass(tolua_S,"alloc_entry","alloc_entry","",NULL);
 tolua_beginmodule(tolua_S,"alloc_entry");
 tolua_variable(tolua_S,"index",tolua_get_alloc_entry_index,tolua_set_alloc_entry_index);
 tolua_variable(tolua_S,"level",tolua_get_alloc_entry_level,tolua_set_alloc_entry_level);
 tolua_variable(tolua_S,"prob1",tolua_get_alloc_entry_prob1,tolua_set_alloc_entry_prob1);
 tolua_variable(tolua_S,"prob2",tolua_get_alloc_entry_prob2,tolua_set_alloc_entry_prob2);
 tolua_variable(tolua_S,"prob3",tolua_get_alloc_entry_prob3,tolua_set_alloc_entry_prob3);
 tolua_variable(tolua_S,"total",tolua_get_alloc_entry_total,tolua_set_alloc_entry_total);
 tolua_endmodule(tolua_S);
 tolua_cclass(tolua_S,"option_type","option_type","",NULL);
 tolua_beginmodule(tolua_S,"option_type");
 tolua_variable(tolua_S,"o_var",tolua_get_option_type_o_var,tolua_set_option_type_o_var);
 tolua_variable(tolua_S,"o_norm",tolua_get_option_type_o_norm,tolua_set_option_type_o_norm);
 tolua_variable(tolua_S,"o_page",tolua_get_option_type_o_page,tolua_set_option_type_o_page);
 tolua_variable(tolua_S,"o_set",tolua_get_option_type_o_set,tolua_set_option_type_o_set);
 tolua_variable(tolua_S,"o_bit",tolua_get_option_type_o_bit,tolua_set_option_type_o_bit);
 tolua_variable(tolua_S,"o_text",tolua_get_option_type_o_text,tolua_set_option_type_o_text);
 tolua_variable(tolua_S,"o_desc",tolua_get_option_type_o_desc,tolua_set_option_type_o_desc);
 tolua_endmodule(tolua_S);
 tolua_cclass(tolua_S,"owner_type","owner_type","",NULL);
 tolua_beginmodule(tolua_S,"owner_type");
 tolua_variable(tolua_S,"owner_name",tolua_get_owner_type_owner_name,tolua_set_owner_type_owner_name);
 tolua_variable(tolua_S,"max_cost",tolua_get_owner_type_max_cost,tolua_set_owner_type_max_cost);
 tolua_variable(tolua_S,"max_inflate",tolua_get_owner_type_max_inflate,tolua_set_owner_type_max_inflate);
 tolua_variable(tolua_S,"min_inflate",tolua_get_owner_type_min_inflate,tolua_set_owner_type_min_inflate);
 tolua_variable(tolua_S,"haggle_per",tolua_get_owner_type_haggle_per,tolua_set_owner_type_haggle_per);
 tolua_variable(tolua_S,"insult_max",tolua_get_owner_type_insult_max,tolua_set_owner_type_insult_max);
 tolua_variable(tolua_S,"owner_race",tolua_get_owner_type_owner_race,tolua_set_owner_type_owner_race);
 tolua_endmodule(tolua_S);
 tolua_cclass(tolua_S,"store_type","store_type","",NULL);
 tolua_beginmodule(tolua_S,"store_type");
 tolua_variable(tolua_S,"type",tolua_get_store_type_type,tolua_set_store_type_type);
 tolua_variable(tolua_S,"owner",tolua_get_store_type_owner,tolua_set_store_type_owner);
 tolua_variable(tolua_S,"extra",tolua_get_store_type_extra,tolua_set_store_type_extra);
 tolua_variable(tolua_S,"insult_cur",tolua_get_store_type_insult_cur,tolua_set_store_type_insult_cur);
 tolua_variable(tolua_S,"good_buy",tolua_get_store_type_good_buy,tolua_set_store_type_good_buy);
 tolua_variable(tolua_S,"bad_buy",tolua_get_store_type_bad_buy,tolua_set_store_type_bad_buy);
 tolua_variable(tolua_S,"store_open",tolua_get_store_type_store_open,tolua_set_store_type_store_open);
 tolua_variable(tolua_S,"last_visit",tolua_get_store_type_last_visit,tolua_set_store_type_last_visit);
 tolua_variable(tolua_S,"table_num",tolua_get_store_type_table_num,tolua_set_store_type_table_num);
 tolua_variable(tolua_S,"table_size",tolua_get_store_type_table_size,tolua_set_store_type_table_size);
 tolua_variable(tolua_S,"table",tolua_get_store_type_table,tolua_set_store_type_table);
 tolua_variable(tolua_S,"stock_num",tolua_get_store_type_stock_num,tolua_set_store_type_stock_num);
 tolua_variable(tolua_S,"stock_size",tolua_get_store_type_stock_size,tolua_set_store_type_stock_size);
 tolua_variable(tolua_S,"stock",tolua_get_store_type_stock_ptr,tolua_set_store_type_stock_ptr);
 tolua_endmodule(tolua_S);
 tolua_cclass(tolua_S,"player_sex","player_sex","",NULL);
 tolua_beginmodule(tolua_S,"player_sex");
 tolua_variable(tolua_S,"title",tolua_get_player_sex_title,tolua_set_player_sex_title);
 tolua_variable(tolua_S,"winner",tolua_get_player_sex_winner,tolua_set_player_sex_winner);
 tolua_endmodule(tolua_S);
 tolua_cclass(tolua_S,"player_race","player_race","",NULL);
 tolua_beginmodule(tolua_S,"player_race");
 tolua_variable(tolua_S,"title",tolua_get_player_race_title,tolua_set_player_race_title);
 tolua_array(tolua_S,"r_adj",tolua_get_types_player_race_r_adj,tolua_set_types_player_race_r_adj);
 tolua_variable(tolua_S,"r_dis",tolua_get_player_race_r_dis,tolua_set_player_race_r_dis);
 tolua_variable(tolua_S,"r_dev",tolua_get_player_race_r_dev,tolua_set_player_race_r_dev);
 tolua_variable(tolua_S,"r_sav",tolua_get_player_race_r_sav,tolua_set_player_race_r_sav);
 tolua_variable(tolua_S,"r_stl",tolua_get_player_race_r_stl,tolua_set_player_race_r_stl);
 tolua_variable(tolua_S,"r_srh",tolua_get_player_race_r_srh,tolua_set_player_race_r_srh);
 tolua_variable(tolua_S,"r_fos",tolua_get_player_race_r_fos,tolua_set_player_race_r_fos);
 tolua_variable(tolua_S,"r_thn",tolua_get_player_race_r_thn,tolua_set_player_race_r_thn);
 tolua_variable(tolua_S,"r_thb",tolua_get_player_race_r_thb,tolua_set_player_race_r_thb);
 tolua_variable(tolua_S,"r_mhp",tolua_get_player_race_r_mhp,tolua_set_player_race_r_mhp);
 tolua_variable(tolua_S,"r_exp",tolua_get_player_race_r_exp,tolua_set_player_race_r_exp);
 tolua_variable(tolua_S,"b_age",tolua_get_player_race_b_age,tolua_set_player_race_b_age);
 tolua_variable(tolua_S,"m_age",tolua_get_player_race_m_age,tolua_set_player_race_m_age);
 tolua_variable(tolua_S,"m_b_ht",tolua_get_player_race_m_b_ht,tolua_set_player_race_m_b_ht);
 tolua_variable(tolua_S,"m_m_ht",tolua_get_player_race_m_m_ht,tolua_set_player_race_m_m_ht);
 tolua_variable(tolua_S,"m_b_wt",tolua_get_player_race_m_b_wt,tolua_set_player_race_m_b_wt);
 tolua_variable(tolua_S,"m_m_wt",tolua_get_player_race_m_m_wt,tolua_set_player_race_m_m_wt);
 tolua_variable(tolua_S,"f_b_ht",tolua_get_player_race_f_b_ht,tolua_set_player_race_f_b_ht);
 tolua_variable(tolua_S,"f_m_ht",tolua_get_player_race_f_m_ht,tolua_set_player_race_f_m_ht);
 tolua_variable(tolua_S,"f_b_wt",tolua_get_player_race_f_b_wt,tolua_set_player_race_f_b_wt);
 tolua_variable(tolua_S,"f_m_wt",tolua_get_player_race_f_m_wt,tolua_set_player_race_f_m_wt);
 tolua_variable(tolua_S,"infra",tolua_get_player_race_infra,tolua_set_player_race_infra);
 tolua_variable(tolua_S,"choice",tolua_get_player_race_choice,tolua_set_player_race_choice);
 tolua_endmodule(tolua_S);
 tolua_cclass(tolua_S,"player_class","player_class","",NULL);
 tolua_beginmodule(tolua_S,"player_class");
 tolua_variable(tolua_S,"title",tolua_get_player_class_title,tolua_set_player_class_title);
 tolua_array(tolua_S,"c_adj",tolua_get_types_player_class_c_adj,tolua_set_types_player_class_c_adj);
 tolua_variable(tolua_S,"c_dis",tolua_get_player_class_c_dis,tolua_set_player_class_c_dis);
 tolua_variable(tolua_S,"c_dev",tolua_get_player_class_c_dev,tolua_set_player_class_c_dev);
 tolua_variable(tolua_S,"c_sav",tolua_get_player_class_c_sav,tolua_set_player_class_c_sav);
 tolua_variable(tolua_S,"c_stl",tolua_get_player_class_c_stl,tolua_set_player_class_c_stl);
 tolua_variable(tolua_S,"c_srh",tolua_get_player_class_c_srh,tolua_set_player_class_c_srh);
 tolua_variable(tolua_S,"c_fos",tolua_get_player_class_c_fos,tolua_set_player_class_c_fos);
 tolua_variable(tolua_S,"c_thn",tolua_get_player_class_c_thn,tolua_set_player_class_c_thn);
 tolua_variable(tolua_S,"c_thb",tolua_get_player_class_c_thb,tolua_set_player_class_c_thb);
 tolua_variable(tolua_S,"x_dis",tolua_get_player_class_x_dis,tolua_set_player_class_x_dis);
 tolua_variable(tolua_S,"x_dev",tolua_get_player_class_x_dev,tolua_set_player_class_x_dev);
 tolua_variable(tolua_S,"x_sav",tolua_get_player_class_x_sav,tolua_set_player_class_x_sav);
 tolua_variable(tolua_S,"x_stl",tolua_get_player_class_x_stl,tolua_set_player_class_x_stl);
 tolua_variable(tolua_S,"x_srh",tolua_get_player_class_x_srh,tolua_set_player_class_x_srh);
 tolua_variable(tolua_S,"x_fos",tolua_get_player_class_x_fos,tolua_set_player_class_x_fos);
 tolua_variable(tolua_S,"x_thn",tolua_get_player_class_x_thn,tolua_set_player_class_x_thn);
 tolua_variable(tolua_S,"x_thb",tolua_get_player_class_x_thb,tolua_set_player_class_x_thb);
 tolua_variable(tolua_S,"c_mhp",tolua_get_player_class_c_mhp,tolua_set_player_class_c_mhp);
 tolua_variable(tolua_S,"c_exp",tolua_get_player_class_c_exp,tolua_set_player_class_c_exp);
 tolua_endmodule(tolua_S);
 tolua_cclass(tolua_S,"player_type","player_type","",NULL);
 tolua_beginmodule(tolua_S,"player_type");
 tolua_variable(tolua_S,"oldpy",tolua_get_player_type_oldpy,tolua_set_player_type_oldpy);
 tolua_variable(tolua_S,"oldpx",tolua_get_player_type_oldpx,tolua_set_player_type_oldpx);
 tolua_variable(tolua_S,"psex",tolua_get_player_type_psex,tolua_set_player_type_psex);
 tolua_variable(tolua_S,"prace",tolua_get_player_type_prace,tolua_set_player_type_prace);
 tolua_variable(tolua_S,"pclass",tolua_get_player_type_pclass,tolua_set_player_type_pclass);
 tolua_variable(tolua_S,"oops",tolua_get_player_type_oops,tolua_set_player_type_oops);
 tolua_variable(tolua_S,"hitdie",tolua_get_player_type_hitdie,tolua_set_player_type_hitdie);
 tolua_variable(tolua_S,"expfact",tolua_get_player_type_expfact,tolua_set_player_type_expfact);
 tolua_variable(tolua_S,"maximize",tolua_get_player_type_maximize,tolua_set_player_type_maximize);
 tolua_variable(tolua_S,"preserve",tolua_get_player_type_preserve,tolua_set_player_type_preserve);
 tolua_variable(tolua_S,"special",tolua_get_player_type_special,tolua_set_player_type_special);
 tolua_variable(tolua_S,"allow_one_death",tolua_get_player_type_allow_one_death,tolua_set_player_type_allow_one_death);
 tolua_variable(tolua_S,"age",tolua_get_player_type_age,tolua_set_player_type_age);
 tolua_variable(tolua_S,"ht",tolua_get_player_type_ht,tolua_set_player_type_ht);
 tolua_variable(tolua_S,"wt",tolua_get_player_type_wt,tolua_set_player_type_wt);
 tolua_variable(tolua_S,"sc",tolua_get_player_type_sc,tolua_set_player_type_sc);
 tolua_variable(tolua_S,"au",tolua_get_player_type_au,tolua_set_player_type_au);
 tolua_variable(tolua_S,"max_exp",tolua_get_player_type_max_exp,tolua_set_player_type_max_exp);
 tolua_variable(tolua_S,"exp",tolua_get_player_type_exp,tolua_set_player_type_exp);
 tolua_variable(tolua_S,"exp_frac",tolua_get_player_type_exp_frac,tolua_set_player_type_exp_frac);
 tolua_variable(tolua_S,"lev",tolua_get_player_type_lev,tolua_set_player_type_lev);
 tolua_variable(tolua_S,"town_num",tolua_get_player_type_town_num,tolua_set_player_type_town_num);
 tolua_variable(tolua_S,"town_name",tolua_get_player_type_town_name,tolua_set_player_type_town_name);
 tolua_variable(tolua_S,"inside_quest",tolua_get_player_type_inside_quest,tolua_set_player_type_inside_quest);
 tolua_variable(tolua_S,"quest_name",tolua_get_player_type_quest_name,tolua_set_player_type_quest_name);
 tolua_variable(tolua_S,"death_dialog",tolua_get_player_type_death_dialog,tolua_set_player_type_death_dialog);
 tolua_variable(tolua_S,"eventdeath",tolua_get_player_type_eventdeath,tolua_set_player_type_eventdeath);
 tolua_variable(tolua_S,"eventdeathset",tolua_get_player_type_eventdeathset,tolua_set_player_type_eventdeathset);
 tolua_variable(tolua_S,"wild_x",tolua_get_player_type_wild_x,tolua_set_player_type_wild_x);
 tolua_variable(tolua_S,"wild_y",tolua_get_player_type_wild_y,tolua_set_player_type_wild_y);
 tolua_variable(tolua_S,"wild_mode",tolua_get_player_type_wild_mode,tolua_set_player_type_wild_mode);
 tolua_variable(tolua_S,"mhp",tolua_get_player_type_mhp,tolua_set_player_type_mhp);
 tolua_variable(tolua_S,"chp",tolua_get_player_type_chp,tolua_set_player_type_chp);
 tolua_variable(tolua_S,"chp_frac",tolua_get_player_type_chp_frac,tolua_set_player_type_chp_frac);
 tolua_variable(tolua_S,"msp",tolua_get_player_type_msp,tolua_set_player_type_msp);
 tolua_variable(tolua_S,"csp",tolua_get_player_type_csp,tolua_set_player_type_csp);
 tolua_variable(tolua_S,"csp_frac",tolua_get_player_type_csp_frac,tolua_set_player_type_csp_frac);
 tolua_variable(tolua_S,"max_plv",tolua_get_player_type_max_plv,tolua_set_player_type_max_plv);
 tolua_array(tolua_S,"stat_max",tolua_get_types_player_type_stat_max,tolua_set_types_player_type_stat_max);
 tolua_array(tolua_S,"stat_cur",tolua_get_types_player_type_stat_cur,tolua_set_types_player_type_stat_cur);
 tolua_variable(tolua_S,"fast",tolua_get_player_type_fast,tolua_set_player_type_fast);
 tolua_variable(tolua_S,"slow",tolua_get_player_type_slow,tolua_set_player_type_slow);
 tolua_variable(tolua_S,"blind",tolua_get_player_type_blind,tolua_set_player_type_blind);
 tolua_variable(tolua_S,"paralyzed",tolua_get_player_type_paralyzed,tolua_set_player_type_paralyzed);
 tolua_variable(tolua_S,"confused",tolua_get_player_type_confused,tolua_set_player_type_confused);
 tolua_variable(tolua_S,"afraid",tolua_get_player_type_afraid,tolua_set_player_type_afraid);
 tolua_variable(tolua_S,"image",tolua_get_player_type_image,tolua_set_player_type_image);
 tolua_variable(tolua_S,"poisoned",tolua_get_player_type_poisoned,tolua_set_player_type_poisoned);
 tolua_variable(tolua_S,"cut",tolua_get_player_type_cut,tolua_set_player_type_cut);
 tolua_variable(tolua_S,"stun",tolua_get_player_type_stun,tolua_set_player_type_stun);
 tolua_variable(tolua_S,"hero",tolua_get_player_type_hero,tolua_set_player_type_hero);
 tolua_variable(tolua_S,"shero",tolua_get_player_type_shero,tolua_set_player_type_shero);
 tolua_variable(tolua_S,"shield",tolua_get_player_type_shield,tolua_set_player_type_shield);
 tolua_variable(tolua_S,"shield_power",tolua_get_player_type_shield_power,tolua_set_player_type_shield_power);
 tolua_variable(tolua_S,"blessed",tolua_get_player_type_blessed,tolua_set_player_type_blessed);
 tolua_variable(tolua_S,"tim_invis",tolua_get_player_type_tim_invis,tolua_set_player_type_tim_invis);
 tolua_variable(tolua_S,"tim_infra",tolua_get_player_type_tim_infra,tolua_set_player_type_tim_infra);
 tolua_variable(tolua_S,"tim_esp",tolua_get_player_type_tim_esp,tolua_set_player_type_tim_esp);
 tolua_variable(tolua_S,"wraith_form",tolua_get_player_type_wraith_form,tolua_set_player_type_wraith_form);
 tolua_variable(tolua_S,"tim_ffall",tolua_get_player_type_tim_ffall,tolua_set_player_type_tim_ffall);
 tolua_variable(tolua_S,"tim_invisible",tolua_get_player_type_tim_invisible,tolua_set_player_type_tim_invisible);
 tolua_variable(tolua_S,"tim_inv_pow",tolua_get_player_type_tim_inv_pow,tolua_set_player_type_tim_inv_pow);
 tolua_variable(tolua_S,"muta1",tolua_get_player_type_muta1,tolua_set_player_type_muta1);
 tolua_variable(tolua_S,"muta2",tolua_get_player_type_muta2,tolua_set_player_type_muta2);
 tolua_variable(tolua_S,"muta3",tolua_get_player_type_muta3,tolua_set_player_type_muta3);
 tolua_variable(tolua_S,"recall_dungeon",tolua_get_player_type_recall_dungeon,tolua_set_player_type_recall_dungeon);
 tolua_variable(tolua_S,"word_recall",tolua_get_player_type_word_recall,tolua_set_player_type_word_recall);
 tolua_variable(tolua_S,"energy",tolua_get_player_type_energy,tolua_set_player_type_energy);
 tolua_variable(tolua_S,"food",tolua_get_player_type_food,tolua_set_player_type_food);
 tolua_variable(tolua_S,"confusing",tolua_get_player_type_confusing,tolua_set_player_type_confusing);
 tolua_variable(tolua_S,"searching",tolua_get_player_type_searching,tolua_set_player_type_searching);
 tolua_variable(tolua_S,"old_lite",tolua_get_player_type_old_lite,tolua_set_player_type_old_lite);
 tolua_variable(tolua_S,"old_view",tolua_get_player_type_old_view,tolua_set_player_type_old_view);
 tolua_variable(tolua_S,"old_food_aux",tolua_get_player_type_old_food_aux,tolua_set_player_type_old_food_aux);
 tolua_variable(tolua_S,"cur_lite",tolua_get_player_type_cur_lite,tolua_set_player_type_cur_lite);
 tolua_variable(tolua_S,"notice",tolua_get_player_type_notice,tolua_set_player_type_notice);
 tolua_variable(tolua_S,"update",tolua_get_player_type_update,tolua_set_player_type_update);
 tolua_variable(tolua_S,"redraw",tolua_get_player_type_redraw,tolua_set_player_type_redraw);
 tolua_variable(tolua_S,"window",tolua_get_player_type_window,tolua_set_player_type_window);
 tolua_array(tolua_S,"stat_use",tolua_get_types_player_type_stat_use,tolua_set_types_player_type_stat_use);
 tolua_array(tolua_S,"stat_top",tolua_get_types_player_type_stat_top,tolua_set_types_player_type_stat_top);
 tolua_array(tolua_S,"stat_add",tolua_get_types_player_type_stat_add,tolua_set_types_player_type_stat_add);
 tolua_array(tolua_S,"stat_ind",tolua_get_types_player_type_stat_ind,tolua_set_types_player_type_stat_ind);
 tolua_array(tolua_S,"stat_cnt",tolua_get_types_player_type_stat_cnt,tolua_set_types_player_type_stat_cnt);
 tolua_array(tolua_S,"stat_los",tolua_get_types_player_type_stat_los,tolua_set_types_player_type_stat_los);
 tolua_array(tolua_S,"stat_mut",tolua_get_types_player_type_stat_mut,tolua_set_types_player_type_stat_mut);
 tolua_variable(tolua_S,"resist_conf",tolua_get_player_type_resist_conf,tolua_set_player_type_resist_conf);
 tolua_variable(tolua_S,"resist_blind",tolua_get_player_type_resist_blind,tolua_set_player_type_resist_blind);
 tolua_variable(tolua_S,"resist_fear",tolua_get_player_type_resist_fear,tolua_set_player_type_resist_fear);
 tolua_variable(tolua_S,"reflect",tolua_get_player_type_reflect,tolua_set_player_type_reflect);
 tolua_variable(tolua_S,"sh_fire",tolua_get_player_type_sh_fire,tolua_set_player_type_sh_fire);
 tolua_variable(tolua_S,"sh_elec",tolua_get_player_type_sh_elec,tolua_set_player_type_sh_elec);
 tolua_variable(tolua_S,"sustain_str",tolua_get_player_type_sustain_str,tolua_set_player_type_sustain_str);
 tolua_variable(tolua_S,"sustain_int",tolua_get_player_type_sustain_int,tolua_set_player_type_sustain_int);
 tolua_variable(tolua_S,"sustain_wis",tolua_get_player_type_sustain_wis,tolua_set_player_type_sustain_wis);
 tolua_variable(tolua_S,"sustain_dex",tolua_get_player_type_sustain_dex,tolua_set_player_type_sustain_dex);
 tolua_variable(tolua_S,"sustain_con",tolua_get_player_type_sustain_con,tolua_set_player_type_sustain_con);
 tolua_variable(tolua_S,"sustain_chr",tolua_get_player_type_sustain_chr,tolua_set_player_type_sustain_chr);
 tolua_variable(tolua_S,"aggravate",tolua_get_player_type_aggravate,tolua_set_player_type_aggravate);
 tolua_variable(tolua_S,"teleport",tolua_get_player_type_teleport,tolua_set_player_type_teleport);
 tolua_variable(tolua_S,"exp_drain",tolua_get_player_type_exp_drain,tolua_set_player_type_exp_drain);
 tolua_variable(tolua_S,"climb",tolua_get_player_type_climb,tolua_set_player_type_climb);
 tolua_variable(tolua_S,"fly",tolua_get_player_type_fly,tolua_set_player_type_fly);
 tolua_variable(tolua_S,"ffall",tolua_get_player_type_ffall,tolua_set_player_type_ffall);
 tolua_variable(tolua_S,"lite",tolua_get_player_type_lite,tolua_set_player_type_lite);
 tolua_variable(tolua_S,"free_act",tolua_get_player_type_free_act,tolua_set_player_type_free_act);
 tolua_variable(tolua_S,"see_inv",tolua_get_player_type_see_inv,tolua_set_player_type_see_inv);
 tolua_variable(tolua_S,"regenerate",tolua_get_player_type_regenerate,tolua_set_player_type_regenerate);
 tolua_variable(tolua_S,"hold_life",tolua_get_player_type_hold_life,tolua_set_player_type_hold_life);
 tolua_variable(tolua_S,"telepathy",tolua_get_player_type_telepathy,tolua_set_player_type_telepathy);
 tolua_variable(tolua_S,"slow_digest",tolua_get_player_type_slow_digest,tolua_set_player_type_slow_digest);
 tolua_variable(tolua_S,"xtra_might",tolua_get_player_type_xtra_might,tolua_set_player_type_xtra_might);
 tolua_variable(tolua_S,"invis",tolua_get_player_type_invis,tolua_set_player_type_invis);
 tolua_variable(tolua_S,"dis_to_h",tolua_get_player_type_dis_to_h,tolua_set_player_type_dis_to_h);
 tolua_variable(tolua_S,"dis_to_d",tolua_get_player_type_dis_to_d,tolua_set_player_type_dis_to_d);
 tolua_variable(tolua_S,"dis_to_a",tolua_get_player_type_dis_to_a,tolua_set_player_type_dis_to_a);
 tolua_variable(tolua_S,"dis_ac",tolua_get_player_type_dis_ac,tolua_set_player_type_dis_ac);
 tolua_variable(tolua_S,"to_m",tolua_get_player_type_to_m,tolua_set_player_type_to_m);
 tolua_variable(tolua_S,"to_s",tolua_get_player_type_to_s,tolua_set_player_type_to_s);
 tolua_variable(tolua_S,"to_h",tolua_get_player_type_to_h,tolua_set_player_type_to_h);
 tolua_variable(tolua_S,"to_d",tolua_get_player_type_to_d,tolua_set_player_type_to_d);
 tolua_variable(tolua_S,"to_a",tolua_get_player_type_to_a,tolua_set_player_type_to_a);
 tolua_variable(tolua_S,"ac",tolua_get_player_type_ac,tolua_set_player_type_ac);
 tolua_variable(tolua_S,"see_infra",tolua_get_player_type_see_infra,tolua_set_player_type_see_infra);
 tolua_variable(tolua_S,"num_blow",tolua_get_player_type_num_blow,tolua_set_player_type_num_blow);
 tolua_variable(tolua_S,"num_fire",tolua_get_player_type_num_fire,tolua_set_player_type_num_fire);
 tolua_variable(tolua_S,"tval_xtra",tolua_get_player_type_tval_xtra,tolua_set_player_type_tval_xtra);
 tolua_variable(tolua_S,"tval_ammo",tolua_get_player_type_tval_ammo,tolua_set_player_type_tval_ammo);
 tolua_variable(tolua_S,"pspeed",tolua_get_player_type_pspeed,tolua_set_player_type_pspeed);
 tolua_variable(tolua_S,"pet_follow_distance",tolua_get_player_type_pet_follow_distance,tolua_set_player_type_pet_follow_distance);
 tolua_variable(tolua_S,"pet_open_doors",tolua_get_player_type_pet_open_doors,tolua_set_player_type_pet_open_doors);
 tolua_variable(tolua_S,"pet_pickup_items",tolua_get_player_type_pet_pickup_items,tolua_set_player_type_pet_pickup_items);
 tolua_variable(tolua_S,"body_monster",tolua_get_player_type_body_monster,tolua_set_player_type_body_monster);
 tolua_variable(tolua_S,"disembodied",tolua_get_player_type_disembodied,tolua_set_player_type_disembodied);
 tolua_array(tolua_S,"body_parts",tolua_get_types_player_type_body_parts,tolua_set_types_player_type_body_parts);
 tolua_variable(tolua_S,"leaving",tolua_get_player_type_leaving,tolua_set_player_type_leaving);
 tolua_variable(tolua_S,"ability_points",tolua_get_player_type_ability_points,tolua_set_player_type_ability_points);
 tolua_variable(tolua_S,"memorized",tolua_get_player_type_memorized,tolua_set_player_type_memorized);
 tolua_variable(tolua_S,"elemlord",tolua_get_player_type_elemlord,tolua_set_player_type_elemlord);
 tolua_variable(tolua_S,"statpoints",tolua_get_player_type_statpoints,tolua_set_player_type_statpoints);
 tolua_variable(tolua_S,"skillpoints",tolua_get_player_type_skillpoints,tolua_set_player_type_skillpoints);
 tolua_array(tolua_S,"skill_base",tolua_get_types_player_type_skill_base,tolua_set_types_player_type_skill_base);
 tolua_array(tolua_S,"skill_bonus",tolua_get_types_player_type_skill_bonus,tolua_set_types_player_type_skill_bonus);
 tolua_array(tolua_S,"skill",tolua_get_types_player_type_skill,tolua_set_types_player_type_skill);
 tolua_variable(tolua_S,"str_boost",tolua_get_player_type_str_boost,tolua_set_player_type_str_boost);
 tolua_variable(tolua_S,"str_boost_dur",tolua_get_player_type_str_boost_dur,tolua_set_player_type_str_boost_dur);
 tolua_variable(tolua_S,"int_boost",tolua_get_player_type_int_boost,tolua_set_player_type_int_boost);
 tolua_variable(tolua_S,"int_boost_dur",tolua_get_player_type_int_boost_dur,tolua_set_player_type_int_boost_dur);
 tolua_variable(tolua_S,"wis_boost",tolua_get_player_type_wis_boost,tolua_set_player_type_wis_boost);
 tolua_variable(tolua_S,"wis_boost_dur",tolua_get_player_type_wis_boost_dur,tolua_set_player_type_wis_boost_dur);
 tolua_variable(tolua_S,"dex_boost",tolua_get_player_type_dex_boost,tolua_set_player_type_dex_boost);
 tolua_variable(tolua_S,"dex_boost_dur",tolua_get_player_type_dex_boost_dur,tolua_set_player_type_dex_boost_dur);
 tolua_variable(tolua_S,"con_boost",tolua_get_player_type_con_boost,tolua_set_player_type_con_boost);
 tolua_variable(tolua_S,"con_boost_dur",tolua_get_player_type_con_boost_dur,tolua_set_player_type_con_boost_dur);
 tolua_variable(tolua_S,"chr_boost",tolua_get_player_type_chr_boost,tolua_set_player_type_chr_boost);
 tolua_variable(tolua_S,"chr_boost_dur",tolua_get_player_type_chr_boost_dur,tolua_set_player_type_chr_boost_dur);
 tolua_variable(tolua_S,"pres",tolua_get_player_type_pres,tolua_set_player_type_pres);
 tolua_variable(tolua_S,"pres_dur",tolua_get_player_type_pres_dur,tolua_set_player_type_pres_dur);
 tolua_variable(tolua_S,"mres",tolua_get_player_type_mres,tolua_set_player_type_mres);
 tolua_variable(tolua_S,"mres_dur",tolua_get_player_type_mres_dur,tolua_set_player_type_mres_dur);
 tolua_variable(tolua_S,"ac_boost",tolua_get_player_type_ac_boost,tolua_set_player_type_ac_boost);
 tolua_variable(tolua_S,"ac_boost_dur",tolua_get_player_type_ac_boost_dur,tolua_set_player_type_ac_boost_dur);
 tolua_variable(tolua_S,"elem_shield",tolua_get_player_type_elem_shield,tolua_set_player_type_elem_shield);
 tolua_variable(tolua_S,"elemental",tolua_get_player_type_elemental,tolua_set_player_type_elemental);
 tolua_variable(tolua_S,"elemental_effects",tolua_get_player_type_elemental_effects,tolua_set_player_type_elemental_effects);
 tolua_variable(tolua_S,"alteration",tolua_get_player_type_alteration,tolua_set_player_type_alteration);
 tolua_variable(tolua_S,"alteration_effects",tolua_get_player_type_alteration_effects,tolua_set_player_type_alteration_effects);
 tolua_variable(tolua_S,"healing",tolua_get_player_type_healing,tolua_set_player_type_healing);
 tolua_variable(tolua_S,"healing_effects",tolua_get_player_type_healing_effects,tolua_set_player_type_healing_effects);
 tolua_variable(tolua_S,"conjuration",tolua_get_player_type_conjuration,tolua_set_player_type_conjuration);
 tolua_variable(tolua_S,"conjuration_effects",tolua_get_player_type_conjuration_effects,tolua_set_player_type_conjuration_effects);
 tolua_variable(tolua_S,"divination",tolua_get_player_type_divination,tolua_set_player_type_divination);
 tolua_variable(tolua_S,"divination_effects",tolua_get_player_type_divination_effects,tolua_set_player_type_divination_effects);
 tolua_array(tolua_S,"class_level",tolua_get_types_player_type_class_level,tolua_set_types_player_type_class_level);
 tolua_array(tolua_S,"class_kills",tolua_get_types_player_type_class_kills,tolua_set_types_player_type_class_kills);
 tolua_array(tolua_S,"abilities",tolua_get_types_player_type_abilities,tolua_set_types_player_type_abilities);
 tolua_variable(tolua_S,"num_abilities",tolua_get_player_type_num_abilities,tolua_set_player_type_num_abilities);
 tolua_array(tolua_S,"abilities_powers",tolua_get_types_player_type_abilities_powers,tolua_set_types_player_type_abilities_powers);
 tolua_array(tolua_S,"abilities_monster_attacks",tolua_get_types_player_type_abilities_monster_attacks,tolua_set_types_player_type_abilities_monster_attacks);
 tolua_array(tolua_S,"abilities_monster_spells",tolua_get_types_player_type_abilities_monster_spells,tolua_set_types_player_type_abilities_monster_spells);
 tolua_variable(tolua_S,"boss_abilities",tolua_get_player_type_boss_abilities,tolua_set_player_type_boss_abilities);
 tolua_variable(tolua_S,"magic_mode",tolua_get_player_type_magic_mode,tolua_set_player_type_magic_mode);
 tolua_variable(tolua_S,"auraon",tolua_get_player_type_auraon,tolua_set_player_type_auraon);
 tolua_variable(tolua_S,"deathcount",tolua_get_player_type_deathcount,tolua_set_player_type_deathcount);
 tolua_variable(tolua_S,"guardconfuse",tolua_get_player_type_guardconfuse,tolua_set_player_type_guardconfuse);
 tolua_variable(tolua_S,"learning",tolua_get_player_type_learning,tolua_set_player_type_learning);
 tolua_variable(tolua_S,"startx",tolua_get_player_type_startx,tolua_set_player_type_startx);
 tolua_variable(tolua_S,"starty",tolua_get_player_type_starty,tolua_set_player_type_starty);
 tolua_array(tolua_S,"events",tolua_get_types_player_type_events,tolua_set_types_player_type_events);
 tolua_array(tolua_S,"resistances",tolua_get_types_player_type_resistances,tolua_set_types_player_type_resistances);
 tolua_variable(tolua_S,"cur_wid",tolua_get_player_type_cur_wid,tolua_set_player_type_cur_wid);
 tolua_variable(tolua_S,"cur_hgt",tolua_get_player_type_cur_hgt,tolua_set_player_type_cur_hgt);
 tolua_variable(tolua_S,"wild_startx",tolua_get_player_type_wild_startx,tolua_set_player_type_wild_startx);
 tolua_variable(tolua_S,"wild_starty",tolua_get_player_type_wild_starty,tolua_set_player_type_wild_starty);
 tolua_variable(tolua_S,"questx",tolua_get_player_type_questx,tolua_set_player_type_questx);
 tolua_variable(tolua_S,"questy",tolua_get_player_type_questy,tolua_set_player_type_questy);
 tolua_variable(tolua_S,"powerattack",tolua_get_player_type_powerattack,tolua_set_player_type_powerattack);
 tolua_variable(tolua_S,"powerlevel",tolua_get_player_type_powerlevel,tolua_set_player_type_powerlevel);
 tolua_variable(tolua_S,"num_blow2",tolua_get_player_type_num_blow2,tolua_set_player_type_num_blow2);
 tolua_variable(tolua_S,"num_fire2",tolua_get_player_type_num_fire2,tolua_set_player_type_num_fire2);
 tolua_variable(tolua_S,"dualwield",tolua_get_player_type_dualwield,tolua_set_player_type_dualwield);
 tolua_variable(tolua_S,"alignment",tolua_get_player_type_alignment,tolua_set_player_type_alignment);
 tolua_variable(tolua_S,"cursed",tolua_get_player_type_cursed,tolua_set_player_type_cursed);
 tolua_array(tolua_S,"towns",tolua_get_types_player_type_towns,tolua_set_types_player_type_towns);
 tolua_endmodule(tolua_S);
 tolua_cclass(tolua_S,"border_type","border_type","",NULL);
 tolua_beginmodule(tolua_S,"border_type");
 tolua_array(tolua_S,"north",tolua_get_types_border_type_north,tolua_set_types_border_type_north);
 tolua_array(tolua_S,"south",tolua_get_types_border_type_south,tolua_set_types_border_type_south);
 tolua_array(tolua_S,"east",tolua_get_types_border_type_east,tolua_set_types_border_type_east);
 tolua_array(tolua_S,"west",tolua_get_types_border_type_west,tolua_set_types_border_type_west);
 tolua_variable(tolua_S,"north_west",tolua_get_border_type_north_west,tolua_set_border_type_north_west);
 tolua_variable(tolua_S,"north_east",tolua_get_border_type_north_east,tolua_set_border_type_north_east);
 tolua_variable(tolua_S,"south_west",tolua_get_border_type_south_west,tolua_set_border_type_south_west);
 tolua_variable(tolua_S,"south_east",tolua_get_border_type_south_east,tolua_set_border_type_south_east);
 tolua_endmodule(tolua_S);
 tolua_cclass(tolua_S,"dungeon_info_type","dungeon_info_type","",NULL);
 tolua_beginmodule(tolua_S,"dungeon_info_type");
 tolua_variable(tolua_S,"name",tolua_get_dungeon_info_type_name,tolua_set_dungeon_info_type_name);
 tolua_variable(tolua_S,"text",tolua_get_dungeon_info_type_text,tolua_set_dungeon_info_type_text);
 tolua_variable(tolua_S,"floor1",tolua_get_dungeon_info_type_floor1,tolua_set_dungeon_info_type_floor1);
 tolua_variable(tolua_S,"floor_percent1",tolua_get_dungeon_info_type_floor_percent1,tolua_set_dungeon_info_type_floor_percent1);
 tolua_variable(tolua_S,"floor2",tolua_get_dungeon_info_type_floor2,tolua_set_dungeon_info_type_floor2);
 tolua_variable(tolua_S,"floor_percent2",tolua_get_dungeon_info_type_floor_percent2,tolua_set_dungeon_info_type_floor_percent2);
 tolua_variable(tolua_S,"floor3",tolua_get_dungeon_info_type_floor3,tolua_set_dungeon_info_type_floor3);
 tolua_variable(tolua_S,"floor_percent3",tolua_get_dungeon_info_type_floor_percent3,tolua_set_dungeon_info_type_floor_percent3);
 tolua_variable(tolua_S,"outer_wall",tolua_get_dungeon_info_type_outer_wall,tolua_set_dungeon_info_type_outer_wall);
 tolua_variable(tolua_S,"inner_wall",tolua_get_dungeon_info_type_inner_wall,tolua_set_dungeon_info_type_inner_wall);
 tolua_variable(tolua_S,"fill_type1",tolua_get_dungeon_info_type_fill_type1,tolua_set_dungeon_info_type_fill_type1);
 tolua_variable(tolua_S,"fill_percent1",tolua_get_dungeon_info_type_fill_percent1,tolua_set_dungeon_info_type_fill_percent1);
 tolua_variable(tolua_S,"fill_type2",tolua_get_dungeon_info_type_fill_type2,tolua_set_dungeon_info_type_fill_type2);
 tolua_variable(tolua_S,"fill_percent2",tolua_get_dungeon_info_type_fill_percent2,tolua_set_dungeon_info_type_fill_percent2);
 tolua_variable(tolua_S,"fill_type3",tolua_get_dungeon_info_type_fill_type3,tolua_set_dungeon_info_type_fill_type3);
 tolua_variable(tolua_S,"fill_percent3",tolua_get_dungeon_info_type_fill_percent3,tolua_set_dungeon_info_type_fill_percent3);
 tolua_variable(tolua_S,"mindepth",tolua_get_dungeon_info_type_mindepth,tolua_set_dungeon_info_type_mindepth);
 tolua_variable(tolua_S,"maxdepth",tolua_get_dungeon_info_type_maxdepth,tolua_set_dungeon_info_type_maxdepth);
 tolua_variable(tolua_S,"principal",tolua_get_dungeon_info_type_principal,tolua_set_dungeon_info_type_principal);
 tolua_variable(tolua_S,"next",tolua_get_dungeon_info_type_next,tolua_set_dungeon_info_type_next);
 tolua_variable(tolua_S,"min_plev",tolua_get_dungeon_info_type_min_plev,tolua_set_dungeon_info_type_min_plev);
 tolua_variable(tolua_S,"mode",tolua_get_dungeon_info_type_mode,tolua_set_dungeon_info_type_mode);
 tolua_variable(tolua_S,"min_m_alloc_level",tolua_get_dungeon_info_type_min_m_alloc_level,tolua_set_dungeon_info_type_min_m_alloc_level);
 tolua_variable(tolua_S,"max_m_alloc_chance",tolua_get_dungeon_info_type_max_m_alloc_chance,tolua_set_dungeon_info_type_max_m_alloc_chance);
 tolua_variable(tolua_S,"flags1",tolua_get_dungeon_info_type_flags1,tolua_set_dungeon_info_type_flags1);
 tolua_variable(tolua_S,"mflags1",tolua_get_dungeon_info_type_mflags1,tolua_set_dungeon_info_type_mflags1);
 tolua_variable(tolua_S,"mflags2",tolua_get_dungeon_info_type_mflags2,tolua_set_dungeon_info_type_mflags2);
 tolua_variable(tolua_S,"mflags3",tolua_get_dungeon_info_type_mflags3,tolua_set_dungeon_info_type_mflags3);
 tolua_variable(tolua_S,"mflags4",tolua_get_dungeon_info_type_mflags4,tolua_set_dungeon_info_type_mflags4);
 tolua_variable(tolua_S,"mflags5",tolua_get_dungeon_info_type_mflags5,tolua_set_dungeon_info_type_mflags5);
 tolua_variable(tolua_S,"mflags6",tolua_get_dungeon_info_type_mflags6,tolua_set_dungeon_info_type_mflags6);
 tolua_variable(tolua_S,"mflags7",tolua_get_dungeon_info_type_mflags7,tolua_set_dungeon_info_type_mflags7);
 tolua_variable(tolua_S,"mflags8",tolua_get_dungeon_info_type_mflags8,tolua_set_dungeon_info_type_mflags8);
 tolua_variable(tolua_S,"mflags9",tolua_get_dungeon_info_type_mflags9,tolua_set_dungeon_info_type_mflags9);
 tolua_variable(tolua_S,"r_char",tolua_get_dungeon_info_type_r_char,tolua_set_dungeon_info_type_r_char);
 tolua_variable(tolua_S,"final_artifact",tolua_get_dungeon_info_type_final_artifact,tolua_set_dungeon_info_type_final_artifact);
 tolua_variable(tolua_S,"final_guardian",tolua_get_dungeon_info_type_final_guardian,tolua_set_dungeon_info_type_final_guardian);
 tolua_variable(tolua_S,"special_percent",tolua_get_dungeon_info_type_special_percent,tolua_set_dungeon_info_type_special_percent);
 tolua_variable(tolua_S,"quest",tolua_get_dungeon_info_type_quest,tolua_set_dungeon_info_type_quest);
 tolua_endmodule(tolua_S);
 tolua_cclass(tolua_S,"monster_magics","monster_magics","",NULL);
 tolua_beginmodule(tolua_S,"monster_magics");
 tolua_variable(tolua_S,"name",tolua_get_monster_magics_name,tolua_set_monster_magics_name);
 tolua_variable(tolua_S,"act",tolua_get_monster_magics_act,tolua_set_monster_magics_act);
 tolua_variable(tolua_S,"type",tolua_get_monster_magics_type,tolua_set_monster_magics_type);
 tolua_variable(tolua_S,"power",tolua_get_monster_magics_power,tolua_set_monster_magics_power);
 tolua_variable(tolua_S,"special1",tolua_get_monster_magics_special1,tolua_set_monster_magics_special1);
 tolua_variable(tolua_S,"special2",tolua_get_monster_magics_special2,tolua_set_monster_magics_special2);
 tolua_variable(tolua_S,"special3",tolua_get_monster_magics_special3,tolua_set_monster_magics_special3);
 tolua_variable(tolua_S,"summchar",tolua_get_monster_magics_summchar,tolua_set_monster_magics_summchar);
 tolua_variable(tolua_S,"cost",tolua_get_monster_magics_cost,tolua_set_monster_magics_cost);
 tolua_endmodule(tolua_S);
 tolua_cclass(tolua_S,"dialog_answers","dialog_answers","",NULL);
 tolua_beginmodule(tolua_S,"dialog_answers");
 tolua_variable(tolua_S,"name",tolua_get_dialog_answers_name,tolua_set_dialog_answers_name);
 tolua_variable(tolua_S,"ctype",tolua_get_dialog_answers_ctype,tolua_set_dialog_answers_ctype);
 tolua_variable(tolua_S,"cparam1",tolua_get_dialog_answers_cparam1,tolua_set_dialog_answers_cparam1);
 tolua_variable(tolua_S,"cparam2",tolua_get_dialog_answers_cparam2,tolua_set_dialog_answers_cparam2);
 tolua_variable(tolua_S,"effect",tolua_get_dialog_answers_effect,tolua_set_dialog_answers_effect);
 tolua_variable(tolua_S,"eparam1",tolua_get_dialog_answers_eparam1,tolua_set_dialog_answers_eparam1);
 tolua_variable(tolua_S,"eparam2",tolua_get_dialog_answers_eparam2,tolua_set_dialog_answers_eparam2);
 tolua_variable(tolua_S,"eparam3",tolua_get_dialog_answers_eparam3,tolua_set_dialog_answers_eparam3);
 tolua_variable(tolua_S,"eparam4",tolua_get_dialog_answers_eparam4,tolua_set_dialog_answers_eparam4);
 tolua_variable(tolua_S,"eparam5",tolua_get_dialog_answers_eparam5,tolua_set_dialog_answers_eparam5);
 tolua_variable(tolua_S,"valid",tolua_get_dialog_answers_valid,tolua_set_dialog_answers_valid);
 tolua_endmodule(tolua_S);
 tolua_cclass(tolua_S,"wild_info","wild_info","",NULL);
 tolua_beginmodule(tolua_S,"wild_info");
 tolua_variable(tolua_S,"town",tolua_get_wild_info_town,tolua_set_wild_info_town);
 tolua_variable(tolua_S,"feat",tolua_get_wild_info_feat,tolua_set_wild_info_feat);
 tolua_variable(tolua_S,"revive",tolua_get_wild_info_revive,tolua_set_wild_info_revive);
 tolua_endmodule(tolua_S);
 tolua_cclass(tolua_S,"class_def","class_def","",NULL);
 tolua_beginmodule(tolua_S,"class_def");
 tolua_variable(tolua_S,"name",tolua_get_class_def_name,tolua_set_class_def_name);
 tolua_variable(tolua_S,"ranksm",tolua_get_class_def_ranksm,tolua_set_class_def_ranksm);
 tolua_variable(tolua_S,"ranksf",tolua_get_class_def_ranksf,tolua_set_class_def_ranksf);
 tolua_variable(tolua_S,"advanced",tolua_get_class_def_advanced,tolua_set_class_def_advanced);
 tolua_variable(tolua_S,"req_str",tolua_get_class_def_req_str,tolua_set_class_def_req_str);
 tolua_variable(tolua_S,"req_int",tolua_get_class_def_req_int,tolua_set_class_def_req_int);
 tolua_variable(tolua_S,"req_wis",tolua_get_class_def_req_wis,tolua_set_class_def_req_wis);
 tolua_variable(tolua_S,"req_dex",tolua_get_class_def_req_dex,tolua_set_class_def_req_dex);
 tolua_variable(tolua_S,"req_con",tolua_get_class_def_req_con,tolua_set_class_def_req_con);
 tolua_variable(tolua_S,"req_chr",tolua_get_class_def_req_chr,tolua_set_class_def_req_chr);
 tolua_variable(tolua_S,"str_bonus",tolua_get_class_def_str_bonus,tolua_set_class_def_str_bonus);
 tolua_variable(tolua_S,"int_bonus",tolua_get_class_def_int_bonus,tolua_set_class_def_int_bonus);
 tolua_variable(tolua_S,"wis_bonus",tolua_get_class_def_wis_bonus,tolua_set_class_def_wis_bonus);
 tolua_variable(tolua_S,"dex_bonus",tolua_get_class_def_dex_bonus,tolua_set_class_def_dex_bonus);
 tolua_variable(tolua_S,"con_bonus",tolua_get_class_def_con_bonus,tolua_set_class_def_con_bonus);
 tolua_variable(tolua_S,"chr_bonus",tolua_get_class_def_chr_bonus,tolua_set_class_def_chr_bonus);
 tolua_array(tolua_S,"req_skills",tolua_get_types_class_def_req_skills,tolua_set_types_class_def_req_skills);
 tolua_array(tolua_S,"req_classes",tolua_get_types_class_def_req_classes,tolua_set_types_class_def_req_classes);
 tolua_array(tolua_S,"skills_bonus",tolua_get_types_class_def_skills_bonus,tolua_set_types_class_def_skills_bonus);
 tolua_endmodule(tolua_S);
 tolua_cclass(tolua_S,"ability_def","ability_def","",NULL);
 tolua_beginmodule(tolua_S,"ability_def");
 tolua_variable(tolua_S,"name",tolua_get_ability_def_name,tolua_set_ability_def_name);
 tolua_variable(tolua_S,"abtype",tolua_get_ability_def_abtype,tolua_set_ability_def_abtype);
 tolua_variable(tolua_S,"hardcode",tolua_get_ability_def_hardcode,tolua_set_ability_def_hardcode);
 tolua_variable(tolua_S,"powerid",tolua_get_ability_def_powerid,tolua_set_ability_def_powerid);
 tolua_variable(tolua_S,"combatfeat",tolua_get_ability_def_combatfeat,tolua_set_ability_def_combatfeat);
 tolua_variable(tolua_S,"skill",tolua_get_ability_def_skill,tolua_set_ability_def_skill);
 tolua_variable(tolua_S,"reqskill",tolua_get_ability_def_reqskill,tolua_set_ability_def_reqskill);
 tolua_endmodule(tolua_S);
 tolua_cclass(tolua_S,"resist_def","resist_def","",NULL);
 tolua_beginmodule(tolua_S,"resist_def");
 tolua_variable(tolua_S,"name",tolua_get_resist_def_name,tolua_set_resist_def_name);
 tolua_variable(tolua_S,"element",tolua_get_resist_def_element,tolua_set_resist_def_element);
 tolua_variable(tolua_S,"magicitem",tolua_get_resist_def_magicitem,tolua_set_resist_def_magicitem);
 tolua_endmodule(tolua_S);
 tolua_cclass(tolua_S,"vault_def","vault_def","",NULL);
 tolua_beginmodule(tolua_S,"vault_def");
 tolua_variable(tolua_S,"created",tolua_get_vault_def_created,tolua_set_vault_def_created);
 tolua_variable(tolua_S,"num",tolua_get_vault_def_num,tolua_set_vault_def_num);
 tolua_variable(tolua_S,"width",tolua_get_vault_def_width,tolua_set_vault_def_width);
 tolua_variable(tolua_S,"height",tolua_get_vault_def_height,tolua_set_vault_def_height);
 tolua_variable(tolua_S,"mindlv",tolua_get_vault_def_mindlv,tolua_set_vault_def_mindlv);
 tolua_variable(tolua_S,"maxdlv",tolua_get_vault_def_maxdlv,tolua_set_vault_def_maxdlv);
 tolua_variable(tolua_S,"teleport",tolua_get_vault_def_teleport,tolua_set_vault_def_teleport);
 tolua_variable(tolua_S,"type",tolua_get_vault_def_type,tolua_set_vault_def_type);
 tolua_variable(tolua_S,"rarity",tolua_get_vault_def_rarity,tolua_set_vault_def_rarity);
 tolua_endmodule(tolua_S);
 tolua_endmodule(tolua_S);
 return 1;
}
