/*
** Lua binding: init
** Generated automatically by tolua 5.0a on 05/24/09 20:03:28.
*/

#ifndef __cplusplus
#include "stdlib.h"
#endif
#include "string.h"

#include "lua/tolua.h"

/* Exported function */
TOLUA_API int tolua_init_open (lua_State* tolua_S);

#include "angband.h"

/* function to release collected object via destructor */
#ifdef __cplusplus

static int tolua_collect_ability_def (lua_State* tolua_S)
{
 ability_def* self = (ability_def*) tolua_tousertype(tolua_S,1,0);
 delete self;
 return 0;
}

static int tolua_collect_sint (lua_State* tolua_S)
{
 sint* self = (sint*) tolua_tousertype(tolua_S,1,0);
 delete self;
 return 0;
}

static int tolua_collect_music_songs (lua_State* tolua_S)
{
 music_songs* self = (music_songs*) tolua_tousertype(tolua_S,1,0);
 delete self;
 return 0;
}

static int tolua_collect_class_def (lua_State* tolua_S)
{
 class_def* self = (class_def*) tolua_tousertype(tolua_S,1,0);
 delete self;
 return 0;
}

static int tolua_collect_vault_def (lua_State* tolua_S)
{
 vault_def* self = (vault_def*) tolua_tousertype(tolua_S,1,0);
 delete self;
 return 0;
}

static int tolua_collect_magic_spells (lua_State* tolua_S)
{
 magic_spells* self = (magic_spells*) tolua_tousertype(tolua_S,1,0);
 delete self;
 return 0;
}

static int tolua_collect_monster_magics (lua_State* tolua_S)
{
 monster_magics* self = (monster_magics*) tolua_tousertype(tolua_S,1,0);
 delete self;
 return 0;
}

static int tolua_collect_store_type (lua_State* tolua_S)
{
 store_type* self = (store_type*) tolua_tousertype(tolua_S,1,0);
 delete self;
 return 0;
}
#endif


/* function to register type */
static void tolua_reg_types (lua_State* tolua_S)
{
 tolua_usertype(tolua_S,"ability_def");
 tolua_usertype(tolua_S,"monster_type");
 tolua_usertype(tolua_S,"feature_type");
 tolua_usertype(tolua_S,"ego_item_type");
 tolua_usertype(tolua_S,"cave_type");
 tolua_usertype(tolua_S,"class_def");
 tolua_usertype(tolua_S,"artifact_type");
 tolua_usertype(tolua_S,"vault_def");
 tolua_usertype(tolua_S,"term");
 tolua_usertype(tolua_S,"FILE");
 tolua_usertype(tolua_S,"alloc_entry");
 tolua_usertype(tolua_S,"monster_magics");
 tolua_usertype(tolua_S,"store_type");
 tolua_usertype(tolua_S,"music_songs");
 tolua_usertype(tolua_S,"player_type");
 tolua_usertype(tolua_S,"sint");
 tolua_usertype(tolua_S,"trap_type");
 tolua_usertype(tolua_S,"monster_race");
 tolua_usertype(tolua_S,"header");
 tolua_usertype(tolua_S,"object_kind");
 tolua_usertype(tolua_S,"magic_spells");
 tolua_usertype(tolua_S,"dungeon_info_type");
 tolua_usertype(tolua_S,"vault_type");
 tolua_usertype(tolua_S,"object_type");
}

/* function: notice_stuff */
static int tolua_init_notice_stuff00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnoobj(tolua_S,1,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
 {
  notice_stuff();
 }
 }
 return 0;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'notice_stuff'.",&tolua_err);
 return 0;
#endif
}

/* function: update_stuff */
static int tolua_init_update_stuff00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnoobj(tolua_S,1,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
 {
  update_stuff();
 }
 }
 return 0;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'update_stuff'.",&tolua_err);
 return 0;
#endif
}

/* function: redraw_stuff */
static int tolua_init_redraw_stuff00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnoobj(tolua_S,1,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
 {
  redraw_stuff();
 }
 }
 return 0;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'redraw_stuff'.",&tolua_err);
 return 0;
#endif
}

/* function: window_stuff */
static int tolua_init_window_stuff00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnoobj(tolua_S,1,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
 {
  window_stuff();
 }
 }
 return 0;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'window_stuff'.",&tolua_err);
 return 0;
#endif
}

/* function: handle_stuff */
static int tolua_init_handle_stuff00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnoobj(tolua_S,1,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
 {
  handle_stuff();
 }
 }
 return 0;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'handle_stuff'.",&tolua_err);
 return 0;
#endif
}

/* function: update_and_handle */
static int tolua_init_update_and_handle00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnoobj(tolua_S,1,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
 {
  update_and_handle();
 }
 }
 return 0;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'update_and_handle'.",&tolua_err);
 return 0;
#endif
}

/* function: inkey */
static int tolua_init_inkey00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnoobj(tolua_S,1,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
 {
  char tolua_ret = (char)  inkey();
 tolua_pushnumber(tolua_S,(long)tolua_ret);
 }
 }
 return 1;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'inkey'.",&tolua_err);
 return 0;
#endif
}

/* function: bell */
static int tolua_init_bell00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnoobj(tolua_S,1,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
 {
  bell();
 }
 }
 return 0;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'bell'.",&tolua_err);
 return 0;
#endif
}

/* function: sound */
static int tolua_init_sound00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnumber(tolua_S,1,0,&tolua_err) ||
 !tolua_isnoobj(tolua_S,2,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
  int val = ((int)  tolua_tonumber(tolua_S,1,0));
 {
  sound(val);
 }
 }
 return 0;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'sound'.",&tolua_err);
 return 0;
#endif
}

/* function: msg_print */
static int tolua_init_msg_print00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isstring(tolua_S,1,0,&tolua_err) ||
 !tolua_isnoobj(tolua_S,2,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
  cptr msg = ((cptr)  tolua_tostring(tolua_S,1,0));
 {
  msg_print(msg);
 }
 }
 return 0;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'msg_print'.",&tolua_err);
 return 0;
#endif
}

/* function: screen_save */
static int tolua_init_screen_save00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnoobj(tolua_S,1,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
 {
  screen_save();
 }
 }
 return 0;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'screen_save'.",&tolua_err);
 return 0;
#endif
}

/* function: screen_load */
static int tolua_init_screen_load00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnoobj(tolua_S,1,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
 {
  screen_load();
 }
 }
 return 0;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'screen_load'.",&tolua_err);
 return 0;
#endif
}

/* function: c_put_str */
static int tolua_init_c_put_str00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnumber(tolua_S,1,0,&tolua_err) ||
 !tolua_isstring(tolua_S,2,0,&tolua_err) ||
 !tolua_isnumber(tolua_S,3,0,&tolua_err) ||
 !tolua_isnumber(tolua_S,4,0,&tolua_err) ||
 !tolua_isnoobj(tolua_S,5,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
  byte attr = ((byte)  tolua_tonumber(tolua_S,1,0));
  cptr str = ((cptr)  tolua_tostring(tolua_S,2,0));
  int row = ((int)  tolua_tonumber(tolua_S,3,0));
  int col = ((int)  tolua_tonumber(tolua_S,4,0));
 {
  c_put_str(attr,str,row,col);
 }
 }
 return 0;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'c_put_str'.",&tolua_err);
 return 0;
#endif
}

/* function: put_str */
static int tolua_init_put_str00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isstring(tolua_S,1,0,&tolua_err) ||
 !tolua_isnumber(tolua_S,2,0,&tolua_err) ||
 !tolua_isnumber(tolua_S,3,0,&tolua_err) ||
 !tolua_isnoobj(tolua_S,4,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
  cptr str = ((cptr)  tolua_tostring(tolua_S,1,0));
  int row = ((int)  tolua_tonumber(tolua_S,2,0));
  int col = ((int)  tolua_tonumber(tolua_S,3,0));
 {
  put_str(str,row,col);
 }
 }
 return 0;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'put_str'.",&tolua_err);
 return 0;
#endif
}

/* function: c_prt */
static int tolua_init_c_prt00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnumber(tolua_S,1,0,&tolua_err) ||
 !tolua_isstring(tolua_S,2,0,&tolua_err) ||
 !tolua_isnumber(tolua_S,3,0,&tolua_err) ||
 !tolua_isnumber(tolua_S,4,0,&tolua_err) ||
 !tolua_isnoobj(tolua_S,5,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
  byte attr = ((byte)  tolua_tonumber(tolua_S,1,0));
  cptr str = ((cptr)  tolua_tostring(tolua_S,2,0));
  int row = ((int)  tolua_tonumber(tolua_S,3,0));
  int col = ((int)  tolua_tonumber(tolua_S,4,0));
 {
  c_prt(attr,str,row,col);
 }
 }
 return 0;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'c_prt'.",&tolua_err);
 return 0;
#endif
}

/* function: prt */
static int tolua_init_prt00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isstring(tolua_S,1,0,&tolua_err) ||
 !tolua_isnumber(tolua_S,2,0,&tolua_err) ||
 !tolua_isnumber(tolua_S,3,0,&tolua_err) ||
 !tolua_isnoobj(tolua_S,4,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
  cptr str = ((cptr)  tolua_tostring(tolua_S,1,0));
  int row = ((int)  tolua_tonumber(tolua_S,2,0));
  int col = ((int)  tolua_tonumber(tolua_S,3,0));
 {
  prt(str,row,col);
 }
 }
 return 0;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'prt'.",&tolua_err);
 return 0;
#endif
}

/* function: clear_from */
static int tolua_init_clear_from00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnumber(tolua_S,1,0,&tolua_err) ||
 !tolua_isnoobj(tolua_S,2,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
  int row = ((int)  tolua_tonumber(tolua_S,1,0));
 {
  clear_from(row);
 }
 }
 return 0;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'clear_from'.",&tolua_err);
 return 0;
#endif
}

/* function: pause_line */
static int tolua_init_pause_line00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnumber(tolua_S,1,0,&tolua_err) ||
 !tolua_isnoobj(tolua_S,2,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
  int row = ((int)  tolua_tonumber(tolua_S,1,0));
 {
  pause_line(row);
 }
 }
 return 0;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'pause_line'.",&tolua_err);
 return 0;
#endif
}

/* function: request_command */
static int tolua_init_request_command00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isboolean(tolua_S,1,0,&tolua_err) ||
 !tolua_isnoobj(tolua_S,2,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
  bool shopping = ((bool)  tolua_toboolean(tolua_S,1,0));
 {
  request_command(shopping);
 }
 }
 return 0;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'request_command'.",&tolua_err);
 return 0;
#endif
}

/* function: askfor_aux */
static int tolua_init_askfor_aux00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isstring(tolua_S,1,0,&tolua_err) ||
 !tolua_isnumber(tolua_S,2,0,&tolua_err) ||
 !tolua_isnoobj(tolua_S,3,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
  char* buf = ((char*)  tolua_tostring(tolua_S,1,0));
  int len = ((int)  tolua_tonumber(tolua_S,2,0));
 {
  bool tolua_ret = (bool)  askfor_aux(buf,len);
 tolua_pushboolean(tolua_S,(bool)tolua_ret);
 }
 }
 return 1;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'askfor_aux'.",&tolua_err);
 return 0;
#endif
}

/* function: get_string */
static int tolua_init_get_string00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isstring(tolua_S,1,0,&tolua_err) ||
 !tolua_isstring(tolua_S,2,0,&tolua_err) ||
 !tolua_isnumber(tolua_S,3,0,&tolua_err) ||
 !tolua_isnoobj(tolua_S,4,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
  cptr prompt = ((cptr)  tolua_tostring(tolua_S,1,0));
  char* buf = ((char*)  tolua_tostring(tolua_S,2,0));
  int len = ((int)  tolua_tonumber(tolua_S,3,0));
 {
  bool tolua_ret = (bool)  get_string(prompt,buf,len);
 tolua_pushboolean(tolua_S,(bool)tolua_ret);
 }
 }
 return 1;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'get_string'.",&tolua_err);
 return 0;
#endif
}

/* function: get_check */
static int tolua_init_get_check00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isstring(tolua_S,1,0,&tolua_err) ||
 !tolua_isnoobj(tolua_S,2,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
  cptr prompt = ((cptr)  tolua_tostring(tolua_S,1,0));
 {
  bool tolua_ret = (bool)  get_check(prompt);
 tolua_pushboolean(tolua_S,(bool)tolua_ret);
 }
 }
 return 1;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'get_check'.",&tolua_err);
 return 0;
#endif
}

/* function: get_com */
static int tolua_init_get_com00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isstring(tolua_S,1,0,&tolua_err) ||
 !tolua_isstring(tolua_S,2,0,&tolua_err) ||
 !tolua_isnoobj(tolua_S,3,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
  cptr prompt = ((cptr)  tolua_tostring(tolua_S,1,0));
  char* command = ((char*)  tolua_tostring(tolua_S,2,0));
 {
  bool tolua_ret = (bool)  get_com(prompt,command);
 tolua_pushboolean(tolua_S,(bool)tolua_ret);
 }
 }
 return 1;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'get_com'.",&tolua_err);
 return 0;
#endif
}

/* function: get_quantity */
static int tolua_init_get_quantity00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isstring(tolua_S,1,0,&tolua_err) ||
 !tolua_isnumber(tolua_S,2,0,&tolua_err) ||
 !tolua_isnoobj(tolua_S,3,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
  cptr prompt = ((cptr)  tolua_tostring(tolua_S,1,0));
  int max = ((int)  tolua_tonumber(tolua_S,2,0));
 {
  s16b tolua_ret = (s16b)  get_quantity(prompt,max);
 tolua_pushnumber(tolua_S,(long)tolua_ret);
 }
 }
 return 1;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'get_quantity'.",&tolua_err);
 return 0;
#endif
}

/* function: Term_clear */
static int tolua_init_Term_clear00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnoobj(tolua_S,1,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
 {
  errr tolua_ret = (errr)  Term_clear();
 tolua_pushnumber(tolua_S,(long)tolua_ret);
 }
 }
 return 1;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'Term_clear'.",&tolua_err);
 return 0;
#endif
}

/* function: flush */
static int tolua_init_flush00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnoobj(tolua_S,1,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
 {
  flush();
 }
 }
 return 0;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'flush'.",&tolua_err);
 return 0;
#endif
}

/* function: process_dialog */
static int tolua_init_process_dialog00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnumber(tolua_S,1,0,&tolua_err) ||
 !tolua_isusertype(tolua_S,2,"FILE",0,&tolua_err) ||
 !tolua_isnoobj(tolua_S,3,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
  int dnum = ((int)  tolua_tonumber(tolua_S,1,0));
  FILE* fp = ((FILE*)  tolua_tousertype(tolua_S,2,0));
 {
  int tolua_ret = (int)  process_dialog(dnum,fp);
 tolua_pushnumber(tolua_S,(long)tolua_ret);
 }
 }
 return 1;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'process_dialog'.",&tolua_err);
 return 0;
#endif
}

/* function: show_dialog */
static int tolua_init_show_dialog00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnumber(tolua_S,1,0,&tolua_err) ||
 !tolua_isnoobj(tolua_S,2,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
  int dialognum = ((int)  tolua_tonumber(tolua_S,1,0));
 {
  show_dialog(dialognum);
 }
 }
 return 0;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'show_dialog'.",&tolua_err);
 return 0;
#endif
}

/* function: get_quantity_s32b */
static int tolua_init_get_quantity_s32b00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isstring(tolua_S,1,0,&tolua_err) ||
 !tolua_isnumber(tolua_S,2,0,&tolua_err) ||
 !tolua_isnoobj(tolua_S,3,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
  cptr prompt = ((cptr)  tolua_tostring(tolua_S,1,0));
  s32b max = ((s32b)  tolua_tonumber(tolua_S,2,0));
 {
  s32b tolua_ret = (s32b)  get_quantity_s32b(prompt,max);
 tolua_pushnumber(tolua_S,(long)tolua_ret);
 }
 }
 return 1;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'get_quantity_s32b'.",&tolua_err);
 return 0;
#endif
}

/* function: script_do_file */
static int tolua_init_script_do_file00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isstring(tolua_S,1,0,&tolua_err) ||
 !tolua_isnoobj(tolua_S,2,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
  cptr filename = ((cptr)  tolua_tostring(tolua_S,1,0));
 {
  bool tolua_ret = (bool)  script_do_file(filename);
 tolua_pushboolean(tolua_S,(bool)tolua_ret);
 }
 }
 return 1;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'script_do_file'.",&tolua_err);
 return 0;
#endif
}

/* function: lua_randint */
static int tolua_init_lua_randint00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnumber(tolua_S,1,0,&tolua_err) ||
 !tolua_isnoobj(tolua_S,2,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
  int num = ((int)  tolua_tonumber(tolua_S,1,0));
 {
  s32b tolua_ret = (s32b)  lua_randint(num);
 tolua_pushnumber(tolua_S,(long)tolua_ret);
 }
 }
 return 1;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'lua_randint'.",&tolua_err);
 return 0;
#endif
}

/* function: distance */
static int tolua_init_distance00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnumber(tolua_S,1,0,&tolua_err) ||
 !tolua_isnumber(tolua_S,2,0,&tolua_err) ||
 !tolua_isnumber(tolua_S,3,0,&tolua_err) ||
 !tolua_isnumber(tolua_S,4,0,&tolua_err) ||
 !tolua_isnoobj(tolua_S,5,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
  int y1 = ((int)  tolua_tonumber(tolua_S,1,0));
  int x1 = ((int)  tolua_tonumber(tolua_S,2,0));
  int y2 = ((int)  tolua_tonumber(tolua_S,3,0));
  int x2 = ((int)  tolua_tonumber(tolua_S,4,0));
 {
  int tolua_ret = (int)  distance(y1,x1,y2,x2);
 tolua_pushnumber(tolua_S,(long)tolua_ret);
 }
 }
 return 1;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'distance'.",&tolua_err);
 return 0;
#endif
}

/* function: los */
static int tolua_init_los00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnumber(tolua_S,1,0,&tolua_err) ||
 !tolua_isnumber(tolua_S,2,0,&tolua_err) ||
 !tolua_isnumber(tolua_S,3,0,&tolua_err) ||
 !tolua_isnumber(tolua_S,4,0,&tolua_err) ||
 !tolua_isnoobj(tolua_S,5,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
  int y1 = ((int)  tolua_tonumber(tolua_S,1,0));
  int x1 = ((int)  tolua_tonumber(tolua_S,2,0));
  int y2 = ((int)  tolua_tonumber(tolua_S,3,0));
  int x2 = ((int)  tolua_tonumber(tolua_S,4,0));
 {
  bool tolua_ret = (bool)  los(y1,x1,y2,x2);
 tolua_pushboolean(tolua_S,(bool)tolua_ret);
 }
 }
 return 1;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'los'.",&tolua_err);
 return 0;
#endif
}

/* function: player_can_see_bold */
static int tolua_init_player_can_see_bold00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnumber(tolua_S,1,0,&tolua_err) ||
 !tolua_isnumber(tolua_S,2,0,&tolua_err) ||
 !tolua_isnoobj(tolua_S,3,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
  int y = ((int)  tolua_tonumber(tolua_S,1,0));
  int x = ((int)  tolua_tonumber(tolua_S,2,0));
 {
  bool tolua_ret = (bool)  player_can_see_bold(y,x);
 tolua_pushboolean(tolua_S,(bool)tolua_ret);
 }
 }
 return 1;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'player_can_see_bold'.",&tolua_err);
 return 0;
#endif
}

/* function: cave_valid_bold */
static int tolua_init_cave_valid_bold00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnumber(tolua_S,1,0,&tolua_err) ||
 !tolua_isnumber(tolua_S,2,0,&tolua_err) ||
 !tolua_isnoobj(tolua_S,3,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
  int y = ((int)  tolua_tonumber(tolua_S,1,0));
  int x = ((int)  tolua_tonumber(tolua_S,2,0));
 {
  bool tolua_ret = (bool)  cave_valid_bold(y,x);
 tolua_pushboolean(tolua_S,(bool)tolua_ret);
 }
 }
 return 1;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'cave_valid_bold'.",&tolua_err);
 return 0;
#endif
}

/* function: no_lite */
static int tolua_init_no_lite00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnoobj(tolua_S,1,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
 {
  bool tolua_ret = (bool)  no_lite();
 tolua_pushboolean(tolua_S,(bool)tolua_ret);
 }
 }
 return 1;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'no_lite'.",&tolua_err);
 return 0;
#endif
}

/* function: move_cursor_relative */
static int tolua_init_move_cursor_relative00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnumber(tolua_S,1,0,&tolua_err) ||
 !tolua_isnumber(tolua_S,2,0,&tolua_err) ||
 !tolua_isnoobj(tolua_S,3,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
  int row = ((int)  tolua_tonumber(tolua_S,1,0));
  int col = ((int)  tolua_tonumber(tolua_S,2,0));
 {
  move_cursor_relative(row,col);
 }
 }
 return 0;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'move_cursor_relative'.",&tolua_err);
 return 0;
#endif
}

/* function: print_rel */
static int tolua_init_print_rel00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnumber(tolua_S,1,0,&tolua_err) ||
 !tolua_isnumber(tolua_S,2,0,&tolua_err) ||
 !tolua_isnumber(tolua_S,3,0,&tolua_err) ||
 !tolua_isnumber(tolua_S,4,0,&tolua_err) ||
 !tolua_isnoobj(tolua_S,5,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
  char c = ((char)  tolua_tonumber(tolua_S,1,0));
  byte a = ((byte)  tolua_tonumber(tolua_S,2,0));
  int y = ((int)  tolua_tonumber(tolua_S,3,0));
  int x = ((int)  tolua_tonumber(tolua_S,4,0));
 {
  print_rel(c,a,y,x);
 }
 }
 return 0;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'print_rel'.",&tolua_err);
 return 0;
#endif
}

/* function: note_spot */
static int tolua_init_note_spot00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnumber(tolua_S,1,0,&tolua_err) ||
 !tolua_isnumber(tolua_S,2,0,&tolua_err) ||
 !tolua_isnoobj(tolua_S,3,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
  int y = ((int)  tolua_tonumber(tolua_S,1,0));
  int x = ((int)  tolua_tonumber(tolua_S,2,0));
 {
  note_spot(y,x);
 }
 }
 return 0;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'note_spot'.",&tolua_err);
 return 0;
#endif
}

/* function: lite_spot */
static int tolua_init_lite_spot00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnumber(tolua_S,1,0,&tolua_err) ||
 !tolua_isnumber(tolua_S,2,0,&tolua_err) ||
 !tolua_isnoobj(tolua_S,3,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
  int y = ((int)  tolua_tonumber(tolua_S,1,0));
  int x = ((int)  tolua_tonumber(tolua_S,2,0));
 {
  lite_spot(y,x);
 }
 }
 return 0;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'lite_spot'.",&tolua_err);
 return 0;
#endif
}

/* function: prt_map */
static int tolua_init_prt_map00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnoobj(tolua_S,1,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
 {
  prt_map();
 }
 }
 return 0;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'prt_map'.",&tolua_err);
 return 0;
#endif
}

/* function: display_map */
static int tolua_init_display_map00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnumber(tolua_S,1,0,&tolua_err) ||
 !tolua_isnumber(tolua_S,2,0,&tolua_err) ||
 !tolua_isnoobj(tolua_S,3,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
  int cy = ((int)  tolua_tonumber(tolua_S,1,0));
  int cx = ((int)  tolua_tonumber(tolua_S,2,0));
 {
  display_map(&cy,&cx);
 tolua_pushnumber(tolua_S,(long)cy);
 tolua_pushnumber(tolua_S,(long)cx);
 }
 }
 return 2;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'display_map'.",&tolua_err);
 return 0;
#endif
}

/* function: do_cmd_view_map */
static int tolua_init_do_cmd_view_map00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnoobj(tolua_S,1,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
 {
  do_cmd_view_map();
 }
 }
 return 0;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'do_cmd_view_map'.",&tolua_err);
 return 0;
#endif
}

/* function: forget_lite */
static int tolua_init_forget_lite00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnoobj(tolua_S,1,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
 {
  forget_lite();
 }
 }
 return 0;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'forget_lite'.",&tolua_err);
 return 0;
#endif
}

/* function: update_lite */
static int tolua_init_update_lite00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnoobj(tolua_S,1,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
 {
  update_lite();
 }
 }
 return 0;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'update_lite'.",&tolua_err);
 return 0;
#endif
}

/* function: forget_view */
static int tolua_init_forget_view00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnoobj(tolua_S,1,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
 {
  forget_view();
 }
 }
 return 0;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'forget_view'.",&tolua_err);
 return 0;
#endif
}

/* function: update_view */
static int tolua_init_update_view00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnoobj(tolua_S,1,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
 {
  update_view();
 }
 }
 return 0;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'update_view'.",&tolua_err);
 return 0;
#endif
}

/* function: forget_flow */
static int tolua_init_forget_flow00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnoobj(tolua_S,1,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
 {
  forget_flow();
 }
 }
 return 0;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'forget_flow'.",&tolua_err);
 return 0;
#endif
}

/* function: update_flow */
static int tolua_init_update_flow00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnoobj(tolua_S,1,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
 {
  update_flow();
 }
 }
 return 0;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'update_flow'.",&tolua_err);
 return 0;
#endif
}

/* function: map_area */
static int tolua_init_map_area00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnoobj(tolua_S,1,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
 {
  map_area();
 }
 }
 return 0;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'map_area'.",&tolua_err);
 return 0;
#endif
}

/* function: wiz_lite */
static int tolua_init_wiz_lite00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnoobj(tolua_S,1,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
 {
  wiz_lite();
 }
 }
 return 0;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'wiz_lite'.",&tolua_err);
 return 0;
#endif
}

/* function: wiz_lite_extra */
static int tolua_init_wiz_lite_extra00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnoobj(tolua_S,1,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
 {
  wiz_lite_extra();
 }
 }
 return 0;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'wiz_lite_extra'.",&tolua_err);
 return 0;
#endif
}

/* function: wiz_dark */
static int tolua_init_wiz_dark00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnoobj(tolua_S,1,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
 {
  wiz_dark();
 }
 }
 return 0;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'wiz_dark'.",&tolua_err);
 return 0;
#endif
}

/* function: cave_set_feat */
static int tolua_init_cave_set_feat00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnumber(tolua_S,1,0,&tolua_err) ||
 !tolua_isnumber(tolua_S,2,0,&tolua_err) ||
 !tolua_isnumber(tolua_S,3,0,&tolua_err) ||
 !tolua_isnoobj(tolua_S,4,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
  int y = ((int)  tolua_tonumber(tolua_S,1,0));
  int x = ((int)  tolua_tonumber(tolua_S,2,0));
  int feat = ((int)  tolua_tonumber(tolua_S,3,0));
 {
  cave_set_feat(y,x,feat);
 }
 }
 return 0;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'cave_set_feat'.",&tolua_err);
 return 0;
#endif
}

/* function: mmove2 */
static int tolua_init_mmove200(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnumber(tolua_S,1,0,&tolua_err) ||
 !tolua_isnumber(tolua_S,2,0,&tolua_err) ||
 !tolua_isnumber(tolua_S,3,0,&tolua_err) ||
 !tolua_isnumber(tolua_S,4,0,&tolua_err) ||
 !tolua_isnumber(tolua_S,5,0,&tolua_err) ||
 !tolua_isnumber(tolua_S,6,0,&tolua_err) ||
 !tolua_isnoobj(tolua_S,7,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
  int y = ((int)  tolua_tonumber(tolua_S,1,0));
  int x = ((int)  tolua_tonumber(tolua_S,2,0));
  int y1 = ((int)  tolua_tonumber(tolua_S,3,0));
  int x1 = ((int)  tolua_tonumber(tolua_S,4,0));
  int y2 = ((int)  tolua_tonumber(tolua_S,5,0));
  int x2 = ((int)  tolua_tonumber(tolua_S,6,0));
 {
  mmove2(&y,&x,y1,x1,y2,x2);
 tolua_pushnumber(tolua_S,(long)y);
 tolua_pushnumber(tolua_S,(long)x);
 }
 }
 return 2;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'mmove2'.",&tolua_err);
 return 0;
#endif
}

/* function: projectable */
static int tolua_init_projectable00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnumber(tolua_S,1,0,&tolua_err) ||
 !tolua_isnumber(tolua_S,2,0,&tolua_err) ||
 !tolua_isnumber(tolua_S,3,0,&tolua_err) ||
 !tolua_isnumber(tolua_S,4,0,&tolua_err) ||
 !tolua_isnoobj(tolua_S,5,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
  int y1 = ((int)  tolua_tonumber(tolua_S,1,0));
  int x1 = ((int)  tolua_tonumber(tolua_S,2,0));
  int y2 = ((int)  tolua_tonumber(tolua_S,3,0));
  int x2 = ((int)  tolua_tonumber(tolua_S,4,0));
 {
  bool tolua_ret = (bool)  projectable(y1,x1,y2,x2);
 tolua_pushboolean(tolua_S,(bool)tolua_ret);
 }
 }
 return 1;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'projectable'.",&tolua_err);
 return 0;
#endif
}

/* function: scatter */
static int tolua_init_scatter00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnumber(tolua_S,1,0,&tolua_err) ||
 !tolua_isnumber(tolua_S,2,0,&tolua_err) ||
 !tolua_isnumber(tolua_S,3,0,&tolua_err) ||
 !tolua_isnumber(tolua_S,4,0,&tolua_err) ||
 !tolua_isnumber(tolua_S,5,0,&tolua_err) ||
 !tolua_isnumber(tolua_S,6,0,&tolua_err) ||
 !tolua_isnoobj(tolua_S,7,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
  int yp = ((int)  tolua_tonumber(tolua_S,1,0));
  int xp = ((int)  tolua_tonumber(tolua_S,2,0));
  int y = ((int)  tolua_tonumber(tolua_S,3,0));
  int x = ((int)  tolua_tonumber(tolua_S,4,0));
  int d = ((int)  tolua_tonumber(tolua_S,5,0));
  int m = ((int)  tolua_tonumber(tolua_S,6,0));
 {
  scatter(&yp,&xp,y,x,d,m);
 tolua_pushnumber(tolua_S,(long)yp);
 tolua_pushnumber(tolua_S,(long)xp);
 }
 }
 return 2;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'scatter'.",&tolua_err);
 return 0;
#endif
}

/* function: health_track */
static int tolua_init_health_track00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnumber(tolua_S,1,0,&tolua_err) ||
 !tolua_isnoobj(tolua_S,2,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
  int m_idx = ((int)  tolua_tonumber(tolua_S,1,0));
 {
  health_track(m_idx);
 }
 }
 return 0;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'health_track'.",&tolua_err);
 return 0;
#endif
}

/* function: monster_race_track */
static int tolua_init_monster_race_track00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnumber(tolua_S,1,0,&tolua_err) ||
 !tolua_isnoobj(tolua_S,2,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
  int r_idx = ((int)  tolua_tonumber(tolua_S,1,0));
 {
  monster_race_track(r_idx);
 }
 }
 return 0;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'monster_race_track'.",&tolua_err);
 return 0;
#endif
}

/* function: object_kind_track */
static int tolua_init_object_kind_track00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnumber(tolua_S,1,0,&tolua_err) ||
 !tolua_isnoobj(tolua_S,2,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
  int k_idx = ((int)  tolua_tonumber(tolua_S,1,0));
 {
  object_kind_track(k_idx);
 }
 }
 return 0;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'object_kind_track'.",&tolua_err);
 return 0;
#endif
}

/* function: disturb */
static int tolua_init_disturb00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnumber(tolua_S,1,0,&tolua_err) ||
 !tolua_isnumber(tolua_S,2,0,&tolua_err) ||
 !tolua_isnoobj(tolua_S,3,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
  int stop_search = ((int)  tolua_tonumber(tolua_S,1,0));
  int flush_output = ((int)  tolua_tonumber(tolua_S,2,0));
 {
  disturb(stop_search,flush_output);
 }
 }
 return 0;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'disturb'.",&tolua_err);
 return 0;
#endif
}

/* function: is_quest */
static int tolua_init_is_quest00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnumber(tolua_S,1,0,&tolua_err) ||
 !tolua_isnoobj(tolua_S,2,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
  int level = ((int)  tolua_tonumber(tolua_S,1,0));
 {
  bool tolua_ret = (bool)  is_quest(level);
 tolua_pushboolean(tolua_S,(bool)tolua_ret);
 }
 }
 return 1;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'is_quest'.",&tolua_err);
 return 0;
#endif
}

/* function: tot_dam_aux */
static int tolua_init_tot_dam_aux00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isusertype(tolua_S,1,"object_type",0,&tolua_err) ||
 !tolua_isnumber(tolua_S,2,0,&tolua_err) ||
 !tolua_isusertype(tolua_S,3,"monster_type",0,&tolua_err) ||
 !tolua_isnoobj(tolua_S,4,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
  object_type* o_ptr = ((object_type*)  tolua_tousertype(tolua_S,1,0));
  s32b tdam = ((s32b)  tolua_tonumber(tolua_S,2,0));
  monster_type* m_ptr = ((monster_type*)  tolua_tousertype(tolua_S,3,0));
 {
  s32b tolua_ret = (s32b)  tot_dam_aux(o_ptr,tdam,m_ptr);
 tolua_pushnumber(tolua_S,(long)tolua_ret);
 }
 }
 return 1;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'tot_dam_aux'.",&tolua_err);
 return 0;
#endif
}

/* function: search */
static int tolua_init_search00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnoobj(tolua_S,1,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
 {
  search();
 }
 }
 return 0;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'search'.",&tolua_err);
 return 0;
#endif
}

/* function: carry */
static int tolua_init_carry00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnumber(tolua_S,1,0,&tolua_err) ||
 !tolua_isnoobj(tolua_S,2,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
  int pickup = ((int)  tolua_tonumber(tolua_S,1,0));
 {
  carry(pickup);
 }
 }
 return 0;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'carry'.",&tolua_err);
 return 0;
#endif
}

/* function: py_attack */
static int tolua_init_py_attack00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnumber(tolua_S,1,0,&tolua_err) ||
 !tolua_isnumber(tolua_S,2,0,&tolua_err) ||
 !tolua_isnumber(tolua_S,3,0,&tolua_err) ||
 !tolua_isnoobj(tolua_S,4,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
  int y = ((int)  tolua_tonumber(tolua_S,1,0));
  int x = ((int)  tolua_tonumber(tolua_S,2,0));
  int max_blow = ((int)  tolua_tonumber(tolua_S,3,0));
 {
  py_attack(y,x,max_blow);
 }
 }
 return 0;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'py_attack'.",&tolua_err);
 return 0;
#endif
}

/* function: player_can_enter */
static int tolua_init_player_can_enter00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnumber(tolua_S,1,0,&tolua_err) ||
 !tolua_isnoobj(tolua_S,2,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
  byte feature = ((byte)  tolua_tonumber(tolua_S,1,0));
 {
  bool tolua_ret = (bool)  player_can_enter(feature);
 tolua_pushboolean(tolua_S,(bool)tolua_ret);
 }
 }
 return 1;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'player_can_enter'.",&tolua_err);
 return 0;
#endif
}

/* function: move_player */
static int tolua_init_move_player00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnumber(tolua_S,1,0,&tolua_err) ||
 !tolua_isnumber(tolua_S,2,0,&tolua_err) ||
 !tolua_isnoobj(tolua_S,3,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
  int dir = ((int)  tolua_tonumber(tolua_S,1,0));
  int do_pickup = ((int)  tolua_tonumber(tolua_S,2,0));
 {
  move_player(dir,do_pickup);
 }
 }
 return 0;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'move_player'.",&tolua_err);
 return 0;
#endif
}

/* function: run_step */
static int tolua_init_run_step00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnumber(tolua_S,1,0,&tolua_err) ||
 !tolua_isnoobj(tolua_S,2,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
  int dir = ((int)  tolua_tonumber(tolua_S,1,0));
 {
  run_step(dir);
 }
 }
 return 0;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'run_step'.",&tolua_err);
 return 0;
#endif
}

/* function: step_effects */
static int tolua_init_step_effects00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnumber(tolua_S,1,0,&tolua_err) ||
 !tolua_isnumber(tolua_S,2,0,&tolua_err) ||
 !tolua_isnumber(tolua_S,3,0,&tolua_err) ||
 !tolua_isnoobj(tolua_S,4,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
  int y = ((int)  tolua_tonumber(tolua_S,1,0));
  int x = ((int)  tolua_tonumber(tolua_S,2,0));
  int do_pickup = ((int)  tolua_tonumber(tolua_S,3,0));
 {
  step_effects(y,x,do_pickup);
 }
 }
 return 0;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'step_effects'.",&tolua_err);
 return 0;
#endif
}

/* function: do_cmd_pet */
static int tolua_init_do_cmd_pet00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnoobj(tolua_S,1,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
 {
  do_cmd_pet();
 }
 }
 return 0;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'do_cmd_pet'.",&tolua_err);
 return 0;
#endif
}

/* function: incarnate_monster_attack */
static int tolua_init_incarnate_monster_attack00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnumber(tolua_S,1,0,&tolua_err) ||
 !tolua_isnumber(tolua_S,2,0,&tolua_err) ||
 !tolua_isnumber(tolua_S,3,0,&tolua_err) ||
 !tolua_isnoobj(tolua_S,4,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
  s16b m_idx = ((s16b)  tolua_tonumber(tolua_S,1,0));
  int x = ((int)  tolua_tonumber(tolua_S,2,0));
  int y = ((int)  tolua_tonumber(tolua_S,3,0));
 {
  incarnate_monster_attack(m_idx,x,y);
 }
 }
 return 0;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'incarnate_monster_attack'.",&tolua_err);
 return 0;
#endif
}

/* function: critical_hits */
static int tolua_init_critical_hits00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnumber(tolua_S,1,0,&tolua_err) ||
 !tolua_isusertype(tolua_S,2,"monster_type",0,&tolua_err) ||
 !tolua_isnoobj(tolua_S,3,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
  s32b dam = ((s32b)  tolua_tonumber(tolua_S,1,0));
  monster_type* m_ptr = ((monster_type*)  tolua_tousertype(tolua_S,2,0));
 {
  s32b tolua_ret = (s32b)  critical_hits(dam,m_ptr);
 tolua_pushnumber(tolua_S,(long)tolua_ret);
 }
 }
 return 1;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'critical_hits'.",&tolua_err);
 return 0;
#endif
}

/* function: standing_on_forest */
static int tolua_init_standing_on_forest00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnoobj(tolua_S,1,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
 {
  bool tolua_ret = (bool)  standing_on_forest();
 tolua_pushboolean(tolua_S,(bool)tolua_ret);
 }
 }
 return 1;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'standing_on_forest'.",&tolua_err);
 return 0;
#endif
}

/* function: do_cmd_go_up */
static int tolua_init_do_cmd_go_up00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnoobj(tolua_S,1,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
 {
  do_cmd_go_up();
 }
 }
 return 0;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'do_cmd_go_up'.",&tolua_err);
 return 0;
#endif
}

/* function: do_cmd_go_down */
static int tolua_init_do_cmd_go_down00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnoobj(tolua_S,1,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
 {
  do_cmd_go_down();
 }
 }
 return 0;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'do_cmd_go_down'.",&tolua_err);
 return 0;
#endif
}

/* function: do_cmd_search */
static int tolua_init_do_cmd_search00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnoobj(tolua_S,1,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
 {
  do_cmd_search();
 }
 }
 return 0;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'do_cmd_search'.",&tolua_err);
 return 0;
#endif
}

/* function: do_cmd_toggle_search */
static int tolua_init_do_cmd_toggle_search00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnoobj(tolua_S,1,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
 {
  do_cmd_toggle_search();
 }
 }
 return 0;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'do_cmd_toggle_search'.",&tolua_err);
 return 0;
#endif
}

/* function: do_cmd_open */
static int tolua_init_do_cmd_open00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnoobj(tolua_S,1,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
 {
  do_cmd_open();
 }
 }
 return 0;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'do_cmd_open'.",&tolua_err);
 return 0;
#endif
}

/* function: do_cmd_close */
static int tolua_init_do_cmd_close00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnoobj(tolua_S,1,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
 {
  do_cmd_close();
 }
 }
 return 0;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'do_cmd_close'.",&tolua_err);
 return 0;
#endif
}

/* function: do_cmd_tunnel */
static int tolua_init_do_cmd_tunnel00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnoobj(tolua_S,1,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
 {
  do_cmd_tunnel();
 }
 }
 return 0;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'do_cmd_tunnel'.",&tolua_err);
 return 0;
#endif
}

/* function: do_cmd_tunnel_aux */
static int tolua_init_do_cmd_tunnel_aux00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnumber(tolua_S,1,0,&tolua_err) ||
 !tolua_isnumber(tolua_S,2,0,&tolua_err) ||
 !tolua_isnumber(tolua_S,3,0,&tolua_err) ||
 !tolua_isnoobj(tolua_S,4,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
  int y = ((int)  tolua_tonumber(tolua_S,1,0));
  int x = ((int)  tolua_tonumber(tolua_S,2,0));
  int dir = ((int)  tolua_tonumber(tolua_S,3,0));
 {
  bool tolua_ret = (bool)  do_cmd_tunnel_aux(y,x,dir);
 tolua_pushboolean(tolua_S,(bool)tolua_ret);
 }
 }
 return 1;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'do_cmd_tunnel_aux'.",&tolua_err);
 return 0;
#endif
}

/* function: do_cmd_disarm */
static int tolua_init_do_cmd_disarm00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnoobj(tolua_S,1,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
 {
  do_cmd_disarm();
 }
 }
 return 0;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'do_cmd_disarm'.",&tolua_err);
 return 0;
#endif
}

/* function: do_cmd_bash */
static int tolua_init_do_cmd_bash00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnoobj(tolua_S,1,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
 {
  do_cmd_bash();
 }
 }
 return 0;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'do_cmd_bash'.",&tolua_err);
 return 0;
#endif
}

/* function: do_cmd_alter */
static int tolua_init_do_cmd_alter00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnoobj(tolua_S,1,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
 {
  do_cmd_alter();
 }
 }
 return 0;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'do_cmd_alter'.",&tolua_err);
 return 0;
#endif
}

/* function: do_cmd_spike */
static int tolua_init_do_cmd_spike00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnoobj(tolua_S,1,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
 {
  do_cmd_spike();
 }
 }
 return 0;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'do_cmd_spike'.",&tolua_err);
 return 0;
#endif
}

/* function: do_cmd_walk */
static int tolua_init_do_cmd_walk00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnumber(tolua_S,1,0,&tolua_err) ||
 !tolua_isnoobj(tolua_S,2,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
  int pickup = ((int)  tolua_tonumber(tolua_S,1,0));
 {
  do_cmd_walk(pickup);
 }
 }
 return 0;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'do_cmd_walk'.",&tolua_err);
 return 0;
#endif
}

/* function: do_cmd_stay */
static int tolua_init_do_cmd_stay00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnumber(tolua_S,1,0,&tolua_err) ||
 !tolua_isnoobj(tolua_S,2,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
  int pickup = ((int)  tolua_tonumber(tolua_S,1,0));
 {
  do_cmd_stay(pickup);
 }
 }
 return 0;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'do_cmd_stay'.",&tolua_err);
 return 0;
#endif
}

/* function: do_cmd_run */
static int tolua_init_do_cmd_run00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnoobj(tolua_S,1,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
 {
  do_cmd_run();
 }
 }
 return 0;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'do_cmd_run'.",&tolua_err);
 return 0;
#endif
}

/* function: do_cmd_rest */
static int tolua_init_do_cmd_rest00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnoobj(tolua_S,1,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
 {
  do_cmd_rest();
 }
 }
 return 0;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'do_cmd_rest'.",&tolua_err);
 return 0;
#endif
}

/* function: do_cmd_fire */
static int tolua_init_do_cmd_fire00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnoobj(tolua_S,1,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
 {
  do_cmd_fire();
 }
 }
 return 0;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'do_cmd_fire'.",&tolua_err);
 return 0;
#endif
}

/* function: do_cmd_steal */
static int tolua_init_do_cmd_steal00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnoobj(tolua_S,1,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
 {
  do_cmd_steal();
 }
 }
 return 0;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'do_cmd_steal'.",&tolua_err);
 return 0;
#endif
}

/* function: use_monster_ranged_attack */
static int tolua_init_use_monster_ranged_attack00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnumber(tolua_S,1,0,&tolua_err) ||
 !tolua_isnoobj(tolua_S,2,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
  int r_idx = ((int)  tolua_tonumber(tolua_S,1,0));
 {
  use_monster_ranged_attack(r_idx);
 }
 }
 return 0;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'use_monster_ranged_attack'.",&tolua_err);
 return 0;
#endif
}

/* function: choose_current_weapon */
static int tolua_init_choose_current_weapon00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnoobj(tolua_S,1,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
 {
  choose_current_weapon();
 }
 }
 return 0;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'choose_current_weapon'.",&tolua_err);
 return 0;
#endif
}

/* function: use_hardcode_ability */
static int tolua_init_use_hardcode_ability00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnumber(tolua_S,1,0,&tolua_err) ||
 !tolua_isnoobj(tolua_S,2,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
  int powernum = ((int)  tolua_tonumber(tolua_S,1,0));
 {
  use_hardcode_ability(powernum);
 }
 }
 return 0;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'use_hardcode_ability'.",&tolua_err);
 return 0;
#endif
}

/* function: reload_ranged */
static int tolua_init_reload_ranged00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnoobj(tolua_S,1,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
 {
  reload_ranged();
 }
 }
 return 0;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'reload_ranged'.",&tolua_err);
 return 0;
#endif
}

/* function: throw_select */
static int tolua_init_throw_select00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnoobj(tolua_S,1,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
 {
  bool tolua_ret = (bool)  throw_select();
 tolua_pushboolean(tolua_S,(bool)tolua_ret);
 }
 }
 return 1;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'throw_select'.",&tolua_err);
 return 0;
#endif
}

/* function: alchemy_brand */
static int tolua_init_alchemy_brand00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnoobj(tolua_S,1,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
 {
  alchemy_brand();
 }
 }
 return 0;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'alchemy_brand'.",&tolua_err);
 return 0;
#endif
}

/* function: alchemy_resist */
static int tolua_init_alchemy_resist00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnoobj(tolua_S,1,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
 {
  alchemy_resist();
 }
 }
 return 0;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'alchemy_resist'.",&tolua_err);
 return 0;
#endif
}

/* function: essence_transfer */
static int tolua_init_essence_transfer00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnoobj(tolua_S,1,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
 {
  essence_transfer();
 }
 }
 return 0;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'essence_transfer'.",&tolua_err);
 return 0;
#endif
}

/* function: defense_transfer */
static int tolua_init_defense_transfer00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnoobj(tolua_S,1,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
 {
  defense_transfer();
 }
 }
 return 0;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'defense_transfer'.",&tolua_err);
 return 0;
#endif
}

/* function: diviner_wish */
static int tolua_init_diviner_wish00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnoobj(tolua_S,1,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
 {
  diviner_wish();
 }
 }
 return 0;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'diviner_wish'.",&tolua_err);
 return 0;
#endif
}

/* function: do_cmd_inven */
static int tolua_init_do_cmd_inven00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnoobj(tolua_S,1,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
 {
  do_cmd_inven();
 }
 }
 return 0;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'do_cmd_inven'.",&tolua_err);
 return 0;
#endif
}

/* function: do_cmd_equip */
static int tolua_init_do_cmd_equip00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnoobj(tolua_S,1,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
 {
  do_cmd_equip();
 }
 }
 return 0;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'do_cmd_equip'.",&tolua_err);
 return 0;
#endif
}

/* function: do_cmd_wield */
static int tolua_init_do_cmd_wield00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnoobj(tolua_S,1,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
 {
  do_cmd_wield();
 }
 }
 return 0;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'do_cmd_wield'.",&tolua_err);
 return 0;
#endif
}

/* function: do_cmd_takeoff */
static int tolua_init_do_cmd_takeoff00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnoobj(tolua_S,1,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
 {
  do_cmd_takeoff();
 }
 }
 return 0;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'do_cmd_takeoff'.",&tolua_err);
 return 0;
#endif
}

/* function: do_cmd_drop */
static int tolua_init_do_cmd_drop00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnoobj(tolua_S,1,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
 {
  do_cmd_drop();
 }
 }
 return 0;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'do_cmd_drop'.",&tolua_err);
 return 0;
#endif
}

/* function: do_cmd_destroy */
static int tolua_init_do_cmd_destroy00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnoobj(tolua_S,1,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
 {
  do_cmd_destroy();
 }
 }
 return 0;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'do_cmd_destroy'.",&tolua_err);
 return 0;
#endif
}

/* function: do_cmd_observe */
static int tolua_init_do_cmd_observe00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnoobj(tolua_S,1,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
 {
  do_cmd_observe();
 }
 }
 return 0;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'do_cmd_observe'.",&tolua_err);
 return 0;
#endif
}

/* function: do_cmd_uninscribe */
static int tolua_init_do_cmd_uninscribe00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnoobj(tolua_S,1,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
 {
  do_cmd_uninscribe();
 }
 }
 return 0;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'do_cmd_uninscribe'.",&tolua_err);
 return 0;
#endif
}

/* function: do_cmd_inscribe */
static int tolua_init_do_cmd_inscribe00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnoobj(tolua_S,1,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
 {
  do_cmd_inscribe();
 }
 }
 return 0;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'do_cmd_inscribe'.",&tolua_err);
 return 0;
#endif
}

/* function: do_cmd_refill */
static int tolua_init_do_cmd_refill00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnoobj(tolua_S,1,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
 {
  do_cmd_refill();
 }
 }
 return 0;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'do_cmd_refill'.",&tolua_err);
 return 0;
#endif
}

/* function: do_cmd_target */
static int tolua_init_do_cmd_target00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnoobj(tolua_S,1,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
 {
  do_cmd_target();
 }
 }
 return 0;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'do_cmd_target'.",&tolua_err);
 return 0;
#endif
}

/* function: do_cmd_look */
static int tolua_init_do_cmd_look00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnoobj(tolua_S,1,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
 {
  do_cmd_look();
 }
 }
 return 0;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'do_cmd_look'.",&tolua_err);
 return 0;
#endif
}

/* function: do_cmd_locate */
static int tolua_init_do_cmd_locate00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnoobj(tolua_S,1,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
 {
  do_cmd_locate();
 }
 }
 return 0;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'do_cmd_locate'.",&tolua_err);
 return 0;
#endif
}

/* function: do_cmd_locate_center */
static int tolua_init_do_cmd_locate_center00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnumber(tolua_S,1,0,&tolua_err) ||
 !tolua_isnumber(tolua_S,2,0,&tolua_err) ||
 !tolua_isnoobj(tolua_S,3,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
  int y = ((int)  tolua_tonumber(tolua_S,1,0));
  int x = ((int)  tolua_tonumber(tolua_S,2,0));
 {
  do_cmd_locate_center(y,x);
 }
 }
 return 0;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'do_cmd_locate_center'.",&tolua_err);
 return 0;
#endif
}

/* function: do_cmd_query_symbol */
static int tolua_init_do_cmd_query_symbol00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnoobj(tolua_S,1,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
 {
  do_cmd_query_symbol();
 }
 }
 return 0;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'do_cmd_query_symbol'.",&tolua_err);
 return 0;
#endif
}

/* function: do_cmd_racial_power */
static int tolua_init_do_cmd_racial_power00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnumber(tolua_S,1,0,&tolua_err) ||
 !tolua_isnoobj(tolua_S,2,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
  int combat_feat = ((int)  tolua_tonumber(tolua_S,1,0));
 {
  do_cmd_racial_power(combat_feat);
 }
 }
 return 0;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'do_cmd_racial_power'.",&tolua_err);
 return 0;
#endif
}

/* function: research_mon */
static int tolua_init_research_mon00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnoobj(tolua_S,1,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
 {
  bool tolua_ret = (bool)  research_mon();
 tolua_pushboolean(tolua_S,(bool)tolua_ret);
 }
 }
 return 1;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'research_mon'.",&tolua_err);
 return 0;
#endif
}

/* function: do_cmd_sense_grid_mana */
static int tolua_init_do_cmd_sense_grid_mana00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnoobj(tolua_S,1,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
 {
  do_cmd_sense_grid_mana();
 }
 }
 return 0;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'do_cmd_sense_grid_mana'.",&tolua_err);
 return 0;
#endif
}

/* function: max_carry */
static int tolua_init_max_carry00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnoobj(tolua_S,1,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
 {
  int tolua_ret = (int)  max_carry();
 tolua_pushnumber(tolua_S,(long)tolua_ret);
 }
 }
 return 1;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'max_carry'.",&tolua_err);
 return 0;
#endif
}

/* function: summoned_item */
static int tolua_init_summoned_item00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isusertype(tolua_S,1,"object_type",0,&tolua_err) ||
 !tolua_isnoobj(tolua_S,2,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
  object_type* o_ptr = ((object_type*)  tolua_tousertype(tolua_S,1,0));
 {
  bool tolua_ret = (bool)  summoned_item(o_ptr);
 tolua_pushboolean(tolua_S,(bool)tolua_ret);
 }
 }
 return 1;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'summoned_item'.",&tolua_err);
 return 0;
#endif
}

/* function: one_weapon_wield */
static int tolua_init_one_weapon_wield00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnoobj(tolua_S,1,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
 {
  bool tolua_ret = (bool)  one_weapon_wield();
 tolua_pushboolean(tolua_S,(bool)tolua_ret);
 }
 }
 return 1;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'one_weapon_wield'.",&tolua_err);
 return 0;
#endif
}

/* function: two_weapon_wield */
static int tolua_init_two_weapon_wield00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnoobj(tolua_S,1,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
 {
  bool tolua_ret = (bool)  two_weapon_wield();
 tolua_pushboolean(tolua_S,(bool)tolua_ret);
 }
 }
 return 1;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'two_weapon_wield'.",&tolua_err);
 return 0;
#endif
}

/* function: do_cmd_redraw */
static int tolua_init_do_cmd_redraw00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnoobj(tolua_S,1,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
 {
  do_cmd_redraw();
 }
 }
 return 0;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'do_cmd_redraw'.",&tolua_err);
 return 0;
#endif
}

/* function: do_cmd_change_name */
static int tolua_init_do_cmd_change_name00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnoobj(tolua_S,1,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
 {
  do_cmd_change_name();
 }
 }
 return 0;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'do_cmd_change_name'.",&tolua_err);
 return 0;
#endif
}

/* function: do_cmd_message_one */
static int tolua_init_do_cmd_message_one00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnoobj(tolua_S,1,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
 {
  do_cmd_message_one();
 }
 }
 return 0;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'do_cmd_message_one'.",&tolua_err);
 return 0;
#endif
}

/* function: do_cmd_messages */
static int tolua_init_do_cmd_messages00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnoobj(tolua_S,1,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
 {
  do_cmd_messages();
 }
 }
 return 0;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'do_cmd_messages'.",&tolua_err);
 return 0;
#endif
}

/* function: do_cmd_options */
static int tolua_init_do_cmd_options00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnoobj(tolua_S,1,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
 {
  do_cmd_options();
 }
 }
 return 0;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'do_cmd_options'.",&tolua_err);
 return 0;
#endif
}

/* function: do_cmd_pref */
static int tolua_init_do_cmd_pref00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnoobj(tolua_S,1,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
 {
  do_cmd_pref();
 }
 }
 return 0;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'do_cmd_pref'.",&tolua_err);
 return 0;
#endif
}

/* function: do_cmd_macros */
static int tolua_init_do_cmd_macros00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnoobj(tolua_S,1,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
 {
  do_cmd_macros();
 }
 }
 return 0;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'do_cmd_macros'.",&tolua_err);
 return 0;
#endif
}

/* function: do_cmd_visuals */
static int tolua_init_do_cmd_visuals00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnoobj(tolua_S,1,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
 {
  do_cmd_visuals();
 }
 }
 return 0;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'do_cmd_visuals'.",&tolua_err);
 return 0;
#endif
}

/* function: do_cmd_colors */
static int tolua_init_do_cmd_colors00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnoobj(tolua_S,1,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
 {
  do_cmd_colors();
 }
 }
 return 0;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'do_cmd_colors'.",&tolua_err);
 return 0;
#endif
}

/* function: do_cmd_note */
static int tolua_init_do_cmd_note00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnoobj(tolua_S,1,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
 {
  do_cmd_note();
 }
 }
 return 0;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'do_cmd_note'.",&tolua_err);
 return 0;
#endif
}

/* function: do_cmd_version */
static int tolua_init_do_cmd_version00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnoobj(tolua_S,1,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
 {
  do_cmd_version();
 }
 }
 return 0;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'do_cmd_version'.",&tolua_err);
 return 0;
#endif
}

/* function: do_cmd_feeling */
static int tolua_init_do_cmd_feeling00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnoobj(tolua_S,1,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
 {
  do_cmd_feeling();
 }
 }
 return 0;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'do_cmd_feeling'.",&tolua_err);
 return 0;
#endif
}

/* function: do_cmd_load_screen */
static int tolua_init_do_cmd_load_screen00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnoobj(tolua_S,1,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
 {
  do_cmd_load_screen();
 }
 }
 return 0;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'do_cmd_load_screen'.",&tolua_err);
 return 0;
#endif
}

/* function: do_cmd_save_screen */
static int tolua_init_do_cmd_save_screen00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnoobj(tolua_S,1,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
 {
  do_cmd_save_screen();
 }
 }
 return 0;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'do_cmd_save_screen'.",&tolua_err);
 return 0;
#endif
}

/* function: do_cmd_knowledge */
static int tolua_init_do_cmd_knowledge00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnoobj(tolua_S,1,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
 {
  do_cmd_knowledge();
 }
 }
 return 0;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'do_cmd_knowledge'.",&tolua_err);
 return 0;
#endif
}

/* function: plural_aux */
static int tolua_init_plural_aux00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isstring(tolua_S,1,0,&tolua_err) ||
 !tolua_isnoobj(tolua_S,2,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
  char* Name = ((char*)  tolua_tostring(tolua_S,1,0));
 {
  plural_aux(Name);
 }
 }
 return 0;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'plural_aux'.",&tolua_err);
 return 0;
#endif
}

/* function: do_cmd_time */
static int tolua_init_do_cmd_time00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnoobj(tolua_S,1,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
 {
  do_cmd_time();
 }
 }
 return 0;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'do_cmd_time'.",&tolua_err);
 return 0;
#endif
}

/* function: do_cmd_options_aux */
static int tolua_init_do_cmd_options_aux00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnumber(tolua_S,1,0,&tolua_err) ||
 !tolua_isstring(tolua_S,2,0,&tolua_err) ||
 !tolua_isnoobj(tolua_S,3,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
  int page = ((int)  tolua_tonumber(tolua_S,1,0));
  cptr info = ((cptr)  tolua_tostring(tolua_S,2,0));
 {
  do_cmd_options_aux(page,info);
 }
 }
 return 0;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'do_cmd_options_aux'.",&tolua_err);
 return 0;
#endif
}

/* function: do_cmd_study */
static int tolua_init_do_cmd_study00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnoobj(tolua_S,1,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
 {
  do_cmd_study();
 }
 }
 return 0;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'do_cmd_study'.",&tolua_err);
 return 0;
#endif
}

/* function: mutate_player */
static int tolua_init_mutate_player00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnoobj(tolua_S,1,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
 {
  mutate_player();
 }
 }
 return 0;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'mutate_player'.",&tolua_err);
 return 0;
#endif
}

/* function: item_tester_hook_armour */
static int tolua_init_item_tester_hook_armour00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isusertype(tolua_S,1,"object_type",0,&tolua_err) ||
 !tolua_isnoobj(tolua_S,2,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
  object_type* o_ptr = ((object_type*)  tolua_tousertype(tolua_S,1,0));
 {
  bool tolua_ret = (bool)  item_tester_hook_armour(o_ptr);
 tolua_pushboolean(tolua_S,(bool)tolua_ret);
 }
 }
 return 1;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'item_tester_hook_armour'.",&tolua_err);
 return 0;
#endif
}

/* function: use_body_power */
static int tolua_init_use_body_power00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnumber(tolua_S,1,0,&tolua_err) ||
 !tolua_isboolean(tolua_S,2,0,&tolua_err) ||
 !tolua_isnoobj(tolua_S,3,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
  int r_idx = ((int)  tolua_tonumber(tolua_S,1,0));
  bool only_number = ((bool)  tolua_toboolean(tolua_S,2,0));
 {
  int tolua_ret = (int)  use_body_power(r_idx,only_number);
 tolua_pushnumber(tolua_S,(long)tolua_ret);
 }
 }
 return 1;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'use_body_power'.",&tolua_err);
 return 0;
#endif
}

/* function: conjure_item */
static int tolua_init_conjure_item00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnumber(tolua_S,1,0,&tolua_err) ||
 !tolua_isnumber(tolua_S,2,0,&tolua_err) ||
 !tolua_isnumber(tolua_S,3,0,&tolua_err) ||
 !tolua_isboolean(tolua_S,4,0,&tolua_err) ||
 !tolua_isboolean(tolua_S,5,0,&tolua_err) ||
 !tolua_isnoobj(tolua_S,6,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
  int itemtval = ((int)  tolua_tonumber(tolua_S,1,0));
  int itemsval = ((int)  tolua_tonumber(tolua_S,2,0));
  int duration = ((int)  tolua_tonumber(tolua_S,3,0));
  bool magic = ((bool)  tolua_toboolean(tolua_S,4,0));
  bool special = ((bool)  tolua_toboolean(tolua_S,5,0));
 {
  conjure_item(itemtval,itemsval,duration,magic,special);
 }
 }
 return 0;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'conjure_item'.",&tolua_err);
 return 0;
#endif
}

/* function: conjure_item_any */
static int tolua_init_conjure_item_any00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnumber(tolua_S,1,0,&tolua_err) ||
 !tolua_isnumber(tolua_S,2,0,&tolua_err) ||
 !tolua_isnumber(tolua_S,3,0,&tolua_err) ||
 !tolua_isnumber(tolua_S,4,0,&tolua_err) ||
 !tolua_isboolean(tolua_S,5,0,&tolua_err) ||
 !tolua_isboolean(tolua_S,6,0,&tolua_err) ||
 !tolua_isnoobj(tolua_S,7,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
  int itemtval = ((int)  tolua_tonumber(tolua_S,1,0));
  int itemsval = ((int)  tolua_tonumber(tolua_S,2,0));
  int duration = ((int)  tolua_tonumber(tolua_S,3,0));
  int quantity = ((int)  tolua_tonumber(tolua_S,4,0));
  bool magic = ((bool)  tolua_toboolean(tolua_S,5,0));
  bool special = ((bool)  tolua_toboolean(tolua_S,6,0));
 {
  conjure_item_any(itemtval,itemsval,duration,quantity,magic,special);
 }
 }
 return 0;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'conjure_item_any'.",&tolua_err);
 return 0;
#endif
}

/* function: place_field */
static int tolua_init_place_field00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnumber(tolua_S,1,0,&tolua_err) ||
 !tolua_isnumber(tolua_S,2,0,&tolua_err) ||
 !tolua_isnumber(tolua_S,3,0,&tolua_err) ||
 !tolua_isnumber(tolua_S,4,0,&tolua_err) ||
 !tolua_isnumber(tolua_S,5,0,&tolua_err) ||
 !tolua_isnoobj(tolua_S,6,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
  int ftype = ((int)  tolua_tonumber(tolua_S,1,0));
  byte rad = ((byte)  tolua_tonumber(tolua_S,2,0));
  int x = ((int)  tolua_tonumber(tolua_S,3,0));
  int y = ((int)  tolua_tonumber(tolua_S,4,0));
  s32b dam = ((s32b)  tolua_tonumber(tolua_S,5,0));
 {
  place_field(ftype,rad,x,y,dam);
 }
 }
 return 0;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'place_field'.",&tolua_err);
 return 0;
#endif
}

/* function: recharge_crystal */
static int tolua_init_recharge_crystal00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnoobj(tolua_S,1,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
 {
  recharge_crystal();
 }
 }
 return 0;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'recharge_crystal'.",&tolua_err);
 return 0;
#endif
}

/* function: object_eternality */
static int tolua_init_object_eternality00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnoobj(tolua_S,1,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
 {
  object_eternality();
 }
 }
 return 0;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'object_eternality'.",&tolua_err);
 return 0;
#endif
}

/* function: make_item_magic */
static int tolua_init_make_item_magic00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnoobj(tolua_S,1,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
 {
  make_item_magic();
 }
 }
 return 0;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'make_item_magic'.",&tolua_err);
 return 0;
#endif
}

/* function: make_item_levelable */
static int tolua_init_make_item_levelable00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnoobj(tolua_S,1,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
 {
  bool tolua_ret = (bool)  make_item_levelable();
 tolua_pushboolean(tolua_S,(bool)tolua_ret);
 }
 }
 return 1;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'make_item_levelable'.",&tolua_err);
 return 0;
#endif
}

/* function: place_field_monsters */
static int tolua_init_place_field_monsters00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnumber(tolua_S,1,0,&tolua_err) ||
 !tolua_isnumber(tolua_S,2,0,&tolua_err) ||
 !tolua_isnumber(tolua_S,3,0,&tolua_err) ||
 !tolua_isnumber(tolua_S,4,0,&tolua_err) ||
 !tolua_isnumber(tolua_S,5,0,&tolua_err) ||
 !tolua_isnoobj(tolua_S,6,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
  int ftype = ((int)  tolua_tonumber(tolua_S,1,0));
  byte rad = ((byte)  tolua_tonumber(tolua_S,2,0));
  int x = ((int)  tolua_tonumber(tolua_S,3,0));
  int y = ((int)  tolua_tonumber(tolua_S,4,0));
  s32b dam = ((s32b)  tolua_tonumber(tolua_S,5,0));
 {
  place_field_monsters(ftype,rad,x,y,dam);
 }
 }
 return 0;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'place_field_monsters'.",&tolua_err);
 return 0;
#endif
}

/* function: do_cmd_quaff_potion */
static int tolua_init_do_cmd_quaff_potion00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnoobj(tolua_S,1,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
 {
  do_cmd_quaff_potion();
 }
 }
 return 0;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'do_cmd_quaff_potion'.",&tolua_err);
 return 0;
#endif
}

/* function: do_cmd_read_scroll */
static int tolua_init_do_cmd_read_scroll00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnoobj(tolua_S,1,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
 {
  do_cmd_read_scroll();
 }
 }
 return 0;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'do_cmd_read_scroll'.",&tolua_err);
 return 0;
#endif
}

/* function: do_cmd_aim_wand */
static int tolua_init_do_cmd_aim_wand00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnoobj(tolua_S,1,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
 {
  do_cmd_aim_wand();
 }
 }
 return 0;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'do_cmd_aim_wand'.",&tolua_err);
 return 0;
#endif
}

/* function: do_cmd_use_staff */
static int tolua_init_do_cmd_use_staff00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnoobj(tolua_S,1,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
 {
  do_cmd_use_staff();
 }
 }
 return 0;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'do_cmd_use_staff'.",&tolua_err);
 return 0;
#endif
}

/* function: do_cmd_zap_rod */
static int tolua_init_do_cmd_zap_rod00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnoobj(tolua_S,1,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
 {
  do_cmd_zap_rod();
 }
 }
 return 0;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'do_cmd_zap_rod'.",&tolua_err);
 return 0;
#endif
}

/* function: do_cmd_activate */
static int tolua_init_do_cmd_activate00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnoobj(tolua_S,1,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
 {
  do_cmd_activate();
 }
 }
 return 0;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'do_cmd_activate'.",&tolua_err);
 return 0;
#endif
}

/* function: place_object */
static int tolua_init_place_object00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnumber(tolua_S,1,0,&tolua_err) ||
 !tolua_isnumber(tolua_S,2,0,&tolua_err) ||
 !tolua_isboolean(tolua_S,3,0,&tolua_err) ||
 !tolua_isboolean(tolua_S,4,0,&tolua_err) ||
 !tolua_isnoobj(tolua_S,5,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
  int y = ((int)  tolua_tonumber(tolua_S,1,0));
  int x = ((int)  tolua_tonumber(tolua_S,2,0));
  bool good = ((bool)  tolua_toboolean(tolua_S,3,0));
  bool great = ((bool)  tolua_toboolean(tolua_S,4,0));
 {
  place_object(y,x,good,great);
 }
 }
 return 0;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'place_object'.",&tolua_err);
 return 0;
#endif
}

/* function: place_gold */
static int tolua_init_place_gold00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnumber(tolua_S,1,0,&tolua_err) ||
 !tolua_isnumber(tolua_S,2,0,&tolua_err) ||
 !tolua_isnoobj(tolua_S,3,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
  int y = ((int)  tolua_tonumber(tolua_S,1,0));
  int x = ((int)  tolua_tonumber(tolua_S,2,0));
 {
  place_gold(y,x);
 }
 }
 return 0;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'place_gold'.",&tolua_err);
 return 0;
#endif
}

/* function: object_value */
static int tolua_init_object_value00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isusertype(tolua_S,1,"object_type",0,&tolua_err) ||
 !tolua_isnoobj(tolua_S,2,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
  object_type* o_ptr = ((object_type*)  tolua_tousertype(tolua_S,1,0));
 {
  s32b tolua_ret = (s32b)  object_value(o_ptr);
 tolua_pushnumber(tolua_S,(long)tolua_ret);
 }
 }
 return 1;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'object_value'.",&tolua_err);
 return 0;
#endif
}

/* function: object_value_real */
static int tolua_init_object_value_real00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isusertype(tolua_S,1,"object_type",0,&tolua_err) ||
 !tolua_isnoobj(tolua_S,2,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
  object_type* o_ptr = ((object_type*)  tolua_tousertype(tolua_S,1,0));
 {
  s32b tolua_ret = (s32b)  object_value_real(o_ptr);
 tolua_pushnumber(tolua_S,(long)tolua_ret);
 }
 }
 return 1;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'object_value_real'.",&tolua_err);
 return 0;
#endif
}

/* function: is_identified */
static int tolua_init_is_identified00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isusertype(tolua_S,1,"object_type",0,&tolua_err) ||
 !tolua_isnoobj(tolua_S,2,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
  object_type* o_ptr = ((object_type*)  tolua_tousertype(tolua_S,1,0));
 {
  bool tolua_ret = (bool)  is_identified(o_ptr);
 tolua_pushboolean(tolua_S,(bool)tolua_ret);
 }
 }
 return 1;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'is_identified'.",&tolua_err);
 return 0;
#endif
}

/* function: lua_pick_item */
static int tolua_init_lua_pick_item00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnumber(tolua_S,1,0,&tolua_err) ||
 !tolua_isnoobj(tolua_S,2,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
  int tval = ((int)  tolua_tonumber(tolua_S,1,0));
 {
  object_type* tolua_ret = (object_type*)  lua_pick_item(tval);
 tolua_pushusertype(tolua_S,(void*)tolua_ret,"object_type");
 }
 }
 return 1;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'lua_pick_item'.",&tolua_err);
 return 0;
#endif
}

/* function: lua_get_item */
static int tolua_init_lua_get_item00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnumber(tolua_S,1,0,&tolua_err) ||
 !tolua_isnoobj(tolua_S,2,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
  int tval = ((int)  tolua_tonumber(tolua_S,1,0));
 {
  int tolua_ret = (int)  lua_get_item(tval);
 tolua_pushnumber(tolua_S,(long)tolua_ret);
 }
 }
 return 1;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'lua_get_item'.",&tolua_err);
 return 0;
#endif
}

/* function: get_obj_num_tval */
static int tolua_init_get_obj_num_tval00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnumber(tolua_S,1,0,&tolua_err) ||
 !tolua_isnumber(tolua_S,2,0,&tolua_err) ||
 !tolua_isnoobj(tolua_S,3,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
  int tval = ((int)  tolua_tonumber(tolua_S,1,0));
  int level = ((int)  tolua_tonumber(tolua_S,2,0));
 {
  s16b tolua_ret = (s16b)  get_obj_num_tval(tval,level);
 tolua_pushnumber(tolua_S,(long)tolua_ret);
 }
 }
 return 1;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'get_obj_num_tval'.",&tolua_err);
 return 0;
#endif
}

/* function: make_object_tval */
static int tolua_init_make_object_tval00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isusertype(tolua_S,1,"object_type",0,&tolua_err) ||
 !tolua_isnumber(tolua_S,2,0,&tolua_err) ||
 !tolua_isboolean(tolua_S,3,0,&tolua_err) ||
 !tolua_isboolean(tolua_S,4,0,&tolua_err) ||
 !tolua_isnoobj(tolua_S,5,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
  object_type* j_ptr = ((object_type*)  tolua_tousertype(tolua_S,1,0));
  int tval = ((int)  tolua_tonumber(tolua_S,2,0));
  bool good = ((bool)  tolua_toboolean(tolua_S,3,0));
  bool great = ((bool)  tolua_toboolean(tolua_S,4,0));
 {
  bool tolua_ret = (bool)  make_object_tval(j_ptr,tval,good,great);
 tolua_pushboolean(tolua_S,(bool)tolua_ret);
 }
 }
 return 1;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'make_object_tval'.",&tolua_err);
 return 0;
#endif
}

/* function: place_object_tval */
static int tolua_init_place_object_tval00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnumber(tolua_S,1,0,&tolua_err) ||
 !tolua_isnumber(tolua_S,2,0,&tolua_err) ||
 !tolua_isnumber(tolua_S,3,0,&tolua_err) ||
 !tolua_isboolean(tolua_S,4,0,&tolua_err) ||
 !tolua_isboolean(tolua_S,5,0,&tolua_err) ||
 !tolua_isnoobj(tolua_S,6,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
  int y = ((int)  tolua_tonumber(tolua_S,1,0));
  int x = ((int)  tolua_tonumber(tolua_S,2,0));
  int tval = ((int)  tolua_tonumber(tolua_S,3,0));
  bool good = ((bool)  tolua_toboolean(tolua_S,4,0));
  bool great = ((bool)  tolua_toboolean(tolua_S,5,0));
 {
  place_object_tval(y,x,tval,good,great);
 }
 }
 return 0;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'place_object_tval'.",&tolua_err);
 return 0;
#endif
}

/* function: drop_global_object */
static int tolua_init_drop_global_object00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnumber(tolua_S,1,0,&tolua_err) ||
 !tolua_isnumber(tolua_S,2,0,&tolua_err) ||
 !tolua_isnoobj(tolua_S,3,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
  int x = ((int)  tolua_tonumber(tolua_S,1,0));
  int y = ((int)  tolua_tonumber(tolua_S,2,0));
 {
  drop_global_object(x,y);
 }
 }
 return 0;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'drop_global_object'.",&tolua_err);
 return 0;
#endif
}

/* function: drop_object_specific */
static int tolua_init_drop_object_specific00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnumber(tolua_S,1,0,&tolua_err) ||
 !tolua_isnumber(tolua_S,2,0,&tolua_err) ||
 !tolua_isnumber(tolua_S,3,0,&tolua_err) ||
 !tolua_isnumber(tolua_S,4,0,&tolua_err) ||
 !tolua_isnumber(tolua_S,5,0,&tolua_err) ||
 !tolua_isnumber(tolua_S,6,0,&tolua_err) ||
 !tolua_isnoobj(tolua_S,7,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
  int y = ((int)  tolua_tonumber(tolua_S,1,0));
  int x = ((int)  tolua_tonumber(tolua_S,2,0));
  int tval = ((int)  tolua_tonumber(tolua_S,3,0));
  int sval = ((int)  tolua_tonumber(tolua_S,4,0));
  int num = ((int)  tolua_tonumber(tolua_S,5,0));
  int magic = ((int)  tolua_tonumber(tolua_S,6,0));
 {
  drop_object_specific(y,x,tval,sval,num,magic);
 }
 }
 return 0;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'drop_object_specific'.",&tolua_err);
 return 0;
#endif
}

/* get function: summoner_monster */
static int tolua_get_summoner_monster_ptr(lua_State* tolua_S)
{
 tolua_pushusertype(tolua_S,(void*)summoner_monster,"monster_type");
 return 1;
}

/* set function: summoner_monster */
static int tolua_set_summoner_monster_ptr(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!tolua_isusertype(tolua_S,2,"monster_type",0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  summoner_monster = ((monster_type*)  tolua_tousertype(tolua_S,2,0));
 return 0;
}

/* function: get_wilderness_flag */
static int tolua_init_get_wilderness_flag00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnoobj(tolua_S,1,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
 {
  int tolua_ret = (int)  get_wilderness_flag();
 tolua_pushnumber(tolua_S,(long)tolua_ret);
 }
 }
 return 1;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'get_wilderness_flag'.",&tolua_err);
 return 0;
#endif
}

/* function: delete_monster_idx */
static int tolua_init_delete_monster_idx00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnumber(tolua_S,1,0,&tolua_err) ||
 !tolua_isnoobj(tolua_S,2,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
  int i = ((int)  tolua_tonumber(tolua_S,1,0));
 {
  delete_monster_idx(i);
 }
 }
 return 0;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'delete_monster_idx'.",&tolua_err);
 return 0;
#endif
}

/* function: delete_monster */
static int tolua_init_delete_monster00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnumber(tolua_S,1,0,&tolua_err) ||
 !tolua_isnumber(tolua_S,2,0,&tolua_err) ||
 !tolua_isnoobj(tolua_S,3,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
  int y = ((int)  tolua_tonumber(tolua_S,1,0));
  int x = ((int)  tolua_tonumber(tolua_S,2,0));
 {
  delete_monster(y,x);
 }
 }
 return 0;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'delete_monster'.",&tolua_err);
 return 0;
#endif
}

/* function: compact_monsters */
static int tolua_init_compact_monsters00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnumber(tolua_S,1,0,&tolua_err) ||
 !tolua_isnoobj(tolua_S,2,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
  int size = ((int)  tolua_tonumber(tolua_S,1,0));
 {
  compact_monsters(size);
 }
 }
 return 0;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'compact_monsters'.",&tolua_err);
 return 0;
#endif
}

/* function: wipe_m_list */
static int tolua_init_wipe_m_list00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnoobj(tolua_S,1,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
 {
  wipe_m_list();
 }
 }
 return 0;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'wipe_m_list'.",&tolua_err);
 return 0;
#endif
}

/* function: m_pop */
static int tolua_init_m_pop00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnoobj(tolua_S,1,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
 {
  s16b tolua_ret = (s16b)  m_pop();
 tolua_pushnumber(tolua_S,(long)tolua_ret);
 }
 }
 return 1;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'm_pop'.",&tolua_err);
 return 0;
#endif
}

/* function: get_mon_num_prep */
static int tolua_init_get_mon_num_prep00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnoobj(tolua_S,1,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
 {
  errr tolua_ret = (errr)  get_mon_num_prep();
 tolua_pushnumber(tolua_S,(long)tolua_ret);
 }
 }
 return 1;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'get_mon_num_prep'.",&tolua_err);
 return 0;
#endif
}

/* function: get_mon_num */
static int tolua_init_get_mon_num00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnumber(tolua_S,1,0,&tolua_err) ||
 !tolua_isnoobj(tolua_S,2,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
  int level = ((int)  tolua_tonumber(tolua_S,1,0));
 {
  s16b tolua_ret = (s16b)  get_mon_num(level);
 tolua_pushnumber(tolua_S,(long)tolua_ret);
 }
 }
 return 1;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'get_mon_num'.",&tolua_err);
 return 0;
#endif
}

/* function: monster_desc */
static int tolua_init_monster_desc00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isstring(tolua_S,1,0,&tolua_err) ||
 !tolua_isusertype(tolua_S,2,"monster_type",0,&tolua_err) ||
 !tolua_isnumber(tolua_S,3,0,&tolua_err) ||
 !tolua_isnoobj(tolua_S,4,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
  char* desc = ((char*)  tolua_tostring(tolua_S,1,0));
  monster_type* m_ptr = ((monster_type*)  tolua_tousertype(tolua_S,2,0));
  int mode = ((int)  tolua_tonumber(tolua_S,3,0));
 {
  monster_desc(desc,m_ptr,mode);
 }
 }
 return 0;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'monster_desc'.",&tolua_err);
 return 0;
#endif
}

/* function: monster_race_desc */
static int tolua_init_monster_race_desc00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isstring(tolua_S,1,0,&tolua_err) ||
 !tolua_isnumber(tolua_S,2,0,&tolua_err) ||
 !tolua_isnoobj(tolua_S,3,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
  char* desc = ((char*)  tolua_tostring(tolua_S,1,0));
  int r_idx = ((int)  tolua_tonumber(tolua_S,2,0));
 {
  monster_race_desc(desc,r_idx);
 }
 }
 return 0;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'monster_race_desc'.",&tolua_err);
 return 0;
#endif
}

/* function: lore_do_probe */
static int tolua_init_lore_do_probe00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnumber(tolua_S,1,0,&tolua_err) ||
 !tolua_isnoobj(tolua_S,2,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
  int m_idx = ((int)  tolua_tonumber(tolua_S,1,0));
 {
  lore_do_probe(m_idx);
 }
 }
 return 0;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'lore_do_probe'.",&tolua_err);
 return 0;
#endif
}

/* function: lore_treasure */
static int tolua_init_lore_treasure00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnumber(tolua_S,1,0,&tolua_err) ||
 !tolua_isnumber(tolua_S,2,0,&tolua_err) ||
 !tolua_isnumber(tolua_S,3,0,&tolua_err) ||
 !tolua_isnoobj(tolua_S,4,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
  int m_idx = ((int)  tolua_tonumber(tolua_S,1,0));
  int num_item = ((int)  tolua_tonumber(tolua_S,2,0));
  int num_gold = ((int)  tolua_tonumber(tolua_S,3,0));
 {
  lore_treasure(m_idx,num_item,num_gold);
 }
 }
 return 0;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'lore_treasure'.",&tolua_err);
 return 0;
#endif
}

/* function: update_mon */
static int tolua_init_update_mon00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnumber(tolua_S,1,0,&tolua_err) ||
 !tolua_isboolean(tolua_S,2,0,&tolua_err) ||
 !tolua_isnoobj(tolua_S,3,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
  int m_idx = ((int)  tolua_tonumber(tolua_S,1,0));
  bool full = ((bool)  tolua_toboolean(tolua_S,2,0));
 {
  update_mon(m_idx,full);
 }
 }
 return 0;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'update_mon'.",&tolua_err);
 return 0;
#endif
}

/* function: update_monsters */
static int tolua_init_update_monsters00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isboolean(tolua_S,1,0,&tolua_err) ||
 !tolua_isnoobj(tolua_S,2,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
  bool full = ((bool)  tolua_toboolean(tolua_S,1,0));
 {
  update_monsters(full);
 }
 }
 return 0;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'update_monsters'.",&tolua_err);
 return 0;
#endif
}

/* function: place_monster_aux */
static int tolua_init_place_monster_aux00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnumber(tolua_S,1,0,&tolua_err) ||
 !tolua_isnumber(tolua_S,2,0,&tolua_err) ||
 !tolua_isnumber(tolua_S,3,0,&tolua_err) ||
 !tolua_isboolean(tolua_S,4,0,&tolua_err) ||
 !tolua_isboolean(tolua_S,5,0,&tolua_err) ||
 !tolua_isboolean(tolua_S,6,0,&tolua_err) ||
 !tolua_isnumber(tolua_S,7,0,&tolua_err) ||
 !tolua_isnoobj(tolua_S,8,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
  int y = ((int)  tolua_tonumber(tolua_S,1,0));
  int x = ((int)  tolua_tonumber(tolua_S,2,0));
  int r_idx = ((int)  tolua_tonumber(tolua_S,3,0));
  bool slp = ((bool)  tolua_toboolean(tolua_S,4,0));
  bool grp = ((bool)  tolua_toboolean(tolua_S,5,0));
  bool charm = ((bool)  tolua_toboolean(tolua_S,6,0));
  int dur = ((int)  tolua_tonumber(tolua_S,7,0));
 {
  bool tolua_ret = (bool)  place_monster_aux(y,x,r_idx,slp,grp,charm,dur);
 tolua_pushboolean(tolua_S,(bool)tolua_ret);
 }
 }
 return 1;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'place_monster_aux'.",&tolua_err);
 return 0;
#endif
}

/* function: place_monster */
static int tolua_init_place_monster00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnumber(tolua_S,1,0,&tolua_err) ||
 !tolua_isnumber(tolua_S,2,0,&tolua_err) ||
 !tolua_isboolean(tolua_S,3,0,&tolua_err) ||
 !tolua_isboolean(tolua_S,4,0,&tolua_err) ||
 !tolua_isnumber(tolua_S,5,0,&tolua_err) ||
 !tolua_isnoobj(tolua_S,6,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
  int y = ((int)  tolua_tonumber(tolua_S,1,0));
  int x = ((int)  tolua_tonumber(tolua_S,2,0));
  bool slp = ((bool)  tolua_toboolean(tolua_S,3,0));
  bool grp = ((bool)  tolua_toboolean(tolua_S,4,0));
  int dur = ((int)  tolua_tonumber(tolua_S,5,0));
 {
  bool tolua_ret = (bool)  place_monster(y,x,slp,grp,dur);
 tolua_pushboolean(tolua_S,(bool)tolua_ret);
 }
 }
 return 1;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'place_monster'.",&tolua_err);
 return 0;
#endif
}

/* function: alloc_horde */
static int tolua_init_alloc_horde00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnumber(tolua_S,1,0,&tolua_err) ||
 !tolua_isnumber(tolua_S,2,0,&tolua_err) ||
 !tolua_isnoobj(tolua_S,3,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
  int y = ((int)  tolua_tonumber(tolua_S,1,0));
  int x = ((int)  tolua_tonumber(tolua_S,2,0));
 {
  bool tolua_ret = (bool)  alloc_horde(y,x);
 tolua_pushboolean(tolua_S,(bool)tolua_ret);
 }
 }
 return 1;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'alloc_horde'.",&tolua_err);
 return 0;
#endif
}

/* function: alloc_monster */
static int tolua_init_alloc_monster00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnumber(tolua_S,1,0,&tolua_err) ||
 !tolua_isboolean(tolua_S,2,0,&tolua_err) ||
 !tolua_isnoobj(tolua_S,3,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
  int dis = ((int)  tolua_tonumber(tolua_S,1,0));
  bool slp = ((bool)  tolua_toboolean(tolua_S,2,0));
 {
  bool tolua_ret = (bool)  alloc_monster(dis,slp);
 tolua_pushboolean(tolua_S,(bool)tolua_ret);
 }
 }
 return 1;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'alloc_monster'.",&tolua_err);
 return 0;
#endif
}

/* function: summon_specific */
static int tolua_init_summon_specific00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnumber(tolua_S,1,0,&tolua_err) ||
 !tolua_isnumber(tolua_S,2,0,&tolua_err) ||
 !tolua_isnumber(tolua_S,3,0,&tolua_err) ||
 !tolua_isnumber(tolua_S,4,0,&tolua_err) ||
 !tolua_isnumber(tolua_S,5,0,&tolua_err) ||
 !tolua_isnoobj(tolua_S,6,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
  int y1 = ((int)  tolua_tonumber(tolua_S,1,0));
  int x1 = ((int)  tolua_tonumber(tolua_S,2,0));
  int lev = ((int)  tolua_tonumber(tolua_S,3,0));
  int type = ((int)  tolua_tonumber(tolua_S,4,0));
  int dur = ((int)  tolua_tonumber(tolua_S,5,0));
 {
  bool tolua_ret = (bool)  summon_specific(y1,x1,lev,type,dur);
 tolua_pushboolean(tolua_S,(bool)tolua_ret);
 }
 }
 return 1;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'summon_specific'.",&tolua_err);
 return 0;
#endif
}

/* function: monster_swap */
static int tolua_init_monster_swap00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnumber(tolua_S,1,0,&tolua_err) ||
 !tolua_isnumber(tolua_S,2,0,&tolua_err) ||
 !tolua_isnumber(tolua_S,3,0,&tolua_err) ||
 !tolua_isnumber(tolua_S,4,0,&tolua_err) ||
 !tolua_isnoobj(tolua_S,5,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
  int y1 = ((int)  tolua_tonumber(tolua_S,1,0));
  int x1 = ((int)  tolua_tonumber(tolua_S,2,0));
  int y2 = ((int)  tolua_tonumber(tolua_S,3,0));
  int x2 = ((int)  tolua_tonumber(tolua_S,4,0));
 {
  monster_swap(y1,x1,y2,x2);
 }
 }
 return 0;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'monster_swap'.",&tolua_err);
 return 0;
#endif
}

/* function: multiply_monster */
static int tolua_init_multiply_monster00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnumber(tolua_S,1,0,&tolua_err) ||
 !tolua_isboolean(tolua_S,2,0,&tolua_err) ||
 !tolua_isboolean(tolua_S,3,0,&tolua_err) ||
 !tolua_isnoobj(tolua_S,4,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
  int m_idx = ((int)  tolua_tonumber(tolua_S,1,0));
  bool charm = ((bool)  tolua_toboolean(tolua_S,2,0));
  bool clone = ((bool)  tolua_toboolean(tolua_S,3,0));
 {
  bool tolua_ret = (bool)  multiply_monster(m_idx,charm,clone);
 tolua_pushboolean(tolua_S,(bool)tolua_ret);
 }
 }
 return 1;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'multiply_monster'.",&tolua_err);
 return 0;
#endif
}

/* function: update_smart_learn */
static int tolua_init_update_smart_learn00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnumber(tolua_S,1,0,&tolua_err) ||
 !tolua_isnumber(tolua_S,2,0,&tolua_err) ||
 !tolua_isnoobj(tolua_S,3,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
  int m_idx = ((int)  tolua_tonumber(tolua_S,1,0));
  int what = ((int)  tolua_tonumber(tolua_S,2,0));
 {
  update_smart_learn(m_idx,what);
 }
 }
 return 0;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'update_smart_learn'.",&tolua_err);
 return 0;
#endif
}

/* function: summon_specific_friendly */
static int tolua_init_summon_specific_friendly00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnumber(tolua_S,1,0,&tolua_err) ||
 !tolua_isnumber(tolua_S,2,0,&tolua_err) ||
 !tolua_isnumber(tolua_S,3,0,&tolua_err) ||
 !tolua_isnumber(tolua_S,4,0,&tolua_err) ||
 !tolua_isboolean(tolua_S,5,0,&tolua_err) ||
 !tolua_isnumber(tolua_S,6,0,&tolua_err) ||
 !tolua_isnoobj(tolua_S,7,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
  int y1 = ((int)  tolua_tonumber(tolua_S,1,0));
  int x1 = ((int)  tolua_tonumber(tolua_S,2,0));
  int lev = ((int)  tolua_tonumber(tolua_S,3,0));
  int type = ((int)  tolua_tonumber(tolua_S,4,0));
  bool Group_ok = ((bool)  tolua_toboolean(tolua_S,5,0));
  int dur = ((int)  tolua_tonumber(tolua_S,6,0));
 {
  bool tolua_ret = (bool)  summon_specific_friendly(y1,x1,lev,type,Group_ok,dur);
 tolua_pushboolean(tolua_S,(bool)tolua_ret);
 }
 }
 return 1;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'summon_specific_friendly'.",&tolua_err);
 return 0;
#endif
}

/* function: place_monster_one */
static int tolua_init_place_monster_one00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnumber(tolua_S,1,0,&tolua_err) ||
 !tolua_isnumber(tolua_S,2,0,&tolua_err) ||
 !tolua_isnumber(tolua_S,3,0,&tolua_err) ||
 !tolua_isboolean(tolua_S,4,0,&tolua_err) ||
 !tolua_isboolean(tolua_S,5,0,&tolua_err) ||
 !tolua_isnumber(tolua_S,6,0,&tolua_err) ||
 !tolua_isnoobj(tolua_S,7,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
  int y = ((int)  tolua_tonumber(tolua_S,1,0));
  int x = ((int)  tolua_tonumber(tolua_S,2,0));
  int r_idx = ((int)  tolua_tonumber(tolua_S,3,0));
  bool slp = ((bool)  tolua_toboolean(tolua_S,4,0));
  bool charm = ((bool)  tolua_toboolean(tolua_S,5,0));
  int dur = ((int)  tolua_tonumber(tolua_S,6,0));
 {
  bool tolua_ret = (bool)  place_monster_one(y,x,r_idx,slp,charm,dur);
 tolua_pushboolean(tolua_S,(bool)tolua_ret);
 }
 }
 return 1;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'place_monster_one'.",&tolua_err);
 return 0;
#endif
}

/* function: place_monster_one_return */
static int tolua_init_place_monster_one_return00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnumber(tolua_S,1,0,&tolua_err) ||
 !tolua_isnumber(tolua_S,2,0,&tolua_err) ||
 !tolua_isnumber(tolua_S,3,0,&tolua_err) ||
 !tolua_isboolean(tolua_S,4,0,&tolua_err) ||
 !tolua_isboolean(tolua_S,5,0,&tolua_err) ||
 !tolua_isnumber(tolua_S,6,0,&tolua_err) ||
 !tolua_isnumber(tolua_S,7,0,&tolua_err) ||
 !tolua_isnoobj(tolua_S,8,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
  int y = ((int)  tolua_tonumber(tolua_S,1,0));
  int x = ((int)  tolua_tonumber(tolua_S,2,0));
  int r_idx = ((int)  tolua_tonumber(tolua_S,3,0));
  bool slp = ((bool)  tolua_toboolean(tolua_S,4,0));
  bool charm = ((bool)  tolua_toboolean(tolua_S,5,0));
  int monlevel = ((int)  tolua_tonumber(tolua_S,6,0));
  int dur = ((int)  tolua_tonumber(tolua_S,7,0));
 {
  s16b tolua_ret = (s16b)  place_monster_one_return(y,x,r_idx,slp,charm,monlevel,dur);
 tolua_pushnumber(tolua_S,(long)tolua_ret);
 }
 }
 return 1;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'place_monster_one_return'.",&tolua_err);
 return 0;
#endif
}

/* function: player_place */
static int tolua_init_player_place00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnumber(tolua_S,1,0,&tolua_err) ||
 !tolua_isnumber(tolua_S,2,0,&tolua_err) ||
 !tolua_isnoobj(tolua_S,3,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
  int y = ((int)  tolua_tonumber(tolua_S,1,0));
  int x = ((int)  tolua_tonumber(tolua_S,2,0));
 {
  s16b tolua_ret = (s16b)  player_place(y,x);
 tolua_pushnumber(tolua_S,(long)tolua_ret);
 }
 }
 return 1;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'player_place'.",&tolua_err);
 return 0;
#endif
}

/* function: monster_drop_carried_objects */
static int tolua_init_monster_drop_carried_objects00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isusertype(tolua_S,1,"monster_type",0,&tolua_err) ||
 !tolua_isnoobj(tolua_S,2,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
  monster_type* m_ptr = ((monster_type*)  tolua_tousertype(tolua_S,1,0));
 {
  monster_drop_carried_objects(m_ptr);
 }
 }
 return 0;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'monster_drop_carried_objects'.",&tolua_err);
 return 0;
#endif
}

/* function: apply_monster_level_hp */
static int tolua_init_apply_monster_level_hp00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isusertype(tolua_S,1,"monster_type",0,&tolua_err) ||
 !tolua_isnoobj(tolua_S,2,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
  monster_type* m_ptr = ((monster_type*)  tolua_tousertype(tolua_S,1,0));
 {
  apply_monster_level_hp(m_ptr);
 }
 }
 return 0;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'apply_monster_level_hp'.",&tolua_err);
 return 0;
#endif
}

/* function: get_boss_ability */
static int tolua_init_get_boss_ability00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isusertype(tolua_S,1,"monster_type",0,&tolua_err) ||
 !tolua_isnumber(tolua_S,2,0,&tolua_err) ||
 !tolua_isnoobj(tolua_S,3,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
  monster_type* m_ptr = ((monster_type*)  tolua_tousertype(tolua_S,1,0));
  int number = ((int)  tolua_tonumber(tolua_S,2,0));
 {
  get_boss_ability(m_ptr,number);
 }
 }
 return 0;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'get_boss_ability'.",&tolua_err);
 return 0;
#endif
}

/* function: place_monster_one_no_boss */
static int tolua_init_place_monster_one_no_boss00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnumber(tolua_S,1,0,&tolua_err) ||
 !tolua_isnumber(tolua_S,2,0,&tolua_err) ||
 !tolua_isnumber(tolua_S,3,0,&tolua_err) ||
 !tolua_isboolean(tolua_S,4,0,&tolua_err) ||
 !tolua_isboolean(tolua_S,5,0,&tolua_err) ||
 !tolua_isnumber(tolua_S,6,0,&tolua_err) ||
 !tolua_isnoobj(tolua_S,7,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
  int y = ((int)  tolua_tonumber(tolua_S,1,0));
  int x = ((int)  tolua_tonumber(tolua_S,2,0));
  int r_idx = ((int)  tolua_tonumber(tolua_S,3,0));
  bool slp = ((bool)  tolua_toboolean(tolua_S,4,0));
  bool charm = ((bool)  tolua_toboolean(tolua_S,5,0));
  int dur = ((int)  tolua_tonumber(tolua_S,6,0));
 {
  bool tolua_ret = (bool)  place_monster_one_no_boss(y,x,r_idx,slp,charm,dur);
 tolua_pushboolean(tolua_S,(bool)tolua_ret);
 }
 }
 return 1;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'place_monster_one_no_boss'.",&tolua_err);
 return 0;
#endif
}

/* function: place_monster_one_return_no_boss */
static int tolua_init_place_monster_one_return_no_boss00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnumber(tolua_S,1,0,&tolua_err) ||
 !tolua_isnumber(tolua_S,2,0,&tolua_err) ||
 !tolua_isnumber(tolua_S,3,0,&tolua_err) ||
 !tolua_isboolean(tolua_S,4,0,&tolua_err) ||
 !tolua_isboolean(tolua_S,5,0,&tolua_err) ||
 !tolua_isnumber(tolua_S,6,0,&tolua_err) ||
 !tolua_isnumber(tolua_S,7,0,&tolua_err) ||
 !tolua_isnumber(tolua_S,8,0,&tolua_err) ||
 !tolua_isnumber(tolua_S,9,0,&tolua_err) ||
 !tolua_isnoobj(tolua_S,10,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
  int y = ((int)  tolua_tonumber(tolua_S,1,0));
  int x = ((int)  tolua_tonumber(tolua_S,2,0));
  int r_idx = ((int)  tolua_tonumber(tolua_S,3,0));
  bool slp = ((bool)  tolua_toboolean(tolua_S,4,0));
  bool charm = ((bool)  tolua_toboolean(tolua_S,5,0));
  int petlevel = ((int)  tolua_tonumber(tolua_S,6,0));
  s32b pethp = ((s32b)  tolua_tonumber(tolua_S,7,0));
  s32b petmaxhp = ((s32b)  tolua_tonumber(tolua_S,8,0));
  int dur = ((int)  tolua_tonumber(tolua_S,9,0));
 {
  s16b tolua_ret = (s16b)  place_monster_one_return_no_boss(y,x,r_idx,slp,charm,petlevel,pethp,petmaxhp,dur);
 tolua_pushnumber(tolua_S,(long)tolua_ret);
 }
 }
 return 1;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'place_monster_one_return_no_boss'.",&tolua_err);
 return 0;
#endif
}

/* function: place_monster_aux_no_boss */
static int tolua_init_place_monster_aux_no_boss00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnumber(tolua_S,1,0,&tolua_err) ||
 !tolua_isnumber(tolua_S,2,0,&tolua_err) ||
 !tolua_isnumber(tolua_S,3,0,&tolua_err) ||
 !tolua_isboolean(tolua_S,4,0,&tolua_err) ||
 !tolua_isboolean(tolua_S,5,0,&tolua_err) ||
 !tolua_isboolean(tolua_S,6,0,&tolua_err) ||
 !tolua_isnumber(tolua_S,7,0,&tolua_err) ||
 !tolua_isnoobj(tolua_S,8,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
  int y = ((int)  tolua_tonumber(tolua_S,1,0));
  int x = ((int)  tolua_tonumber(tolua_S,2,0));
  int r_idx = ((int)  tolua_tonumber(tolua_S,3,0));
  bool slp = ((bool)  tolua_toboolean(tolua_S,4,0));
  bool grp = ((bool)  tolua_toboolean(tolua_S,5,0));
  bool charm = ((bool)  tolua_toboolean(tolua_S,6,0));
  int dur = ((int)  tolua_tonumber(tolua_S,7,0));
 {
  bool tolua_ret = (bool)  place_monster_aux_no_boss(y,x,r_idx,slp,grp,charm,dur);
 tolua_pushboolean(tolua_S,(bool)tolua_ret);
 }
 }
 return 1;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'place_monster_aux_no_boss'.",&tolua_err);
 return 0;
#endif
}

/* function: place_monster_one_simulacrum */
static int tolua_init_place_monster_one_simulacrum00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnumber(tolua_S,1,0,&tolua_err) ||
 !tolua_isnumber(tolua_S,2,0,&tolua_err) ||
 !tolua_isnumber(tolua_S,3,0,&tolua_err) ||
 !tolua_isboolean(tolua_S,4,0,&tolua_err) ||
 !tolua_isboolean(tolua_S,5,0,&tolua_err) ||
 !tolua_isnumber(tolua_S,6,0,&tolua_err) ||
 !tolua_isnumber(tolua_S,7,0,&tolua_err) ||
 !tolua_isnumber(tolua_S,8,0,&tolua_err) ||
 !tolua_isnoobj(tolua_S,9,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
  int y = ((int)  tolua_tonumber(tolua_S,1,0));
  int x = ((int)  tolua_tonumber(tolua_S,2,0));
  int r_idx = ((int)  tolua_tonumber(tolua_S,3,0));
  bool slp = ((bool)  tolua_toboolean(tolua_S,4,0));
  bool charm = ((bool)  tolua_toboolean(tolua_S,5,0));
  int petlevel = ((int)  tolua_tonumber(tolua_S,6,0));
  s32b pethp = ((s32b)  tolua_tonumber(tolua_S,7,0));
  int dur = ((int)  tolua_tonumber(tolua_S,8,0));
 {
  s16b tolua_ret = (s16b)  place_monster_one_simulacrum(y,x,r_idx,slp,charm,petlevel,pethp,dur);
 tolua_pushnumber(tolua_S,(long)tolua_ret);
 }
 }
 return 1;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'place_monster_one_simulacrum'.",&tolua_err);
 return 0;
#endif
}

/* function: summon_specific_friendly_kind */
static int tolua_init_summon_specific_friendly_kind00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnumber(tolua_S,1,0,&tolua_err) ||
 !tolua_isnumber(tolua_S,2,0,&tolua_err) ||
 !tolua_isnumber(tolua_S,3,0,&tolua_err) ||
 !tolua_isnumber(tolua_S,4,0,&tolua_err) ||
 !tolua_isboolean(tolua_S,5,0,&tolua_err) ||
 !tolua_isnumber(tolua_S,6,0,&tolua_err) ||
 !tolua_isnoobj(tolua_S,7,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
  int y1 = ((int)  tolua_tonumber(tolua_S,1,0));
  int x1 = ((int)  tolua_tonumber(tolua_S,2,0));
  int lev = ((int)  tolua_tonumber(tolua_S,3,0));
  char kind = ((char)  tolua_tonumber(tolua_S,4,0));
  bool Group_ok = ((bool)  tolua_toboolean(tolua_S,5,0));
  int dur = ((int)  tolua_tonumber(tolua_S,6,0));
 {
  bool tolua_ret = (bool)  summon_specific_friendly_kind(y1,x1,lev,kind,Group_ok,dur);
 tolua_pushboolean(tolua_S,(bool)tolua_ret);
 }
 }
 return 1;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'summon_specific_friendly_kind'.",&tolua_err);
 return 0;
#endif
}

/* function: get_mon_num_kind */
static int tolua_init_get_mon_num_kind00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnumber(tolua_S,1,0,&tolua_err) ||
 !tolua_isnumber(tolua_S,2,0,&tolua_err) ||
 !tolua_isnoobj(tolua_S,3,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
  int lev = ((int)  tolua_tonumber(tolua_S,1,0));
  char kind = ((char)  tolua_tonumber(tolua_S,2,0));
 {
  int tolua_ret = (int)  get_mon_num_kind(lev,kind);
 tolua_pushnumber(tolua_S,(long)tolua_ret);
 }
 }
 return 1;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'get_mon_num_kind'.",&tolua_err);
 return 0;
#endif
}

/* function: summon_specific_friendly_name */
static int tolua_init_summon_specific_friendly_name00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnumber(tolua_S,1,0,&tolua_err) ||
 !tolua_isnumber(tolua_S,2,0,&tolua_err) ||
 !tolua_isstring(tolua_S,3,0,&tolua_err) ||
 !tolua_isboolean(tolua_S,4,0,&tolua_err) ||
 !tolua_isnumber(tolua_S,5,0,&tolua_err) ||
 !tolua_isnoobj(tolua_S,6,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
  int y1 = ((int)  tolua_tonumber(tolua_S,1,0));
  int x1 = ((int)  tolua_tonumber(tolua_S,2,0));
  char* name[30];
  bool Group_ok = ((bool)  tolua_toboolean(tolua_S,4,0));
  int dur = ((int)  tolua_tonumber(tolua_S,5,0));
 {
#ifndef TOLUA_RELEASE
 if (!tolua_isstringarray(tolua_S,3,30,0,&tolua_err))
 goto tolua_lerror;
 else
#endif
 {
 int i;
 for(i=0; i<30;i++)
  name[i] = ((char*)  tolua_tofieldstring(tolua_S,3,i+1,0));
 }
 }
 {
  bool tolua_ret = (bool)  summon_specific_friendly_name(y1,x1,name,Group_ok,dur);
 tolua_pushboolean(tolua_S,(bool)tolua_ret);
 }
 {
 int i;
 for(i=0; i<30;i++)
 tolua_pushfieldstring(tolua_S,3,i+1,(const char*) name[i]);
 }
 }
 return 1;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'summon_specific_friendly_name'.",&tolua_err);
 return 0;
#endif
}

/* function: get_mon_num_name */
static int tolua_init_get_mon_num_name00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isstring(tolua_S,1,0,&tolua_err) ||
 !tolua_isnoobj(tolua_S,2,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
  char* name[30];
 {
#ifndef TOLUA_RELEASE
 if (!tolua_isstringarray(tolua_S,1,30,0,&tolua_err))
 goto tolua_lerror;
 else
#endif
 {
 int i;
 for(i=0; i<30;i++)
  name[i] = ((char*)  tolua_tofieldstring(tolua_S,1,i+1,0));
 }
 }
 {
  int tolua_ret = (int)  get_mon_num_name(name);
 tolua_pushnumber(tolua_S,(long)tolua_ret);
 }
 {
 int i;
 for(i=0; i<30;i++)
 tolua_pushfieldstring(tolua_S,1,i+1,(const char*) name[i]);
 }
 }
 return 1;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'get_mon_num_name'.",&tolua_err);
 return 0;
#endif
}

/* function: place_monster_one_image */
static int tolua_init_place_monster_one_image00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnumber(tolua_S,1,0,&tolua_err) ||
 !tolua_isnumber(tolua_S,2,0,&tolua_err) ||
 !tolua_isnumber(tolua_S,3,0,&tolua_err) ||
 !tolua_isboolean(tolua_S,4,0,&tolua_err) ||
 !tolua_isboolean(tolua_S,5,0,&tolua_err) ||
 !tolua_isnumber(tolua_S,6,0,&tolua_err) ||
 !tolua_isnoobj(tolua_S,7,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
  int y = ((int)  tolua_tonumber(tolua_S,1,0));
  int x = ((int)  tolua_tonumber(tolua_S,2,0));
  int r_idx = ((int)  tolua_tonumber(tolua_S,3,0));
  bool slp = ((bool)  tolua_toboolean(tolua_S,4,0));
  bool charm = ((bool)  tolua_toboolean(tolua_S,5,0));
  int dur = ((int)  tolua_tonumber(tolua_S,6,0));
 {
  bool tolua_ret = (bool)  place_monster_one_image(y,x,r_idx,slp,charm,dur);
 tolua_pushboolean(tolua_S,(bool)tolua_ret);
 }
 }
 return 1;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'place_monster_one_image'.",&tolua_err);
 return 0;
#endif
}

/* function: place_monster_animated */
static int tolua_init_place_monster_animated00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnumber(tolua_S,1,0,&tolua_err) ||
 !tolua_isnumber(tolua_S,2,0,&tolua_err) ||
 !tolua_isnumber(tolua_S,3,0,&tolua_err) ||
 !tolua_isboolean(tolua_S,4,0,&tolua_err) ||
 !tolua_isboolean(tolua_S,5,0,&tolua_err) ||
 !tolua_isnumber(tolua_S,6,0,&tolua_err) ||
 !tolua_isnumber(tolua_S,7,0,&tolua_err) ||
 !tolua_isnumber(tolua_S,8,0,&tolua_err) ||
 !tolua_isnumber(tolua_S,9,0,&tolua_err) ||
 !tolua_isnoobj(tolua_S,10,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
  int y = ((int)  tolua_tonumber(tolua_S,1,0));
  int x = ((int)  tolua_tonumber(tolua_S,2,0));
  int r_idx = ((int)  tolua_tonumber(tolua_S,3,0));
  bool slp = ((bool)  tolua_toboolean(tolua_S,4,0));
  bool charm = ((bool)  tolua_toboolean(tolua_S,5,0));
  s32b basehp = ((s32b)  tolua_tonumber(tolua_S,6,0));
  s32b hit_bonus = ((s32b)  tolua_tonumber(tolua_S,7,0));
  int d_d = ((int)  tolua_tonumber(tolua_S,8,0));
  int d_s = ((int)  tolua_tonumber(tolua_S,9,0));
 {
  s16b tolua_ret = (s16b)  place_monster_animated(y,x,r_idx,slp,charm,basehp,hit_bonus,d_d,d_s);
 tolua_pushnumber(tolua_S,(long)tolua_ret);
 }
 }
 return 1;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'place_monster_animated'.",&tolua_err);
 return 0;
#endif
}

/* function: summon_specific_kind */
static int tolua_init_summon_specific_kind00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnumber(tolua_S,1,0,&tolua_err) ||
 !tolua_isnumber(tolua_S,2,0,&tolua_err) ||
 !tolua_isnumber(tolua_S,3,0,&tolua_err) ||
 !tolua_isnumber(tolua_S,4,0,&tolua_err) ||
 !tolua_isboolean(tolua_S,5,0,&tolua_err) ||
 !tolua_isboolean(tolua_S,6,0,&tolua_err) ||
 !tolua_isnumber(tolua_S,7,0,&tolua_err) ||
 !tolua_isnoobj(tolua_S,8,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
  int y1 = ((int)  tolua_tonumber(tolua_S,1,0));
  int x1 = ((int)  tolua_tonumber(tolua_S,2,0));
  int lev = ((int)  tolua_tonumber(tolua_S,3,0));
  char kind = ((char)  tolua_tonumber(tolua_S,4,0));
  bool Group_ok = ((bool)  tolua_toboolean(tolua_S,5,0));
  bool friendly = ((bool)  tolua_toboolean(tolua_S,6,0));
  int dur = ((int)  tolua_tonumber(tolua_S,7,0));
 {
  bool tolua_ret = (bool)  summon_specific_kind(y1,x1,lev,kind,Group_ok,friendly,dur);
 tolua_pushboolean(tolua_S,(bool)tolua_ret);
 }
 }
 return 1;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'summon_specific_kind'.",&tolua_err);
 return 0;
#endif
}

/* function: summon_specific_ridx */
static int tolua_init_summon_specific_ridx00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnumber(tolua_S,1,0,&tolua_err) ||
 !tolua_isnumber(tolua_S,2,0,&tolua_err) ||
 !tolua_isnumber(tolua_S,3,0,&tolua_err) ||
 !tolua_isboolean(tolua_S,4,0,&tolua_err) ||
 !tolua_isboolean(tolua_S,5,0,&tolua_err) ||
 !tolua_isnumber(tolua_S,6,0,&tolua_err) ||
 !tolua_isnoobj(tolua_S,7,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
  int y1 = ((int)  tolua_tonumber(tolua_S,1,0));
  int x1 = ((int)  tolua_tonumber(tolua_S,2,0));
  int ridx = ((int)  tolua_tonumber(tolua_S,3,0));
  bool Group_ok = ((bool)  tolua_toboolean(tolua_S,4,0));
  bool friendly = ((bool)  tolua_toboolean(tolua_S,5,0));
  int dur = ((int)  tolua_tonumber(tolua_S,6,0));
 {
  bool tolua_ret = (bool)  summon_specific_ridx(y1,x1,ridx,Group_ok,friendly,dur);
 tolua_pushboolean(tolua_S,(bool)tolua_ret);
 }
 }
 return 1;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'summon_specific_ridx'.",&tolua_err);
 return 0;
#endif
}

/* function: apply_monster_level_stats */
static int tolua_init_apply_monster_level_stats00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isusertype(tolua_S,1,"monster_type",0,&tolua_err) ||
 !tolua_isnoobj(tolua_S,2,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
  monster_type* m_ptr = ((monster_type*)  tolua_tousertype(tolua_S,1,0));
 {
  apply_monster_level_stats(m_ptr);
 }
 }
 return 0;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'apply_monster_level_stats'.",&tolua_err);
 return 0;
#endif
}

/* function: is_pet */
static int tolua_init_is_pet00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isusertype(tolua_S,1,"monster_type",0,&tolua_err) ||
 !tolua_isnoobj(tolua_S,2,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
  monster_type* m_ptr = ((monster_type*)  tolua_tousertype(tolua_S,1,0));
 {
  bool tolua_ret = (bool)  is_pet(m_ptr);
 tolua_pushboolean(tolua_S,(bool)tolua_ret);
 }
 }
 return 1;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'is_pet'.",&tolua_err);
 return 0;
#endif
}

/* function: set_pet */
static int tolua_init_set_pet00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isusertype(tolua_S,1,"monster_type",0,&tolua_err) ||
 !tolua_isboolean(tolua_S,2,0,&tolua_err) ||
 !tolua_isnoobj(tolua_S,3,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
  monster_type* m_ptr = ((monster_type*)  tolua_tousertype(tolua_S,1,0));
  bool pet = ((bool)  tolua_toboolean(tolua_S,2,0));
 {
  set_pet(m_ptr,pet);
 }
 }
 return 0;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'set_pet'.",&tolua_err);
 return 0;
#endif
}

/* function: boss_of_global_object */
static int tolua_init_boss_of_global_object00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnumber(tolua_S,1,0,&tolua_err) ||
 !tolua_isnoobj(tolua_S,2,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
  int r_idx = ((int)  tolua_tonumber(tolua_S,1,0));
 {
  boss_of_global_object(r_idx);
 }
 }
 return 0;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'boss_of_global_object'.",&tolua_err);
 return 0;
#endif
}

/* function: get_mon_num_rflag */
static int tolua_init_get_mon_num_rflag00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnumber(tolua_S,1,0,&tolua_err) ||
 !tolua_isnumber(tolua_S,2,0,&tolua_err) ||
 !tolua_isnoobj(tolua_S,3,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
  int lev = ((int)  tolua_tonumber(tolua_S,1,0));
  u32b rflag = ((u32b)  tolua_tonumber(tolua_S,2,0));
 {
  int tolua_ret = (int)  get_mon_num_rflag(lev,rflag);
 tolua_pushnumber(tolua_S,(long)tolua_ret);
 }
 }
 return 1;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'get_mon_num_rflag'.",&tolua_err);
 return 0;
#endif
}

/* function: summon_specific_rflag */
static int tolua_init_summon_specific_rflag00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnumber(tolua_S,1,0,&tolua_err) ||
 !tolua_isnumber(tolua_S,2,0,&tolua_err) ||
 !tolua_isnumber(tolua_S,3,0,&tolua_err) ||
 !tolua_isnumber(tolua_S,4,0,&tolua_err) ||
 !tolua_isboolean(tolua_S,5,0,&tolua_err) ||
 !tolua_isboolean(tolua_S,6,0,&tolua_err) ||
 !tolua_isnumber(tolua_S,7,0,&tolua_err) ||
 !tolua_isnoobj(tolua_S,8,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
  int y1 = ((int)  tolua_tonumber(tolua_S,1,0));
  int x1 = ((int)  tolua_tonumber(tolua_S,2,0));
  int lev = ((int)  tolua_tonumber(tolua_S,3,0));
  u32b rflag = ((u32b)  tolua_tonumber(tolua_S,4,0));
  bool Group_ok = ((bool)  tolua_toboolean(tolua_S,5,0));
  bool friendly = ((bool)  tolua_toboolean(tolua_S,6,0));
  int dur = ((int)  tolua_tonumber(tolua_S,7,0));
 {
  bool tolua_ret = (bool)  summon_specific_rflag(y1,x1,lev,rflag,Group_ok,friendly,dur);
 tolua_pushboolean(tolua_S,(bool)tolua_ret);
 }
 }
 return 1;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'summon_specific_rflag'.",&tolua_err);
 return 0;
#endif
}

/* function: get_race_kills */
static int tolua_init_get_race_kills00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnumber(tolua_S,1,0,&tolua_err) ||
 !tolua_isnoobj(tolua_S,2,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
  char mr = ((char)  tolua_tonumber(tolua_S,1,0));
 {
  int tolua_ret = (int)  get_race_kills(mr);
 tolua_pushnumber(tolua_S,(long)tolua_ret);
 }
 }
 return 1;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'get_race_kills'.",&tolua_err);
 return 0;
#endif
}

/* function: shield_has */
static int tolua_init_shield_has00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnoobj(tolua_S,1,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
 {
  bool tolua_ret = (bool)  shield_has();
 tolua_pushboolean(tolua_S,(bool)tolua_ret);
 }
 }
 return 1;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'shield_has'.",&tolua_err);
 return 0;
#endif
}

/* function: sword_has */
static int tolua_init_sword_has00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnoobj(tolua_S,1,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
 {
  bool tolua_ret = (bool)  sword_has();
 tolua_pushboolean(tolua_S,(bool)tolua_ret);
 }
 }
 return 1;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'sword_has'.",&tolua_err);
 return 0;
#endif
}

/* function: hafted_has */
static int tolua_init_hafted_has00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnoobj(tolua_S,1,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
 {
  bool tolua_ret = (bool)  hafted_has();
 tolua_pushboolean(tolua_S,(bool)tolua_ret);
 }
 }
 return 1;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'hafted_has'.",&tolua_err);
 return 0;
#endif
}

/* function: polearm_has */
static int tolua_init_polearm_has00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnoobj(tolua_S,1,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
 {
  bool tolua_ret = (bool)  polearm_has();
 tolua_pushboolean(tolua_S,(bool)tolua_ret);
 }
 }
 return 1;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'polearm_has'.",&tolua_err);
 return 0;
#endif
}

/* function: rod_has */
static int tolua_init_rod_has00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnoobj(tolua_S,1,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
 {
  bool tolua_ret = (bool)  rod_has();
 tolua_pushboolean(tolua_S,(bool)tolua_ret);
 }
 }
 return 1;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'rod_has'.",&tolua_err);
 return 0;
#endif
}

/* function: unarmed */
static int tolua_init_unarmed00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnoobj(tolua_S,1,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
 {
  bool tolua_ret = (bool)  unarmed();
 tolua_pushboolean(tolua_S,(bool)tolua_ret);
 }
 }
 return 1;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'unarmed'.",&tolua_err);
 return 0;
#endif
}

/* function: heavy_armor */
static int tolua_init_heavy_armor00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnoobj(tolua_S,1,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
 {
  bool tolua_ret = (bool)  heavy_armor();
 tolua_pushboolean(tolua_S,(bool)tolua_ret);
 }
 }
 return 1;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'heavy_armor'.",&tolua_err);
 return 0;
#endif
}

/* function: player_invis */
static int tolua_init_player_invis00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isusertype(tolua_S,1,"monster_type",0,&tolua_err) ||
 !tolua_isnoobj(tolua_S,2,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
  monster_type* m_ptr = ((monster_type*)  tolua_tousertype(tolua_S,1,0));
 {
  bool tolua_ret = (bool)  player_invis(m_ptr);
 tolua_pushboolean(tolua_S,(bool)tolua_ret);
 }
 }
 return 1;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'player_invis'.",&tolua_err);
 return 0;
#endif
}

/* function: lua_bolt */
static int tolua_init_lua_bolt00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnumber(tolua_S,1,0,&tolua_err) ||
 !tolua_isnumber(tolua_S,2,0,&tolua_err) ||
 !tolua_isnumber(tolua_S,3,0,&tolua_err) ||
 !tolua_isnoobj(tolua_S,4,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
  int m_idx = ((int)  tolua_tonumber(tolua_S,1,0));
  int typ = ((int)  tolua_tonumber(tolua_S,2,0));
  s32b dam_hp = ((s32b)  tolua_tonumber(tolua_S,3,0));
 {
  lua_bolt(m_idx,typ,dam_hp);
 }
 }
 return 0;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'lua_bolt'.",&tolua_err);
 return 0;
#endif
}

/* function: lua_ball */
static int tolua_init_lua_ball00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnumber(tolua_S,1,0,&tolua_err) ||
 !tolua_isnumber(tolua_S,2,0,&tolua_err) ||
 !tolua_isnumber(tolua_S,3,0,&tolua_err) ||
 !tolua_isnumber(tolua_S,4,0,&tolua_err) ||
 !tolua_isnoobj(tolua_S,5,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
  int m_idx = ((int)  tolua_tonumber(tolua_S,1,0));
  int typ = ((int)  tolua_tonumber(tolua_S,2,0));
  s32b dam_hp = ((s32b)  tolua_tonumber(tolua_S,3,0));
  int rad = ((int)  tolua_tonumber(tolua_S,4,0));
 {
  lua_ball(m_idx,typ,dam_hp,rad);
 }
 }
 return 0;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'lua_ball'.",&tolua_err);
 return 0;
#endif
}

/* function: poly_r_idx */
static int tolua_init_poly_r_idx00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnumber(tolua_S,1,0,&tolua_err) ||
 !tolua_isnoobj(tolua_S,2,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
  int r_idx = ((int)  tolua_tonumber(tolua_S,1,0));
 {
  s16b tolua_ret = (s16b)  poly_r_idx(r_idx);
 tolua_pushnumber(tolua_S,(long)tolua_ret);
 }
 }
 return 1;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'poly_r_idx'.",&tolua_err);
 return 0;
#endif
}

/* function: get_pos_player */
static int tolua_init_get_pos_player00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnumber(tolua_S,1,0,&tolua_err) ||
 !tolua_isnumber(tolua_S,2,0,&tolua_err) ||
 !tolua_isnumber(tolua_S,3,0,&tolua_err) ||
 !tolua_isnoobj(tolua_S,4,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
  int dis = ((int)  tolua_tonumber(tolua_S,1,0));
  int ny = ((int)  tolua_tonumber(tolua_S,2,0));
  int nx = ((int)  tolua_tonumber(tolua_S,3,0));
 {
  get_pos_player(dis,&ny,&nx);
 tolua_pushnumber(tolua_S,(long)ny);
 tolua_pushnumber(tolua_S,(long)nx);
 }
 }
 return 2;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'get_pos_player'.",&tolua_err);
 return 0;
#endif
}

/* function: teleport_to_player */
static int tolua_init_teleport_to_player00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnumber(tolua_S,1,0,&tolua_err) ||
 !tolua_isnoobj(tolua_S,2,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
  int m_idx = ((int)  tolua_tonumber(tolua_S,1,0));
 {
  teleport_to_player(m_idx);
 }
 }
 return 0;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'teleport_to_player'.",&tolua_err);
 return 0;
#endif
}

/* function: teleport_player_directed */
static int tolua_init_teleport_player_directed00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnumber(tolua_S,1,0,&tolua_err) ||
 !tolua_isnumber(tolua_S,2,0,&tolua_err) ||
 !tolua_isnoobj(tolua_S,3,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
  int rad = ((int)  tolua_tonumber(tolua_S,1,0));
  int dir = ((int)  tolua_tonumber(tolua_S,2,0));
 {
  teleport_player_directed(rad,dir);
 }
 }
 return 0;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'teleport_player_directed'.",&tolua_err);
 return 0;
#endif
}

/* function: teleport_away */
static int tolua_init_teleport_away00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnumber(tolua_S,1,0,&tolua_err) ||
 !tolua_isnumber(tolua_S,2,0,&tolua_err) ||
 !tolua_isnoobj(tolua_S,3,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
  int m_idx = ((int)  tolua_tonumber(tolua_S,1,0));
  int dis = ((int)  tolua_tonumber(tolua_S,2,0));
 {
  teleport_away(m_idx,dis);
 }
 }
 return 0;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'teleport_away'.",&tolua_err);
 return 0;
#endif
}

/* function: teleport_player */
static int tolua_init_teleport_player00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnumber(tolua_S,1,0,&tolua_err) ||
 !tolua_isnoobj(tolua_S,2,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
  int dis = ((int)  tolua_tonumber(tolua_S,1,0));
 {
  teleport_player(dis);
 }
 }
 return 0;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'teleport_player'.",&tolua_err);
 return 0;
#endif
}

/* function: teleport_player_to */
static int tolua_init_teleport_player_to00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnumber(tolua_S,1,0,&tolua_err) ||
 !tolua_isnumber(tolua_S,2,0,&tolua_err) ||
 !tolua_isnoobj(tolua_S,3,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
  int ny = ((int)  tolua_tonumber(tolua_S,1,0));
  int nx = ((int)  tolua_tonumber(tolua_S,2,0));
 {
  teleport_player_to(ny,nx);
 }
 }
 return 0;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'teleport_player_to'.",&tolua_err);
 return 0;
#endif
}

/* function: teleport_monster_to */
static int tolua_init_teleport_monster_to00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnumber(tolua_S,1,0,&tolua_err) ||
 !tolua_isnumber(tolua_S,2,0,&tolua_err) ||
 !tolua_isnumber(tolua_S,3,0,&tolua_err) ||
 !tolua_isnoobj(tolua_S,4,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
  int m_idx = ((int)  tolua_tonumber(tolua_S,1,0));
  int ny = ((int)  tolua_tonumber(tolua_S,2,0));
  int nx = ((int)  tolua_tonumber(tolua_S,3,0));
 {
  teleport_monster_to(m_idx,ny,nx);
 }
 }
 return 0;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'teleport_monster_to'.",&tolua_err);
 return 0;
#endif
}

/* function: teleport_player_level */
static int tolua_init_teleport_player_level00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnoobj(tolua_S,1,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
 {
  teleport_player_level();
 }
 }
 return 0;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'teleport_player_level'.",&tolua_err);
 return 0;
#endif
}

/* function: recall_player */
static int tolua_init_recall_player00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnoobj(tolua_S,1,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
 {
  recall_player();
 }
 }
 return 0;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'recall_player'.",&tolua_err);
 return 0;
#endif
}

/* function: take_hit */
static int tolua_init_take_hit00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnumber(tolua_S,1,0,&tolua_err) ||
 !tolua_isstring(tolua_S,2,0,&tolua_err) ||
 !tolua_isnoobj(tolua_S,3,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
  s32b damage = ((s32b)  tolua_tonumber(tolua_S,1,0));
  cptr kb_str = ((cptr)  tolua_tostring(tolua_S,2,0));
 {
  take_hit(damage,kb_str);
 }
 }
 return 0;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'take_hit'.",&tolua_err);
 return 0;
#endif
}

/* function: acid_dam */
static int tolua_init_acid_dam00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnumber(tolua_S,1,0,&tolua_err) ||
 !tolua_isstring(tolua_S,2,0,&tolua_err) ||
 !tolua_isnoobj(tolua_S,3,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
  s32b dam = ((s32b)  tolua_tonumber(tolua_S,1,0));
  cptr kb_str = ((cptr)  tolua_tostring(tolua_S,2,0));
 {
  acid_dam(dam,kb_str);
 }
 }
 return 0;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'acid_dam'.",&tolua_err);
 return 0;
#endif
}

/* function: elec_dam */
static int tolua_init_elec_dam00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnumber(tolua_S,1,0,&tolua_err) ||
 !tolua_isstring(tolua_S,2,0,&tolua_err) ||
 !tolua_isnoobj(tolua_S,3,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
  s32b dam = ((s32b)  tolua_tonumber(tolua_S,1,0));
  cptr kb_str = ((cptr)  tolua_tostring(tolua_S,2,0));
 {
  elec_dam(dam,kb_str);
 }
 }
 return 0;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'elec_dam'.",&tolua_err);
 return 0;
#endif
}

/* function: fire_dam */
static int tolua_init_fire_dam00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnumber(tolua_S,1,0,&tolua_err) ||
 !tolua_isstring(tolua_S,2,0,&tolua_err) ||
 !tolua_isnoobj(tolua_S,3,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
  s32b dam = ((s32b)  tolua_tonumber(tolua_S,1,0));
  cptr kb_str = ((cptr)  tolua_tostring(tolua_S,2,0));
 {
  fire_dam(dam,kb_str);
 }
 }
 return 0;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'fire_dam'.",&tolua_err);
 return 0;
#endif
}

/* function: cold_dam */
static int tolua_init_cold_dam00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnumber(tolua_S,1,0,&tolua_err) ||
 !tolua_isstring(tolua_S,2,0,&tolua_err) ||
 !tolua_isnoobj(tolua_S,3,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
  s32b dam = ((s32b)  tolua_tonumber(tolua_S,1,0));
  cptr kb_str = ((cptr)  tolua_tostring(tolua_S,2,0));
 {
  cold_dam(dam,kb_str);
 }
 }
 return 0;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'cold_dam'.",&tolua_err);
 return 0;
#endif
}

/* function: inc_stat */
static int tolua_init_inc_stat00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnumber(tolua_S,1,0,&tolua_err) ||
 !tolua_isnoobj(tolua_S,2,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
  int stat = ((int)  tolua_tonumber(tolua_S,1,0));
 {
  bool tolua_ret = (bool)  inc_stat(stat);
 tolua_pushboolean(tolua_S,(bool)tolua_ret);
 }
 }
 return 1;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'inc_stat'.",&tolua_err);
 return 0;
#endif
}

/* function: dec_stat */
static int tolua_init_dec_stat00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnumber(tolua_S,1,0,&tolua_err) ||
 !tolua_isnumber(tolua_S,2,0,&tolua_err) ||
 !tolua_isnumber(tolua_S,3,0,&tolua_err) ||
 !tolua_isnoobj(tolua_S,4,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
  int stat = ((int)  tolua_tonumber(tolua_S,1,0));
  int amount = ((int)  tolua_tonumber(tolua_S,2,0));
  int mode = ((int)  tolua_tonumber(tolua_S,3,0));
 {
  bool tolua_ret = (bool)  dec_stat(stat,amount,mode);
 tolua_pushboolean(tolua_S,(bool)tolua_ret);
 }
 }
 return 1;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'dec_stat'.",&tolua_err);
 return 0;
#endif
}

/* function: res_stat */
static int tolua_init_res_stat00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnumber(tolua_S,1,0,&tolua_err) ||
 !tolua_isnoobj(tolua_S,2,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
  int stat = ((int)  tolua_tonumber(tolua_S,1,0));
 {
  bool tolua_ret = (bool)  res_stat(stat);
 tolua_pushboolean(tolua_S,(bool)tolua_ret);
 }
 }
 return 1;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'res_stat'.",&tolua_err);
 return 0;
#endif
}

/* function: apply_disenchant */
static int tolua_init_apply_disenchant00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnumber(tolua_S,1,0,&tolua_err) ||
 !tolua_isnoobj(tolua_S,2,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
  int mode = ((int)  tolua_tonumber(tolua_S,1,0));
 {
  bool tolua_ret = (bool)  apply_disenchant(mode);
 tolua_pushboolean(tolua_S,(bool)tolua_ret);
 }
 }
 return 1;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'apply_disenchant'.",&tolua_err);
 return 0;
#endif
}

/* function: project_m */
static int tolua_init_project_m00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnumber(tolua_S,1,0,&tolua_err) ||
 !tolua_isnumber(tolua_S,2,0,&tolua_err) ||
 !tolua_isnumber(tolua_S,3,0,&tolua_err) ||
 !tolua_isnumber(tolua_S,4,0,&tolua_err) ||
 !tolua_isnumber(tolua_S,5,0,&tolua_err) ||
 !tolua_isnumber(tolua_S,6,0,&tolua_err) ||
 !tolua_isnoobj(tolua_S,7,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
  int who = ((int)  tolua_tonumber(tolua_S,1,0));
  int r = ((int)  tolua_tonumber(tolua_S,2,0));
  int y = ((int)  tolua_tonumber(tolua_S,3,0));
  int x = ((int)  tolua_tonumber(tolua_S,4,0));
  s32b dam = ((s32b)  tolua_tonumber(tolua_S,5,0));
  int typ = ((int)  tolua_tonumber(tolua_S,6,0));
 {
  bool tolua_ret = (bool)  project_m(who,r,y,x,dam,typ);
 tolua_pushboolean(tolua_S,(bool)tolua_ret);
 }
 }
 return 1;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'project_m'.",&tolua_err);
 return 0;
#endif
}

/* function: project_path */
static int tolua_init_project_path00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnumber(tolua_S,1,0,&tolua_err) ||
 !tolua_isnumber(tolua_S,2,0,&tolua_err) ||
 !tolua_isnumber(tolua_S,3,0,&tolua_err) ||
 !tolua_isnumber(tolua_S,4,0,&tolua_err) ||
 !tolua_isnumber(tolua_S,5,0,&tolua_err) ||
 !tolua_isnumber(tolua_S,6,0,&tolua_err) ||
 !tolua_isnumber(tolua_S,7,0,&tolua_err) ||
 !tolua_isnoobj(tolua_S,8,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
  u16b gp = ((u16b)  tolua_tonumber(tolua_S,1,0));
  int range = ((int)  tolua_tonumber(tolua_S,2,0));
  int y1 = ((int)  tolua_tonumber(tolua_S,3,0));
  int x1 = ((int)  tolua_tonumber(tolua_S,4,0));
  int y2 = ((int)  tolua_tonumber(tolua_S,5,0));
  int x2 = ((int)  tolua_tonumber(tolua_S,6,0));
  int flg = ((int)  tolua_tonumber(tolua_S,7,0));
 {
  sint tolua_ret = (sint)  project_path(&gp,range,y1,x1,y2,x2,flg);
 {
#ifdef __cplusplus
 void* tolua_obj = new sint(tolua_ret);
 tolua_pushusertype(tolua_S,tolua_clone(tolua_S,tolua_obj,tolua_collect_sint),"sint");
#else
 void* tolua_obj = tolua_copy(tolua_S,(void*)&tolua_ret,sizeof(sint));
 tolua_pushusertype(tolua_S,tolua_clone(tolua_S,tolua_obj,NULL),"sint");
#endif
 }
 tolua_pushnumber(tolua_S,(long)gp);
 }
 }
 return 2;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'project_path'.",&tolua_err);
 return 0;
#endif
}

/* function: project */
static int tolua_init_project00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnumber(tolua_S,1,0,&tolua_err) ||
 !tolua_isnumber(tolua_S,2,0,&tolua_err) ||
 !tolua_isnumber(tolua_S,3,0,&tolua_err) ||
 !tolua_isnumber(tolua_S,4,0,&tolua_err) ||
 !tolua_isnumber(tolua_S,5,0,&tolua_err) ||
 !tolua_isnumber(tolua_S,6,0,&tolua_err) ||
 !tolua_isnumber(tolua_S,7,0,&tolua_err) ||
 !tolua_isnoobj(tolua_S,8,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
  int who = ((int)  tolua_tonumber(tolua_S,1,0));
  int rad = ((int)  tolua_tonumber(tolua_S,2,0));
  int y = ((int)  tolua_tonumber(tolua_S,3,0));
  int x = ((int)  tolua_tonumber(tolua_S,4,0));
  s32b dam = ((s32b)  tolua_tonumber(tolua_S,5,0));
  int typ = ((int)  tolua_tonumber(tolua_S,6,0));
  int flg = ((int)  tolua_tonumber(tolua_S,7,0));
 {
  bool tolua_ret = (bool)  project(who,rad,y,x,dam,typ,flg);
 tolua_pushboolean(tolua_S,(bool)tolua_ret);
 }
 }
 return 1;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'project'.",&tolua_err);
 return 0;
#endif
}

/* function: mutate_player */
static int tolua_init_mutate_player01(lua_State* tolua_S)
{
 tolua_Error tolua_err;
 if (
 !tolua_isnoobj(tolua_S,1,&tolua_err)
 )
 goto tolua_lerror;
 else
 {
 {
  mutate_player();
 }
 }
 return 0;
tolua_lerror:
 return tolua_init_mutate_player00(tolua_S);
}

/* function: generate_spell */
static int tolua_init_generate_spell00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnumber(tolua_S,1,0,&tolua_err) ||
 !tolua_isnoobj(tolua_S,2,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
  int plev = ((int)  tolua_tonumber(tolua_S,1,0));
 {
  generate_spell(plev);
 }
 }
 return 0;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'generate_spell'.",&tolua_err);
 return 0;
#endif
}

/* get function: unsafe */
static int tolua_get_unsafe(lua_State* tolua_S)
{
 tolua_pushboolean(tolua_S,(bool)unsafe);
 return 1;
}

/* set function: unsafe */
static int tolua_set_unsafe(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!tolua_isboolean(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  unsafe = ((bool)  tolua_toboolean(tolua_S,2,0));
 return 0;
}

/* function: describe_attack_fully */
static int tolua_init_describe_attack_fully00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnumber(tolua_S,1,0,&tolua_err) ||
 !tolua_isstring(tolua_S,2,0,&tolua_err) ||
 !tolua_isnoobj(tolua_S,3,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
  int type = ((int)  tolua_tonumber(tolua_S,1,0));
  char* r = ((char*)  tolua_tostring(tolua_S,2,0));
 {
  describe_attack_fully(type,r);
 }
 }
 return 0;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'describe_attack_fully'.",&tolua_err);
 return 0;
#endif
}

/* function: lord_piercing */
static int tolua_init_lord_piercing00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnumber(tolua_S,1,0,&tolua_err) ||
 !tolua_isnumber(tolua_S,2,0,&tolua_err) ||
 !tolua_isnumber(tolua_S,3,0,&tolua_err) ||
 !tolua_isusertype(tolua_S,4,"monster_type",0,&tolua_err) ||
 !tolua_isnumber(tolua_S,5,0,&tolua_err) ||
 !tolua_isnoobj(tolua_S,6,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
  int basechance = ((int)  tolua_tonumber(tolua_S,1,0));
  int factor = ((int)  tolua_tonumber(tolua_S,2,0));
  int typ = ((int)  tolua_tonumber(tolua_S,3,0));
  monster_type* m_ptr = ((monster_type*)  tolua_tousertype(tolua_S,4,0));
  int checktype = ((int)  tolua_tonumber(tolua_S,5,0));
 {
  bool tolua_ret = (bool)  lord_piercing(basechance,factor,typ,m_ptr,checktype);
 tolua_pushboolean(tolua_S,(bool)tolua_ret);
 }
 }
 return 1;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'lord_piercing'.",&tolua_err);
 return 0;
#endif
}

/* function: grow_trees */
static int tolua_init_grow_trees00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnumber(tolua_S,1,0,&tolua_err) ||
 !tolua_isnoobj(tolua_S,2,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
  int rad = ((int)  tolua_tonumber(tolua_S,1,0));
 {
  grow_trees(rad);
 }
 }
 return 0;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'grow_trees'.",&tolua_err);
 return 0;
#endif
}

/* function: hp_player */
static int tolua_init_hp_player00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnumber(tolua_S,1,0,&tolua_err) ||
 !tolua_isnoobj(tolua_S,2,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
  int num = ((int)  tolua_tonumber(tolua_S,1,0));
 {
  bool tolua_ret = (bool)  hp_player(num);
 tolua_pushboolean(tolua_S,(bool)tolua_ret);
 }
 }
 return 1;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'hp_player'.",&tolua_err);
 return 0;
#endif
}

/* function: warding_glyph */
static int tolua_init_warding_glyph00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnoobj(tolua_S,1,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
 {
  warding_glyph();
 }
 }
 return 0;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'warding_glyph'.",&tolua_err);
 return 0;
#endif
}

/* function: explosive_rune */
static int tolua_init_explosive_rune00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnoobj(tolua_S,1,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
 {
  explosive_rune();
 }
 }
 return 0;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'explosive_rune'.",&tolua_err);
 return 0;
#endif
}

/* function: do_dec_stat */
static int tolua_init_do_dec_stat00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnumber(tolua_S,1,0,&tolua_err) ||
 !tolua_isnumber(tolua_S,2,0,&tolua_err) ||
 !tolua_isnoobj(tolua_S,3,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
  int stat = ((int)  tolua_tonumber(tolua_S,1,0));
  int mode = ((int)  tolua_tonumber(tolua_S,2,0));
 {
  bool tolua_ret = (bool)  do_dec_stat(stat,mode);
 tolua_pushboolean(tolua_S,(bool)tolua_ret);
 }
 }
 return 1;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'do_dec_stat'.",&tolua_err);
 return 0;
#endif
}

/* function: do_res_stat */
static int tolua_init_do_res_stat00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnumber(tolua_S,1,0,&tolua_err) ||
 !tolua_isnoobj(tolua_S,2,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
  int stat = ((int)  tolua_tonumber(tolua_S,1,0));
 {
  bool tolua_ret = (bool)  do_res_stat(stat);
 tolua_pushboolean(tolua_S,(bool)tolua_ret);
 }
 }
 return 1;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'do_res_stat'.",&tolua_err);
 return 0;
#endif
}

/* function: do_inc_stat */
static int tolua_init_do_inc_stat00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnumber(tolua_S,1,0,&tolua_err) ||
 !tolua_isnoobj(tolua_S,2,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
  int stat = ((int)  tolua_tonumber(tolua_S,1,0));
 {
  bool tolua_ret = (bool)  do_inc_stat(stat);
 tolua_pushboolean(tolua_S,(bool)tolua_ret);
 }
 }
 return 1;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'do_inc_stat'.",&tolua_err);
 return 0;
#endif
}

/* function: identify_pack */
static int tolua_init_identify_pack00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnoobj(tolua_S,1,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
 {
  identify_pack();
 }
 }
 return 0;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'identify_pack'.",&tolua_err);
 return 0;
#endif
}

/* function: message_pain */
static int tolua_init_message_pain00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnumber(tolua_S,1,0,&tolua_err) ||
 !tolua_isnumber(tolua_S,2,0,&tolua_err) ||
 !tolua_isnoobj(tolua_S,3,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
  int m_idx = ((int)  tolua_tonumber(tolua_S,1,0));
  s32b dam = ((s32b)  tolua_tonumber(tolua_S,2,0));
 {
  message_pain(m_idx,dam);
 }
 }
 return 0;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'message_pain'.",&tolua_err);
 return 0;
#endif
}

/* function: remove_curse */
static int tolua_init_remove_curse00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnoobj(tolua_S,1,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
 {
  bool tolua_ret = (bool)  remove_curse();
 tolua_pushboolean(tolua_S,(bool)tolua_ret);
 }
 }
 return 1;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'remove_curse'.",&tolua_err);
 return 0;
#endif
}

/* function: remove_all_curse */
static int tolua_init_remove_all_curse00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnoobj(tolua_S,1,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
 {
  bool tolua_ret = (bool)  remove_all_curse();
 tolua_pushboolean(tolua_S,(bool)tolua_ret);
 }
 }
 return 1;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'remove_all_curse'.",&tolua_err);
 return 0;
#endif
}

/* function: restore_level */
static int tolua_init_restore_level00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnoobj(tolua_S,1,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
 {
  bool tolua_ret = (bool)  restore_level();
 tolua_pushboolean(tolua_S,(bool)tolua_ret);
 }
 }
 return 1;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'restore_level'.",&tolua_err);
 return 0;
#endif
}

/* function: self_knowledge */
static int tolua_init_self_knowledge00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isusertype(tolua_S,1,"FILE",0,&tolua_err) ||
 !tolua_isnoobj(tolua_S,2,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
  FILE* fff = ((FILE*)  tolua_tousertype(tolua_S,1,0));
 {
  self_knowledge(fff);
 }
 }
 return 0;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'self_knowledge'.",&tolua_err);
 return 0;
#endif
}

/* function: lose_all_info */
static int tolua_init_lose_all_info00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnoobj(tolua_S,1,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
 {
  bool tolua_ret = (bool)  lose_all_info();
 tolua_pushboolean(tolua_S,(bool)tolua_ret);
 }
 }
 return 1;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'lose_all_info'.",&tolua_err);
 return 0;
#endif
}

/* function: detect_traps */
static int tolua_init_detect_traps00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnoobj(tolua_S,1,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
 {
  bool tolua_ret = (bool)  detect_traps();
 tolua_pushboolean(tolua_S,(bool)tolua_ret);
 }
 }
 return 1;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'detect_traps'.",&tolua_err);
 return 0;
#endif
}

/* function: detect_doors */
static int tolua_init_detect_doors00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnoobj(tolua_S,1,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
 {
  bool tolua_ret = (bool)  detect_doors();
 tolua_pushboolean(tolua_S,(bool)tolua_ret);
 }
 }
 return 1;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'detect_doors'.",&tolua_err);
 return 0;
#endif
}

/* function: detect_stairs */
static int tolua_init_detect_stairs00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnoobj(tolua_S,1,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
 {
  bool tolua_ret = (bool)  detect_stairs();
 tolua_pushboolean(tolua_S,(bool)tolua_ret);
 }
 }
 return 1;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'detect_stairs'.",&tolua_err);
 return 0;
#endif
}

/* function: detect_treasure */
static int tolua_init_detect_treasure00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnoobj(tolua_S,1,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
 {
  bool tolua_ret = (bool)  detect_treasure();
 tolua_pushboolean(tolua_S,(bool)tolua_ret);
 }
 }
 return 1;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'detect_treasure'.",&tolua_err);
 return 0;
#endif
}

/* function: detect_objects_gold */
static int tolua_init_detect_objects_gold00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnoobj(tolua_S,1,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
 {
  bool tolua_ret = (bool)  detect_objects_gold();
 tolua_pushboolean(tolua_S,(bool)tolua_ret);
 }
 }
 return 1;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'detect_objects_gold'.",&tolua_err);
 return 0;
#endif
}

/* function: detect_objects_normal */
static int tolua_init_detect_objects_normal00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnoobj(tolua_S,1,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
 {
  bool tolua_ret = (bool)  detect_objects_normal();
 tolua_pushboolean(tolua_S,(bool)tolua_ret);
 }
 }
 return 1;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'detect_objects_normal'.",&tolua_err);
 return 0;
#endif
}

/* function: detect_objects_magic */
static int tolua_init_detect_objects_magic00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnoobj(tolua_S,1,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
 {
  bool tolua_ret = (bool)  detect_objects_magic();
 tolua_pushboolean(tolua_S,(bool)tolua_ret);
 }
 }
 return 1;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'detect_objects_magic'.",&tolua_err);
 return 0;
#endif
}

/* function: detect_monsters_normal */
static int tolua_init_detect_monsters_normal00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnoobj(tolua_S,1,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
 {
  bool tolua_ret = (bool)  detect_monsters_normal();
 tolua_pushboolean(tolua_S,(bool)tolua_ret);
 }
 }
 return 1;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'detect_monsters_normal'.",&tolua_err);
 return 0;
#endif
}

/* function: detect_monsters_invis */
static int tolua_init_detect_monsters_invis00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnoobj(tolua_S,1,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
 {
  bool tolua_ret = (bool)  detect_monsters_invis();
 tolua_pushboolean(tolua_S,(bool)tolua_ret);
 }
 }
 return 1;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'detect_monsters_invis'.",&tolua_err);
 return 0;
#endif
}

/* function: detect_monsters_evil */
static int tolua_init_detect_monsters_evil00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnoobj(tolua_S,1,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
 {
  bool tolua_ret = (bool)  detect_monsters_evil();
 tolua_pushboolean(tolua_S,(bool)tolua_ret);
 }
 }
 return 1;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'detect_monsters_evil'.",&tolua_err);
 return 0;
#endif
}

/* function: detect_monsters_xxx */
static int tolua_init_detect_monsters_xxx00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnumber(tolua_S,1,0,&tolua_err) ||
 !tolua_isnoobj(tolua_S,2,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
  u32b match_flag = ((u32b)  tolua_tonumber(tolua_S,1,0));
 {
  bool tolua_ret = (bool)  detect_monsters_xxx(match_flag);
 tolua_pushboolean(tolua_S,(bool)tolua_ret);
 }
 }
 return 1;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'detect_monsters_xxx'.",&tolua_err);
 return 0;
#endif
}

/* function: detect_monsters_string */
static int tolua_init_detect_monsters_string00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isstring(tolua_S,1,0,&tolua_err) ||
 !tolua_isnoobj(tolua_S,2,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
  cptr tolua_var_1 = ((cptr)  tolua_tostring(tolua_S,1,0));
 {
  bool tolua_ret = (bool)  detect_monsters_string(tolua_var_1);
 tolua_pushboolean(tolua_S,(bool)tolua_ret);
 }
 }
 return 1;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'detect_monsters_string'.",&tolua_err);
 return 0;
#endif
}

/* function: detect_monsters_nonliving */
static int tolua_init_detect_monsters_nonliving00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnoobj(tolua_S,1,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
 {
  bool tolua_ret = (bool)  detect_monsters_nonliving();
 tolua_pushboolean(tolua_S,(bool)tolua_ret);
 }
 }
 return 1;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'detect_monsters_nonliving'.",&tolua_err);
 return 0;
#endif
}

/* function: detect_all */
static int tolua_init_detect_all00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnoobj(tolua_S,1,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
 {
  bool tolua_ret = (bool)  detect_all();
 tolua_pushboolean(tolua_S,(bool)tolua_ret);
 }
 }
 return 1;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'detect_all'.",&tolua_err);
 return 0;
#endif
}

/* function: stair_creation */
static int tolua_init_stair_creation00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnoobj(tolua_S,1,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
 {
  stair_creation();
 }
 }
 return 0;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'stair_creation'.",&tolua_err);
 return 0;
#endif
}

/* function: wall_stone */
static int tolua_init_wall_stone00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnoobj(tolua_S,1,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
 {
  bool tolua_ret = (bool)  wall_stone();
 tolua_pushboolean(tolua_S,(bool)tolua_ret);
 }
 }
 return 1;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'wall_stone'.",&tolua_err);
 return 0;
#endif
}

/* function: ident_spell */
static int tolua_init_ident_spell00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnoobj(tolua_S,1,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
 {
  bool tolua_ret = (bool)  ident_spell();
 tolua_pushboolean(tolua_S,(bool)tolua_ret);
 }
 }
 return 1;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'ident_spell'.",&tolua_err);
 return 0;
#endif
}

/* function: identify_fully */
static int tolua_init_identify_fully00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnoobj(tolua_S,1,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
 {
  bool tolua_ret = (bool)  identify_fully();
 tolua_pushboolean(tolua_S,(bool)tolua_ret);
 }
 }
 return 1;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'identify_fully'.",&tolua_err);
 return 0;
#endif
}

/* function: recharge */
static int tolua_init_recharge00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnumber(tolua_S,1,0,&tolua_err) ||
 !tolua_isnoobj(tolua_S,2,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
  int num = ((int)  tolua_tonumber(tolua_S,1,0));
 {
  bool tolua_ret = (bool)  recharge(num);
 tolua_pushboolean(tolua_S,(bool)tolua_ret);
 }
 }
 return 1;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'recharge'.",&tolua_err);
 return 0;
#endif
}

/* function: speed_monsters */
static int tolua_init_speed_monsters00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnoobj(tolua_S,1,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
 {
  bool tolua_ret = (bool)  speed_monsters();
 tolua_pushboolean(tolua_S,(bool)tolua_ret);
 }
 }
 return 1;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'speed_monsters'.",&tolua_err);
 return 0;
#endif
}

/* function: slow_monsters */
static int tolua_init_slow_monsters00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnoobj(tolua_S,1,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
 {
  bool tolua_ret = (bool)  slow_monsters();
 tolua_pushboolean(tolua_S,(bool)tolua_ret);
 }
 }
 return 1;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'slow_monsters'.",&tolua_err);
 return 0;
#endif
}

/* function: sleep_monsters */
static int tolua_init_sleep_monsters00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnoobj(tolua_S,1,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
 {
  bool tolua_ret = (bool)  sleep_monsters();
 tolua_pushboolean(tolua_S,(bool)tolua_ret);
 }
 }
 return 1;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'sleep_monsters'.",&tolua_err);
 return 0;
#endif
}

/* function: conf_monsters */
static int tolua_init_conf_monsters00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnoobj(tolua_S,1,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
 {
  bool tolua_ret = (bool)  conf_monsters();
 tolua_pushboolean(tolua_S,(bool)tolua_ret);
 }
 }
 return 1;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'conf_monsters'.",&tolua_err);
 return 0;
#endif
}

/* function: aggravate_monsters */
static int tolua_init_aggravate_monsters00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnumber(tolua_S,1,0,&tolua_err) ||
 !tolua_isnoobj(tolua_S,2,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
  int who = ((int)  tolua_tonumber(tolua_S,1,0));
 {
  aggravate_monsters(who);
 }
 }
 return 0;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'aggravate_monsters'.",&tolua_err);
 return 0;
#endif
}

/* function: genocide */
static int tolua_init_genocide00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isboolean(tolua_S,1,0,&tolua_err) ||
 !tolua_isnoobj(tolua_S,2,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
  bool player_cast = ((bool)  tolua_toboolean(tolua_S,1,0));
 {
  bool tolua_ret = (bool)  genocide(player_cast);
 tolua_pushboolean(tolua_S,(bool)tolua_ret);
 }
 }
 return 1;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'genocide'.",&tolua_err);
 return 0;
#endif
}

/* function: mass_genocide */
static int tolua_init_mass_genocide00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isboolean(tolua_S,1,0,&tolua_err) ||
 !tolua_isnoobj(tolua_S,2,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
  bool player_cast = ((bool)  tolua_toboolean(tolua_S,1,0));
 {
  bool tolua_ret = (bool)  mass_genocide(player_cast);
 tolua_pushboolean(tolua_S,(bool)tolua_ret);
 }
 }
 return 1;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'mass_genocide'.",&tolua_err);
 return 0;
#endif
}

/* function: probing */
static int tolua_init_probing00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnoobj(tolua_S,1,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
 {
  bool tolua_ret = (bool)  probing();
 tolua_pushboolean(tolua_S,(bool)tolua_ret);
 }
 }
 return 1;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'probing'.",&tolua_err);
 return 0;
#endif
}

/* function: change_wild_mode */
static int tolua_init_change_wild_mode00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnoobj(tolua_S,1,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
 {
  change_wild_mode();
 }
 }
 return 0;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'change_wild_mode'.",&tolua_err);
 return 0;
#endif
}

/* function: banish_evil */
static int tolua_init_banish_evil00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnumber(tolua_S,1,0,&tolua_err) ||
 !tolua_isnoobj(tolua_S,2,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
  int dist = ((int)  tolua_tonumber(tolua_S,1,0));
 {
  bool tolua_ret = (bool)  banish_evil(dist);
 tolua_pushboolean(tolua_S,(bool)tolua_ret);
 }
 }
 return 1;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'banish_evil'.",&tolua_err);
 return 0;
#endif
}

/* function: dispel_evil */
static int tolua_init_dispel_evil00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnumber(tolua_S,1,0,&tolua_err) ||
 !tolua_isnoobj(tolua_S,2,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
  s32b dam = ((s32b)  tolua_tonumber(tolua_S,1,0));
 {
  bool tolua_ret = (bool)  dispel_evil(dam);
 tolua_pushboolean(tolua_S,(bool)tolua_ret);
 }
 }
 return 1;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'dispel_evil'.",&tolua_err);
 return 0;
#endif
}

/* function: dispel_good */
static int tolua_init_dispel_good00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnumber(tolua_S,1,0,&tolua_err) ||
 !tolua_isnoobj(tolua_S,2,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
  s32b dam = ((s32b)  tolua_tonumber(tolua_S,1,0));
 {
  bool tolua_ret = (bool)  dispel_good(dam);
 tolua_pushboolean(tolua_S,(bool)tolua_ret);
 }
 }
 return 1;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'dispel_good'.",&tolua_err);
 return 0;
#endif
}

/* function: dispel_undead */
static int tolua_init_dispel_undead00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnumber(tolua_S,1,0,&tolua_err) ||
 !tolua_isnoobj(tolua_S,2,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
  s32b dam = ((s32b)  tolua_tonumber(tolua_S,1,0));
 {
  bool tolua_ret = (bool)  dispel_undead(dam);
 tolua_pushboolean(tolua_S,(bool)tolua_ret);
 }
 }
 return 1;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'dispel_undead'.",&tolua_err);
 return 0;
#endif
}

/* function: dispel_monsters */
static int tolua_init_dispel_monsters00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnumber(tolua_S,1,0,&tolua_err) ||
 !tolua_isnoobj(tolua_S,2,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
  s32b dam = ((s32b)  tolua_tonumber(tolua_S,1,0));
 {
  bool tolua_ret = (bool)  dispel_monsters(dam);
 tolua_pushboolean(tolua_S,(bool)tolua_ret);
 }
 }
 return 1;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'dispel_monsters'.",&tolua_err);
 return 0;
#endif
}

/* function: dispel_living */
static int tolua_init_dispel_living00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnumber(tolua_S,1,0,&tolua_err) ||
 !tolua_isnoobj(tolua_S,2,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
  s32b dam = ((s32b)  tolua_tonumber(tolua_S,1,0));
 {
  bool tolua_ret = (bool)  dispel_living(dam);
 tolua_pushboolean(tolua_S,(bool)tolua_ret);
 }
 }
 return 1;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'dispel_living'.",&tolua_err);
 return 0;
#endif
}

/* function: dispel_demons */
static int tolua_init_dispel_demons00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnumber(tolua_S,1,0,&tolua_err) ||
 !tolua_isnoobj(tolua_S,2,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
  s32b dam = ((s32b)  tolua_tonumber(tolua_S,1,0));
 {
  bool tolua_ret = (bool)  dispel_demons(dam);
 tolua_pushboolean(tolua_S,(bool)tolua_ret);
 }
 }
 return 1;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'dispel_demons'.",&tolua_err);
 return 0;
#endif
}

/* function: turn_undead */
static int tolua_init_turn_undead00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnoobj(tolua_S,1,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
 {
  bool tolua_ret = (bool)  turn_undead();
 tolua_pushboolean(tolua_S,(bool)tolua_ret);
 }
 }
 return 1;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'turn_undead'.",&tolua_err);
 return 0;
#endif
}

/* function: destroy_area */
static int tolua_init_destroy_area00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnumber(tolua_S,1,0,&tolua_err) ||
 !tolua_isnumber(tolua_S,2,0,&tolua_err) ||
 !tolua_isnumber(tolua_S,3,0,&tolua_err) ||
 !tolua_isboolean(tolua_S,4,0,&tolua_err) ||
 !tolua_isnoobj(tolua_S,5,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
  int y1 = ((int)  tolua_tonumber(tolua_S,1,0));
  int x1 = ((int)  tolua_tonumber(tolua_S,2,0));
  int r = ((int)  tolua_tonumber(tolua_S,3,0));
  bool full = ((bool)  tolua_toboolean(tolua_S,4,0));
 {
  destroy_area(y1,x1,r,full);
 }
 }
 return 0;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'destroy_area'.",&tolua_err);
 return 0;
#endif
}

/* function: earthquake */
static int tolua_init_earthquake00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnumber(tolua_S,1,0,&tolua_err) ||
 !tolua_isnumber(tolua_S,2,0,&tolua_err) ||
 !tolua_isnumber(tolua_S,3,0,&tolua_err) ||
 !tolua_isnoobj(tolua_S,4,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
  int cy = ((int)  tolua_tonumber(tolua_S,1,0));
  int cx = ((int)  tolua_tonumber(tolua_S,2,0));
  int r = ((int)  tolua_tonumber(tolua_S,3,0));
 {
  earthquake(cy,cx,r);
 }
 }
 return 0;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'earthquake'.",&tolua_err);
 return 0;
#endif
}

/* function: lite_room */
static int tolua_init_lite_room00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnumber(tolua_S,1,0,&tolua_err) ||
 !tolua_isnumber(tolua_S,2,0,&tolua_err) ||
 !tolua_isnoobj(tolua_S,3,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
  int y1 = ((int)  tolua_tonumber(tolua_S,1,0));
  int x1 = ((int)  tolua_tonumber(tolua_S,2,0));
 {
  lite_room(y1,x1);
 }
 }
 return 0;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'lite_room'.",&tolua_err);
 return 0;
#endif
}

/* function: unlite_room */
static int tolua_init_unlite_room00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnumber(tolua_S,1,0,&tolua_err) ||
 !tolua_isnumber(tolua_S,2,0,&tolua_err) ||
 !tolua_isnoobj(tolua_S,3,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
  int y1 = ((int)  tolua_tonumber(tolua_S,1,0));
  int x1 = ((int)  tolua_tonumber(tolua_S,2,0));
 {
  unlite_room(y1,x1);
 }
 }
 return 0;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'unlite_room'.",&tolua_err);
 return 0;
#endif
}

/* function: lite_area */
static int tolua_init_lite_area00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnumber(tolua_S,1,0,&tolua_err) ||
 !tolua_isnumber(tolua_S,2,0,&tolua_err) ||
 !tolua_isnoobj(tolua_S,3,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
  s32b dam = ((s32b)  tolua_tonumber(tolua_S,1,0));
  int rad = ((int)  tolua_tonumber(tolua_S,2,0));
 {
  bool tolua_ret = (bool)  lite_area(dam,rad);
 tolua_pushboolean(tolua_S,(bool)tolua_ret);
 }
 }
 return 1;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'lite_area'.",&tolua_err);
 return 0;
#endif
}

/* function: unlite_area */
static int tolua_init_unlite_area00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnumber(tolua_S,1,0,&tolua_err) ||
 !tolua_isnumber(tolua_S,2,0,&tolua_err) ||
 !tolua_isnoobj(tolua_S,3,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
  s32b dam = ((s32b)  tolua_tonumber(tolua_S,1,0));
  int rad = ((int)  tolua_tonumber(tolua_S,2,0));
 {
  bool tolua_ret = (bool)  unlite_area(dam,rad);
 tolua_pushboolean(tolua_S,(bool)tolua_ret);
 }
 }
 return 1;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'unlite_area'.",&tolua_err);
 return 0;
#endif
}

/* function: fire_ball_beam */
static int tolua_init_fire_ball_beam00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnumber(tolua_S,1,0,&tolua_err) ||
 !tolua_isnumber(tolua_S,2,0,&tolua_err) ||
 !tolua_isnumber(tolua_S,3,0,&tolua_err) ||
 !tolua_isnumber(tolua_S,4,0,&tolua_err) ||
 !tolua_isnoobj(tolua_S,5,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
  int typ = ((int)  tolua_tonumber(tolua_S,1,0));
  int dir = ((int)  tolua_tonumber(tolua_S,2,0));
  s32b dam = ((s32b)  tolua_tonumber(tolua_S,3,0));
  int rad = ((int)  tolua_tonumber(tolua_S,4,0));
 {
  bool tolua_ret = (bool)  fire_ball_beam(typ,dir,dam,rad);
 tolua_pushboolean(tolua_S,(bool)tolua_ret);
 }
 }
 return 1;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'fire_ball_beam'.",&tolua_err);
 return 0;
#endif
}

/* function: fire_ball */
static int tolua_init_fire_ball00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnumber(tolua_S,1,0,&tolua_err) ||
 !tolua_isnumber(tolua_S,2,0,&tolua_err) ||
 !tolua_isnumber(tolua_S,3,0,&tolua_err) ||
 !tolua_isnumber(tolua_S,4,0,&tolua_err) ||
 !tolua_isnoobj(tolua_S,5,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
  int typ = ((int)  tolua_tonumber(tolua_S,1,0));
  int dir = ((int)  tolua_tonumber(tolua_S,2,0));
  s32b dam = ((s32b)  tolua_tonumber(tolua_S,3,0));
  int rad = ((int)  tolua_tonumber(tolua_S,4,0));
 {
  bool tolua_ret = (bool)  fire_ball(typ,dir,dam,rad);
 tolua_pushboolean(tolua_S,(bool)tolua_ret);
 }
 }
 return 1;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'fire_ball'.",&tolua_err);
 return 0;
#endif
}

/* function: fire_bolt */
static int tolua_init_fire_bolt00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnumber(tolua_S,1,0,&tolua_err) ||
 !tolua_isnumber(tolua_S,2,0,&tolua_err) ||
 !tolua_isnumber(tolua_S,3,0,&tolua_err) ||
 !tolua_isnoobj(tolua_S,4,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
  int typ = ((int)  tolua_tonumber(tolua_S,1,0));
  int dir = ((int)  tolua_tonumber(tolua_S,2,0));
  s32b dam = ((s32b)  tolua_tonumber(tolua_S,3,0));
 {
  bool tolua_ret = (bool)  fire_bolt(typ,dir,dam);
 tolua_pushboolean(tolua_S,(bool)tolua_ret);
 }
 }
 return 1;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'fire_bolt'.",&tolua_err);
 return 0;
#endif
}

/* function: fire_beam */
static int tolua_init_fire_beam00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnumber(tolua_S,1,0,&tolua_err) ||
 !tolua_isnumber(tolua_S,2,0,&tolua_err) ||
 !tolua_isnumber(tolua_S,3,0,&tolua_err) ||
 !tolua_isnoobj(tolua_S,4,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
  int typ = ((int)  tolua_tonumber(tolua_S,1,0));
  int dir = ((int)  tolua_tonumber(tolua_S,2,0));
  s32b dam = ((s32b)  tolua_tonumber(tolua_S,3,0));
 {
  bool tolua_ret = (bool)  fire_beam(typ,dir,dam);
 tolua_pushboolean(tolua_S,(bool)tolua_ret);
 }
 }
 return 1;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'fire_beam'.",&tolua_err);
 return 0;
#endif
}

/* function: fire_bolt_or_beam */
static int tolua_init_fire_bolt_or_beam00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnumber(tolua_S,1,0,&tolua_err) ||
 !tolua_isnumber(tolua_S,2,0,&tolua_err) ||
 !tolua_isnumber(tolua_S,3,0,&tolua_err) ||
 !tolua_isnumber(tolua_S,4,0,&tolua_err) ||
 !tolua_isnoobj(tolua_S,5,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
  int prob = ((int)  tolua_tonumber(tolua_S,1,0));
  int typ = ((int)  tolua_tonumber(tolua_S,2,0));
  int dir = ((int)  tolua_tonumber(tolua_S,3,0));
  s32b dam = ((s32b)  tolua_tonumber(tolua_S,4,0));
 {
  bool tolua_ret = (bool)  fire_bolt_or_beam(prob,typ,dir,dam);
 tolua_pushboolean(tolua_S,(bool)tolua_ret);
 }
 }
 return 1;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'fire_bolt_or_beam'.",&tolua_err);
 return 0;
#endif
}

/* function: lite_line */
static int tolua_init_lite_line00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnumber(tolua_S,1,0,&tolua_err) ||
 !tolua_isnoobj(tolua_S,2,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
  int dir = ((int)  tolua_tonumber(tolua_S,1,0));
 {
  bool tolua_ret = (bool)  lite_line(dir);
 tolua_pushboolean(tolua_S,(bool)tolua_ret);
 }
 }
 return 1;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'lite_line'.",&tolua_err);
 return 0;
#endif
}

/* function: drain_life */
static int tolua_init_drain_life00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnumber(tolua_S,1,0,&tolua_err) ||
 !tolua_isnumber(tolua_S,2,0,&tolua_err) ||
 !tolua_isnoobj(tolua_S,3,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
  int dir = ((int)  tolua_tonumber(tolua_S,1,0));
  s32b dam = ((s32b)  tolua_tonumber(tolua_S,2,0));
 {
  bool tolua_ret = (bool)  drain_life(dir,dam);
 tolua_pushboolean(tolua_S,(bool)tolua_ret);
 }
 }
 return 1;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'drain_life'.",&tolua_err);
 return 0;
#endif
}

/* function: death_ray */
static int tolua_init_death_ray00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnumber(tolua_S,1,0,&tolua_err) ||
 !tolua_isnumber(tolua_S,2,0,&tolua_err) ||
 !tolua_isnoobj(tolua_S,3,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
  int dir = ((int)  tolua_tonumber(tolua_S,1,0));
  int plev = ((int)  tolua_tonumber(tolua_S,2,0));
 {
  bool tolua_ret = (bool)  death_ray(dir,plev);
 tolua_pushboolean(tolua_S,(bool)tolua_ret);
 }
 }
 return 1;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'death_ray'.",&tolua_err);
 return 0;
#endif
}

/* function: wall_to_mud */
static int tolua_init_wall_to_mud00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnumber(tolua_S,1,0,&tolua_err) ||
 !tolua_isnoobj(tolua_S,2,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
  int dir = ((int)  tolua_tonumber(tolua_S,1,0));
 {
  bool tolua_ret = (bool)  wall_to_mud(dir);
 tolua_pushboolean(tolua_S,(bool)tolua_ret);
 }
 }
 return 1;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'wall_to_mud'.",&tolua_err);
 return 0;
#endif
}

/* function: destroy_door */
static int tolua_init_destroy_door00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnumber(tolua_S,1,0,&tolua_err) ||
 !tolua_isnoobj(tolua_S,2,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
  int dir = ((int)  tolua_tonumber(tolua_S,1,0));
 {
  bool tolua_ret = (bool)  destroy_door(dir);
 tolua_pushboolean(tolua_S,(bool)tolua_ret);
 }
 }
 return 1;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'destroy_door'.",&tolua_err);
 return 0;
#endif
}

/* function: disarm_trap */
static int tolua_init_disarm_trap00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnumber(tolua_S,1,0,&tolua_err) ||
 !tolua_isnoobj(tolua_S,2,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
  int dir = ((int)  tolua_tonumber(tolua_S,1,0));
 {
  bool tolua_ret = (bool)  disarm_trap(dir);
 tolua_pushboolean(tolua_S,(bool)tolua_ret);
 }
 }
 return 1;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'disarm_trap'.",&tolua_err);
 return 0;
#endif
}

/* function: wizard_lock */
static int tolua_init_wizard_lock00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnumber(tolua_S,1,0,&tolua_err) ||
 !tolua_isnoobj(tolua_S,2,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
  int dir = ((int)  tolua_tonumber(tolua_S,1,0));
 {
  bool tolua_ret = (bool)  wizard_lock(dir);
 tolua_pushboolean(tolua_S,(bool)tolua_ret);
 }
 }
 return 1;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'wizard_lock'.",&tolua_err);
 return 0;
#endif
}

/* function: heal_monster */
static int tolua_init_heal_monster00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnumber(tolua_S,1,0,&tolua_err) ||
 !tolua_isnoobj(tolua_S,2,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
  int dir = ((int)  tolua_tonumber(tolua_S,1,0));
 {
  bool tolua_ret = (bool)  heal_monster(dir);
 tolua_pushboolean(tolua_S,(bool)tolua_ret);
 }
 }
 return 1;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'heal_monster'.",&tolua_err);
 return 0;
#endif
}

/* function: speed_monster */
static int tolua_init_speed_monster00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnumber(tolua_S,1,0,&tolua_err) ||
 !tolua_isnoobj(tolua_S,2,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
  int dir = ((int)  tolua_tonumber(tolua_S,1,0));
 {
  bool tolua_ret = (bool)  speed_monster(dir);
 tolua_pushboolean(tolua_S,(bool)tolua_ret);
 }
 }
 return 1;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'speed_monster'.",&tolua_err);
 return 0;
#endif
}

/* function: slow_monster */
static int tolua_init_slow_monster00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnumber(tolua_S,1,0,&tolua_err) ||
 !tolua_isnoobj(tolua_S,2,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
  int dir = ((int)  tolua_tonumber(tolua_S,1,0));
 {
  bool tolua_ret = (bool)  slow_monster(dir);
 tolua_pushboolean(tolua_S,(bool)tolua_ret);
 }
 }
 return 1;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'slow_monster'.",&tolua_err);
 return 0;
#endif
}

/* function: sleep_monster */
static int tolua_init_sleep_monster00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnumber(tolua_S,1,0,&tolua_err) ||
 !tolua_isnoobj(tolua_S,2,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
  int dir = ((int)  tolua_tonumber(tolua_S,1,0));
 {
  bool tolua_ret = (bool)  sleep_monster(dir);
 tolua_pushboolean(tolua_S,(bool)tolua_ret);
 }
 }
 return 1;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'sleep_monster'.",&tolua_err);
 return 0;
#endif
}

/* function: confuse_monster */
static int tolua_init_confuse_monster00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnumber(tolua_S,1,0,&tolua_err) ||
 !tolua_isnumber(tolua_S,2,0,&tolua_err) ||
 !tolua_isnoobj(tolua_S,3,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
  int dir = ((int)  tolua_tonumber(tolua_S,1,0));
  int plev = ((int)  tolua_tonumber(tolua_S,2,0));
 {
  bool tolua_ret = (bool)  confuse_monster(dir,plev);
 tolua_pushboolean(tolua_S,(bool)tolua_ret);
 }
 }
 return 1;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'confuse_monster'.",&tolua_err);
 return 0;
#endif
}

/* function: stun_monster */
static int tolua_init_stun_monster00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnumber(tolua_S,1,0,&tolua_err) ||
 !tolua_isnumber(tolua_S,2,0,&tolua_err) ||
 !tolua_isnoobj(tolua_S,3,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
  int dir = ((int)  tolua_tonumber(tolua_S,1,0));
  int plev = ((int)  tolua_tonumber(tolua_S,2,0));
 {
  bool tolua_ret = (bool)  stun_monster(dir,plev);
 tolua_pushboolean(tolua_S,(bool)tolua_ret);
 }
 }
 return 1;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'stun_monster'.",&tolua_err);
 return 0;
#endif
}

/* function: fear_monster */
static int tolua_init_fear_monster00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnumber(tolua_S,1,0,&tolua_err) ||
 !tolua_isnumber(tolua_S,2,0,&tolua_err) ||
 !tolua_isnoobj(tolua_S,3,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
  int dir = ((int)  tolua_tonumber(tolua_S,1,0));
  int plev = ((int)  tolua_tonumber(tolua_S,2,0));
 {
  bool tolua_ret = (bool)  fear_monster(dir,plev);
 tolua_pushboolean(tolua_S,(bool)tolua_ret);
 }
 }
 return 1;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'fear_monster'.",&tolua_err);
 return 0;
#endif
}

/* function: scare_monsters */
static int tolua_init_scare_monsters00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnoobj(tolua_S,1,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
 {
  bool tolua_ret = (bool)  scare_monsters();
 tolua_pushboolean(tolua_S,(bool)tolua_ret);
 }
 }
 return 1;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'scare_monsters'.",&tolua_err);
 return 0;
#endif
}

/* function: poly_monster */
static int tolua_init_poly_monster00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnumber(tolua_S,1,0,&tolua_err) ||
 !tolua_isnoobj(tolua_S,2,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
  int dir = ((int)  tolua_tonumber(tolua_S,1,0));
 {
  bool tolua_ret = (bool)  poly_monster(dir);
 tolua_pushboolean(tolua_S,(bool)tolua_ret);
 }
 }
 return 1;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'poly_monster'.",&tolua_err);
 return 0;
#endif
}

/* function: clone_monster */
static int tolua_init_clone_monster00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnumber(tolua_S,1,0,&tolua_err) ||
 !tolua_isnoobj(tolua_S,2,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
  int dir = ((int)  tolua_tonumber(tolua_S,1,0));
 {
  bool tolua_ret = (bool)  clone_monster(dir);
 tolua_pushboolean(tolua_S,(bool)tolua_ret);
 }
 }
 return 1;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'clone_monster'.",&tolua_err);
 return 0;
#endif
}

/* function: teleport_monster */
static int tolua_init_teleport_monster00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnumber(tolua_S,1,0,&tolua_err) ||
 !tolua_isnoobj(tolua_S,2,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
  int dir = ((int)  tolua_tonumber(tolua_S,1,0));
 {
  bool tolua_ret = (bool)  teleport_monster(dir);
 tolua_pushboolean(tolua_S,(bool)tolua_ret);
 }
 }
 return 1;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'teleport_monster'.",&tolua_err);
 return 0;
#endif
}

/* function: door_creation */
static int tolua_init_door_creation00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnoobj(tolua_S,1,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
 {
  bool tolua_ret = (bool)  door_creation();
 tolua_pushboolean(tolua_S,(bool)tolua_ret);
 }
 }
 return 1;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'door_creation'.",&tolua_err);
 return 0;
#endif
}

/* function: trap_creation */
static int tolua_init_trap_creation00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnoobj(tolua_S,1,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
 {
  bool tolua_ret = (bool)  trap_creation();
 tolua_pushboolean(tolua_S,(bool)tolua_ret);
 }
 }
 return 1;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'trap_creation'.",&tolua_err);
 return 0;
#endif
}

/* function: glyph_creation */
static int tolua_init_glyph_creation00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnoobj(tolua_S,1,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
 {
  bool tolua_ret = (bool)  glyph_creation();
 tolua_pushboolean(tolua_S,(bool)tolua_ret);
 }
 }
 return 1;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'glyph_creation'.",&tolua_err);
 return 0;
#endif
}

/* function: destroy_doors_touch */
static int tolua_init_destroy_doors_touch00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnoobj(tolua_S,1,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
 {
  bool tolua_ret = (bool)  destroy_doors_touch();
 tolua_pushboolean(tolua_S,(bool)tolua_ret);
 }
 }
 return 1;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'destroy_doors_touch'.",&tolua_err);
 return 0;
#endif
}

/* function: sleep_monsters_touch */
static int tolua_init_sleep_monsters_touch00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnoobj(tolua_S,1,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
 {
  bool tolua_ret = (bool)  sleep_monsters_touch();
 tolua_pushboolean(tolua_S,(bool)tolua_ret);
 }
 }
 return 1;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'sleep_monsters_touch'.",&tolua_err);
 return 0;
#endif
}

/* function: alchemy */
static int tolua_init_alchemy00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnoobj(tolua_S,1,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
 {
  bool tolua_ret = (bool)  alchemy();
 tolua_pushboolean(tolua_S,(bool)tolua_ret);
 }
 }
 return 1;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'alchemy'.",&tolua_err);
 return 0;
#endif
}

/* function: wall_breaker */
static int tolua_init_wall_breaker00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnoobj(tolua_S,1,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
 {
  wall_breaker();
 }
 }
 return 0;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'wall_breaker'.",&tolua_err);
 return 0;
#endif
}

/* function: bless_weapon */
static int tolua_init_bless_weapon00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnoobj(tolua_S,1,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
 {
  bless_weapon();
 }
 }
 return 0;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'bless_weapon'.",&tolua_err);
 return 0;
#endif
}

/* function: confuse_monsters */
static int tolua_init_confuse_monsters00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnumber(tolua_S,1,0,&tolua_err) ||
 !tolua_isnoobj(tolua_S,2,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
  s32b dam = ((s32b)  tolua_tonumber(tolua_S,1,0));
 {
  bool tolua_ret = (bool)  confuse_monsters(dam);
 tolua_pushboolean(tolua_S,(bool)tolua_ret);
 }
 }
 return 1;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'confuse_monsters'.",&tolua_err);
 return 0;
#endif
}

/* function: charm_animals */
static int tolua_init_charm_animals00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnumber(tolua_S,1,0,&tolua_err) ||
 !tolua_isnoobj(tolua_S,2,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
  s32b dam = ((s32b)  tolua_tonumber(tolua_S,1,0));
 {
  bool tolua_ret = (bool)  charm_animals(dam);
 tolua_pushboolean(tolua_S,(bool)tolua_ret);
 }
 }
 return 1;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'charm_animals'.",&tolua_err);
 return 0;
#endif
}

/* function: stun_monsters */
static int tolua_init_stun_monsters00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnumber(tolua_S,1,0,&tolua_err) ||
 !tolua_isnoobj(tolua_S,2,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
  s32b dam = ((s32b)  tolua_tonumber(tolua_S,1,0));
 {
  bool tolua_ret = (bool)  stun_monsters(dam);
 tolua_pushboolean(tolua_S,(bool)tolua_ret);
 }
 }
 return 1;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'stun_monsters'.",&tolua_err);
 return 0;
#endif
}

/* function: banish_monsters */
static int tolua_init_banish_monsters00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnumber(tolua_S,1,0,&tolua_err) ||
 !tolua_isnoobj(tolua_S,2,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
  int dist = ((int)  tolua_tonumber(tolua_S,1,0));
 {
  bool tolua_ret = (bool)  banish_monsters(dist);
 tolua_pushboolean(tolua_S,(bool)tolua_ret);
 }
 }
 return 1;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'banish_monsters'.",&tolua_err);
 return 0;
#endif
}

/* function: turn_monsters */
static int tolua_init_turn_monsters00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnumber(tolua_S,1,0,&tolua_err) ||
 !tolua_isnoobj(tolua_S,2,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
  s32b dam = ((s32b)  tolua_tonumber(tolua_S,1,0));
 {
  bool tolua_ret = (bool)  turn_monsters(dam);
 tolua_pushboolean(tolua_S,(bool)tolua_ret);
 }
 }
 return 1;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'turn_monsters'.",&tolua_err);
 return 0;
#endif
}

/* function: turn_evil */
static int tolua_init_turn_evil00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnumber(tolua_S,1,0,&tolua_err) ||
 !tolua_isnoobj(tolua_S,2,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
  s32b dam = ((s32b)  tolua_tonumber(tolua_S,1,0));
 {
  bool tolua_ret = (bool)  turn_evil(dam);
 tolua_pushboolean(tolua_S,(bool)tolua_ret);
 }
 }
 return 1;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'turn_evil'.",&tolua_err);
 return 0;
#endif
}

/* function: deathray_monsters */
static int tolua_init_deathray_monsters00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnoobj(tolua_S,1,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
 {
  bool tolua_ret = (bool)  deathray_monsters();
 tolua_pushboolean(tolua_S,(bool)tolua_ret);
 }
 }
 return 1;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'deathray_monsters'.",&tolua_err);
 return 0;
#endif
}

/* function: control_one_undead */
static int tolua_init_control_one_undead00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnumber(tolua_S,1,0,&tolua_err) ||
 !tolua_isnumber(tolua_S,2,0,&tolua_err) ||
 !tolua_isnoobj(tolua_S,3,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
  int dir = ((int)  tolua_tonumber(tolua_S,1,0));
  int plev = ((int)  tolua_tonumber(tolua_S,2,0));
 {
  bool tolua_ret = (bool)  control_one_undead(dir,plev);
 tolua_pushboolean(tolua_S,(bool)tolua_ret);
 }
 }
 return 1;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'control_one_undead'.",&tolua_err);
 return 0;
#endif
}

/* function: charm_animal */
static int tolua_init_charm_animal00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnumber(tolua_S,1,0,&tolua_err) ||
 !tolua_isnumber(tolua_S,2,0,&tolua_err) ||
 !tolua_isnoobj(tolua_S,3,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
  int dir = ((int)  tolua_tonumber(tolua_S,1,0));
  int plev = ((int)  tolua_tonumber(tolua_S,2,0));
 {
  bool tolua_ret = (bool)  charm_animal(dir,plev);
 tolua_pushboolean(tolua_S,(bool)tolua_ret);
 }
 }
 return 1;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'charm_animal'.",&tolua_err);
 return 0;
#endif
}

/* function: get_table_name */
static int tolua_init_get_table_name00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isstring(tolua_S,1,0,&tolua_err) ||
 !tolua_isnoobj(tolua_S,2,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
  char* out_string = ((char*)  tolua_tostring(tolua_S,1,0));
 {
  get_table_name(out_string);
 }
 }
 return 0;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'get_table_name'.",&tolua_err);
 return 0;
#endif
}

/* function: flag_cost */
static int tolua_init_flag_cost00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isusertype(tolua_S,1,"object_type",0,&tolua_err) ||
 !tolua_isnumber(tolua_S,2,0,&tolua_err) ||
 !tolua_isnoobj(tolua_S,3,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
  object_type* o_ptr = ((object_type*)  tolua_tousertype(tolua_S,1,0));
  int plusses = ((int)  tolua_tonumber(tolua_S,2,0));
 {
  s32b tolua_ret = (s32b)  flag_cost(o_ptr,plusses);
 tolua_pushnumber(tolua_S,(long)tolua_ret);
 }
 }
 return 1;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'flag_cost'.",&tolua_err);
 return 0;
#endif
}

/* function: alter_reality */
static int tolua_init_alter_reality00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnoobj(tolua_S,1,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
 {
  alter_reality();
 }
 }
 return 0;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'alter_reality'.",&tolua_err);
 return 0;
#endif
}

/* function: report_magics */
static int tolua_init_report_magics00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnoobj(tolua_S,1,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
 {
  report_magics();
 }
 }
 return 0;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'report_magics'.",&tolua_err);
 return 0;
#endif
}

/* function: teleport_swap */
static int tolua_init_teleport_swap00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnumber(tolua_S,1,0,&tolua_err) ||
 !tolua_isnoobj(tolua_S,2,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
  int dir = ((int)  tolua_tonumber(tolua_S,1,0));
 {
  teleport_swap(dir);
 }
 }
 return 0;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'teleport_swap'.",&tolua_err);
 return 0;
#endif
}

/* function: swap_position */
static int tolua_init_swap_position00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnumber(tolua_S,1,0,&tolua_err) ||
 !tolua_isnumber(tolua_S,2,0,&tolua_err) ||
 !tolua_isnoobj(tolua_S,3,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
  int lty = ((int)  tolua_tonumber(tolua_S,1,0));
  int ltx = ((int)  tolua_tonumber(tolua_S,2,0));
 {
  swap_position(lty,ltx);
 }
 }
 return 0;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'swap_position'.",&tolua_err);
 return 0;
#endif
}

/* function: item_tester_hook_recharge */
static int tolua_init_item_tester_hook_recharge00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isusertype(tolua_S,1,"object_type",0,&tolua_err) ||
 !tolua_isnoobj(tolua_S,2,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
  object_type* o_ptr = ((object_type*)  tolua_tousertype(tolua_S,1,0));
 {
  bool tolua_ret = (bool)  item_tester_hook_recharge(o_ptr);
 tolua_pushboolean(tolua_S,(bool)tolua_ret);
 }
 }
 return 1;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'item_tester_hook_recharge'.",&tolua_err);
 return 0;
#endif
}

/* function: get_activation_power */
static int tolua_init_get_activation_power00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnoobj(tolua_S,1,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
 {
  int tolua_ret = (int)  get_activation_power();
 tolua_pushnumber(tolua_S,(long)tolua_ret);
 }
 }
 return 1;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'get_activation_power'.",&tolua_err);
 return 0;
#endif
}

/* function: invoke */
static int tolua_init_invoke00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnumber(tolua_S,1,0,&tolua_err) ||
 !tolua_isnumber(tolua_S,2,0,&tolua_err) ||
 !tolua_isnoobj(tolua_S,3,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
  s32b dam = ((s32b)  tolua_tonumber(tolua_S,1,0));
  int typ = ((int)  tolua_tonumber(tolua_S,2,0));
 {
  bool tolua_ret = (bool)  invoke(dam,typ);
 tolua_pushboolean(tolua_S,(bool)tolua_ret);
 }
 }
 return 1;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'invoke'.",&tolua_err);
 return 0;
#endif
}

/* function: project_hack */
static int tolua_init_project_hack00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnumber(tolua_S,1,0,&tolua_err) ||
 !tolua_isnumber(tolua_S,2,0,&tolua_err) ||
 !tolua_isnoobj(tolua_S,3,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
  int typ = ((int)  tolua_tonumber(tolua_S,1,0));
  s32b dam = ((s32b)  tolua_tonumber(tolua_S,2,0));
 {
  bool tolua_ret = (bool)  project_hack(typ,dam);
 tolua_pushboolean(tolua_S,(bool)tolua_ret);
 }
 }
 return 1;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'project_hack'.",&tolua_err);
 return 0;
#endif
}

/* function: project_meteor */
static int tolua_init_project_meteor00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnumber(tolua_S,1,0,&tolua_err) ||
 !tolua_isnumber(tolua_S,2,0,&tolua_err) ||
 !tolua_isnumber(tolua_S,3,0,&tolua_err) ||
 !tolua_isnumber(tolua_S,4,0,&tolua_err) ||
 !tolua_isnoobj(tolua_S,5,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
  int radius = ((int)  tolua_tonumber(tolua_S,1,0));
  int typ = ((int)  tolua_tonumber(tolua_S,2,0));
  s32b dam = ((s32b)  tolua_tonumber(tolua_S,3,0));
  u32b flg = ((u32b)  tolua_tonumber(tolua_S,4,0));
 {
  project_meteor(radius,typ,dam,flg);
 }
 }
 return 0;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'project_meteor'.",&tolua_err);
 return 0;
#endif
}

/* function: item_tester_hook_artifactable */
static int tolua_init_item_tester_hook_artifactable00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isusertype(tolua_S,1,"object_type",0,&tolua_err) ||
 !tolua_isnoobj(tolua_S,2,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
  object_type* o_ptr = ((object_type*)  tolua_tousertype(tolua_S,1,0));
 {
  bool tolua_ret = (bool)  item_tester_hook_artifactable(o_ptr);
 tolua_pushboolean(tolua_S,(bool)tolua_ret);
 }
 }
 return 1;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'item_tester_hook_artifactable'.",&tolua_err);
 return 0;
#endif
}

/* function: project_hook */
static int tolua_init_project_hook00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnumber(tolua_S,1,0,&tolua_err) ||
 !tolua_isnumber(tolua_S,2,0,&tolua_err) ||
 !tolua_isnumber(tolua_S,3,0,&tolua_err) ||
 !tolua_isnumber(tolua_S,4,0,&tolua_err) ||
 !tolua_isnoobj(tolua_S,5,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
  int typ = ((int)  tolua_tonumber(tolua_S,1,0));
  int dir = ((int)  tolua_tonumber(tolua_S,2,0));
  s32b dam = ((s32b)  tolua_tonumber(tolua_S,3,0));
  int flg = ((int)  tolua_tonumber(tolua_S,4,0));
 {
  bool tolua_ret = (bool)  project_hook(typ,dir,dam,flg);
 tolua_pushboolean(tolua_S,(bool)tolua_ret);
 }
 }
 return 1;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'project_hook'.",&tolua_err);
 return 0;
#endif
}

/* function: random_misc */
static int tolua_init_random_misc00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isusertype(tolua_S,1,"object_type",0,&tolua_err) ||
 !tolua_isboolean(tolua_S,2,0,&tolua_err) ||
 !tolua_isnoobj(tolua_S,3,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
  object_type* o_ptr = ((object_type*)  tolua_tousertype(tolua_S,1,0));
  bool is_scroll = ((bool)  tolua_toboolean(tolua_S,2,0));
 {
  random_misc(o_ptr,is_scroll);
 }
 }
 return 0;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'random_misc'.",&tolua_err);
 return 0;
#endif
}

/* function: random_plus */
static int tolua_init_random_plus00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isusertype(tolua_S,1,"object_type",0,&tolua_err) ||
 !tolua_isboolean(tolua_S,2,0,&tolua_err) ||
 !tolua_isnoobj(tolua_S,3,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
  object_type* o_ptr = ((object_type*)  tolua_tousertype(tolua_S,1,0));
  bool is_scroll = ((bool)  tolua_toboolean(tolua_S,2,0));
 {
  random_plus(o_ptr,is_scroll);
 }
 }
 return 0;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'random_plus'.",&tolua_err);
 return 0;
#endif
}

/* function: fire_jump_ball */
static int tolua_init_fire_jump_ball00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnumber(tolua_S,1,0,&tolua_err) ||
 !tolua_isnumber(tolua_S,2,0,&tolua_err) ||
 !tolua_isnumber(tolua_S,3,0,&tolua_err) ||
 !tolua_isnumber(tolua_S,4,0,&tolua_err) ||
 !tolua_isnumber(tolua_S,5,0,&tolua_err) ||
 !tolua_isboolean(tolua_S,6,0,&tolua_err) ||
 !tolua_isnoobj(tolua_S,7,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
  int typ = ((int)  tolua_tonumber(tolua_S,1,0));
  s32b dam = ((s32b)  tolua_tonumber(tolua_S,2,0));
  int rad = ((int)  tolua_tonumber(tolua_S,3,0));
  int x = ((int)  tolua_tonumber(tolua_S,4,0));
  int y = ((int)  tolua_tonumber(tolua_S,5,0));
  bool nomagic = ((bool)  tolua_toboolean(tolua_S,6,0));
 {
  fire_jump_ball(typ,dam,rad,x,y,nomagic);
 }
 }
 return 0;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'fire_jump_ball'.",&tolua_err);
 return 0;
#endif
}

/* function: mass_change_allegiance */
static int tolua_init_mass_change_allegiance00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnumber(tolua_S,1,0,&tolua_err) ||
 !tolua_isboolean(tolua_S,2,0,&tolua_err) ||
 !tolua_isnoobj(tolua_S,3,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
  int r_idx = ((int)  tolua_tonumber(tolua_S,1,0));
  bool friendly = ((bool)  tolua_toboolean(tolua_S,2,0));
 {
  mass_change_allegiance(r_idx,friendly);
 }
 }
 return 0;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'mass_change_allegiance'.",&tolua_err);
 return 0;
#endif
}

/* function: chain_attack */
static int tolua_init_chain_attack00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnumber(tolua_S,1,0,&tolua_err) ||
 !tolua_isnumber(tolua_S,2,0,&tolua_err) ||
 !tolua_isnumber(tolua_S,3,0,&tolua_err) ||
 !tolua_isnumber(tolua_S,4,0,&tolua_err) ||
 !tolua_isnumber(tolua_S,5,0,&tolua_err) ||
 !tolua_isnoobj(tolua_S,6,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
  int dir = ((int)  tolua_tonumber(tolua_S,1,0));
  int typ = ((int)  tolua_tonumber(tolua_S,2,0));
  s32b dam = ((s32b)  tolua_tonumber(tolua_S,3,0));
  int rad = ((int)  tolua_tonumber(tolua_S,4,0));
  int range = ((int)  tolua_tonumber(tolua_S,5,0));
 {
  bool tolua_ret = (bool)  chain_attack(dir,typ,dam,rad,range);
 tolua_pushboolean(tolua_S,(bool)tolua_ret);
 }
 }
 return 1;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'chain_attack'.",&tolua_err);
 return 0;
#endif
}

/* function: chain_attack_fields */
static int tolua_init_chain_attack_fields00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnumber(tolua_S,1,0,&tolua_err) ||
 !tolua_isnumber(tolua_S,2,0,&tolua_err) ||
 !tolua_isnumber(tolua_S,3,0,&tolua_err) ||
 !tolua_isnumber(tolua_S,4,0,&tolua_err) ||
 !tolua_isnumber(tolua_S,5,0,&tolua_err) ||
 !tolua_isnumber(tolua_S,6,0,&tolua_err) ||
 !tolua_isnumber(tolua_S,7,0,&tolua_err) ||
 !tolua_isnoobj(tolua_S,8,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
  int dir = ((int)  tolua_tonumber(tolua_S,1,0));
  int typ = ((int)  tolua_tonumber(tolua_S,2,0));
  s32b dam = ((s32b)  tolua_tonumber(tolua_S,3,0));
  int rad = ((int)  tolua_tonumber(tolua_S,4,0));
  int range = ((int)  tolua_tonumber(tolua_S,5,0));
  int fldtype = ((int)  tolua_tonumber(tolua_S,6,0));
  int fldam = ((int)  tolua_tonumber(tolua_S,7,0));
 {
  bool tolua_ret = (bool)  chain_attack_fields(dir,typ,dam,rad,range,fldtype,fldam);
 tolua_pushboolean(tolua_S,(bool)tolua_ret);
 }
 }
 return 1;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'chain_attack_fields'.",&tolua_err);
 return 0;
#endif
}

/* function: fire_ball_specific_grid */
static int tolua_init_fire_ball_specific_grid00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnumber(tolua_S,1,0,&tolua_err) ||
 !tolua_isnumber(tolua_S,2,0,&tolua_err) ||
 !tolua_isnumber(tolua_S,3,0,&tolua_err) ||
 !tolua_isnumber(tolua_S,4,0,&tolua_err) ||
 !tolua_isnumber(tolua_S,5,0,&tolua_err) ||
 !tolua_isnoobj(tolua_S,6,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
  s32b dam = ((s32b)  tolua_tonumber(tolua_S,1,0));
  int x = ((int)  tolua_tonumber(tolua_S,2,0));
  int y = ((int)  tolua_tonumber(tolua_S,3,0));
  int rad = ((int)  tolua_tonumber(tolua_S,4,0));
  int typ = ((int)  tolua_tonumber(tolua_S,5,0));
 {
  fire_ball_specific_grid(dam,x,y,rad,typ);
 }
 }
 return 0;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'fire_ball_specific_grid'.",&tolua_err);
 return 0;
#endif
}

/* function: hard_kick */
static int tolua_init_hard_kick00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnumber(tolua_S,1,0,&tolua_err) ||
 !tolua_isnumber(tolua_S,2,0,&tolua_err) ||
 !tolua_isnumber(tolua_S,3,0,&tolua_err) ||
 !tolua_isnoobj(tolua_S,4,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
  int dir = ((int)  tolua_tonumber(tolua_S,1,0));
  s32b dam = ((s32b)  tolua_tonumber(tolua_S,2,0));
  int range = ((int)  tolua_tonumber(tolua_S,3,0));
 {
  hard_kick(dir,dam,range);
 }
 }
 return 0;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'hard_kick'.",&tolua_err);
 return 0;
#endif
}

/* function: identify_fully_specific */
static int tolua_init_identify_fully_specific00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isusertype(tolua_S,1,"object_type",0,&tolua_err) ||
 !tolua_isnoobj(tolua_S,2,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
  object_type* o_ptr = ((object_type*)  tolua_tousertype(tolua_S,1,0));
 {
  identify_fully_specific(o_ptr);
 }
 }
 return 0;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'identify_fully_specific'.",&tolua_err);
 return 0;
#endif
}

/* function: multiply_divide */
static int tolua_init_multiply_divide00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnumber(tolua_S,1,0,&tolua_err) ||
 !tolua_isnumber(tolua_S,2,0,&tolua_err) ||
 !tolua_isnumber(tolua_S,3,0,&tolua_err) ||
 !tolua_isnoobj(tolua_S,4,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
  s32b value = ((s32b)  tolua_tonumber(tolua_S,1,0));
  s32b mult = ((s32b)  tolua_tonumber(tolua_S,2,0));
  s32b div = ((s32b)  tolua_tonumber(tolua_S,3,0));
 {
  s32b tolua_ret = (s32b)  multiply_divide(value,mult,div);
 tolua_pushnumber(tolua_S,(long)tolua_ret);
 }
 }
 return 1;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'multiply_divide'.",&tolua_err);
 return 0;
#endif
}

/* function: safety_check */
static int tolua_init_safety_check00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnoobj(tolua_S,1,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
 {
  bool tolua_ret = (bool)  safety_check();
 tolua_pushboolean(tolua_S,(bool)tolua_ret);
 }
 }
 return 1;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'safety_check'.",&tolua_err);
 return 0;
#endif
}

/* function: modify_stat_value */
static int tolua_init_modify_stat_value00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnumber(tolua_S,1,0,&tolua_err) ||
 !tolua_isnumber(tolua_S,2,0,&tolua_err) ||
 !tolua_isnoobj(tolua_S,3,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
  int value = ((int)  tolua_tonumber(tolua_S,1,0));
  int amount = ((int)  tolua_tonumber(tolua_S,2,0));
 {
  s16b tolua_ret = (s16b)  modify_stat_value(value,amount);
 tolua_pushnumber(tolua_S,(long)tolua_ret);
 }
 }
 return 1;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'modify_stat_value'.",&tolua_err);
 return 0;
#endif
}

/* function: weight_limit */
static int tolua_init_weight_limit00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnoobj(tolua_S,1,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
 {
  int tolua_ret = (int)  weight_limit();
 tolua_pushnumber(tolua_S,(long)tolua_ret);
 }
 }
 return 1;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'weight_limit'.",&tolua_err);
 return 0;
#endif
}

/* function: calc_skills */
static int tolua_init_calc_skills00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnumber(tolua_S,1,0,&tolua_err) ||
 !tolua_isnoobj(tolua_S,2,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
  int mode = ((int)  tolua_tonumber(tolua_S,1,0));
 {
  calc_skills(mode);
 }
 }
 return 0;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'calc_skills'.",&tolua_err);
 return 0;
#endif
}

/* function: calc_stats */
static int tolua_init_calc_stats00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnumber(tolua_S,1,0,&tolua_err) ||
 !tolua_isnoobj(tolua_S,2,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
  int mode = ((int)  tolua_tonumber(tolua_S,1,0));
 {
  calc_stats(mode);
 }
 }
 return 0;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'calc_stats'.",&tolua_err);
 return 0;
#endif
}

/* function: calc_equipment */
static int tolua_init_calc_equipment00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnoobj(tolua_S,1,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
 {
  calc_equipment();
 }
 }
 return 0;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'calc_equipment'.",&tolua_err);
 return 0;
#endif
}

/* function: calc_resistances */
static int tolua_init_calc_resistances00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnumber(tolua_S,1,0,&tolua_err) ||
 !tolua_isnoobj(tolua_S,2,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
  int mode = ((int)  tolua_tonumber(tolua_S,1,0));
 {
  calc_resistances(mode);
 }
 }
 return 0;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'calc_resistances'.",&tolua_err);
 return 0;
#endif
}

/* function: calc_cursed */
static int tolua_init_calc_cursed00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnoobj(tolua_S,1,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
 {
  calc_cursed();
 }
 }
 return 0;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'calc_cursed'.",&tolua_err);
 return 0;
#endif
}

/* function: set_powerattack */
static int tolua_init_set_powerattack00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnumber(tolua_S,1,0,&tolua_err) ||
 !tolua_isnoobj(tolua_S,2,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
  int v = ((int)  tolua_tonumber(tolua_S,1,0));
 {
  bool tolua_ret = (bool)  set_powerattack(v);
 tolua_pushboolean(tolua_S,(bool)tolua_ret);
 }
 }
 return 1;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'set_powerattack'.",&tolua_err);
 return 0;
#endif
}

/* function: mon_take_hit */
static int tolua_init_mon_take_hit00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnumber(tolua_S,1,0,&tolua_err) ||
 !tolua_isnumber(tolua_S,2,0,&tolua_err) ||
 !tolua_isboolean(tolua_S,3,0,&tolua_err) ||
 !tolua_isstring(tolua_S,4,0,&tolua_err) ||
 !tolua_isnoobj(tolua_S,5,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
  int m_idx = ((int)  tolua_tonumber(tolua_S,1,0));
  s32b dam = ((s32b)  tolua_tonumber(tolua_S,2,0));
  bool fear = ((bool)  tolua_toboolean(tolua_S,3,0));
  cptr note = ((cptr)  tolua_tostring(tolua_S,4,0));
 {
  bool tolua_ret = (bool)  mon_take_hit(m_idx,dam,&fear,note);
 tolua_pushboolean(tolua_S,(bool)tolua_ret);
 tolua_pushboolean(tolua_S,(bool)fear);
 }
 }
 return 2;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'mon_take_hit'.",&tolua_err);
 return 0;
#endif
}

/* function: tgt_pt */
static int tolua_init_tgt_pt00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnumber(tolua_S,1,0,&tolua_err) ||
 !tolua_isnumber(tolua_S,2,0,&tolua_err) ||
 !tolua_isnoobj(tolua_S,3,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
  int x = ((int)  tolua_tonumber(tolua_S,1,0));
  int y = ((int)  tolua_tonumber(tolua_S,2,0));
 {
  bool tolua_ret = (bool)  tgt_pt(&x,&y);
 tolua_pushboolean(tolua_S,(bool)tolua_ret);
 tolua_pushnumber(tolua_S,(long)x);
 tolua_pushnumber(tolua_S,(long)y);
 }
 }
 return 3;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'tgt_pt'.",&tolua_err);
 return 0;
#endif
}

/* function: dagger_check */
static int tolua_init_dagger_check00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnoobj(tolua_S,1,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
 {
  bool tolua_ret = (bool)  dagger_check();
 tolua_pushboolean(tolua_S,(bool)tolua_ret);
 }
 }
 return 1;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'dagger_check'.",&tolua_err);
 return 0;
#endif
}

/* function: axe_check */
static int tolua_init_axe_check00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnoobj(tolua_S,1,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
 {
  bool tolua_ret = (bool)  axe_check();
 tolua_pushboolean(tolua_S,(bool)tolua_ret);
 }
 }
 return 1;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'axe_check'.",&tolua_err);
 return 0;
#endif
}

/* function: activate_item */
static int tolua_init_activate_item00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isusertype(tolua_S,1,"object_type",0,&tolua_err) ||
 !tolua_isboolean(tolua_S,2,0,&tolua_err) ||
 !tolua_isnoobj(tolua_S,3,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
  object_type* o_ptr = ((object_type*)  tolua_tousertype(tolua_S,1,0));
  bool wisdom = ((bool)  tolua_toboolean(tolua_S,2,0));
 {
  activate_item(o_ptr,wisdom);
 }
 }
 return 0;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'activate_item'.",&tolua_err);
 return 0;
#endif
}

/* function: pick_spell */
static int tolua_init_pick_spell00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnoobj(tolua_S,1,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
 {
  int tolua_ret = (int)  pick_spell();
 tolua_pushnumber(tolua_S,(long)tolua_ret);
 }
 }
 return 1;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'pick_spell'.",&tolua_err);
 return 0;
#endif
}

/* function: fate_monsters */
static int tolua_init_fate_monsters00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnumber(tolua_S,1,0,&tolua_err) ||
 !tolua_isnoobj(tolua_S,2,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
  int mode = ((int)  tolua_tonumber(tolua_S,1,0));
 {
  s16b tolua_ret = (s16b)  fate_monsters(mode);
 tolua_pushnumber(tolua_S,(long)tolua_ret);
 }
 }
 return 1;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'fate_monsters'.",&tolua_err);
 return 0;
#endif
}

/* function: fate_items */
static int tolua_init_fate_items00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnumber(tolua_S,1,0,&tolua_err) ||
 !tolua_isnoobj(tolua_S,2,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
  int mode = ((int)  tolua_tonumber(tolua_S,1,0));
 {
  s16b tolua_ret = (s16b)  fate_items(mode);
 tolua_pushnumber(tolua_S,(long)tolua_ret);
 }
 }
 return 1;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'fate_items'.",&tolua_err);
 return 0;
#endif
}

/* function: pick_song */
static int tolua_init_pick_song00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnumber(tolua_S,1,0,&tolua_err) ||
 !tolua_isnoobj(tolua_S,2,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
  int reduction = ((int)  tolua_tonumber(tolua_S,1,0));
 {
  int tolua_ret = (int)  pick_song(reduction);
 tolua_pushnumber(tolua_S,(long)tolua_ret);
 }
 }
 return 1;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'pick_song'.",&tolua_err);
 return 0;
#endif
}

/* function: is_elemental */
static int tolua_init_is_elemental00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnumber(tolua_S,1,0,&tolua_err) ||
 !tolua_isnoobj(tolua_S,2,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
  int power = ((int)  tolua_tonumber(tolua_S,1,0));
 {
  bool tolua_ret = (bool)  is_elemental(power);
 tolua_pushboolean(tolua_S,(bool)tolua_ret);
 }
 }
 return 1;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'is_elemental'.",&tolua_err);
 return 0;
#endif
}

/* function: is_alteration */
static int tolua_init_is_alteration00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnumber(tolua_S,1,0,&tolua_err) ||
 !tolua_isnoobj(tolua_S,2,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
  int power = ((int)  tolua_tonumber(tolua_S,1,0));
 {
  bool tolua_ret = (bool)  is_alteration(power);
 tolua_pushboolean(tolua_S,(bool)tolua_ret);
 }
 }
 return 1;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'is_alteration'.",&tolua_err);
 return 0;
#endif
}

/* function: is_mysticism */
static int tolua_init_is_mysticism00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnumber(tolua_S,1,0,&tolua_err) ||
 !tolua_isnoobj(tolua_S,2,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
  int power = ((int)  tolua_tonumber(tolua_S,1,0));
 {
  bool tolua_ret = (bool)  is_mysticism(power);
 tolua_pushboolean(tolua_S,(bool)tolua_ret);
 }
 }
 return 1;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'is_mysticism'.",&tolua_err);
 return 0;
#endif
}

/* function: is_divination */
static int tolua_init_is_divination00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnumber(tolua_S,1,0,&tolua_err) ||
 !tolua_isnoobj(tolua_S,2,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
  int power = ((int)  tolua_tonumber(tolua_S,1,0));
 {
  bool tolua_ret = (bool)  is_divination(power);
 tolua_pushboolean(tolua_S,(bool)tolua_ret);
 }
 }
 return 1;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'is_divination'.",&tolua_err);
 return 0;
#endif
}

/* function: generate_town */
static int tolua_init_generate_town00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnoobj(tolua_S,1,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
 {
  int tolua_ret = (int)  generate_town();
 tolua_pushnumber(tolua_S,(long)tolua_ret);
 }
 }
 return 1;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'generate_town'.",&tolua_err);
 return 0;
#endif
}

/* function: generate_quest */
static int tolua_init_generate_quest00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnoobj(tolua_S,1,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
 {
  int tolua_ret = (int)  generate_quest();
 tolua_pushnumber(tolua_S,(long)tolua_ret);
 }
 }
 return 1;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'generate_quest'.",&tolua_err);
 return 0;
#endif
}

/* function: generate_wilderness */
static int tolua_init_generate_wilderness00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnoobj(tolua_S,1,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
 {
  int tolua_ret = (int)  generate_wilderness();
 tolua_pushnumber(tolua_S,(long)tolua_ret);
 }
 }
 return 1;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'generate_wilderness'.",&tolua_err);
 return 0;
#endif
}

/* function: generate_vault */
static int tolua_init_generate_vault00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnumber(tolua_S,1,0,&tolua_err) ||
 !tolua_isnumber(tolua_S,2,0,&tolua_err) ||
 !tolua_isnumber(tolua_S,3,0,&tolua_err) ||
 !tolua_isnoobj(tolua_S,4,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
  int vy = ((int)  tolua_tonumber(tolua_S,1,0));
  int vx = ((int)  tolua_tonumber(tolua_S,2,0));
  int num = ((int)  tolua_tonumber(tolua_S,3,0));
 {
  generate_vault(vy,vx,num);
 }
 }
 return 0;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'generate_vault'.",&tolua_err);
 return 0;
#endif
}

/* function: quest_artifact_prep */
static int tolua_init_quest_artifact_prep00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnumber(tolua_S,1,0,&tolua_err) ||
 !tolua_isnumber(tolua_S,2,0,&tolua_err) ||
 !tolua_isnumber(tolua_S,3,0,&tolua_err) ||
 !tolua_isnoobj(tolua_S,4,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
  int a_idx = ((int)  tolua_tonumber(tolua_S,1,0));
  int x = ((int)  tolua_tonumber(tolua_S,2,0));
  int y = ((int)  tolua_tonumber(tolua_S,3,0));
 {
  quest_artifact_prep(a_idx,x,y);
 }
 }
 return 0;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'quest_artifact_prep'.",&tolua_err);
 return 0;
#endif
}

/* function: lua_update_monsters */
static int tolua_init_lua_update_monsters00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnoobj(tolua_S,1,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
 {
  lua_update_monsters();
 }
 }
 return 0;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'lua_update_monsters'.",&tolua_err);
 return 0;
#endif
}

/* function: lua_update_stuff */
static int tolua_init_lua_update_stuff00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnoobj(tolua_S,1,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
 {
  lua_update_stuff();
 }
 }
 return 0;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'lua_update_stuff'.",&tolua_err);
 return 0;
#endif
}

/* function: lua_get_aim_dir */
static int tolua_init_lua_get_aim_dir00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnoobj(tolua_S,1,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
 {
  int tolua_ret = (int)  lua_get_aim_dir();
 tolua_pushnumber(tolua_S,(long)tolua_ret);
 }
 }
 return 1;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'lua_get_aim_dir'.",&tolua_err);
 return 0;
#endif
}

/* function: lua_get_rep_dir */
static int tolua_init_lua_get_rep_dir00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnoobj(tolua_S,1,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
 {
  int tolua_ret = (int)  lua_get_rep_dir();
 tolua_pushnumber(tolua_S,(long)tolua_ret);
 }
 }
 return 1;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'lua_get_rep_dir'.",&tolua_err);
 return 0;
#endif
}

/* function: lua_cave_empty_bold */
static int tolua_init_lua_cave_empty_bold00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnumber(tolua_S,1,0,&tolua_err) ||
 !tolua_isnumber(tolua_S,2,0,&tolua_err) ||
 !tolua_isnoobj(tolua_S,3,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
  int y = ((int)  tolua_tonumber(tolua_S,1,0));
  int x = ((int)  tolua_tonumber(tolua_S,2,0));
 {
  bool tolua_ret = (bool)  lua_cave_empty_bold(y,x);
 tolua_pushboolean(tolua_S,(bool)tolua_ret);
 }
 }
 return 1;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'lua_cave_empty_bold'.",&tolua_err);
 return 0;
#endif
}

/* function: get_cave_info_flag */
static int tolua_init_get_cave_info_flag00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnumber(tolua_S,1,0,&tolua_err) ||
 !tolua_isnumber(tolua_S,2,0,&tolua_err) ||
 !tolua_isnumber(tolua_S,3,0,&tolua_err) ||
 !tolua_isnoobj(tolua_S,4,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
  int y = ((int)  tolua_tonumber(tolua_S,1,0));
  int x = ((int)  tolua_tonumber(tolua_S,2,0));
  u32b flag = ((u32b)  tolua_tonumber(tolua_S,3,0));
 {
  bool tolua_ret = (bool)  get_cave_info_flag(y,x,flag);
 tolua_pushboolean(tolua_S,(bool)tolua_ret);
 }
 }
 return 1;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'get_cave_info_flag'.",&tolua_err);
 return 0;
#endif
}

/* function: lua_tgt_pt */
static int tolua_init_lua_tgt_pt00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnoobj(tolua_S,1,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
 {
  bool tolua_ret = (bool)  lua_tgt_pt();
 tolua_pushboolean(tolua_S,(bool)tolua_ret);
 }
 }
 return 1;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'lua_tgt_pt'.",&tolua_err);
 return 0;
#endif
}

/* function: lua_in_bounds */
static int tolua_init_lua_in_bounds00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnumber(tolua_S,1,0,&tolua_err) ||
 !tolua_isnumber(tolua_S,2,0,&tolua_err) ||
 !tolua_isnoobj(tolua_S,3,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
  int y = ((int)  tolua_tonumber(tolua_S,1,0));
  int x = ((int)  tolua_tonumber(tolua_S,2,0));
 {
  bool tolua_ret = (bool)  lua_in_bounds(y,x);
 tolua_pushboolean(tolua_S,(bool)tolua_ret);
 }
 }
 return 1;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'lua_in_bounds'.",&tolua_err);
 return 0;
#endif
}

/* function: lua_in_bounds2 */
static int tolua_init_lua_in_bounds200(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnumber(tolua_S,1,0,&tolua_err) ||
 !tolua_isnumber(tolua_S,2,0,&tolua_err) ||
 !tolua_isnoobj(tolua_S,3,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
  int y = ((int)  tolua_tonumber(tolua_S,1,0));
  int x = ((int)  tolua_tonumber(tolua_S,2,0));
 {
  bool tolua_ret = (bool)  lua_in_bounds2(y,x);
 tolua_pushboolean(tolua_S,(bool)tolua_ret);
 }
 }
 return 1;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'lua_in_bounds2'.",&tolua_err);
 return 0;
#endif
}

/* function: lua_player_has_los_bold */
static int tolua_init_lua_player_has_los_bold00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnumber(tolua_S,1,0,&tolua_err) ||
 !tolua_isnumber(tolua_S,2,0,&tolua_err) ||
 !tolua_isnoobj(tolua_S,3,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
  int y = ((int)  tolua_tonumber(tolua_S,1,0));
  int x = ((int)  tolua_tonumber(tolua_S,2,0));
 {
  bool tolua_ret = (bool)  lua_player_has_los_bold(y,x);
 tolua_pushboolean(tolua_S,(bool)tolua_ret);
 }
 }
 return 1;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'lua_player_has_los_bold'.",&tolua_err);
 return 0;
#endif
}

/* function: lua_project */
static int tolua_init_lua_project00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnumber(tolua_S,1,0,&tolua_err) ||
 !tolua_isnumber(tolua_S,2,0,&tolua_err) ||
 !tolua_isnumber(tolua_S,3,0,&tolua_err) ||
 !tolua_isnumber(tolua_S,4,0,&tolua_err) ||
 !tolua_isnumber(tolua_S,5,0,&tolua_err) ||
 !tolua_isnumber(tolua_S,6,0,&tolua_err) ||
 !tolua_isnumber(tolua_S,7,0,&tolua_err) ||
 !tolua_isnoobj(tolua_S,8,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
  int who = ((int)  tolua_tonumber(tolua_S,1,0));
  int rad = ((int)  tolua_tonumber(tolua_S,2,0));
  int y = ((int)  tolua_tonumber(tolua_S,3,0));
  int x = ((int)  tolua_tonumber(tolua_S,4,0));
  s32b dam = ((s32b)  tolua_tonumber(tolua_S,5,0));
  int typ = ((int)  tolua_tonumber(tolua_S,6,0));
  int mode = ((int)  tolua_tonumber(tolua_S,7,0));
 {
  lua_project(who,rad,y,x,dam,typ,mode);
 }
 }
 return 0;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'lua_project'.",&tolua_err);
 return 0;
#endif
}

/* function: memorize_race_flag1 */
static int tolua_init_memorize_race_flag100(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnumber(tolua_S,1,0,&tolua_err) ||
 !tolua_isnumber(tolua_S,2,0,&tolua_err) ||
 !tolua_isnoobj(tolua_S,3,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
  int r_idx = ((int)  tolua_tonumber(tolua_S,1,0));
  u32b flag = ((u32b)  tolua_tonumber(tolua_S,2,0));
 {
  memorize_race_flag1(r_idx,flag);
 }
 }
 return 0;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'memorize_race_flag1'.",&tolua_err);
 return 0;
#endif
}

/* function: memorize_race_flag2 */
static int tolua_init_memorize_race_flag200(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnumber(tolua_S,1,0,&tolua_err) ||
 !tolua_isnumber(tolua_S,2,0,&tolua_err) ||
 !tolua_isnoobj(tolua_S,3,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
  int r_idx = ((int)  tolua_tonumber(tolua_S,1,0));
  u32b flag = ((u32b)  tolua_tonumber(tolua_S,2,0));
 {
  memorize_race_flag2(r_idx,flag);
 }
 }
 return 0;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'memorize_race_flag2'.",&tolua_err);
 return 0;
#endif
}

/* function: memorize_race_flag3 */
static int tolua_init_memorize_race_flag300(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnumber(tolua_S,1,0,&tolua_err) ||
 !tolua_isnumber(tolua_S,2,0,&tolua_err) ||
 !tolua_isnoobj(tolua_S,3,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
  int r_idx = ((int)  tolua_tonumber(tolua_S,1,0));
  u32b flag = ((u32b)  tolua_tonumber(tolua_S,2,0));
 {
  memorize_race_flag3(r_idx,flag);
 }
 }
 return 0;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'memorize_race_flag3'.",&tolua_err);
 return 0;
#endif
}

/* function: memorize_race_flag4 */
static int tolua_init_memorize_race_flag400(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnumber(tolua_S,1,0,&tolua_err) ||
 !tolua_isnumber(tolua_S,2,0,&tolua_err) ||
 !tolua_isnoobj(tolua_S,3,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
  int r_idx = ((int)  tolua_tonumber(tolua_S,1,0));
  u32b flag = ((u32b)  tolua_tonumber(tolua_S,2,0));
 {
  memorize_race_flag4(r_idx,flag);
 }
 }
 return 0;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'memorize_race_flag4'.",&tolua_err);
 return 0;
#endif
}

/* function: memorize_race_flag5 */
static int tolua_init_memorize_race_flag500(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnumber(tolua_S,1,0,&tolua_err) ||
 !tolua_isnumber(tolua_S,2,0,&tolua_err) ||
 !tolua_isnoobj(tolua_S,3,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
  int r_idx = ((int)  tolua_tonumber(tolua_S,1,0));
  u32b flag = ((u32b)  tolua_tonumber(tolua_S,2,0));
 {
  memorize_race_flag5(r_idx,flag);
 }
 }
 return 0;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'memorize_race_flag5'.",&tolua_err);
 return 0;
#endif
}

/* function: memorize_race_flag6 */
static int tolua_init_memorize_race_flag600(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnumber(tolua_S,1,0,&tolua_err) ||
 !tolua_isnumber(tolua_S,2,0,&tolua_err) ||
 !tolua_isnoobj(tolua_S,3,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
  int r_idx = ((int)  tolua_tonumber(tolua_S,1,0));
  u32b flag = ((u32b)  tolua_tonumber(tolua_S,2,0));
 {
  memorize_race_flag6(r_idx,flag);
 }
 }
 return 0;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'memorize_race_flag6'.",&tolua_err);
 return 0;
#endif
}

/* function: memorize_race_flag7 */
static int tolua_init_memorize_race_flag700(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnumber(tolua_S,1,0,&tolua_err) ||
 !tolua_isnumber(tolua_S,2,0,&tolua_err) ||
 !tolua_isnoobj(tolua_S,3,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
  int r_idx = ((int)  tolua_tonumber(tolua_S,1,0));
  u32b flag = ((u32b)  tolua_tonumber(tolua_S,2,0));
 {
  memorize_race_flag7(r_idx,flag);
 }
 }
 return 0;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'memorize_race_flag7'.",&tolua_err);
 return 0;
#endif
}

/* function: memorize_race_flag8 */
static int tolua_init_memorize_race_flag800(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnumber(tolua_S,1,0,&tolua_err) ||
 !tolua_isnumber(tolua_S,2,0,&tolua_err) ||
 !tolua_isnoobj(tolua_S,3,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
  int r_idx = ((int)  tolua_tonumber(tolua_S,1,0));
  u32b flag = ((u32b)  tolua_tonumber(tolua_S,2,0));
 {
  memorize_race_flag8(r_idx,flag);
 }
 }
 return 0;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'memorize_race_flag8'.",&tolua_err);
 return 0;
#endif
}

/* function: memorize_race_flag9 */
static int tolua_init_memorize_race_flag900(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnumber(tolua_S,1,0,&tolua_err) ||
 !tolua_isnumber(tolua_S,2,0,&tolua_err) ||
 !tolua_isnoobj(tolua_S,3,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
  int r_idx = ((int)  tolua_tonumber(tolua_S,1,0));
  u32b flag = ((u32b)  tolua_tonumber(tolua_S,2,0));
 {
  memorize_race_flag9(r_idx,flag);
 }
 }
 return 0;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'memorize_race_flag9'.",&tolua_err);
 return 0;
#endif
}

/* function: give_monster_ability */
static int tolua_init_give_monster_ability00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isusertype(tolua_S,1,"monster_type",0,&tolua_err) ||
 !tolua_isnumber(tolua_S,2,0,&tolua_err) ||
 !tolua_isnoobj(tolua_S,3,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
  monster_type* m_ptr = ((monster_type*)  tolua_tousertype(tolua_S,1,0));
  u32b flag = ((u32b)  tolua_tonumber(tolua_S,2,0));
 {
  give_monster_ability(m_ptr,flag);
 }
 }
 return 0;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'give_monster_ability'.",&tolua_err);
 return 0;
#endif
}

/* function: remove_monster_ability */
static int tolua_init_remove_monster_ability00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isusertype(tolua_S,1,"monster_type",0,&tolua_err) ||
 !tolua_isnumber(tolua_S,2,0,&tolua_err) ||
 !tolua_isnoobj(tolua_S,3,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
  monster_type* m_ptr = ((monster_type*)  tolua_tousertype(tolua_S,1,0));
  u32b flag = ((u32b)  tolua_tonumber(tolua_S,2,0));
 {
  remove_monster_ability(m_ptr,flag);
 }
 }
 return 0;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'remove_monster_ability'.",&tolua_err);
 return 0;
#endif
}

/* function: give_object_flag1 */
static int tolua_init_give_object_flag100(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isusertype(tolua_S,1,"object_type",0,&tolua_err) ||
 !tolua_isnumber(tolua_S,2,0,&tolua_err) ||
 !tolua_isnoobj(tolua_S,3,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
  object_type* o_ptr = ((object_type*)  tolua_tousertype(tolua_S,1,0));
  u32b flag = ((u32b)  tolua_tonumber(tolua_S,2,0));
 {
  give_object_flag1(o_ptr,flag);
 }
 }
 return 0;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'give_object_flag1'.",&tolua_err);
 return 0;
#endif
}

/* function: give_object_flag2 */
static int tolua_init_give_object_flag200(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isusertype(tolua_S,1,"object_type",0,&tolua_err) ||
 !tolua_isnumber(tolua_S,2,0,&tolua_err) ||
 !tolua_isnoobj(tolua_S,3,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
  object_type* o_ptr = ((object_type*)  tolua_tousertype(tolua_S,1,0));
  u32b flag = ((u32b)  tolua_tonumber(tolua_S,2,0));
 {
  give_object_flag2(o_ptr,flag);
 }
 }
 return 0;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'give_object_flag2'.",&tolua_err);
 return 0;
#endif
}

/* function: give_object_flag3 */
static int tolua_init_give_object_flag300(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isusertype(tolua_S,1,"object_type",0,&tolua_err) ||
 !tolua_isnumber(tolua_S,2,0,&tolua_err) ||
 !tolua_isnoobj(tolua_S,3,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
  object_type* o_ptr = ((object_type*)  tolua_tousertype(tolua_S,1,0));
  u32b flag = ((u32b)  tolua_tonumber(tolua_S,2,0));
 {
  give_object_flag3(o_ptr,flag);
 }
 }
 return 0;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'give_object_flag3'.",&tolua_err);
 return 0;
#endif
}

/* function: give_object_flag4 */
static int tolua_init_give_object_flag400(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isusertype(tolua_S,1,"object_type",0,&tolua_err) ||
 !tolua_isnumber(tolua_S,2,0,&tolua_err) ||
 !tolua_isnoobj(tolua_S,3,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
  object_type* o_ptr = ((object_type*)  tolua_tousertype(tolua_S,1,0));
  u32b flag = ((u32b)  tolua_tonumber(tolua_S,2,0));
 {
  give_object_flag4(o_ptr,flag);
 }
 }
 return 0;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'give_object_flag4'.",&tolua_err);
 return 0;
#endif
}

/* function: remove_object_flag1 */
static int tolua_init_remove_object_flag100(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isusertype(tolua_S,1,"object_type",0,&tolua_err) ||
 !tolua_isnumber(tolua_S,2,0,&tolua_err) ||
 !tolua_isnoobj(tolua_S,3,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
  object_type* o_ptr = ((object_type*)  tolua_tousertype(tolua_S,1,0));
  u32b flag = ((u32b)  tolua_tonumber(tolua_S,2,0));
 {
  remove_object_flag1(o_ptr,flag);
 }
 }
 return 0;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'remove_object_flag1'.",&tolua_err);
 return 0;
#endif
}

/* function: remove_object_flag2 */
static int tolua_init_remove_object_flag200(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isusertype(tolua_S,1,"object_type",0,&tolua_err) ||
 !tolua_isnumber(tolua_S,2,0,&tolua_err) ||
 !tolua_isnoobj(tolua_S,3,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
  object_type* o_ptr = ((object_type*)  tolua_tousertype(tolua_S,1,0));
  u32b flag = ((u32b)  tolua_tonumber(tolua_S,2,0));
 {
  remove_object_flag2(o_ptr,flag);
 }
 }
 return 0;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'remove_object_flag2'.",&tolua_err);
 return 0;
#endif
}

/* function: remove_object_flag3 */
static int tolua_init_remove_object_flag300(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isusertype(tolua_S,1,"object_type",0,&tolua_err) ||
 !tolua_isnumber(tolua_S,2,0,&tolua_err) ||
 !tolua_isnoobj(tolua_S,3,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
  object_type* o_ptr = ((object_type*)  tolua_tousertype(tolua_S,1,0));
  u32b flag = ((u32b)  tolua_tonumber(tolua_S,2,0));
 {
  remove_object_flag3(o_ptr,flag);
 }
 }
 return 0;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'remove_object_flag3'.",&tolua_err);
 return 0;
#endif
}

/* function: remove_object_flag4 */
static int tolua_init_remove_object_flag400(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isusertype(tolua_S,1,"object_type",0,&tolua_err) ||
 !tolua_isnumber(tolua_S,2,0,&tolua_err) ||
 !tolua_isnoobj(tolua_S,3,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
  object_type* o_ptr = ((object_type*)  tolua_tousertype(tolua_S,1,0));
  u32b flag = ((u32b)  tolua_tonumber(tolua_S,2,0));
 {
  remove_object_flag4(o_ptr,flag);
 }
 }
 return 0;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'remove_object_flag4'.",&tolua_err);
 return 0;
#endif
}

/* function: lua_mod */
static int tolua_init_lua_mod00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnumber(tolua_S,1,0,&tolua_err) ||
 !tolua_isnumber(tolua_S,2,0,&tolua_err) ||
 !tolua_isnoobj(tolua_S,3,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
  int moddedint = ((int)  tolua_tonumber(tolua_S,1,0));
  int modint = ((int)  tolua_tonumber(tolua_S,2,0));
 {
  bool tolua_ret = (bool)  lua_mod(moddedint,modint);
 tolua_pushboolean(tolua_S,(bool)tolua_ret);
 }
 }
 return 1;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'lua_mod'.",&tolua_err);
 return 0;
#endif
}

/* function: get_player_monster_ability */
static int tolua_init_get_player_monster_ability00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnumber(tolua_S,1,0,&tolua_err) ||
 !tolua_isnoobj(tolua_S,2,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
  u32b flag = ((u32b)  tolua_tonumber(tolua_S,1,0));
 {
  bool tolua_ret = (bool)  get_player_monster_ability(flag);
 tolua_pushboolean(tolua_S,(bool)tolua_ret);
 }
 }
 return 1;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'get_player_monster_ability'.",&tolua_err);
 return 0;
#endif
}

/* function: give_monster_race_flag1 */
static int tolua_init_give_monster_race_flag100(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnumber(tolua_S,1,0,&tolua_err) ||
 !tolua_isnumber(tolua_S,2,0,&tolua_err) ||
 !tolua_isnoobj(tolua_S,3,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
  int r_idx = ((int)  tolua_tonumber(tolua_S,1,0));
  u32b flag = ((u32b)  tolua_tonumber(tolua_S,2,0));
 {
  give_monster_race_flag1(r_idx,flag);
 }
 }
 return 0;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'give_monster_race_flag1'.",&tolua_err);
 return 0;
#endif
}

/* function: give_monster_race_flag2 */
static int tolua_init_give_monster_race_flag200(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnumber(tolua_S,1,0,&tolua_err) ||
 !tolua_isnumber(tolua_S,2,0,&tolua_err) ||
 !tolua_isnoobj(tolua_S,3,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
  int r_idx = ((int)  tolua_tonumber(tolua_S,1,0));
  u32b flag = ((u32b)  tolua_tonumber(tolua_S,2,0));
 {
  give_monster_race_flag2(r_idx,flag);
 }
 }
 return 0;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'give_monster_race_flag2'.",&tolua_err);
 return 0;
#endif
}

/* function: give_monster_race_flag3 */
static int tolua_init_give_monster_race_flag300(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnumber(tolua_S,1,0,&tolua_err) ||
 !tolua_isnumber(tolua_S,2,0,&tolua_err) ||
 !tolua_isnoobj(tolua_S,3,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
  int r_idx = ((int)  tolua_tonumber(tolua_S,1,0));
  u32b flag = ((u32b)  tolua_tonumber(tolua_S,2,0));
 {
  give_monster_race_flag3(r_idx,flag);
 }
 }
 return 0;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'give_monster_race_flag3'.",&tolua_err);
 return 0;
#endif
}

/* function: give_monster_race_flag4 */
static int tolua_init_give_monster_race_flag400(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnumber(tolua_S,1,0,&tolua_err) ||
 !tolua_isnumber(tolua_S,2,0,&tolua_err) ||
 !tolua_isnoobj(tolua_S,3,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
  int r_idx = ((int)  tolua_tonumber(tolua_S,1,0));
  u32b flag = ((u32b)  tolua_tonumber(tolua_S,2,0));
 {
  give_monster_race_flag4(r_idx,flag);
 }
 }
 return 0;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'give_monster_race_flag4'.",&tolua_err);
 return 0;
#endif
}

/* function: give_monster_race_flag5 */
static int tolua_init_give_monster_race_flag500(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnumber(tolua_S,1,0,&tolua_err) ||
 !tolua_isnumber(tolua_S,2,0,&tolua_err) ||
 !tolua_isnoobj(tolua_S,3,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
  int r_idx = ((int)  tolua_tonumber(tolua_S,1,0));
  u32b flag = ((u32b)  tolua_tonumber(tolua_S,2,0));
 {
  give_monster_race_flag5(r_idx,flag);
 }
 }
 return 0;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'give_monster_race_flag5'.",&tolua_err);
 return 0;
#endif
}

/* function: give_monster_race_flag6 */
static int tolua_init_give_monster_race_flag600(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnumber(tolua_S,1,0,&tolua_err) ||
 !tolua_isnumber(tolua_S,2,0,&tolua_err) ||
 !tolua_isnoobj(tolua_S,3,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
  int r_idx = ((int)  tolua_tonumber(tolua_S,1,0));
  u32b flag = ((u32b)  tolua_tonumber(tolua_S,2,0));
 {
  give_monster_race_flag6(r_idx,flag);
 }
 }
 return 0;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'give_monster_race_flag6'.",&tolua_err);
 return 0;
#endif
}

/* function: give_monster_race_flag7 */
static int tolua_init_give_monster_race_flag700(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnumber(tolua_S,1,0,&tolua_err) ||
 !tolua_isnumber(tolua_S,2,0,&tolua_err) ||
 !tolua_isnoobj(tolua_S,3,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
  int r_idx = ((int)  tolua_tonumber(tolua_S,1,0));
  u32b flag = ((u32b)  tolua_tonumber(tolua_S,2,0));
 {
  give_monster_race_flag7(r_idx,flag);
 }
 }
 return 0;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'give_monster_race_flag7'.",&tolua_err);
 return 0;
#endif
}

/* function: give_monster_race_flag8 */
static int tolua_init_give_monster_race_flag800(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnumber(tolua_S,1,0,&tolua_err) ||
 !tolua_isnumber(tolua_S,2,0,&tolua_err) ||
 !tolua_isnoobj(tolua_S,3,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
  int r_idx = ((int)  tolua_tonumber(tolua_S,1,0));
  u32b flag = ((u32b)  tolua_tonumber(tolua_S,2,0));
 {
  give_monster_race_flag8(r_idx,flag);
 }
 }
 return 0;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'give_monster_race_flag8'.",&tolua_err);
 return 0;
#endif
}

/* function: give_monster_race_flag9 */
static int tolua_init_give_monster_race_flag900(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnumber(tolua_S,1,0,&tolua_err) ||
 !tolua_isnumber(tolua_S,2,0,&tolua_err) ||
 !tolua_isnoobj(tolua_S,3,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
  int r_idx = ((int)  tolua_tonumber(tolua_S,1,0));
  u32b flag = ((u32b)  tolua_tonumber(tolua_S,2,0));
 {
  give_monster_race_flag9(r_idx,flag);
 }
 }
 return 0;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'give_monster_race_flag9'.",&tolua_err);
 return 0;
#endif
}

/* function: give_dungeon_flag1 */
static int tolua_init_give_dungeon_flag100(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnumber(tolua_S,1,0,&tolua_err) ||
 !tolua_isnumber(tolua_S,2,0,&tolua_err) ||
 !tolua_isnoobj(tolua_S,3,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
  int dinfo = ((int)  tolua_tonumber(tolua_S,1,0));
  u32b flag = ((u32b)  tolua_tonumber(tolua_S,2,0));
 {
  give_dungeon_flag1(dinfo,flag);
 }
 }
 return 0;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'give_dungeon_flag1'.",&tolua_err);
 return 0;
#endif
}

/* function: remove_dungeon_flag1 */
static int tolua_init_remove_dungeon_flag100(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnumber(tolua_S,1,0,&tolua_err) ||
 !tolua_isnumber(tolua_S,2,0,&tolua_err) ||
 !tolua_isnoobj(tolua_S,3,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
  int dinfo = ((int)  tolua_tonumber(tolua_S,1,0));
  u32b flag = ((u32b)  tolua_tonumber(tolua_S,2,0));
 {
  remove_dungeon_flag1(dinfo,flag);
 }
 }
 return 0;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'remove_dungeon_flag1'.",&tolua_err);
 return 0;
#endif
}

/* function: lua_cave_mark */
static int tolua_init_lua_cave_mark00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnumber(tolua_S,1,0,&tolua_err) ||
 !tolua_isnumber(tolua_S,2,0,&tolua_err) ||
 !tolua_isnumber(tolua_S,3,0,&tolua_err) ||
 !tolua_isnoobj(tolua_S,4,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
  int y = ((int)  tolua_tonumber(tolua_S,1,0));
  int x = ((int)  tolua_tonumber(tolua_S,2,0));
  u32b flag = ((u32b)  tolua_tonumber(tolua_S,3,0));
 {
  lua_cave_mark(y,x,flag);
 }
 }
 return 0;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'lua_cave_mark'.",&tolua_err);
 return 0;
#endif
}

/* function: lua_get_string */
static int tolua_init_lua_get_string00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnumber(tolua_S,1,0,&tolua_err) ||
 !tolua_isnoobj(tolua_S,2,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
  int len = ((int)  tolua_tonumber(tolua_S,1,0));
 {
  lua_get_string(len);
 }
 }
 return 0;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'lua_get_string'.",&tolua_err);
 return 0;
#endif
}

/* function: get_feat_flag1 */
static int tolua_init_get_feat_flag100(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnumber(tolua_S,1,0,&tolua_err) ||
 !tolua_isnumber(tolua_S,2,0,&tolua_err) ||
 !tolua_isnoobj(tolua_S,3,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
  int feat = ((int)  tolua_tonumber(tolua_S,1,0));
  u32b flag = ((u32b)  tolua_tonumber(tolua_S,2,0));
 {
  bool tolua_ret = (bool)  get_feat_flag1(feat,flag);
 tolua_pushboolean(tolua_S,(bool)tolua_ret);
 }
 }
 return 1;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'get_feat_flag1'.",&tolua_err);
 return 0;
#endif
}

/* get function: copyright */
static int tolua_get_init_copyright(lua_State* tolua_S)
{
 int tolua_index;
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
 tolua_pushstring(tolua_S,(const char*)copyright[tolua_index]);
 return 1;
}

/* set function: copyright */
static int tolua_set_init_copyright(lua_State* tolua_S)
{
 int tolua_index;
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
  copyright[tolua_index] = ((cptr)  tolua_tostring(tolua_S,3,0));
 return 0;
}

/* get function: version_major */
static int tolua_get_version_major(lua_State* tolua_S)
{
 tolua_pushnumber(tolua_S,(long)version_major);
 return 1;
}

/* set function: version_major */
static int tolua_set_version_major(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  version_major = ((byte)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: version_minor */
static int tolua_get_version_minor(lua_State* tolua_S)
{
 tolua_pushnumber(tolua_S,(long)version_minor);
 return 1;
}

/* set function: version_minor */
static int tolua_set_version_minor(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  version_minor = ((byte)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: version_patch */
static int tolua_get_version_patch(lua_State* tolua_S)
{
 tolua_pushnumber(tolua_S,(long)version_patch);
 return 1;
}

/* set function: version_patch */
static int tolua_set_version_patch(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  version_patch = ((byte)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: version_extra */
static int tolua_get_version_extra(lua_State* tolua_S)
{
 tolua_pushnumber(tolua_S,(long)version_extra);
 return 1;
}

/* set function: version_extra */
static int tolua_set_version_extra(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  version_extra = ((byte)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: sf_major */
static int tolua_get_sf_major(lua_State* tolua_S)
{
 tolua_pushnumber(tolua_S,(long)sf_major);
 return 1;
}

/* set function: sf_major */
static int tolua_set_sf_major(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  sf_major = ((byte)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: sf_minor */
static int tolua_get_sf_minor(lua_State* tolua_S)
{
 tolua_pushnumber(tolua_S,(long)sf_minor);
 return 1;
}

/* set function: sf_minor */
static int tolua_set_sf_minor(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  sf_minor = ((byte)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: sf_patch */
static int tolua_get_sf_patch(lua_State* tolua_S)
{
 tolua_pushnumber(tolua_S,(long)sf_patch);
 return 1;
}

/* set function: sf_patch */
static int tolua_set_sf_patch(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  sf_patch = ((byte)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: sf_extra */
static int tolua_get_sf_extra(lua_State* tolua_S)
{
 tolua_pushnumber(tolua_S,(long)sf_extra);
 return 1;
}

/* set function: sf_extra */
static int tolua_set_sf_extra(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  sf_extra = ((byte)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: sf_xtra */
static int tolua_get_sf_xtra(lua_State* tolua_S)
{
 tolua_pushnumber(tolua_S,(long)sf_xtra);
 return 1;
}

/* set function: sf_xtra */
static int tolua_set_sf_xtra(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  sf_xtra = ((u32b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: z_major */
static int tolua_get_z_major(lua_State* tolua_S)
{
 tolua_pushnumber(tolua_S,(long)z_major);
 return 1;
}

/* set function: z_major */
static int tolua_set_z_major(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  z_major = ((byte)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: z_minor */
static int tolua_get_z_minor(lua_State* tolua_S)
{
 tolua_pushnumber(tolua_S,(long)z_minor);
 return 1;
}

/* set function: z_minor */
static int tolua_set_z_minor(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  z_minor = ((byte)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: z_patch */
static int tolua_get_z_patch(lua_State* tolua_S)
{
 tolua_pushnumber(tolua_S,(long)z_patch);
 return 1;
}

/* set function: z_patch */
static int tolua_set_z_patch(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  z_patch = ((byte)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: sf_when */
static int tolua_get_sf_when(lua_State* tolua_S)
{
 tolua_pushnumber(tolua_S,(long)sf_when);
 return 1;
}

/* set function: sf_when */
static int tolua_set_sf_when(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  sf_when = ((u32b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: sf_lives */
static int tolua_get_sf_lives(lua_State* tolua_S)
{
 tolua_pushnumber(tolua_S,(long)sf_lives);
 return 1;
}

/* set function: sf_lives */
static int tolua_set_sf_lives(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  sf_lives = ((u16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: sf_saves */
static int tolua_get_sf_saves(lua_State* tolua_S)
{
 tolua_pushnumber(tolua_S,(long)sf_saves);
 return 1;
}

/* set function: sf_saves */
static int tolua_set_sf_saves(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  sf_saves = ((u16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: arg_fiddle */
static int tolua_get_arg_fiddle(lua_State* tolua_S)
{
 tolua_pushboolean(tolua_S,(bool)arg_fiddle);
 return 1;
}

/* set function: arg_fiddle */
static int tolua_set_arg_fiddle(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!tolua_isboolean(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  arg_fiddle = ((bool)  tolua_toboolean(tolua_S,2,0));
 return 0;
}

/* get function: arg_wizard */
static int tolua_get_arg_wizard(lua_State* tolua_S)
{
 tolua_pushboolean(tolua_S,(bool)arg_wizard);
 return 1;
}

/* set function: arg_wizard */
static int tolua_set_arg_wizard(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!tolua_isboolean(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  arg_wizard = ((bool)  tolua_toboolean(tolua_S,2,0));
 return 0;
}

/* get function: arg_sound */
static int tolua_get_arg_sound(lua_State* tolua_S)
{
 tolua_pushboolean(tolua_S,(bool)arg_sound);
 return 1;
}

/* set function: arg_sound */
static int tolua_set_arg_sound(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!tolua_isboolean(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  arg_sound = ((bool)  tolua_toboolean(tolua_S,2,0));
 return 0;
}

/* get function: arg_graphics */
static int tolua_get_arg_graphics(lua_State* tolua_S)
{
 tolua_pushboolean(tolua_S,(bool)arg_graphics);
 return 1;
}

/* set function: arg_graphics */
static int tolua_set_arg_graphics(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!tolua_isboolean(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  arg_graphics = ((bool)  tolua_toboolean(tolua_S,2,0));
 return 0;
}

/* get function: arg_force_original */
static int tolua_get_arg_force_original(lua_State* tolua_S)
{
 tolua_pushboolean(tolua_S,(bool)arg_force_original);
 return 1;
}

/* set function: arg_force_original */
static int tolua_set_arg_force_original(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!tolua_isboolean(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  arg_force_original = ((bool)  tolua_toboolean(tolua_S,2,0));
 return 0;
}

/* get function: arg_force_roguelike */
static int tolua_get_arg_force_roguelike(lua_State* tolua_S)
{
 tolua_pushboolean(tolua_S,(bool)arg_force_roguelike);
 return 1;
}

/* set function: arg_force_roguelike */
static int tolua_set_arg_force_roguelike(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!tolua_isboolean(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  arg_force_roguelike = ((bool)  tolua_toboolean(tolua_S,2,0));
 return 0;
}

/* get function: character_generated */
static int tolua_get_character_generated(lua_State* tolua_S)
{
 tolua_pushboolean(tolua_S,(bool)character_generated);
 return 1;
}

/* set function: character_generated */
static int tolua_set_character_generated(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!tolua_isboolean(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  character_generated = ((bool)  tolua_toboolean(tolua_S,2,0));
 return 0;
}

/* get function: character_dungeon */
static int tolua_get_character_dungeon(lua_State* tolua_S)
{
 tolua_pushboolean(tolua_S,(bool)character_dungeon);
 return 1;
}

/* set function: character_dungeon */
static int tolua_set_character_dungeon(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!tolua_isboolean(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  character_dungeon = ((bool)  tolua_toboolean(tolua_S,2,0));
 return 0;
}

/* get function: character_loaded */
static int tolua_get_character_loaded(lua_State* tolua_S)
{
 tolua_pushboolean(tolua_S,(bool)character_loaded);
 return 1;
}

/* set function: character_loaded */
static int tolua_set_character_loaded(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!tolua_isboolean(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  character_loaded = ((bool)  tolua_toboolean(tolua_S,2,0));
 return 0;
}

/* get function: character_saved */
static int tolua_get_character_saved(lua_State* tolua_S)
{
 tolua_pushboolean(tolua_S,(bool)character_saved);
 return 1;
}

/* set function: character_saved */
static int tolua_set_character_saved(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!tolua_isboolean(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  character_saved = ((bool)  tolua_toboolean(tolua_S,2,0));
 return 0;
}

/* get function: character_icky */
static int tolua_get_character_icky(lua_State* tolua_S)
{
 tolua_pushboolean(tolua_S,(bool)character_icky);
 return 1;
}

/* set function: character_icky */
static int tolua_set_character_icky(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!tolua_isboolean(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  character_icky = ((bool)  tolua_toboolean(tolua_S,2,0));
 return 0;
}

/* get function: character_xtra */
static int tolua_get_character_xtra(lua_State* tolua_S)
{
 tolua_pushboolean(tolua_S,(bool)character_xtra);
 return 1;
}

/* set function: character_xtra */
static int tolua_set_character_xtra(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!tolua_isboolean(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  character_xtra = ((bool)  tolua_toboolean(tolua_S,2,0));
 return 0;
}

/* get function: seed_flavor */
static int tolua_get_seed_flavor(lua_State* tolua_S)
{
 tolua_pushnumber(tolua_S,(long)seed_flavor);
 return 1;
}

/* set function: seed_flavor */
static int tolua_set_seed_flavor(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  seed_flavor = ((u32b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: seed_town */
static int tolua_get_seed_town(lua_State* tolua_S)
{
 tolua_pushnumber(tolua_S,(long)seed_town);
 return 1;
}

/* set function: seed_town */
static int tolua_set_seed_town(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  seed_town = ((u32b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: seed_dungeon */
static int tolua_get_seed_dungeon(lua_State* tolua_S)
{
 tolua_pushnumber(tolua_S,(long)seed_dungeon);
 return 1;
}

/* set function: seed_dungeon */
static int tolua_set_seed_dungeon(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  seed_dungeon = ((u32b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: command_cmd */
static int tolua_get_command_cmd(lua_State* tolua_S)
{
 tolua_pushnumber(tolua_S,(long)command_cmd);
 return 1;
}

/* set function: command_cmd */
static int tolua_set_command_cmd(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  command_cmd = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: command_arg */
static int tolua_get_command_arg(lua_State* tolua_S)
{
 tolua_pushnumber(tolua_S,(long)command_arg);
 return 1;
}

/* set function: command_arg */
static int tolua_set_command_arg(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  command_arg = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: command_rep */
static int tolua_get_command_rep(lua_State* tolua_S)
{
 tolua_pushnumber(tolua_S,(long)command_rep);
 return 1;
}

/* set function: command_rep */
static int tolua_set_command_rep(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  command_rep = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: command_dir */
static int tolua_get_command_dir(lua_State* tolua_S)
{
 tolua_pushnumber(tolua_S,(long)command_dir);
 return 1;
}

/* set function: command_dir */
static int tolua_set_command_dir(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  command_dir = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: command_see */
static int tolua_get_command_see(lua_State* tolua_S)
{
 tolua_pushnumber(tolua_S,(long)command_see);
 return 1;
}

/* set function: command_see */
static int tolua_set_command_see(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  command_see = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: command_gap */
static int tolua_get_command_gap(lua_State* tolua_S)
{
 tolua_pushnumber(tolua_S,(long)command_gap);
 return 1;
}

/* set function: command_gap */
static int tolua_set_command_gap(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  command_gap = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: command_wrk */
static int tolua_get_command_wrk(lua_State* tolua_S)
{
 tolua_pushnumber(tolua_S,(long)command_wrk);
 return 1;
}

/* set function: command_wrk */
static int tolua_set_command_wrk(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  command_wrk = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: command_new */
static int tolua_get_command_new(lua_State* tolua_S)
{
 tolua_pushnumber(tolua_S,(long)command_new);
 return 1;
}

/* set function: command_new */
static int tolua_set_command_new(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  command_new = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: energy_use */
static int tolua_get_energy_use(lua_State* tolua_S)
{
 tolua_pushnumber(tolua_S,(long)energy_use);
 return 1;
}

/* set function: energy_use */
static int tolua_set_energy_use(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  energy_use = ((s32b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: create_up_stair */
static int tolua_get_create_up_stair(lua_State* tolua_S)
{
 tolua_pushboolean(tolua_S,(bool)create_up_stair);
 return 1;
}

/* set function: create_up_stair */
static int tolua_set_create_up_stair(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!tolua_isboolean(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  create_up_stair = ((bool)  tolua_toboolean(tolua_S,2,0));
 return 0;
}

/* get function: create_down_stair */
static int tolua_get_create_down_stair(lua_State* tolua_S)
{
 tolua_pushboolean(tolua_S,(bool)create_down_stair);
 return 1;
}

/* set function: create_down_stair */
static int tolua_set_create_down_stair(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!tolua_isboolean(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  create_down_stair = ((bool)  tolua_toboolean(tolua_S,2,0));
 return 0;
}

/* get function: create_up_shaft */
static int tolua_get_create_up_shaft(lua_State* tolua_S)
{
 tolua_pushboolean(tolua_S,(bool)create_up_shaft);
 return 1;
}

/* set function: create_up_shaft */
static int tolua_set_create_up_shaft(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!tolua_isboolean(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  create_up_shaft = ((bool)  tolua_toboolean(tolua_S,2,0));
 return 0;
}

/* get function: create_down_shaft */
static int tolua_get_create_down_shaft(lua_State* tolua_S)
{
 tolua_pushboolean(tolua_S,(bool)create_down_shaft);
 return 1;
}

/* set function: create_down_shaft */
static int tolua_set_create_down_shaft(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!tolua_isboolean(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  create_down_shaft = ((bool)  tolua_toboolean(tolua_S,2,0));
 return 0;
}

/* get function: msg_flag */
static int tolua_get_msg_flag(lua_State* tolua_S)
{
 tolua_pushboolean(tolua_S,(bool)msg_flag);
 return 1;
}

/* set function: msg_flag */
static int tolua_set_msg_flag(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!tolua_isboolean(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  msg_flag = ((bool)  tolua_toboolean(tolua_S,2,0));
 return 0;
}

/* get function: alive */
static int tolua_get_alive(lua_State* tolua_S)
{
 tolua_pushboolean(tolua_S,(bool)alive);
 return 1;
}

/* set function: alive */
static int tolua_set_alive(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!tolua_isboolean(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  alive = ((bool)  tolua_toboolean(tolua_S,2,0));
 return 0;
}

/* get function: death */
static int tolua_get_death(lua_State* tolua_S)
{
 tolua_pushboolean(tolua_S,(bool)death);
 return 1;
}

/* set function: death */
static int tolua_set_death(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!tolua_isboolean(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  death = ((bool)  tolua_toboolean(tolua_S,2,0));
 return 0;
}

/* get function: running */
static int tolua_get_running(lua_State* tolua_S)
{
 tolua_pushnumber(tolua_S,(long)running);
 return 1;
}

/* set function: running */
static int tolua_set_running(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  running = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: resting */
static int tolua_get_resting(lua_State* tolua_S)
{
 tolua_pushnumber(tolua_S,(long)resting);
 return 1;
}

/* set function: resting */
static int tolua_set_resting(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  resting = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: cur_hgt */
static int tolua_get_cur_hgt(lua_State* tolua_S)
{
 tolua_pushnumber(tolua_S,(long)cur_hgt);
 return 1;
}

/* set function: cur_hgt */
static int tolua_set_cur_hgt(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  cur_hgt = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: cur_wid */
static int tolua_get_cur_wid(lua_State* tolua_S)
{
 tolua_pushnumber(tolua_S,(long)cur_wid);
 return 1;
}

/* set function: cur_wid */
static int tolua_set_cur_wid(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  cur_wid = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: dun_level */
static int tolua_get_dun_level(lua_State* tolua_S)
{
 tolua_pushnumber(tolua_S,(long)dun_level);
 return 1;
}

/* set function: dun_level */
static int tolua_set_dun_level(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  dun_level = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: old_dun_level */
static int tolua_get_old_dun_level(lua_State* tolua_S)
{
 tolua_pushnumber(tolua_S,(long)old_dun_level);
 return 1;
}

/* set function: old_dun_level */
static int tolua_set_old_dun_level(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  old_dun_level = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: num_repro */
static int tolua_get_num_repro(lua_State* tolua_S)
{
 tolua_pushnumber(tolua_S,(long)num_repro);
 return 1;
}

/* set function: num_repro */
static int tolua_set_num_repro(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  num_repro = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: object_level */
static int tolua_get_object_level(lua_State* tolua_S)
{
 tolua_pushnumber(tolua_S,(long)object_level);
 return 1;
}

/* set function: object_level */
static int tolua_set_object_level(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  object_level = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: monster_level */
static int tolua_get_monster_level(lua_State* tolua_S)
{
 tolua_pushnumber(tolua_S,(long)monster_level);
 return 1;
}

/* set function: monster_level */
static int tolua_set_monster_level(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  monster_level = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: turn */
static int tolua_get_turn(lua_State* tolua_S)
{
 tolua_pushnumber(tolua_S,(long)turn);
 return 1;
}

/* set function: turn */
static int tolua_set_turn(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  turn = ((s32b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: old_turn */
static int tolua_get_old_turn(lua_State* tolua_S)
{
 tolua_pushnumber(tolua_S,(long)old_turn);
 return 1;
}

/* set function: old_turn */
static int tolua_set_old_turn(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  old_turn = ((s32b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: wizard */
static int tolua_get_wizard(lua_State* tolua_S)
{
 tolua_pushboolean(tolua_S,(bool)wizard);
 return 1;
}

/* set function: wizard */
static int tolua_set_wizard(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!tolua_isboolean(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  wizard = ((bool)  tolua_toboolean(tolua_S,2,0));
 return 0;
}

/* get function: use_sound */
static int tolua_get_use_sound(lua_State* tolua_S)
{
 tolua_pushboolean(tolua_S,(bool)use_sound);
 return 1;
}

/* set function: use_sound */
static int tolua_set_use_sound(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!tolua_isboolean(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  use_sound = ((bool)  tolua_toboolean(tolua_S,2,0));
 return 0;
}

/* get function: use_graphics */
static int tolua_get_use_graphics(lua_State* tolua_S)
{
 tolua_pushboolean(tolua_S,(bool)use_graphics);
 return 1;
}

/* set function: use_graphics */
static int tolua_set_use_graphics(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!tolua_isboolean(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  use_graphics = ((bool)  tolua_toboolean(tolua_S,2,0));
 return 0;
}

/* get function: total_winner */
static int tolua_get_total_winner(lua_State* tolua_S)
{
 tolua_pushnumber(tolua_S,(long)total_winner);
 return 1;
}

/* set function: total_winner */
static int tolua_set_total_winner(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  total_winner = ((u16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: panic_save */
static int tolua_get_panic_save(lua_State* tolua_S)
{
 tolua_pushnumber(tolua_S,(long)panic_save);
 return 1;
}

/* set function: panic_save */
static int tolua_set_panic_save(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  panic_save = ((u16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: noscore */
static int tolua_get_noscore(lua_State* tolua_S)
{
 tolua_pushnumber(tolua_S,(long)noscore);
 return 1;
}

/* set function: noscore */
static int tolua_set_noscore(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  noscore = ((u16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: signal_count */
static int tolua_get_signal_count(lua_State* tolua_S)
{
 tolua_pushnumber(tolua_S,(long)signal_count);
 return 1;
}

/* set function: signal_count */
static int tolua_set_signal_count(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  signal_count = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: inkey_base */
static int tolua_get_inkey_base(lua_State* tolua_S)
{
 tolua_pushboolean(tolua_S,(bool)inkey_base);
 return 1;
}

/* set function: inkey_base */
static int tolua_set_inkey_base(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!tolua_isboolean(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  inkey_base = ((bool)  tolua_toboolean(tolua_S,2,0));
 return 0;
}

/* get function: inkey_xtra */
static int tolua_get_inkey_xtra(lua_State* tolua_S)
{
 tolua_pushboolean(tolua_S,(bool)inkey_xtra);
 return 1;
}

/* set function: inkey_xtra */
static int tolua_set_inkey_xtra(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!tolua_isboolean(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  inkey_xtra = ((bool)  tolua_toboolean(tolua_S,2,0));
 return 0;
}

/* get function: inkey_scan */
static int tolua_get_inkey_scan(lua_State* tolua_S)
{
 tolua_pushboolean(tolua_S,(bool)inkey_scan);
 return 1;
}

/* set function: inkey_scan */
static int tolua_set_inkey_scan(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!tolua_isboolean(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  inkey_scan = ((bool)  tolua_toboolean(tolua_S,2,0));
 return 0;
}

/* get function: inkey_flag */
static int tolua_get_inkey_flag(lua_State* tolua_S)
{
 tolua_pushboolean(tolua_S,(bool)inkey_flag);
 return 1;
}

/* set function: inkey_flag */
static int tolua_set_inkey_flag(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!tolua_isboolean(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  inkey_flag = ((bool)  tolua_toboolean(tolua_S,2,0));
 return 0;
}

/* get function: coin_type */
static int tolua_get_coin_type(lua_State* tolua_S)
{
 tolua_pushnumber(tolua_S,(long)coin_type);
 return 1;
}

/* set function: coin_type */
static int tolua_set_coin_type(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  coin_type = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: opening_chest */
static int tolua_get_opening_chest(lua_State* tolua_S)
{
 tolua_pushboolean(tolua_S,(bool)opening_chest);
 return 1;
}

/* set function: opening_chest */
static int tolua_set_opening_chest(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!tolua_isboolean(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  opening_chest = ((bool)  tolua_toboolean(tolua_S,2,0));
 return 0;
}

/* get function: shimmer_monsters */
static int tolua_get_shimmer_monsters(lua_State* tolua_S)
{
 tolua_pushboolean(tolua_S,(bool)shimmer_monsters);
 return 1;
}

/* set function: shimmer_monsters */
static int tolua_set_shimmer_monsters(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!tolua_isboolean(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  shimmer_monsters = ((bool)  tolua_toboolean(tolua_S,2,0));
 return 0;
}

/* get function: shimmer_objects */
static int tolua_get_shimmer_objects(lua_State* tolua_S)
{
 tolua_pushboolean(tolua_S,(bool)shimmer_objects);
 return 1;
}

/* set function: shimmer_objects */
static int tolua_set_shimmer_objects(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!tolua_isboolean(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  shimmer_objects = ((bool)  tolua_toboolean(tolua_S,2,0));
 return 0;
}

/* get function: repair_monsters */
static int tolua_get_repair_monsters(lua_State* tolua_S)
{
 tolua_pushboolean(tolua_S,(bool)repair_monsters);
 return 1;
}

/* set function: repair_monsters */
static int tolua_set_repair_monsters(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!tolua_isboolean(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  repair_monsters = ((bool)  tolua_toboolean(tolua_S,2,0));
 return 0;
}

/* get function: repair_objects */
static int tolua_get_repair_objects(lua_State* tolua_S)
{
 tolua_pushboolean(tolua_S,(bool)repair_objects);
 return 1;
}

/* set function: repair_objects */
static int tolua_set_repair_objects(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!tolua_isboolean(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  repair_objects = ((bool)  tolua_toboolean(tolua_S,2,0));
 return 0;
}

/* get function: total_weight */
static int tolua_get_total_weight(lua_State* tolua_S)
{
 tolua_pushnumber(tolua_S,(long)total_weight);
 return 1;
}

/* set function: total_weight */
static int tolua_set_total_weight(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  total_weight = ((s32b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: inven_nxt */
static int tolua_get_inven_nxt(lua_State* tolua_S)
{
 tolua_pushnumber(tolua_S,(long)inven_nxt);
 return 1;
}

/* set function: inven_nxt */
static int tolua_set_inven_nxt(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  inven_nxt = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: inven_cnt */
static int tolua_get_inven_cnt(lua_State* tolua_S)
{
 tolua_pushnumber(tolua_S,(long)inven_cnt);
 return 1;
}

/* set function: inven_cnt */
static int tolua_set_inven_cnt(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  inven_cnt = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: equip_cnt */
static int tolua_get_equip_cnt(lua_State* tolua_S)
{
 tolua_pushnumber(tolua_S,(long)equip_cnt);
 return 1;
}

/* set function: equip_cnt */
static int tolua_set_equip_cnt(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  equip_cnt = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: o_max */
static int tolua_get_o_max(lua_State* tolua_S)
{
 tolua_pushnumber(tolua_S,(long)o_max);
 return 1;
}

/* set function: o_max */
static int tolua_set_o_max(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  o_max = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: o_cnt */
static int tolua_get_o_cnt(lua_State* tolua_S)
{
 tolua_pushnumber(tolua_S,(long)o_cnt);
 return 1;
}

/* set function: o_cnt */
static int tolua_set_o_cnt(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  o_cnt = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: m_max */
static int tolua_get_m_max(lua_State* tolua_S)
{
 tolua_pushnumber(tolua_S,(long)m_max);
 return 1;
}

/* set function: m_max */
static int tolua_set_m_max(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  m_max = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: m_cnt */
static int tolua_get_m_cnt(lua_State* tolua_S)
{
 tolua_pushnumber(tolua_S,(long)m_cnt);
 return 1;
}

/* set function: m_cnt */
static int tolua_set_m_cnt(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  m_cnt = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: hack_m_idx */
static int tolua_get_hack_m_idx(lua_State* tolua_S)
{
 tolua_pushnumber(tolua_S,(long)hack_m_idx);
 return 1;
}

/* set function: hack_m_idx */
static int tolua_set_hack_m_idx(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  hack_m_idx = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: hack_m_idx_ii */
static int tolua_get_hack_m_idx_ii(lua_State* tolua_S)
{
 tolua_pushnumber(tolua_S,(long)hack_m_idx_ii);
 return 1;
}

/* set function: hack_m_idx_ii */
static int tolua_set_hack_m_idx_ii(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  hack_m_idx_ii = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: total_friends */
static int tolua_get_total_friends(lua_State* tolua_S)
{
 tolua_pushnumber(tolua_S,(long)total_friends);
 return 1;
}

/* set function: total_friends */
static int tolua_set_total_friends(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  total_friends = ((int)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: total_friend_levels */
static int tolua_get_total_friend_levels(lua_State* tolua_S)
{
 tolua_pushnumber(tolua_S,(long)total_friend_levels);
 return 1;
}

/* set function: total_friend_levels */
static int tolua_set_total_friend_levels(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  total_friend_levels = ((s32b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: leaving_quest */
static int tolua_get_leaving_quest(lua_State* tolua_S)
{
 tolua_pushnumber(tolua_S,(long)leaving_quest);
 return 1;
}

/* set function: leaving_quest */
static int tolua_set_leaving_quest(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  leaving_quest = ((int)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: multi_rew */
static int tolua_get_multi_rew(lua_State* tolua_S)
{
 tolua_pushboolean(tolua_S,(bool)multi_rew);
 return 1;
}

/* set function: multi_rew */
static int tolua_set_multi_rew(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!tolua_isboolean(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  multi_rew = ((bool)  tolua_toboolean(tolua_S,2,0));
 return 0;
}

/* get function: summon_kin_type */
static int tolua_get_summon_kin_type(lua_State* tolua_S)
{
 tolua_pushnumber(tolua_S,(long)summon_kin_type);
 return 1;
}

/* set function: summon_kin_type */
static int tolua_set_summon_kin_type(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  summon_kin_type = ((char)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: hack_mind */
static int tolua_get_hack_mind(lua_State* tolua_S)
{
 tolua_pushboolean(tolua_S,(bool)hack_mind);
 return 1;
}

/* set function: hack_mind */
static int tolua_set_hack_mind(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!tolua_isboolean(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  hack_mind = ((bool)  tolua_toboolean(tolua_S,2,0));
 return 0;
}

/* get function: hack_mutation */
static int tolua_get_hack_mutation(lua_State* tolua_S)
{
 tolua_pushboolean(tolua_S,(bool)hack_mutation);
 return 1;
}

/* set function: hack_mutation */
static int tolua_set_hack_mutation(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!tolua_isboolean(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  hack_mutation = ((bool)  tolua_toboolean(tolua_S,2,0));
 return 0;
}

/* get function: is_autosave */
static int tolua_get_is_autosave(lua_State* tolua_S)
{
 tolua_pushboolean(tolua_S,(bool)is_autosave);
 return 1;
}

/* set function: is_autosave */
static int tolua_set_is_autosave(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!tolua_isboolean(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  is_autosave = ((bool)  tolua_toboolean(tolua_S,2,0));
 return 0;
}

/* get function: artifact_bias */
static int tolua_get_artifact_bias(lua_State* tolua_S)
{
 tolua_pushnumber(tolua_S,(long)artifact_bias);
 return 1;
}

/* set function: artifact_bias */
static int tolua_set_artifact_bias(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  artifact_bias = ((int)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: show_inven_graph */
static int tolua_get_show_inven_graph(lua_State* tolua_S)
{
 tolua_pushboolean(tolua_S,(bool)show_inven_graph);
 return 1;
}

/* set function: show_inven_graph */
static int tolua_set_show_inven_graph(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!tolua_isboolean(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  show_inven_graph = ((bool)  tolua_toboolean(tolua_S,2,0));
 return 0;
}

/* get function: show_store_graph */
static int tolua_get_show_store_graph(lua_State* tolua_S)
{
 tolua_pushboolean(tolua_S,(bool)show_store_graph);
 return 1;
}

/* set function: show_store_graph */
static int tolua_set_show_store_graph(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!tolua_isboolean(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  show_store_graph = ((bool)  tolua_toboolean(tolua_S,2,0));
 return 0;
}

/* get function: show_equip_graph */
static int tolua_get_show_equip_graph(lua_State* tolua_S)
{
 tolua_pushboolean(tolua_S,(bool)show_equip_graph);
 return 1;
}

/* set function: show_equip_graph */
static int tolua_set_show_equip_graph(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!tolua_isboolean(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  show_equip_graph = ((bool)  tolua_toboolean(tolua_S,2,0));
 return 0;
}

/* get function: rogue_like_commands */
static int tolua_get_rogue_like_commands(lua_State* tolua_S)
{
 tolua_pushboolean(tolua_S,(bool)rogue_like_commands);
 return 1;
}

/* set function: rogue_like_commands */
static int tolua_set_rogue_like_commands(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!tolua_isboolean(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  rogue_like_commands = ((bool)  tolua_toboolean(tolua_S,2,0));
 return 0;
}

/* get function: quick_messages */
static int tolua_get_quick_messages(lua_State* tolua_S)
{
 tolua_pushboolean(tolua_S,(bool)quick_messages);
 return 1;
}

/* set function: quick_messages */
static int tolua_set_quick_messages(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!tolua_isboolean(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  quick_messages = ((bool)  tolua_toboolean(tolua_S,2,0));
 return 0;
}

/* get function: other_query_flag */
static int tolua_get_other_query_flag(lua_State* tolua_S)
{
 tolua_pushboolean(tolua_S,(bool)other_query_flag);
 return 1;
}

/* set function: other_query_flag */
static int tolua_set_other_query_flag(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!tolua_isboolean(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  other_query_flag = ((bool)  tolua_toboolean(tolua_S,2,0));
 return 0;
}

/* get function: carry_query_flag */
static int tolua_get_carry_query_flag(lua_State* tolua_S)
{
 tolua_pushboolean(tolua_S,(bool)carry_query_flag);
 return 1;
}

/* set function: carry_query_flag */
static int tolua_set_carry_query_flag(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!tolua_isboolean(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  carry_query_flag = ((bool)  tolua_toboolean(tolua_S,2,0));
 return 0;
}

/* get function: always_pickup */
static int tolua_get_always_pickup(lua_State* tolua_S)
{
 tolua_pushboolean(tolua_S,(bool)always_pickup);
 return 1;
}

/* set function: always_pickup */
static int tolua_set_always_pickup(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!tolua_isboolean(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  always_pickup = ((bool)  tolua_toboolean(tolua_S,2,0));
 return 0;
}

/* get function: no_pickup_corpse */
static int tolua_get_no_pickup_corpse(lua_State* tolua_S)
{
 tolua_pushboolean(tolua_S,(bool)no_pickup_corpse);
 return 1;
}

/* set function: no_pickup_corpse */
static int tolua_set_no_pickup_corpse(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!tolua_isboolean(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  no_pickup_corpse = ((bool)  tolua_toboolean(tolua_S,2,0));
 return 0;
}

/* get function: always_repeat */
static int tolua_get_always_repeat(lua_State* tolua_S)
{
 tolua_pushboolean(tolua_S,(bool)always_repeat);
 return 1;
}

/* set function: always_repeat */
static int tolua_set_always_repeat(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!tolua_isboolean(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  always_repeat = ((bool)  tolua_toboolean(tolua_S,2,0));
 return 0;
}

/* get function: use_old_target */
static int tolua_get_use_old_target(lua_State* tolua_S)
{
 tolua_pushboolean(tolua_S,(bool)use_old_target);
 return 1;
}

/* set function: use_old_target */
static int tolua_set_use_old_target(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!tolua_isboolean(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  use_old_target = ((bool)  tolua_toboolean(tolua_S,2,0));
 return 0;
}

/* get function: depth_in_feet */
static int tolua_get_depth_in_feet(lua_State* tolua_S)
{
 tolua_pushboolean(tolua_S,(bool)depth_in_feet);
 return 1;
}

/* set function: depth_in_feet */
static int tolua_set_depth_in_feet(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!tolua_isboolean(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  depth_in_feet = ((bool)  tolua_toboolean(tolua_S,2,0));
 return 0;
}

/* get function: use_color */
static int tolua_get_use_color(lua_State* tolua_S)
{
 tolua_pushboolean(tolua_S,(bool)use_color);
 return 1;
}

/* set function: use_color */
static int tolua_set_use_color(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!tolua_isboolean(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  use_color = ((bool)  tolua_toboolean(tolua_S,2,0));
 return 0;
}

/* get function: compress_savefile */
static int tolua_get_compress_savefile(lua_State* tolua_S)
{
 tolua_pushboolean(tolua_S,(bool)compress_savefile);
 return 1;
}

/* set function: compress_savefile */
static int tolua_set_compress_savefile(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!tolua_isboolean(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  compress_savefile = ((bool)  tolua_toboolean(tolua_S,2,0));
 return 0;
}

/* get function: hilite_player */
static int tolua_get_hilite_player(lua_State* tolua_S)
{
 tolua_pushboolean(tolua_S,(bool)hilite_player);
 return 1;
}

/* set function: hilite_player */
static int tolua_set_hilite_player(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!tolua_isboolean(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  hilite_player = ((bool)  tolua_toboolean(tolua_S,2,0));
 return 0;
}

/* get function: ring_bell */
static int tolua_get_ring_bell(lua_State* tolua_S)
{
 tolua_pushboolean(tolua_S,(bool)ring_bell);
 return 1;
}

/* set function: ring_bell */
static int tolua_set_ring_bell(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!tolua_isboolean(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  ring_bell = ((bool)  tolua_toboolean(tolua_S,2,0));
 return 0;
}

/* get function: find_ignore_stairs */
static int tolua_get_find_ignore_stairs(lua_State* tolua_S)
{
 tolua_pushboolean(tolua_S,(bool)find_ignore_stairs);
 return 1;
}

/* set function: find_ignore_stairs */
static int tolua_set_find_ignore_stairs(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!tolua_isboolean(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  find_ignore_stairs = ((bool)  tolua_toboolean(tolua_S,2,0));
 return 0;
}

/* get function: find_ignore_doors */
static int tolua_get_find_ignore_doors(lua_State* tolua_S)
{
 tolua_pushboolean(tolua_S,(bool)find_ignore_doors);
 return 1;
}

/* set function: find_ignore_doors */
static int tolua_set_find_ignore_doors(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!tolua_isboolean(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  find_ignore_doors = ((bool)  tolua_toboolean(tolua_S,2,0));
 return 0;
}

/* get function: find_cut */
static int tolua_get_find_cut(lua_State* tolua_S)
{
 tolua_pushboolean(tolua_S,(bool)find_cut);
 return 1;
}

/* set function: find_cut */
static int tolua_set_find_cut(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!tolua_isboolean(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  find_cut = ((bool)  tolua_toboolean(tolua_S,2,0));
 return 0;
}

/* get function: find_examine */
static int tolua_get_find_examine(lua_State* tolua_S)
{
 tolua_pushboolean(tolua_S,(bool)find_examine);
 return 1;
}

/* set function: find_examine */
static int tolua_set_find_examine(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!tolua_isboolean(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  find_examine = ((bool)  tolua_toboolean(tolua_S,2,0));
 return 0;
}

/* get function: disturb_near */
static int tolua_get_disturb_near(lua_State* tolua_S)
{
 tolua_pushboolean(tolua_S,(bool)disturb_near);
 return 1;
}

/* set function: disturb_near */
static int tolua_set_disturb_near(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!tolua_isboolean(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  disturb_near = ((bool)  tolua_toboolean(tolua_S,2,0));
 return 0;
}

/* get function: disturb_move */
static int tolua_get_disturb_move(lua_State* tolua_S)
{
 tolua_pushboolean(tolua_S,(bool)disturb_move);
 return 1;
}

/* set function: disturb_move */
static int tolua_set_disturb_move(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!tolua_isboolean(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  disturb_move = ((bool)  tolua_toboolean(tolua_S,2,0));
 return 0;
}

/* get function: disturb_panel */
static int tolua_get_disturb_panel(lua_State* tolua_S)
{
 tolua_pushboolean(tolua_S,(bool)disturb_panel);
 return 1;
}

/* set function: disturb_panel */
static int tolua_set_disturb_panel(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!tolua_isboolean(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  disturb_panel = ((bool)  tolua_toboolean(tolua_S,2,0));
 return 0;
}

/* get function: disturb_state */
static int tolua_get_disturb_state(lua_State* tolua_S)
{
 tolua_pushboolean(tolua_S,(bool)disturb_state);
 return 1;
}

/* set function: disturb_state */
static int tolua_set_disturb_state(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!tolua_isboolean(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  disturb_state = ((bool)  tolua_toboolean(tolua_S,2,0));
 return 0;
}

/* get function: disturb_minor */
static int tolua_get_disturb_minor(lua_State* tolua_S)
{
 tolua_pushboolean(tolua_S,(bool)disturb_minor);
 return 1;
}

/* set function: disturb_minor */
static int tolua_set_disturb_minor(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!tolua_isboolean(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  disturb_minor = ((bool)  tolua_toboolean(tolua_S,2,0));
 return 0;
}

/* get function: disturb_other */
static int tolua_get_disturb_other(lua_State* tolua_S)
{
 tolua_pushboolean(tolua_S,(bool)disturb_other);
 return 1;
}

/* set function: disturb_other */
static int tolua_set_disturb_other(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!tolua_isboolean(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  disturb_other = ((bool)  tolua_toboolean(tolua_S,2,0));
 return 0;
}

/* get function: avoid_abort */
static int tolua_get_avoid_abort(lua_State* tolua_S)
{
 tolua_pushboolean(tolua_S,(bool)avoid_abort);
 return 1;
}

/* set function: avoid_abort */
static int tolua_set_avoid_abort(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!tolua_isboolean(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  avoid_abort = ((bool)  tolua_toboolean(tolua_S,2,0));
 return 0;
}

/* get function: avoid_other */
static int tolua_get_avoid_other(lua_State* tolua_S)
{
 tolua_pushboolean(tolua_S,(bool)avoid_other);
 return 1;
}

/* set function: avoid_other */
static int tolua_set_avoid_other(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!tolua_isboolean(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  avoid_other = ((bool)  tolua_toboolean(tolua_S,2,0));
 return 0;
}

/* get function: flush_disturb */
static int tolua_get_flush_disturb(lua_State* tolua_S)
{
 tolua_pushboolean(tolua_S,(bool)flush_disturb);
 return 1;
}

/* set function: flush_disturb */
static int tolua_set_flush_disturb(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!tolua_isboolean(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  flush_disturb = ((bool)  tolua_toboolean(tolua_S,2,0));
 return 0;
}

/* get function: flush_failure */
static int tolua_get_flush_failure(lua_State* tolua_S)
{
 tolua_pushboolean(tolua_S,(bool)flush_failure);
 return 1;
}

/* set function: flush_failure */
static int tolua_set_flush_failure(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!tolua_isboolean(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  flush_failure = ((bool)  tolua_toboolean(tolua_S,2,0));
 return 0;
}

/* get function: flush_command */
static int tolua_get_flush_command(lua_State* tolua_S)
{
 tolua_pushboolean(tolua_S,(bool)flush_command);
 return 1;
}

/* set function: flush_command */
static int tolua_set_flush_command(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!tolua_isboolean(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  flush_command = ((bool)  tolua_toboolean(tolua_S,2,0));
 return 0;
}

/* get function: fresh_before */
static int tolua_get_fresh_before(lua_State* tolua_S)
{
 tolua_pushboolean(tolua_S,(bool)fresh_before);
 return 1;
}

/* set function: fresh_before */
static int tolua_set_fresh_before(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!tolua_isboolean(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  fresh_before = ((bool)  tolua_toboolean(tolua_S,2,0));
 return 0;
}

/* get function: fresh_after */
static int tolua_get_fresh_after(lua_State* tolua_S)
{
 tolua_pushboolean(tolua_S,(bool)fresh_after);
 return 1;
}

/* set function: fresh_after */
static int tolua_set_fresh_after(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!tolua_isboolean(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  fresh_after = ((bool)  tolua_toboolean(tolua_S,2,0));
 return 0;
}

/* get function: fresh_message */
static int tolua_get_fresh_message(lua_State* tolua_S)
{
 tolua_pushboolean(tolua_S,(bool)fresh_message);
 return 1;
}

/* set function: fresh_message */
static int tolua_set_fresh_message(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!tolua_isboolean(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  fresh_message = ((bool)  tolua_toboolean(tolua_S,2,0));
 return 0;
}

/* get function: alert_hitpoint */
static int tolua_get_alert_hitpoint(lua_State* tolua_S)
{
 tolua_pushboolean(tolua_S,(bool)alert_hitpoint);
 return 1;
}

/* set function: alert_hitpoint */
static int tolua_set_alert_hitpoint(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!tolua_isboolean(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  alert_hitpoint = ((bool)  tolua_toboolean(tolua_S,2,0));
 return 0;
}

/* get function: alert_failure */
static int tolua_get_alert_failure(lua_State* tolua_S)
{
 tolua_pushboolean(tolua_S,(bool)alert_failure);
 return 1;
}

/* set function: alert_failure */
static int tolua_set_alert_failure(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!tolua_isboolean(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  alert_failure = ((bool)  tolua_toboolean(tolua_S,2,0));
 return 0;
}

/* get function: view_yellow_lite */
static int tolua_get_view_yellow_lite(lua_State* tolua_S)
{
 tolua_pushboolean(tolua_S,(bool)view_yellow_lite);
 return 1;
}

/* set function: view_yellow_lite */
static int tolua_set_view_yellow_lite(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!tolua_isboolean(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  view_yellow_lite = ((bool)  tolua_toboolean(tolua_S,2,0));
 return 0;
}

/* get function: view_bright_lite */
static int tolua_get_view_bright_lite(lua_State* tolua_S)
{
 tolua_pushboolean(tolua_S,(bool)view_bright_lite);
 return 1;
}

/* set function: view_bright_lite */
static int tolua_set_view_bright_lite(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!tolua_isboolean(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  view_bright_lite = ((bool)  tolua_toboolean(tolua_S,2,0));
 return 0;
}

/* get function: view_granite_lite */
static int tolua_get_view_granite_lite(lua_State* tolua_S)
{
 tolua_pushboolean(tolua_S,(bool)view_granite_lite);
 return 1;
}

/* set function: view_granite_lite */
static int tolua_set_view_granite_lite(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!tolua_isboolean(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  view_granite_lite = ((bool)  tolua_toboolean(tolua_S,2,0));
 return 0;
}

/* get function: view_special_lite */
static int tolua_get_view_special_lite(lua_State* tolua_S)
{
 tolua_pushboolean(tolua_S,(bool)view_special_lite);
 return 1;
}

/* set function: view_special_lite */
static int tolua_set_view_special_lite(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!tolua_isboolean(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  view_special_lite = ((bool)  tolua_toboolean(tolua_S,2,0));
 return 0;
}

/* get function: skip_mutations */
static int tolua_get_skip_mutations(lua_State* tolua_S)
{
 tolua_pushboolean(tolua_S,(bool)skip_mutations);
 return 1;
}

/* set function: skip_mutations */
static int tolua_set_skip_mutations(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!tolua_isboolean(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  skip_mutations = ((bool)  tolua_toboolean(tolua_S,2,0));
 return 0;
}

/* get function: plain_descriptions */
static int tolua_get_plain_descriptions(lua_State* tolua_S)
{
 tolua_pushboolean(tolua_S,(bool)plain_descriptions);
 return 1;
}

/* set function: plain_descriptions */
static int tolua_set_plain_descriptions(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!tolua_isboolean(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  plain_descriptions = ((bool)  tolua_toboolean(tolua_S,2,0));
 return 0;
}

/* get function: stupid_monsters */
static int tolua_get_stupid_monsters(lua_State* tolua_S)
{
 tolua_pushboolean(tolua_S,(bool)stupid_monsters);
 return 1;
}

/* set function: stupid_monsters */
static int tolua_set_stupid_monsters(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!tolua_isboolean(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  stupid_monsters = ((bool)  tolua_toboolean(tolua_S,2,0));
 return 0;
}

/* get function: auto_destroy */
static int tolua_get_auto_destroy(lua_State* tolua_S)
{
 tolua_pushboolean(tolua_S,(bool)auto_destroy);
 return 1;
}

/* set function: auto_destroy */
static int tolua_set_auto_destroy(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!tolua_isboolean(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  auto_destroy = ((bool)  tolua_toboolean(tolua_S,2,0));
 return 0;
}

/* get function: wear_confirm */
static int tolua_get_wear_confirm(lua_State* tolua_S)
{
 tolua_pushboolean(tolua_S,(bool)wear_confirm);
 return 1;
}

/* set function: wear_confirm */
static int tolua_set_wear_confirm(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!tolua_isboolean(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  wear_confirm = ((bool)  tolua_toboolean(tolua_S,2,0));
 return 0;
}

/* get function: confirm_stairs */
static int tolua_get_confirm_stairs(lua_State* tolua_S)
{
 tolua_pushboolean(tolua_S,(bool)confirm_stairs);
 return 1;
}

/* set function: confirm_stairs */
static int tolua_set_confirm_stairs(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!tolua_isboolean(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  confirm_stairs = ((bool)  tolua_toboolean(tolua_S,2,0));
 return 0;
}

/* get function: disturb_pets */
static int tolua_get_disturb_pets(lua_State* tolua_S)
{
 tolua_pushboolean(tolua_S,(bool)disturb_pets);
 return 1;
}

/* set function: disturb_pets */
static int tolua_set_disturb_pets(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!tolua_isboolean(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  disturb_pets = ((bool)  tolua_toboolean(tolua_S,2,0));
 return 0;
}

/* get function: view_perma_grids */
static int tolua_get_view_perma_grids(lua_State* tolua_S)
{
 tolua_pushboolean(tolua_S,(bool)view_perma_grids);
 return 1;
}

/* set function: view_perma_grids */
static int tolua_set_view_perma_grids(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!tolua_isboolean(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  view_perma_grids = ((bool)  tolua_toboolean(tolua_S,2,0));
 return 0;
}

/* get function: view_torch_grids */
static int tolua_get_view_torch_grids(lua_State* tolua_S)
{
 tolua_pushboolean(tolua_S,(bool)view_torch_grids);
 return 1;
}

/* set function: view_torch_grids */
static int tolua_set_view_torch_grids(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!tolua_isboolean(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  view_torch_grids = ((bool)  tolua_toboolean(tolua_S,2,0));
 return 0;
}

/* get function: flow_by_sound */
static int tolua_get_flow_by_sound(lua_State* tolua_S)
{
 tolua_pushboolean(tolua_S,(bool)flow_by_sound);
 return 1;
}

/* set function: flow_by_sound */
static int tolua_set_flow_by_sound(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!tolua_isboolean(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  flow_by_sound = ((bool)  tolua_toboolean(tolua_S,2,0));
 return 0;
}

/* get function: flow_by_smell */
static int tolua_get_flow_by_smell(lua_State* tolua_S)
{
 tolua_pushboolean(tolua_S,(bool)flow_by_smell);
 return 1;
}

/* set function: flow_by_smell */
static int tolua_set_flow_by_smell(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!tolua_isboolean(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  flow_by_smell = ((bool)  tolua_toboolean(tolua_S,2,0));
 return 0;
}

/* get function: track_follow */
static int tolua_get_track_follow(lua_State* tolua_S)
{
 tolua_pushboolean(tolua_S,(bool)track_follow);
 return 1;
}

/* set function: track_follow */
static int tolua_set_track_follow(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!tolua_isboolean(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  track_follow = ((bool)  tolua_toboolean(tolua_S,2,0));
 return 0;
}

/* get function: track_target */
static int tolua_get_track_target(lua_State* tolua_S)
{
 tolua_pushboolean(tolua_S,(bool)track_target);
 return 1;
}

/* set function: track_target */
static int tolua_set_track_target(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!tolua_isboolean(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  track_target = ((bool)  tolua_toboolean(tolua_S,2,0));
 return 0;
}

/* get function: stack_allow_items */
static int tolua_get_stack_allow_items(lua_State* tolua_S)
{
 tolua_pushboolean(tolua_S,(bool)stack_allow_items);
 return 1;
}

/* set function: stack_allow_items */
static int tolua_set_stack_allow_items(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!tolua_isboolean(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  stack_allow_items = ((bool)  tolua_toboolean(tolua_S,2,0));
 return 0;
}

/* get function: stack_allow_wands */
static int tolua_get_stack_allow_wands(lua_State* tolua_S)
{
 tolua_pushboolean(tolua_S,(bool)stack_allow_wands);
 return 1;
}

/* set function: stack_allow_wands */
static int tolua_set_stack_allow_wands(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!tolua_isboolean(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  stack_allow_wands = ((bool)  tolua_toboolean(tolua_S,2,0));
 return 0;
}

/* get function: stack_force_notes */
static int tolua_get_stack_force_notes(lua_State* tolua_S)
{
 tolua_pushboolean(tolua_S,(bool)stack_force_notes);
 return 1;
}

/* set function: stack_force_notes */
static int tolua_set_stack_force_notes(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!tolua_isboolean(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  stack_force_notes = ((bool)  tolua_toboolean(tolua_S,2,0));
 return 0;
}

/* get function: stack_force_costs */
static int tolua_get_stack_force_costs(lua_State* tolua_S)
{
 tolua_pushboolean(tolua_S,(bool)stack_force_costs);
 return 1;
}

/* set function: stack_force_costs */
static int tolua_set_stack_force_costs(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!tolua_isboolean(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  stack_force_costs = ((bool)  tolua_toboolean(tolua_S,2,0));
 return 0;
}

/* get function: view_reduce_lite */
static int tolua_get_view_reduce_lite(lua_State* tolua_S)
{
 tolua_pushboolean(tolua_S,(bool)view_reduce_lite);
 return 1;
}

/* set function: view_reduce_lite */
static int tolua_set_view_reduce_lite(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!tolua_isboolean(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  view_reduce_lite = ((bool)  tolua_toboolean(tolua_S,2,0));
 return 0;
}

/* get function: view_reduce_view */
static int tolua_get_view_reduce_view(lua_State* tolua_S)
{
 tolua_pushboolean(tolua_S,(bool)view_reduce_view);
 return 1;
}

/* set function: view_reduce_view */
static int tolua_set_view_reduce_view(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!tolua_isboolean(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  view_reduce_view = ((bool)  tolua_toboolean(tolua_S,2,0));
 return 0;
}

/* get function: auto_haggle */
static int tolua_get_auto_haggle(lua_State* tolua_S)
{
 tolua_pushboolean(tolua_S,(bool)auto_haggle);
 return 1;
}

/* set function: auto_haggle */
static int tolua_set_auto_haggle(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!tolua_isboolean(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  auto_haggle = ((bool)  tolua_toboolean(tolua_S,2,0));
 return 0;
}

/* get function: auto_scum */
static int tolua_get_auto_scum(lua_State* tolua_S)
{
 tolua_pushboolean(tolua_S,(bool)auto_scum);
 return 1;
}

/* set function: auto_scum */
static int tolua_set_auto_scum(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!tolua_isboolean(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  auto_scum = ((bool)  tolua_toboolean(tolua_S,2,0));
 return 0;
}

/* get function: expand_look */
static int tolua_get_expand_look(lua_State* tolua_S)
{
 tolua_pushboolean(tolua_S,(bool)expand_look);
 return 1;
}

/* set function: expand_look */
static int tolua_set_expand_look(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!tolua_isboolean(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  expand_look = ((bool)  tolua_toboolean(tolua_S,2,0));
 return 0;
}

/* get function: expand_list */
static int tolua_get_expand_list(lua_State* tolua_S)
{
 tolua_pushboolean(tolua_S,(bool)expand_list);
 return 1;
}

/* set function: expand_list */
static int tolua_set_expand_list(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!tolua_isboolean(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  expand_list = ((bool)  tolua_toboolean(tolua_S,2,0));
 return 0;
}

/* get function: dungeon_align */
static int tolua_get_dungeon_align(lua_State* tolua_S)
{
 tolua_pushboolean(tolua_S,(bool)dungeon_align);
 return 1;
}

/* set function: dungeon_align */
static int tolua_set_dungeon_align(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!tolua_isboolean(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  dungeon_align = ((bool)  tolua_toboolean(tolua_S,2,0));
 return 0;
}

/* get function: dungeon_stair */
static int tolua_get_dungeon_stair(lua_State* tolua_S)
{
 tolua_pushboolean(tolua_S,(bool)dungeon_stair);
 return 1;
}

/* set function: dungeon_stair */
static int tolua_set_dungeon_stair(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!tolua_isboolean(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  dungeon_stair = ((bool)  tolua_toboolean(tolua_S,2,0));
 return 0;
}

/* get function: smart_learn */
static int tolua_get_smart_learn(lua_State* tolua_S)
{
 tolua_pushboolean(tolua_S,(bool)smart_learn);
 return 1;
}

/* set function: smart_learn */
static int tolua_set_smart_learn(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!tolua_isboolean(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  smart_learn = ((bool)  tolua_toboolean(tolua_S,2,0));
 return 0;
}

/* get function: smart_cheat */
static int tolua_get_smart_cheat(lua_State* tolua_S)
{
 tolua_pushboolean(tolua_S,(bool)smart_cheat);
 return 1;
}

/* set function: smart_cheat */
static int tolua_set_smart_cheat(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!tolua_isboolean(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  smart_cheat = ((bool)  tolua_toboolean(tolua_S,2,0));
 return 0;
}

/* get function: show_labels */
static int tolua_get_show_labels(lua_State* tolua_S)
{
 tolua_pushboolean(tolua_S,(bool)show_labels);
 return 1;
}

/* set function: show_labels */
static int tolua_set_show_labels(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!tolua_isboolean(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  show_labels = ((bool)  tolua_toboolean(tolua_S,2,0));
 return 0;
}

/* get function: show_weights */
static int tolua_get_show_weights(lua_State* tolua_S)
{
 tolua_pushboolean(tolua_S,(bool)show_weights);
 return 1;
}

/* set function: show_weights */
static int tolua_set_show_weights(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!tolua_isboolean(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  show_weights = ((bool)  tolua_toboolean(tolua_S,2,0));
 return 0;
}

/* get function: show_choices */
static int tolua_get_show_choices(lua_State* tolua_S)
{
 tolua_pushboolean(tolua_S,(bool)show_choices);
 return 1;
}

/* set function: show_choices */
static int tolua_set_show_choices(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!tolua_isboolean(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  show_choices = ((bool)  tolua_toboolean(tolua_S,2,0));
 return 0;
}

/* get function: show_details */
static int tolua_get_show_details(lua_State* tolua_S)
{
 tolua_pushboolean(tolua_S,(bool)show_details);
 return 1;
}

/* set function: show_details */
static int tolua_set_show_details(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!tolua_isboolean(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  show_details = ((bool)  tolua_toboolean(tolua_S,2,0));
 return 0;
}

/* get function: testing_stack */
static int tolua_get_testing_stack(lua_State* tolua_S)
{
 tolua_pushboolean(tolua_S,(bool)testing_stack);
 return 1;
}

/* set function: testing_stack */
static int tolua_set_testing_stack(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!tolua_isboolean(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  testing_stack = ((bool)  tolua_toboolean(tolua_S,2,0));
 return 0;
}

/* get function: testing_carry */
static int tolua_get_testing_carry(lua_State* tolua_S)
{
 tolua_pushboolean(tolua_S,(bool)testing_carry);
 return 1;
}

/* set function: testing_carry */
static int tolua_set_testing_carry(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!tolua_isboolean(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  testing_carry = ((bool)  tolua_toboolean(tolua_S,2,0));
 return 0;
}

/* get function: cheat_peek */
static int tolua_get_cheat_peek(lua_State* tolua_S)
{
 tolua_pushboolean(tolua_S,(bool)cheat_peek);
 return 1;
}

/* set function: cheat_peek */
static int tolua_set_cheat_peek(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!tolua_isboolean(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  cheat_peek = ((bool)  tolua_toboolean(tolua_S,2,0));
 return 0;
}

/* get function: cheat_hear */
static int tolua_get_cheat_hear(lua_State* tolua_S)
{
 tolua_pushboolean(tolua_S,(bool)cheat_hear);
 return 1;
}

/* set function: cheat_hear */
static int tolua_set_cheat_hear(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!tolua_isboolean(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  cheat_hear = ((bool)  tolua_toboolean(tolua_S,2,0));
 return 0;
}

/* get function: cheat_room */
static int tolua_get_cheat_room(lua_State* tolua_S)
{
 tolua_pushboolean(tolua_S,(bool)cheat_room);
 return 1;
}

/* set function: cheat_room */
static int tolua_set_cheat_room(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!tolua_isboolean(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  cheat_room = ((bool)  tolua_toboolean(tolua_S,2,0));
 return 0;
}

/* get function: cheat_xtra */
static int tolua_get_cheat_xtra(lua_State* tolua_S)
{
 tolua_pushboolean(tolua_S,(bool)cheat_xtra);
 return 1;
}

/* set function: cheat_xtra */
static int tolua_set_cheat_xtra(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!tolua_isboolean(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  cheat_xtra = ((bool)  tolua_toboolean(tolua_S,2,0));
 return 0;
}

/* get function: cheat_know */
static int tolua_get_cheat_know(lua_State* tolua_S)
{
 tolua_pushboolean(tolua_S,(bool)cheat_know);
 return 1;
}

/* set function: cheat_know */
static int tolua_set_cheat_know(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!tolua_isboolean(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  cheat_know = ((bool)  tolua_toboolean(tolua_S,2,0));
 return 0;
}

/* get function: cheat_live */
static int tolua_get_cheat_live(lua_State* tolua_S)
{
 tolua_pushboolean(tolua_S,(bool)cheat_live);
 return 1;
}

/* set function: cheat_live */
static int tolua_set_cheat_live(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!tolua_isboolean(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  cheat_live = ((bool)  tolua_toboolean(tolua_S,2,0));
 return 0;
}

/* get function: last_words */
static int tolua_get_last_words(lua_State* tolua_S)
{
 tolua_pushboolean(tolua_S,(bool)last_words);
 return 1;
}

/* set function: last_words */
static int tolua_set_last_words(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!tolua_isboolean(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  last_words = ((bool)  tolua_toboolean(tolua_S,2,0));
 return 0;
}

/* get function: speak_unique */
static int tolua_get_speak_unique(lua_State* tolua_S)
{
 tolua_pushboolean(tolua_S,(bool)speak_unique);
 return 1;
}

/* set function: speak_unique */
static int tolua_set_speak_unique(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!tolua_isboolean(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  speak_unique = ((bool)  tolua_toboolean(tolua_S,2,0));
 return 0;
}

/* get function: small_levels */
static int tolua_get_small_levels(lua_State* tolua_S)
{
 tolua_pushboolean(tolua_S,(bool)small_levels);
 return 1;
}

/* set function: small_levels */
static int tolua_set_small_levels(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!tolua_isboolean(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  small_levels = ((bool)  tolua_toboolean(tolua_S,2,0));
 return 0;
}

/* get function: empty_levels */
static int tolua_get_empty_levels(lua_State* tolua_S)
{
 tolua_pushboolean(tolua_S,(bool)empty_levels);
 return 1;
}

/* set function: empty_levels */
static int tolua_set_empty_levels(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!tolua_isboolean(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  empty_levels = ((bool)  tolua_toboolean(tolua_S,2,0));
 return 0;
}

/* get function: water_levels */
static int tolua_get_water_levels(lua_State* tolua_S)
{
 tolua_pushboolean(tolua_S,(bool)water_levels);
 return 1;
}

/* set function: water_levels */
static int tolua_set_water_levels(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!tolua_isboolean(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  water_levels = ((bool)  tolua_toboolean(tolua_S,2,0));
 return 0;
}

/* get function: always_small_level */
static int tolua_get_always_small_level(lua_State* tolua_S)
{
 tolua_pushboolean(tolua_S,(bool)always_small_level);
 return 1;
}

/* set function: always_small_level */
static int tolua_set_always_small_level(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!tolua_isboolean(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  always_small_level = ((bool)  tolua_toboolean(tolua_S,2,0));
 return 0;
}

/* get function: flavored_attacks */
static int tolua_get_flavored_attacks(lua_State* tolua_S)
{
 tolua_pushboolean(tolua_S,(bool)flavored_attacks);
 return 1;
}

/* set function: flavored_attacks */
static int tolua_set_flavored_attacks(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!tolua_isboolean(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  flavored_attacks = ((bool)  tolua_toboolean(tolua_S,2,0));
 return 0;
}

/* get function: player_symbols */
static int tolua_get_player_symbols(lua_State* tolua_S)
{
 tolua_pushboolean(tolua_S,(bool)player_symbols);
 return 1;
}

/* set function: player_symbols */
static int tolua_set_player_symbols(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!tolua_isboolean(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  player_symbols = ((bool)  tolua_toboolean(tolua_S,2,0));
 return 0;
}

/* get function: hitpoint_warn */
static int tolua_get_hitpoint_warn(lua_State* tolua_S)
{
 tolua_pushnumber(tolua_S,(long)hitpoint_warn);
 return 1;
}

/* set function: hitpoint_warn */
static int tolua_set_hitpoint_warn(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  hitpoint_warn = ((byte)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: delay_factor */
static int tolua_get_delay_factor(lua_State* tolua_S)
{
 tolua_pushnumber(tolua_S,(long)delay_factor);
 return 1;
}

/* set function: delay_factor */
static int tolua_set_delay_factor(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  delay_factor = ((byte)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: autosave_freq */
static int tolua_get_autosave_freq(lua_State* tolua_S)
{
 tolua_pushnumber(tolua_S,(long)autosave_freq);
 return 1;
}

/* set function: autosave_freq */
static int tolua_set_autosave_freq(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  autosave_freq = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: autosave_t */
static int tolua_get_autosave_t(lua_State* tolua_S)
{
 tolua_pushboolean(tolua_S,(bool)autosave_t);
 return 1;
}

/* set function: autosave_t */
static int tolua_set_autosave_t(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!tolua_isboolean(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  autosave_t = ((bool)  tolua_toboolean(tolua_S,2,0));
 return 0;
}

/* get function: autosave_l */
static int tolua_get_autosave_l(lua_State* tolua_S)
{
 tolua_pushboolean(tolua_S,(bool)autosave_l);
 return 1;
}

/* set function: autosave_l */
static int tolua_set_autosave_l(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!tolua_isboolean(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  autosave_l = ((bool)  tolua_toboolean(tolua_S,2,0));
 return 0;
}

/* get function: feeling */
static int tolua_get_feeling(lua_State* tolua_S)
{
 tolua_pushnumber(tolua_S,(long)feeling);
 return 1;
}

/* set function: feeling */
static int tolua_set_feeling(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  feeling = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: rating */
static int tolua_get_rating(lua_State* tolua_S)
{
 tolua_pushnumber(tolua_S,(long)rating);
 return 1;
}

/* set function: rating */
static int tolua_set_rating(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  rating = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: good_item_flag */
static int tolua_get_good_item_flag(lua_State* tolua_S)
{
 tolua_pushboolean(tolua_S,(bool)good_item_flag);
 return 1;
}

/* set function: good_item_flag */
static int tolua_set_good_item_flag(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!tolua_isboolean(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  good_item_flag = ((bool)  tolua_toboolean(tolua_S,2,0));
 return 0;
}

/* get function: closing_flag */
static int tolua_get_closing_flag(lua_State* tolua_S)
{
 tolua_pushboolean(tolua_S,(bool)closing_flag);
 return 1;
}

/* set function: closing_flag */
static int tolua_set_closing_flag(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!tolua_isboolean(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  closing_flag = ((bool)  tolua_toboolean(tolua_S,2,0));
 return 0;
}

/* get function: max_panel_rows */
static int tolua_get_max_panel_rows(lua_State* tolua_S)
{
 tolua_pushnumber(tolua_S,(long)max_panel_rows);
 return 1;
}

/* set function: max_panel_rows */
static int tolua_set_max_panel_rows(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  max_panel_rows = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: max_panel_cols */
static int tolua_get_max_panel_cols(lua_State* tolua_S)
{
 tolua_pushnumber(tolua_S,(long)max_panel_cols);
 return 1;
}

/* set function: max_panel_cols */
static int tolua_set_max_panel_cols(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  max_panel_cols = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: panel_row */
static int tolua_get_panel_row(lua_State* tolua_S)
{
 tolua_pushnumber(tolua_S,(long)panel_row);
 return 1;
}

/* set function: panel_row */
static int tolua_set_panel_row(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  panel_row = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: panel_col */
static int tolua_get_panel_col(lua_State* tolua_S)
{
 tolua_pushnumber(tolua_S,(long)panel_col);
 return 1;
}

/* set function: panel_col */
static int tolua_set_panel_col(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  panel_col = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: panel_row_min */
static int tolua_get_panel_row_min(lua_State* tolua_S)
{
 tolua_pushnumber(tolua_S,(long)panel_row_min);
 return 1;
}

/* set function: panel_row_min */
static int tolua_set_panel_row_min(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  panel_row_min = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: panel_row_max */
static int tolua_get_panel_row_max(lua_State* tolua_S)
{
 tolua_pushnumber(tolua_S,(long)panel_row_max);
 return 1;
}

/* set function: panel_row_max */
static int tolua_set_panel_row_max(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  panel_row_max = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: panel_col_min */
static int tolua_get_panel_col_min(lua_State* tolua_S)
{
 tolua_pushnumber(tolua_S,(long)panel_col_min);
 return 1;
}

/* set function: panel_col_min */
static int tolua_set_panel_col_min(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  panel_col_min = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: panel_col_max */
static int tolua_get_panel_col_max(lua_State* tolua_S)
{
 tolua_pushnumber(tolua_S,(long)panel_col_max);
 return 1;
}

/* set function: panel_col_max */
static int tolua_set_panel_col_max(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  panel_col_max = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: panel_col_prt */
static int tolua_get_panel_col_prt(lua_State* tolua_S)
{
 tolua_pushnumber(tolua_S,(long)panel_col_prt);
 return 1;
}

/* set function: panel_col_prt */
static int tolua_set_panel_col_prt(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  panel_col_prt = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: panel_row_prt */
static int tolua_get_panel_row_prt(lua_State* tolua_S)
{
 tolua_pushnumber(tolua_S,(long)panel_row_prt);
 return 1;
}

/* set function: panel_row_prt */
static int tolua_set_panel_row_prt(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  panel_row_prt = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: py */
static int tolua_get_py(lua_State* tolua_S)
{
 tolua_pushnumber(tolua_S,(long)py);
 return 1;
}

/* set function: py */
static int tolua_set_py(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  py = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: px */
static int tolua_get_px(lua_State* tolua_S)
{
 tolua_pushnumber(tolua_S,(long)px);
 return 1;
}

/* set function: px */
static int tolua_set_px(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  px = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: global_x */
static int tolua_get_global_x(lua_State* tolua_S)
{
 tolua_pushnumber(tolua_S,(long)global_x);
 return 1;
}

/* set function: global_x */
static int tolua_set_global_x(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  global_x = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: global_y */
static int tolua_get_global_y(lua_State* tolua_S)
{
 tolua_pushnumber(tolua_S,(long)global_y);
 return 1;
}

/* set function: global_y */
static int tolua_set_global_y(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  global_y = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: nevermiss */
static int tolua_get_nevermiss(lua_State* tolua_S)
{
 tolua_pushboolean(tolua_S,(bool)nevermiss);
 return 1;
}

/* set function: nevermiss */
static int tolua_set_nevermiss(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!tolua_isboolean(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  nevermiss = ((bool)  tolua_toboolean(tolua_S,2,0));
 return 0;
}

/* get function: no_magic_return */
static int tolua_get_no_magic_return(lua_State* tolua_S)
{
 tolua_pushboolean(tolua_S,(bool)no_magic_return);
 return 1;
}

/* set function: no_magic_return */
static int tolua_set_no_magic_return(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!tolua_isboolean(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  no_magic_return = ((bool)  tolua_toboolean(tolua_S,2,0));
 return 0;
}

/* get function: monster_physical */
static int tolua_get_monster_physical(lua_State* tolua_S)
{
 tolua_pushboolean(tolua_S,(bool)monster_physical);
 return 1;
}

/* set function: monster_physical */
static int tolua_set_monster_physical(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!tolua_isboolean(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  monster_physical = ((bool)  tolua_toboolean(tolua_S,2,0));
 return 0;
}

/* get function: monster_ranged */
static int tolua_get_monster_ranged(lua_State* tolua_S)
{
 tolua_pushboolean(tolua_S,(bool)monster_ranged);
 return 1;
}

/* set function: monster_ranged */
static int tolua_set_monster_ranged(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!tolua_isboolean(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  monster_ranged = ((bool)  tolua_toboolean(tolua_S,2,0));
 return 0;
}

/* get function: ranged_attack */
static int tolua_get_ranged_attack(lua_State* tolua_S)
{
 tolua_pushboolean(tolua_S,(bool)ranged_attack);
 return 1;
}

/* set function: ranged_attack */
static int tolua_set_ranged_attack(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!tolua_isboolean(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  ranged_attack = ((bool)  tolua_toboolean(tolua_S,2,0));
 return 0;
}

/* get function: melee_attack */
static int tolua_get_melee_attack(lua_State* tolua_S)
{
 tolua_pushboolean(tolua_S,(bool)melee_attack);
 return 1;
}

/* set function: melee_attack */
static int tolua_set_melee_attack(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!tolua_isboolean(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  melee_attack = ((bool)  tolua_toboolean(tolua_S,2,0));
 return 0;
}

/* get function: throw_attack */
static int tolua_get_throw_attack(lua_State* tolua_S)
{
 tolua_pushboolean(tolua_S,(bool)throw_attack);
 return 1;
}

/* set function: throw_attack */
static int tolua_set_throw_attack(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!tolua_isboolean(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  throw_attack = ((bool)  tolua_toboolean(tolua_S,2,0));
 return 0;
}

/* get function: monster_died */
static int tolua_get_monster_died(lua_State* tolua_S)
{
 tolua_pushboolean(tolua_S,(bool)monster_died);
 return 1;
}

/* set function: monster_died */
static int tolua_set_monster_died(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!tolua_isboolean(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  monster_died = ((bool)  tolua_toboolean(tolua_S,2,0));
 return 0;
}

/* get function: ignore_spellcraft */
static int tolua_get_ignore_spellcraft(lua_State* tolua_S)
{
 tolua_pushboolean(tolua_S,(bool)ignore_spellcraft);
 return 1;
}

/* set function: ignore_spellcraft */
static int tolua_set_ignore_spellcraft(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!tolua_isboolean(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  ignore_spellcraft = ((bool)  tolua_toboolean(tolua_S,2,0));
 return 0;
}

/* get function: damages_counter */
static int tolua_get_damages_counter(lua_State* tolua_S)
{
 tolua_pushnumber(tolua_S,(long)damages_counter);
 return 1;
}

/* set function: damages_counter */
static int tolua_set_damages_counter(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  damages_counter = ((s32b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: damages_counter_player_damages */
static int tolua_get_damages_counter_player_damages(lua_State* tolua_S)
{
 tolua_pushboolean(tolua_S,(bool)damages_counter_player_damages);
 return 1;
}

/* set function: damages_counter_player_damages */
static int tolua_set_damages_counter_player_damages(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!tolua_isboolean(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  damages_counter_player_damages = ((bool)  tolua_toboolean(tolua_S,2,0));
 return 0;
}

/* get function: damages_counter_duration */
static int tolua_get_damages_counter_duration(lua_State* tolua_S)
{
 tolua_pushnumber(tolua_S,(long)damages_counter_duration);
 return 1;
}

/* set function: damages_counter_duration */
static int tolua_set_damages_counter_duration(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  damages_counter_duration = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: stormshadow */
static int tolua_get_stormshadow(lua_State* tolua_S)
{
 tolua_pushboolean(tolua_S,(bool)stormshadow);
 return 1;
}

/* set function: stormshadow */
static int tolua_set_stormshadow(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!tolua_isboolean(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  stormshadow = ((bool)  tolua_toboolean(tolua_S,2,0));
 return 0;
}

/* get function: enemy_immortality */
static int tolua_get_enemy_immortality(lua_State* tolua_S)
{
 tolua_pushboolean(tolua_S,(bool)enemy_immortality);
 return 1;
}

/* set function: enemy_immortality */
static int tolua_set_enemy_immortality(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!tolua_isboolean(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  enemy_immortality = ((bool)  tolua_toboolean(tolua_S,2,0));
 return 0;
}

/* get function: tmpluastring */
static int tolua_get_tmpluastring(lua_State* tolua_S)
{
 tolua_pushstring(tolua_S,(const char*)tmpluastring);
 return 1;
}

/* set function: tmpluastring */
static int tolua_set_tmpluastring(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!tolua_isstring(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
 strncpy(tmpluastring,tolua_tostring(tolua_S,2,0),80-1);
 return 0;
}

/* get function: red_roff */
static int tolua_get_red_roff(lua_State* tolua_S)
{
 tolua_pushboolean(tolua_S,(bool)red_roff);
 return 1;
}

/* set function: red_roff */
static int tolua_set_red_roff(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!tolua_isboolean(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  red_roff = ((bool)  tolua_toboolean(tolua_S,2,0));
 return 0;
}

/* get function: term_saved */
static int tolua_get_term_saved(lua_State* tolua_S)
{
 tolua_pushboolean(tolua_S,(bool)term_saved);
 return 1;
}

/* set function: term_saved */
static int tolua_set_term_saved(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!tolua_isboolean(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  term_saved = ((bool)  tolua_toboolean(tolua_S,2,0));
 return 0;
}

/* get function: dying */
static int tolua_get_dying(lua_State* tolua_S)
{
 tolua_pushboolean(tolua_S,(bool)dying);
 return 1;
}

/* set function: dying */
static int tolua_set_dying(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!tolua_isboolean(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  dying = ((bool)  tolua_toboolean(tolua_S,2,0));
 return 0;
}

/* get function: exblows */
static int tolua_get_exblows(lua_State* tolua_S)
{
 tolua_pushnumber(tolua_S,(long)exblows);
 return 1;
}

/* set function: exblows */
static int tolua_set_exblows(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  exblows = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: exshots */
static int tolua_get_exshots(lua_State* tolua_S)
{
 tolua_pushnumber(tolua_S,(long)exshots);
 return 1;
}

/* set function: exshots */
static int tolua_set_exshots(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  exshots = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: dropshots */
static int tolua_get_dropshots(lua_State* tolua_S)
{
 tolua_pushboolean(tolua_S,(bool)dropshots);
 return 1;
}

/* set function: dropshots */
static int tolua_set_dropshots(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!tolua_isboolean(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  dropshots = ((bool)  tolua_toboolean(tolua_S,2,0));
 return 0;
}

/* get function: dropnum */
static int tolua_get_dropnum(lua_State* tolua_S)
{
 tolua_pushnumber(tolua_S,(long)dropnum);
 return 1;
}

/* set function: dropnum */
static int tolua_set_dropnum(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  dropnum = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: throw_item */
static int tolua_get_throw_item(lua_State* tolua_S)
{
 tolua_pushnumber(tolua_S,(long)throw_item);
 return 1;
}

/* set function: throw_item */
static int tolua_set_throw_item(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  throw_item = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: throw_floorpack */
static int tolua_get_throw_floorpack(lua_State* tolua_S)
{
 tolua_pushnumber(tolua_S,(long)throw_floorpack);
 return 1;
}

/* set function: throw_floorpack */
static int tolua_set_throw_floorpack(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  throw_floorpack = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: testop */
static int tolua_get_testop(lua_State* tolua_S)
{
 tolua_pushstring(tolua_S,(const char*)testop);
 return 1;
}

/* set function: testop */
static int tolua_set_testop(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!tolua_isstring(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
 strncpy(testop,tolua_tostring(tolua_S,2,0),80-1);
 return 0;
}

/* get function: target_who */
static int tolua_get_target_who(lua_State* tolua_S)
{
 tolua_pushnumber(tolua_S,(long)target_who);
 return 1;
}

/* set function: target_who */
static int tolua_set_target_who(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  target_who = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: target_col */
static int tolua_get_target_col(lua_State* tolua_S)
{
 tolua_pushnumber(tolua_S,(long)target_col);
 return 1;
}

/* set function: target_col */
static int tolua_set_target_col(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  target_col = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: target_row */
static int tolua_get_target_row(lua_State* tolua_S)
{
 tolua_pushnumber(tolua_S,(long)target_row);
 return 1;
}

/* set function: target_row */
static int tolua_set_target_row(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  target_row = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: health_who */
static int tolua_get_health_who(lua_State* tolua_S)
{
 tolua_pushnumber(tolua_S,(long)health_who);
 return 1;
}

/* set function: health_who */
static int tolua_set_health_who(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  health_who = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: monster_race_idx */
static int tolua_get_monster_race_idx(lua_State* tolua_S)
{
 tolua_pushnumber(tolua_S,(long)monster_race_idx);
 return 1;
}

/* set function: monster_race_idx */
static int tolua_set_monster_race_idx(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  monster_race_idx = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: monster_type_idx */
static int tolua_get_monster_type_idx(lua_State* tolua_S)
{
 tolua_pushnumber(tolua_S,(long)monster_type_idx);
 return 1;
}

/* set function: monster_type_idx */
static int tolua_set_monster_type_idx(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  monster_type_idx = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: object_kind_idx */
static int tolua_get_object_kind_idx(lua_State* tolua_S)
{
 tolua_pushnumber(tolua_S,(long)object_kind_idx);
 return 1;
}

/* set function: object_kind_idx */
static int tolua_set_object_kind_idx(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  object_kind_idx = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: player_uid */
static int tolua_get_player_uid(lua_State* tolua_S)
{
 tolua_pushnumber(tolua_S,(long)player_uid);
 return 1;
}

/* set function: player_uid */
static int tolua_set_player_uid(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  player_uid = ((int)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: player_euid */
static int tolua_get_player_euid(lua_State* tolua_S)
{
 tolua_pushnumber(tolua_S,(long)player_euid);
 return 1;
}

/* set function: player_euid */
static int tolua_set_player_euid(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  player_euid = ((int)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: player_egid */
static int tolua_get_player_egid(lua_State* tolua_S)
{
 tolua_pushnumber(tolua_S,(long)player_egid);
 return 1;
}

/* set function: player_egid */
static int tolua_set_player_egid(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  player_egid = ((int)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: player_name */
static int tolua_get_player_name(lua_State* tolua_S)
{
 tolua_pushstring(tolua_S,(const char*)player_name);
 return 1;
}

/* set function: player_name */
static int tolua_set_player_name(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!tolua_isstring(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
 strncpy(player_name,tolua_tostring(tolua_S,2,0),32-1);
 return 0;
}

/* get function: player_base */
static int tolua_get_player_base(lua_State* tolua_S)
{
 tolua_pushstring(tolua_S,(const char*)player_base);
 return 1;
}

/* set function: player_base */
static int tolua_set_player_base(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!tolua_isstring(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
 strncpy(player_base,tolua_tostring(tolua_S,2,0),32-1);
 return 0;
}

/* get function: died_from */
static int tolua_get_died_from(lua_State* tolua_S)
{
 tolua_pushstring(tolua_S,(const char*)died_from);
 return 1;
}

/* set function: died_from */
static int tolua_set_died_from(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!tolua_isstring(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
 strncpy(died_from,tolua_tostring(tolua_S,2,0),80-1);
 return 0;
}

/* get function: history */
static int tolua_get_history(lua_State* tolua_S)
{
 tolua_pushstring(tolua_S,(const char*)history);
 return 1;
}

/* set function: history */
static int tolua_set_history(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!tolua_isstring(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
 strncpy(history,tolua_tostring(tolua_S,2,0),4-1);
 return 0;
}

/* get function: savefile */
static int tolua_get_savefile(lua_State* tolua_S)
{
 tolua_pushstring(tolua_S,(const char*)savefile);
 return 1;
}

/* set function: savefile */
static int tolua_set_savefile(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!tolua_isstring(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
 strncpy(savefile,tolua_tostring(tolua_S,2,0),1024-1);
 return 0;
}

/* get function: lite_n */
static int tolua_get_lite_n(lua_State* tolua_S)
{
 tolua_pushnumber(tolua_S,(long)lite_n);
 return 1;
}

/* set function: lite_n */
static int tolua_set_lite_n(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  lite_n = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: lite_y */
static int tolua_get_init_lite_y(lua_State* tolua_S)
{
 int tolua_index;
#ifndef TOLUA_RELEASE
 {
 tolua_Error tolua_err;
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in array indexing.",&tolua_err);
 }
#endif
 tolua_index = (int)tolua_tonumber(tolua_S,2,0)-1;
#ifndef TOLUA_RELEASE
 if (tolua_index<0 || tolua_index>=LITE_MAX)
 tolua_error(tolua_S,"array indexing out of range.",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)lite_y[tolua_index]);
 return 1;
}

/* set function: lite_y */
static int tolua_set_init_lite_y(lua_State* tolua_S)
{
 int tolua_index;
#ifndef TOLUA_RELEASE
 {
 tolua_Error tolua_err;
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in array indexing.",&tolua_err);
 }
#endif
 tolua_index = (int)tolua_tonumber(tolua_S,2,0)-1;
#ifndef TOLUA_RELEASE
 if (tolua_index<0 || tolua_index>=LITE_MAX)
 tolua_error(tolua_S,"array indexing out of range.",NULL);
#endif
  lite_y[tolua_index] = ((s16b)  tolua_tonumber(tolua_S,3,0));
 return 0;
}

/* get function: lite_x */
static int tolua_get_init_lite_x(lua_State* tolua_S)
{
 int tolua_index;
#ifndef TOLUA_RELEASE
 {
 tolua_Error tolua_err;
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in array indexing.",&tolua_err);
 }
#endif
 tolua_index = (int)tolua_tonumber(tolua_S,2,0)-1;
#ifndef TOLUA_RELEASE
 if (tolua_index<0 || tolua_index>=LITE_MAX)
 tolua_error(tolua_S,"array indexing out of range.",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)lite_x[tolua_index]);
 return 1;
}

/* set function: lite_x */
static int tolua_set_init_lite_x(lua_State* tolua_S)
{
 int tolua_index;
#ifndef TOLUA_RELEASE
 {
 tolua_Error tolua_err;
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in array indexing.",&tolua_err);
 }
#endif
 tolua_index = (int)tolua_tonumber(tolua_S,2,0)-1;
#ifndef TOLUA_RELEASE
 if (tolua_index<0 || tolua_index>=LITE_MAX)
 tolua_error(tolua_S,"array indexing out of range.",NULL);
#endif
  lite_x[tolua_index] = ((s16b)  tolua_tonumber(tolua_S,3,0));
 return 0;
}

/* get function: view_n */
static int tolua_get_view_n(lua_State* tolua_S)
{
 tolua_pushnumber(tolua_S,(long)view_n);
 return 1;
}

/* set function: view_n */
static int tolua_set_view_n(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  view_n = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: view_y */
static int tolua_get_init_view_y(lua_State* tolua_S)
{
 int tolua_index;
#ifndef TOLUA_RELEASE
 {
 tolua_Error tolua_err;
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in array indexing.",&tolua_err);
 }
#endif
 tolua_index = (int)tolua_tonumber(tolua_S,2,0)-1;
#ifndef TOLUA_RELEASE
 if (tolua_index<0 || tolua_index>=VIEW_MAX)
 tolua_error(tolua_S,"array indexing out of range.",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)view_y[tolua_index]);
 return 1;
}

/* set function: view_y */
static int tolua_set_init_view_y(lua_State* tolua_S)
{
 int tolua_index;
#ifndef TOLUA_RELEASE
 {
 tolua_Error tolua_err;
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in array indexing.",&tolua_err);
 }
#endif
 tolua_index = (int)tolua_tonumber(tolua_S,2,0)-1;
#ifndef TOLUA_RELEASE
 if (tolua_index<0 || tolua_index>=VIEW_MAX)
 tolua_error(tolua_S,"array indexing out of range.",NULL);
#endif
  view_y[tolua_index] = ((byte)  tolua_tonumber(tolua_S,3,0));
 return 0;
}

/* get function: view_x */
static int tolua_get_init_view_x(lua_State* tolua_S)
{
 int tolua_index;
#ifndef TOLUA_RELEASE
 {
 tolua_Error tolua_err;
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in array indexing.",&tolua_err);
 }
#endif
 tolua_index = (int)tolua_tonumber(tolua_S,2,0)-1;
#ifndef TOLUA_RELEASE
 if (tolua_index<0 || tolua_index>=VIEW_MAX)
 tolua_error(tolua_S,"array indexing out of range.",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)view_x[tolua_index]);
 return 1;
}

/* set function: view_x */
static int tolua_set_init_view_x(lua_State* tolua_S)
{
 int tolua_index;
#ifndef TOLUA_RELEASE
 {
 tolua_Error tolua_err;
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in array indexing.",&tolua_err);
 }
#endif
 tolua_index = (int)tolua_tonumber(tolua_S,2,0)-1;
#ifndef TOLUA_RELEASE
 if (tolua_index<0 || tolua_index>=VIEW_MAX)
 tolua_error(tolua_S,"array indexing out of range.",NULL);
#endif
  view_x[tolua_index] = ((byte)  tolua_tonumber(tolua_S,3,0));
 return 0;
}

/* get function: temp_n */
static int tolua_get_temp_n(lua_State* tolua_S)
{
 tolua_pushnumber(tolua_S,(long)temp_n);
 return 1;
}

/* set function: temp_n */
static int tolua_set_temp_n(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  temp_n = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: temp_y */
static int tolua_get_init_temp_y(lua_State* tolua_S)
{
 int tolua_index;
#ifndef TOLUA_RELEASE
 {
 tolua_Error tolua_err;
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in array indexing.",&tolua_err);
 }
#endif
 tolua_index = (int)tolua_tonumber(tolua_S,2,0)-1;
#ifndef TOLUA_RELEASE
 if (tolua_index<0 || tolua_index>=TEMP_MAX)
 tolua_error(tolua_S,"array indexing out of range.",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)temp_y[tolua_index]);
 return 1;
}

/* set function: temp_y */
static int tolua_set_init_temp_y(lua_State* tolua_S)
{
 int tolua_index;
#ifndef TOLUA_RELEASE
 {
 tolua_Error tolua_err;
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in array indexing.",&tolua_err);
 }
#endif
 tolua_index = (int)tolua_tonumber(tolua_S,2,0)-1;
#ifndef TOLUA_RELEASE
 if (tolua_index<0 || tolua_index>=TEMP_MAX)
 tolua_error(tolua_S,"array indexing out of range.",NULL);
#endif
  temp_y[tolua_index] = ((byte)  tolua_tonumber(tolua_S,3,0));
 return 0;
}

/* get function: temp_x */
static int tolua_get_init_temp_x(lua_State* tolua_S)
{
 int tolua_index;
#ifndef TOLUA_RELEASE
 {
 tolua_Error tolua_err;
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in array indexing.",&tolua_err);
 }
#endif
 tolua_index = (int)tolua_tonumber(tolua_S,2,0)-1;
#ifndef TOLUA_RELEASE
 if (tolua_index<0 || tolua_index>=TEMP_MAX)
 tolua_error(tolua_S,"array indexing out of range.",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)temp_x[tolua_index]);
 return 1;
}

/* set function: temp_x */
static int tolua_set_init_temp_x(lua_State* tolua_S)
{
 int tolua_index;
#ifndef TOLUA_RELEASE
 {
 tolua_Error tolua_err;
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in array indexing.",&tolua_err);
 }
#endif
 tolua_index = (int)tolua_tonumber(tolua_S,2,0)-1;
#ifndef TOLUA_RELEASE
 if (tolua_index<0 || tolua_index>=TEMP_MAX)
 tolua_error(tolua_S,"array indexing out of range.",NULL);
#endif
  temp_x[tolua_index] = ((byte)  tolua_tonumber(tolua_S,3,0));
 return 0;
}

/* get function: macro__num */
static int tolua_get_macro__num(lua_State* tolua_S)
{
 tolua_pushnumber(tolua_S,(long)macro__num);
 return 1;
}

/* set function: macro__num */
static int tolua_set_macro__num(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  macro__num = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: macro__pat */
static int tolua_get_macro__pat(lua_State* tolua_S)
{
 tolua_pushstring(tolua_S,(const char*)macro__pat);
 return 1;
}

/* set function: macro__pat */
static int tolua_set_macro__pat(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!tolua_isstring(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  macro__pat = ((cptr)  tolua_tostring(tolua_S,2,0));
 return 0;
}

/* get function: macro__act */
static int tolua_get_macro__act(lua_State* tolua_S)
{
 tolua_pushstring(tolua_S,(const char*)macro__act);
 return 1;
}

/* set function: macro__act */
static int tolua_set_macro__act(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!tolua_isstring(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  macro__act = ((cptr)  tolua_tostring(tolua_S,2,0));
 return 0;
}

/* get function: macro__cmd */
static int tolua_get_macro__cmd(lua_State* tolua_S)
{
 tolua_pushboolean(tolua_S,(bool)macro__cmd);
 return 1;
}

/* set function: macro__cmd */
static int tolua_set_macro__cmd(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!tolua_isboolean(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  macro__cmd = ((bool)  tolua_toboolean(tolua_S,2,0));
 return 0;
}

/* get function: macro__buf */
static int tolua_get_macro__buf(lua_State* tolua_S)
{
 tolua_pushstring(tolua_S,(const char*)macro__buf);
 return 1;
}

/* set function: macro__buf */
static int tolua_set_macro__buf(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!tolua_isstring(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  macro__buf = ((char*)  tolua_tostring(tolua_S,2,0));
 return 0;
}

/* get function: quark__num */
static int tolua_get_quark__num(lua_State* tolua_S)
{
 tolua_pushnumber(tolua_S,(long)quark__num);
 return 1;
}

/* set function: quark__num */
static int tolua_set_quark__num(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  quark__num = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: quark__str */
static int tolua_get_quark__str(lua_State* tolua_S)
{
 tolua_pushstring(tolua_S,(const char*)quark__str);
 return 1;
}

/* set function: quark__str */
static int tolua_set_quark__str(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!tolua_isstring(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  quark__str = ((cptr)  tolua_tostring(tolua_S,2,0));
 return 0;
}

/* get function: message__next */
static int tolua_get_message__next(lua_State* tolua_S)
{
 tolua_pushnumber(tolua_S,(long)message__next);
 return 1;
}

/* set function: message__next */
static int tolua_set_message__next(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  message__next = ((u16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: message__last */
static int tolua_get_message__last(lua_State* tolua_S)
{
 tolua_pushnumber(tolua_S,(long)message__last);
 return 1;
}

/* set function: message__last */
static int tolua_set_message__last(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  message__last = ((u16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: message__head */
static int tolua_get_message__head(lua_State* tolua_S)
{
 tolua_pushnumber(tolua_S,(long)message__head);
 return 1;
}

/* set function: message__head */
static int tolua_set_message__head(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  message__head = ((u16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: message__tail */
static int tolua_get_message__tail(lua_State* tolua_S)
{
 tolua_pushnumber(tolua_S,(long)message__tail);
 return 1;
}

/* set function: message__tail */
static int tolua_set_message__tail(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  message__tail = ((u16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: message__ptr */
static int tolua_get_message__ptr(lua_State* tolua_S)
{
 tolua_pushnumber(tolua_S,(long)message__ptr);
 return 1;
}

/* set function: message__ptr */
static int tolua_set_message__ptr(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  message__ptr = ((u16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: message__buf */
static int tolua_get_message__buf(lua_State* tolua_S)
{
 tolua_pushstring(tolua_S,(const char*)message__buf);
 return 1;
}

/* set function: message__buf */
static int tolua_set_message__buf(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!tolua_isstring(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  message__buf = ((char*)  tolua_tostring(tolua_S,2,0));
 return 0;
}

/* get function: option_flag */
static int tolua_get_init_option_flag(lua_State* tolua_S)
{
 int tolua_index;
#ifndef TOLUA_RELEASE
 {
 tolua_Error tolua_err;
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in array indexing.",&tolua_err);
 }
#endif
 tolua_index = (int)tolua_tonumber(tolua_S,2,0)-1;
#ifndef TOLUA_RELEASE
 if (tolua_index<0 || tolua_index>=8)
 tolua_error(tolua_S,"array indexing out of range.",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)option_flag[tolua_index]);
 return 1;
}

/* set function: option_flag */
static int tolua_set_init_option_flag(lua_State* tolua_S)
{
 int tolua_index;
#ifndef TOLUA_RELEASE
 {
 tolua_Error tolua_err;
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in array indexing.",&tolua_err);
 }
#endif
 tolua_index = (int)tolua_tonumber(tolua_S,2,0)-1;
#ifndef TOLUA_RELEASE
 if (tolua_index<0 || tolua_index>=8)
 tolua_error(tolua_S,"array indexing out of range.",NULL);
#endif
  option_flag[tolua_index] = ((u32b)  tolua_tonumber(tolua_S,3,0));
 return 0;
}

/* get function: option_mask */
static int tolua_get_init_option_mask(lua_State* tolua_S)
{
 int tolua_index;
#ifndef TOLUA_RELEASE
 {
 tolua_Error tolua_err;
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in array indexing.",&tolua_err);
 }
#endif
 tolua_index = (int)tolua_tonumber(tolua_S,2,0)-1;
#ifndef TOLUA_RELEASE
 if (tolua_index<0 || tolua_index>=8)
 tolua_error(tolua_S,"array indexing out of range.",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)option_mask[tolua_index]);
 return 1;
}

/* set function: option_mask */
static int tolua_set_init_option_mask(lua_State* tolua_S)
{
 int tolua_index;
#ifndef TOLUA_RELEASE
 {
 tolua_Error tolua_err;
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in array indexing.",&tolua_err);
 }
#endif
 tolua_index = (int)tolua_tonumber(tolua_S,2,0)-1;
#ifndef TOLUA_RELEASE
 if (tolua_index<0 || tolua_index>=8)
 tolua_error(tolua_S,"array indexing out of range.",NULL);
#endif
  option_mask[tolua_index] = ((u32b)  tolua_tonumber(tolua_S,3,0));
 return 0;
}

/* get function: window_flag */
static int tolua_get_init_window_flag(lua_State* tolua_S)
{
 int tolua_index;
#ifndef TOLUA_RELEASE
 {
 tolua_Error tolua_err;
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in array indexing.",&tolua_err);
 }
#endif
 tolua_index = (int)tolua_tonumber(tolua_S,2,0)-1;
#ifndef TOLUA_RELEASE
 if (tolua_index<0 || tolua_index>=8)
 tolua_error(tolua_S,"array indexing out of range.",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)window_flag[tolua_index]);
 return 1;
}

/* set function: window_flag */
static int tolua_set_init_window_flag(lua_State* tolua_S)
{
 int tolua_index;
#ifndef TOLUA_RELEASE
 {
 tolua_Error tolua_err;
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in array indexing.",&tolua_err);
 }
#endif
 tolua_index = (int)tolua_tonumber(tolua_S,2,0)-1;
#ifndef TOLUA_RELEASE
 if (tolua_index<0 || tolua_index>=8)
 tolua_error(tolua_S,"array indexing out of range.",NULL);
#endif
  window_flag[tolua_index] = ((u32b)  tolua_tonumber(tolua_S,3,0));
 return 0;
}

/* get function: window_mask */
static int tolua_get_init_window_mask(lua_State* tolua_S)
{
 int tolua_index;
#ifndef TOLUA_RELEASE
 {
 tolua_Error tolua_err;
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in array indexing.",&tolua_err);
 }
#endif
 tolua_index = (int)tolua_tonumber(tolua_S,2,0)-1;
#ifndef TOLUA_RELEASE
 if (tolua_index<0 || tolua_index>=8)
 tolua_error(tolua_S,"array indexing out of range.",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)window_mask[tolua_index]);
 return 1;
}

/* set function: window_mask */
static int tolua_set_init_window_mask(lua_State* tolua_S)
{
 int tolua_index;
#ifndef TOLUA_RELEASE
 {
 tolua_Error tolua_err;
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in array indexing.",&tolua_err);
 }
#endif
 tolua_index = (int)tolua_tonumber(tolua_S,2,0)-1;
#ifndef TOLUA_RELEASE
 if (tolua_index<0 || tolua_index>=8)
 tolua_error(tolua_S,"array indexing out of range.",NULL);
#endif
  window_mask[tolua_index] = ((u32b)  tolua_tonumber(tolua_S,3,0));
 return 0;
}

/* get function: angband_term */
static int tolua_get_init_angband_term(lua_State* tolua_S)
{
 int tolua_index;
#ifndef TOLUA_RELEASE
 {
 tolua_Error tolua_err;
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in array indexing.",&tolua_err);
 }
#endif
 tolua_index = (int)tolua_tonumber(tolua_S,2,0)-1;
#ifndef TOLUA_RELEASE
 if (tolua_index<0 || tolua_index>=8)
 tolua_error(tolua_S,"array indexing out of range.",NULL);
#endif
 tolua_pushusertype(tolua_S,(void*)angband_term[tolua_index],"term");
 return 1;
}

/* set function: angband_term */
static int tolua_set_init_angband_term(lua_State* tolua_S)
{
 int tolua_index;
#ifndef TOLUA_RELEASE
 {
 tolua_Error tolua_err;
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in array indexing.",&tolua_err);
 }
#endif
 tolua_index = (int)tolua_tonumber(tolua_S,2,0)-1;
#ifndef TOLUA_RELEASE
 if (tolua_index<0 || tolua_index>=8)
 tolua_error(tolua_S,"array indexing out of range.",NULL);
#endif
  angband_term[tolua_index] = ((term*)  tolua_tousertype(tolua_S,3,0));
 return 0;
}

/* get function: o_list */
static int tolua_get_o_list_ptr(lua_State* tolua_S)
{
 tolua_pushusertype(tolua_S,(void*)o_list,"object_type");
 return 1;
}

/* set function: o_list */
static int tolua_set_o_list_ptr(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!tolua_isusertype(tolua_S,2,"object_type",0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  o_list = ((object_type*)  tolua_tousertype(tolua_S,2,0));
 return 0;
}

/* get function: m_list */
static int tolua_get_m_list_ptr(lua_State* tolua_S)
{
 tolua_pushusertype(tolua_S,(void*)m_list,"monster_type");
 return 1;
}

/* set function: m_list */
static int tolua_set_m_list_ptr(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!tolua_isusertype(tolua_S,2,"monster_type",0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  m_list = ((monster_type*)  tolua_tousertype(tolua_S,2,0));
 return 0;
}

/* get function: inventory */
static int tolua_get_inventory_ptr(lua_State* tolua_S)
{
 tolua_pushusertype(tolua_S,(void*)inventory,"object_type");
 return 1;
}

/* set function: inventory */
static int tolua_set_inventory_ptr(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!tolua_isusertype(tolua_S,2,"object_type",0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  inventory = ((object_type*)  tolua_tousertype(tolua_S,2,0));
 return 0;
}

/* get function: alloc_kind_size */
static int tolua_get_alloc_kind_size(lua_State* tolua_S)
{
 tolua_pushnumber(tolua_S,(long)alloc_kind_size);
 return 1;
}

/* set function: alloc_kind_size */
static int tolua_set_alloc_kind_size(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  alloc_kind_size = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: alloc_kind_table */
static int tolua_get_alloc_kind_table_ptr(lua_State* tolua_S)
{
 tolua_pushusertype(tolua_S,(void*)alloc_kind_table,"alloc_entry");
 return 1;
}

/* set function: alloc_kind_table */
static int tolua_set_alloc_kind_table_ptr(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!tolua_isusertype(tolua_S,2,"alloc_entry",0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  alloc_kind_table = ((alloc_entry*)  tolua_tousertype(tolua_S,2,0));
 return 0;
}

/* get function: alloc_race_size */
static int tolua_get_alloc_race_size(lua_State* tolua_S)
{
 tolua_pushnumber(tolua_S,(long)alloc_race_size);
 return 1;
}

/* set function: alloc_race_size */
static int tolua_set_alloc_race_size(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  alloc_race_size = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: alloc_race_table */
static int tolua_get_alloc_race_table_ptr(lua_State* tolua_S)
{
 tolua_pushusertype(tolua_S,(void*)alloc_race_table,"alloc_entry");
 return 1;
}

/* set function: alloc_race_table */
static int tolua_set_alloc_race_table_ptr(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!tolua_isusertype(tolua_S,2,"alloc_entry",0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  alloc_race_table = ((alloc_entry*)  tolua_tousertype(tolua_S,2,0));
 return 0;
}

/* get function: misc_to_attr */
static int tolua_get_init_misc_to_attr(lua_State* tolua_S)
{
 int tolua_index;
#ifndef TOLUA_RELEASE
 {
 tolua_Error tolua_err;
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in array indexing.",&tolua_err);
 }
#endif
 tolua_index = (int)tolua_tonumber(tolua_S,2,0)-1;
#ifndef TOLUA_RELEASE
 if (tolua_index<0 || tolua_index>=256)
 tolua_error(tolua_S,"array indexing out of range.",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)misc_to_attr[tolua_index]);
 return 1;
}

/* set function: misc_to_attr */
static int tolua_set_init_misc_to_attr(lua_State* tolua_S)
{
 int tolua_index;
#ifndef TOLUA_RELEASE
 {
 tolua_Error tolua_err;
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in array indexing.",&tolua_err);
 }
#endif
 tolua_index = (int)tolua_tonumber(tolua_S,2,0)-1;
#ifndef TOLUA_RELEASE
 if (tolua_index<0 || tolua_index>=256)
 tolua_error(tolua_S,"array indexing out of range.",NULL);
#endif
  misc_to_attr[tolua_index] = ((byte)  tolua_tonumber(tolua_S,3,0));
 return 0;
}

/* get function: misc_to_char */
static int tolua_get_misc_to_char(lua_State* tolua_S)
{
 tolua_pushstring(tolua_S,(const char*)misc_to_char);
 return 1;
}

/* set function: misc_to_char */
static int tolua_set_misc_to_char(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!tolua_isstring(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
 strncpy(misc_to_char,tolua_tostring(tolua_S,2,0),256-1);
 return 0;
}

/* get function: tval_to_attr */
static int tolua_get_init_tval_to_attr(lua_State* tolua_S)
{
 int tolua_index;
#ifndef TOLUA_RELEASE
 {
 tolua_Error tolua_err;
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in array indexing.",&tolua_err);
 }
#endif
 tolua_index = (int)tolua_tonumber(tolua_S,2,0)-1;
#ifndef TOLUA_RELEASE
 if (tolua_index<0 || tolua_index>=128)
 tolua_error(tolua_S,"array indexing out of range.",NULL);
#endif
 tolua_pushnumber(tolua_S,(long)tval_to_attr[tolua_index]);
 return 1;
}

/* set function: tval_to_attr */
static int tolua_set_init_tval_to_attr(lua_State* tolua_S)
{
 int tolua_index;
#ifndef TOLUA_RELEASE
 {
 tolua_Error tolua_err;
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in array indexing.",&tolua_err);
 }
#endif
 tolua_index = (int)tolua_tonumber(tolua_S,2,0)-1;
#ifndef TOLUA_RELEASE
 if (tolua_index<0 || tolua_index>=128)
 tolua_error(tolua_S,"array indexing out of range.",NULL);
#endif
  tval_to_attr[tolua_index] = ((byte)  tolua_tonumber(tolua_S,3,0));
 return 0;
}

/* get function: tval_to_char */
static int tolua_get_tval_to_char(lua_State* tolua_S)
{
 tolua_pushstring(tolua_S,(const char*)tval_to_char);
 return 1;
}

/* set function: tval_to_char */
static int tolua_set_tval_to_char(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!tolua_isstring(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
 strncpy(tval_to_char,tolua_tostring(tolua_S,2,0),128-1);
 return 0;
}

/* get function: p_body */
static int tolua_get_p_body(lua_State* tolua_S)
{
 tolua_pushusertype(tolua_S,(void*)&p_body,"player_type");
 return 1;
}

/* set function: p_body */
static int tolua_set_p_body(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!tolua_isusertype(tolua_S,2,"player_type",0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  p_body = *((player_type*)  tolua_tousertype(tolua_S,2,0));
 return 0;
}

/* get function: p_ptr */
static int tolua_get_p_ptr_ptr(lua_State* tolua_S)
{
 tolua_pushusertype(tolua_S,(void*)p_ptr,"player_type");
 return 1;
}

/* set function: p_ptr */
static int tolua_set_p_ptr_ptr(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!tolua_isusertype(tolua_S,2,"player_type",0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  p_ptr = ((player_type*)  tolua_tousertype(tolua_S,2,0));
 return 0;
}

/* get function: v_head */
static int tolua_get_v_head_ptr(lua_State* tolua_S)
{
 tolua_pushusertype(tolua_S,(void*)v_head,"header");
 return 1;
}

/* set function: v_head */
static int tolua_set_v_head_ptr(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!tolua_isusertype(tolua_S,2,"header",0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  v_head = ((header*)  tolua_tousertype(tolua_S,2,0));
 return 0;
}

/* get function: v_info */
static int tolua_get_v_info_ptr(lua_State* tolua_S)
{
 tolua_pushusertype(tolua_S,(void*)v_info,"vault_type");
 return 1;
}

/* set function: v_info */
static int tolua_set_v_info_ptr(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!tolua_isusertype(tolua_S,2,"vault_type",0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  v_info = ((vault_type*)  tolua_tousertype(tolua_S,2,0));
 return 0;
}

/* get function: v_name */
static int tolua_get_v_name(lua_State* tolua_S)
{
 tolua_pushstring(tolua_S,(const char*)v_name);
 return 1;
}

/* set function: v_name */
static int tolua_set_v_name(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!tolua_isstring(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  v_name = ((char*)  tolua_tostring(tolua_S,2,0));
 return 0;
}

/* get function: v_text */
static int tolua_get_v_text(lua_State* tolua_S)
{
 tolua_pushstring(tolua_S,(const char*)v_text);
 return 1;
}

/* set function: v_text */
static int tolua_set_v_text(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!tolua_isstring(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  v_text = ((char*)  tolua_tostring(tolua_S,2,0));
 return 0;
}

/* get function: f_head */
static int tolua_get_f_head_ptr(lua_State* tolua_S)
{
 tolua_pushusertype(tolua_S,(void*)f_head,"header");
 return 1;
}

/* set function: f_head */
static int tolua_set_f_head_ptr(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!tolua_isusertype(tolua_S,2,"header",0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  f_head = ((header*)  tolua_tousertype(tolua_S,2,0));
 return 0;
}

/* get function: f_info */
static int tolua_get_f_info_ptr(lua_State* tolua_S)
{
 tolua_pushusertype(tolua_S,(void*)f_info,"feature_type");
 return 1;
}

/* set function: f_info */
static int tolua_set_f_info_ptr(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!tolua_isusertype(tolua_S,2,"feature_type",0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  f_info = ((feature_type*)  tolua_tousertype(tolua_S,2,0));
 return 0;
}

/* get function: f_name */
static int tolua_get_f_name(lua_State* tolua_S)
{
 tolua_pushstring(tolua_S,(const char*)f_name);
 return 1;
}

/* set function: f_name */
static int tolua_set_f_name(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!tolua_isstring(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  f_name = ((char*)  tolua_tostring(tolua_S,2,0));
 return 0;
}

/* get function: f_text */
static int tolua_get_f_text(lua_State* tolua_S)
{
 tolua_pushstring(tolua_S,(const char*)f_text);
 return 1;
}

/* set function: f_text */
static int tolua_set_f_text(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!tolua_isstring(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  f_text = ((char*)  tolua_tostring(tolua_S,2,0));
 return 0;
}

/* get function: k_head */
static int tolua_get_k_head_ptr(lua_State* tolua_S)
{
 tolua_pushusertype(tolua_S,(void*)k_head,"header");
 return 1;
}

/* set function: k_head */
static int tolua_set_k_head_ptr(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!tolua_isusertype(tolua_S,2,"header",0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  k_head = ((header*)  tolua_tousertype(tolua_S,2,0));
 return 0;
}

/* get function: k_info */
static int tolua_get_k_info_ptr(lua_State* tolua_S)
{
 tolua_pushusertype(tolua_S,(void*)k_info,"object_kind");
 return 1;
}

/* set function: k_info */
static int tolua_set_k_info_ptr(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!tolua_isusertype(tolua_S,2,"object_kind",0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  k_info = ((object_kind*)  tolua_tousertype(tolua_S,2,0));
 return 0;
}

/* get function: k_name */
static int tolua_get_k_name(lua_State* tolua_S)
{
 tolua_pushstring(tolua_S,(const char*)k_name);
 return 1;
}

/* set function: k_name */
static int tolua_set_k_name(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!tolua_isstring(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  k_name = ((char*)  tolua_tostring(tolua_S,2,0));
 return 0;
}

/* get function: k_text */
static int tolua_get_k_text(lua_State* tolua_S)
{
 tolua_pushstring(tolua_S,(const char*)k_text);
 return 1;
}

/* set function: k_text */
static int tolua_set_k_text(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!tolua_isstring(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  k_text = ((char*)  tolua_tostring(tolua_S,2,0));
 return 0;
}

/* get function: a_head */
static int tolua_get_a_head_ptr(lua_State* tolua_S)
{
 tolua_pushusertype(tolua_S,(void*)a_head,"header");
 return 1;
}

/* set function: a_head */
static int tolua_set_a_head_ptr(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!tolua_isusertype(tolua_S,2,"header",0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  a_head = ((header*)  tolua_tousertype(tolua_S,2,0));
 return 0;
}

/* get function: a_info */
static int tolua_get_a_info_ptr(lua_State* tolua_S)
{
 tolua_pushusertype(tolua_S,(void*)a_info,"artifact_type");
 return 1;
}

/* set function: a_info */
static int tolua_set_a_info_ptr(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!tolua_isusertype(tolua_S,2,"artifact_type",0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  a_info = ((artifact_type*)  tolua_tousertype(tolua_S,2,0));
 return 0;
}

/* get function: a_name */
static int tolua_get_a_name(lua_State* tolua_S)
{
 tolua_pushstring(tolua_S,(const char*)a_name);
 return 1;
}

/* set function: a_name */
static int tolua_set_a_name(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!tolua_isstring(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  a_name = ((char*)  tolua_tostring(tolua_S,2,0));
 return 0;
}

/* get function: a_text */
static int tolua_get_a_text(lua_State* tolua_S)
{
 tolua_pushstring(tolua_S,(const char*)a_text);
 return 1;
}

/* set function: a_text */
static int tolua_set_a_text(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!tolua_isstring(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  a_text = ((char*)  tolua_tostring(tolua_S,2,0));
 return 0;
}

/* get function: e_head */
static int tolua_get_e_head_ptr(lua_State* tolua_S)
{
 tolua_pushusertype(tolua_S,(void*)e_head,"header");
 return 1;
}

/* set function: e_head */
static int tolua_set_e_head_ptr(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!tolua_isusertype(tolua_S,2,"header",0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  e_head = ((header*)  tolua_tousertype(tolua_S,2,0));
 return 0;
}

/* get function: e_info */
static int tolua_get_e_info_ptr(lua_State* tolua_S)
{
 tolua_pushusertype(tolua_S,(void*)e_info,"ego_item_type");
 return 1;
}

/* set function: e_info */
static int tolua_set_e_info_ptr(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!tolua_isusertype(tolua_S,2,"ego_item_type",0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  e_info = ((ego_item_type*)  tolua_tousertype(tolua_S,2,0));
 return 0;
}

/* get function: e_name */
static int tolua_get_e_name(lua_State* tolua_S)
{
 tolua_pushstring(tolua_S,(const char*)e_name);
 return 1;
}

/* set function: e_name */
static int tolua_set_e_name(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!tolua_isstring(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  e_name = ((char*)  tolua_tostring(tolua_S,2,0));
 return 0;
}

/* get function: e_text */
static int tolua_get_e_text(lua_State* tolua_S)
{
 tolua_pushstring(tolua_S,(const char*)e_text);
 return 1;
}

/* set function: e_text */
static int tolua_set_e_text(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!tolua_isstring(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  e_text = ((char*)  tolua_tostring(tolua_S,2,0));
 return 0;
}

/* get function: r_head */
static int tolua_get_r_head_ptr(lua_State* tolua_S)
{
 tolua_pushusertype(tolua_S,(void*)r_head,"header");
 return 1;
}

/* set function: r_head */
static int tolua_set_r_head_ptr(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!tolua_isusertype(tolua_S,2,"header",0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  r_head = ((header*)  tolua_tousertype(tolua_S,2,0));
 return 0;
}

/* get function: r_info */
static int tolua_get_r_info_ptr(lua_State* tolua_S)
{
 tolua_pushusertype(tolua_S,(void*)r_info,"monster_race");
 return 1;
}

/* set function: r_info */
static int tolua_set_r_info_ptr(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!tolua_isusertype(tolua_S,2,"monster_race",0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  r_info = ((monster_race*)  tolua_tousertype(tolua_S,2,0));
 return 0;
}

/* get function: r_name */
static int tolua_get_r_name(lua_State* tolua_S)
{
 tolua_pushstring(tolua_S,(const char*)r_name);
 return 1;
}

/* set function: r_name */
static int tolua_set_r_name(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!tolua_isstring(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  r_name = ((char*)  tolua_tostring(tolua_S,2,0));
 return 0;
}

/* get function: r_text */
static int tolua_get_r_text(lua_State* tolua_S)
{
 tolua_pushstring(tolua_S,(const char*)r_text);
 return 1;
}

/* set function: r_text */
static int tolua_set_r_text(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!tolua_isstring(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  r_text = ((char*)  tolua_tostring(tolua_S,2,0));
 return 0;
}

/* get function: d_head */
static int tolua_get_d_head_ptr(lua_State* tolua_S)
{
 tolua_pushusertype(tolua_S,(void*)d_head,"header");
 return 1;
}

/* set function: d_head */
static int tolua_set_d_head_ptr(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!tolua_isusertype(tolua_S,2,"header",0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  d_head = ((header*)  tolua_tousertype(tolua_S,2,0));
 return 0;
}

/* get function: d_info */
static int tolua_get_d_info_ptr(lua_State* tolua_S)
{
 tolua_pushusertype(tolua_S,(void*)d_info,"dungeon_info_type");
 return 1;
}

/* set function: d_info */
static int tolua_set_d_info_ptr(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!tolua_isusertype(tolua_S,2,"dungeon_info_type",0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  d_info = ((dungeon_info_type*)  tolua_tousertype(tolua_S,2,0));
 return 0;
}

/* get function: d_name */
static int tolua_get_d_name(lua_State* tolua_S)
{
 tolua_pushstring(tolua_S,(const char*)d_name);
 return 1;
}

/* set function: d_name */
static int tolua_set_d_name(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!tolua_isstring(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  d_name = ((char*)  tolua_tostring(tolua_S,2,0));
 return 0;
}

/* get function: d_text */
static int tolua_get_d_text(lua_State* tolua_S)
{
 tolua_pushstring(tolua_S,(const char*)d_text);
 return 1;
}

/* set function: d_text */
static int tolua_set_d_text(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!tolua_isstring(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  d_text = ((char*)  tolua_tostring(tolua_S,2,0));
 return 0;
}

/* get function: t_head */
static int tolua_get_t_head_ptr(lua_State* tolua_S)
{
 tolua_pushusertype(tolua_S,(void*)t_head,"header");
 return 1;
}

/* set function: t_head */
static int tolua_set_t_head_ptr(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!tolua_isusertype(tolua_S,2,"header",0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  t_head = ((header*)  tolua_tousertype(tolua_S,2,0));
 return 0;
}

/* get function: t_info */
static int tolua_get_t_info_ptr(lua_State* tolua_S)
{
 tolua_pushusertype(tolua_S,(void*)t_info,"trap_type");
 return 1;
}

/* set function: t_info */
static int tolua_set_t_info_ptr(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!tolua_isusertype(tolua_S,2,"trap_type",0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  t_info = ((trap_type*)  tolua_tousertype(tolua_S,2,0));
 return 0;
}

/* get function: t_name */
static int tolua_get_t_name(lua_State* tolua_S)
{
 tolua_pushstring(tolua_S,(const char*)t_name);
 return 1;
}

/* set function: t_name */
static int tolua_set_t_name(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!tolua_isstring(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  t_name = ((char*)  tolua_tostring(tolua_S,2,0));
 return 0;
}

/* get function: t_text */
static int tolua_get_t_text(lua_State* tolua_S)
{
 tolua_pushstring(tolua_S,(const char*)t_text);
 return 1;
}

/* set function: t_text */
static int tolua_set_t_text(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!tolua_isstring(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  t_text = ((char*)  tolua_tostring(tolua_S,2,0));
 return 0;
}

/* get function: ANGBAND_SYS */
static int tolua_get_ANGBAND_SYS(lua_State* tolua_S)
{
 tolua_pushstring(tolua_S,(const char*)ANGBAND_SYS);
 return 1;
}

/* set function: ANGBAND_SYS */
static int tolua_set_ANGBAND_SYS(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!tolua_isstring(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  ANGBAND_SYS = ((cptr)  tolua_tostring(tolua_S,2,0));
 return 0;
}

/* get function: ANGBAND_GRAF */
static int tolua_get_ANGBAND_GRAF(lua_State* tolua_S)
{
 tolua_pushstring(tolua_S,(const char*)ANGBAND_GRAF);
 return 1;
}

/* set function: ANGBAND_GRAF */
static int tolua_set_ANGBAND_GRAF(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!tolua_isstring(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  ANGBAND_GRAF = ((cptr)  tolua_tostring(tolua_S,2,0));
 return 0;
}

/* get function: ANGBAND_DIR */
static int tolua_get_ANGBAND_DIR(lua_State* tolua_S)
{
 tolua_pushstring(tolua_S,(const char*)ANGBAND_DIR);
 return 1;
}

/* set function: ANGBAND_DIR */
static int tolua_set_ANGBAND_DIR(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!tolua_isstring(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  ANGBAND_DIR = ((cptr)  tolua_tostring(tolua_S,2,0));
 return 0;
}

/* get function: ANGBAND_DIR_APEX */
static int tolua_get_ANGBAND_DIR_APEX(lua_State* tolua_S)
{
 tolua_pushstring(tolua_S,(const char*)ANGBAND_DIR_APEX);
 return 1;
}

/* set function: ANGBAND_DIR_APEX */
static int tolua_set_ANGBAND_DIR_APEX(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!tolua_isstring(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  ANGBAND_DIR_APEX = ((cptr)  tolua_tostring(tolua_S,2,0));
 return 0;
}

/* get function: ANGBAND_DIR_BONE */
static int tolua_get_ANGBAND_DIR_BONE(lua_State* tolua_S)
{
 tolua_pushstring(tolua_S,(const char*)ANGBAND_DIR_BONE);
 return 1;
}

/* set function: ANGBAND_DIR_BONE */
static int tolua_set_ANGBAND_DIR_BONE(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!tolua_isstring(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  ANGBAND_DIR_BONE = ((cptr)  tolua_tostring(tolua_S,2,0));
 return 0;
}

/* get function: ANGBAND_DIR_DATA */
static int tolua_get_ANGBAND_DIR_DATA(lua_State* tolua_S)
{
 tolua_pushstring(tolua_S,(const char*)ANGBAND_DIR_DATA);
 return 1;
}

/* set function: ANGBAND_DIR_DATA */
static int tolua_set_ANGBAND_DIR_DATA(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!tolua_isstring(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  ANGBAND_DIR_DATA = ((cptr)  tolua_tostring(tolua_S,2,0));
 return 0;
}

/* get function: ANGBAND_DIR_EDIT */
static int tolua_get_ANGBAND_DIR_EDIT(lua_State* tolua_S)
{
 tolua_pushstring(tolua_S,(const char*)ANGBAND_DIR_EDIT);
 return 1;
}

/* set function: ANGBAND_DIR_EDIT */
static int tolua_set_ANGBAND_DIR_EDIT(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!tolua_isstring(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  ANGBAND_DIR_EDIT = ((cptr)  tolua_tostring(tolua_S,2,0));
 return 0;
}

/* get function: ANGBAND_DIR_FILE */
static int tolua_get_ANGBAND_DIR_FILE(lua_State* tolua_S)
{
 tolua_pushstring(tolua_S,(const char*)ANGBAND_DIR_FILE);
 return 1;
}

/* set function: ANGBAND_DIR_FILE */
static int tolua_set_ANGBAND_DIR_FILE(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!tolua_isstring(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  ANGBAND_DIR_FILE = ((cptr)  tolua_tostring(tolua_S,2,0));
 return 0;
}

/* get function: ANGBAND_DIR_HELP */
static int tolua_get_ANGBAND_DIR_HELP(lua_State* tolua_S)
{
 tolua_pushstring(tolua_S,(const char*)ANGBAND_DIR_HELP);
 return 1;
}

/* set function: ANGBAND_DIR_HELP */
static int tolua_set_ANGBAND_DIR_HELP(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!tolua_isstring(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  ANGBAND_DIR_HELP = ((cptr)  tolua_tostring(tolua_S,2,0));
 return 0;
}

/* get function: ANGBAND_DIR_INFO */
static int tolua_get_ANGBAND_DIR_INFO(lua_State* tolua_S)
{
 tolua_pushstring(tolua_S,(const char*)ANGBAND_DIR_INFO);
 return 1;
}

/* set function: ANGBAND_DIR_INFO */
static int tolua_set_ANGBAND_DIR_INFO(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!tolua_isstring(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  ANGBAND_DIR_INFO = ((cptr)  tolua_tostring(tolua_S,2,0));
 return 0;
}

/* get function: ANGBAND_DIR_SAVE */
static int tolua_get_ANGBAND_DIR_SAVE(lua_State* tolua_S)
{
 tolua_pushstring(tolua_S,(const char*)ANGBAND_DIR_SAVE);
 return 1;
}

/* set function: ANGBAND_DIR_SAVE */
static int tolua_set_ANGBAND_DIR_SAVE(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!tolua_isstring(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  ANGBAND_DIR_SAVE = ((cptr)  tolua_tostring(tolua_S,2,0));
 return 0;
}

/* get function: ANGBAND_DIR_USER */
static int tolua_get_ANGBAND_DIR_USER(lua_State* tolua_S)
{
 tolua_pushstring(tolua_S,(const char*)ANGBAND_DIR_USER);
 return 1;
}

/* set function: ANGBAND_DIR_USER */
static int tolua_set_ANGBAND_DIR_USER(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!tolua_isstring(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  ANGBAND_DIR_USER = ((cptr)  tolua_tostring(tolua_S,2,0));
 return 0;
}

/* get function: ANGBAND_DIR_XTRA */
static int tolua_get_ANGBAND_DIR_XTRA(lua_State* tolua_S)
{
 tolua_pushstring(tolua_S,(const char*)ANGBAND_DIR_XTRA);
 return 1;
}

/* set function: ANGBAND_DIR_XTRA */
static int tolua_set_ANGBAND_DIR_XTRA(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!tolua_isstring(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  ANGBAND_DIR_XTRA = ((cptr)  tolua_tostring(tolua_S,2,0));
 return 0;
}

/* get function: ANGBAND_DIR_PREF */
static int tolua_get_ANGBAND_DIR_PREF(lua_State* tolua_S)
{
 tolua_pushstring(tolua_S,(const char*)ANGBAND_DIR_PREF);
 return 1;
}

/* set function: ANGBAND_DIR_PREF */
static int tolua_set_ANGBAND_DIR_PREF(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!tolua_isstring(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  ANGBAND_DIR_PREF = ((cptr)  tolua_tostring(tolua_S,2,0));
 return 0;
}

/* get function: ANGBAND_DIR_SCRIPT */
static int tolua_get_ANGBAND_DIR_SCRIPT(lua_State* tolua_S)
{
 tolua_pushstring(tolua_S,(const char*)ANGBAND_DIR_SCRIPT);
 return 1;
}

/* set function: ANGBAND_DIR_SCRIPT */
static int tolua_set_ANGBAND_DIR_SCRIPT(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!tolua_isstring(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  ANGBAND_DIR_SCRIPT = ((cptr)  tolua_tostring(tolua_S,2,0));
 return 0;
}

/* get function: item_tester_full */
static int tolua_get_item_tester_full(lua_State* tolua_S)
{
 tolua_pushboolean(tolua_S,(bool)item_tester_full);
 return 1;
}

/* set function: item_tester_full */
static int tolua_set_item_tester_full(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!tolua_isboolean(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  item_tester_full = ((bool)  tolua_toboolean(tolua_S,2,0));
 return 0;
}

/* get function: item_tester_tval */
static int tolua_get_item_tester_tval(lua_State* tolua_S)
{
 tolua_pushnumber(tolua_S,(long)item_tester_tval);
 return 1;
}

/* set function: item_tester_tval */
static int tolua_set_item_tester_tval(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  item_tester_tval = ((byte)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: monk_armour_aux */
static int tolua_get_monk_armour_aux(lua_State* tolua_S)
{
 tolua_pushboolean(tolua_S,(bool)monk_armour_aux);
 return 1;
}

/* set function: monk_armour_aux */
static int tolua_set_monk_armour_aux(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!tolua_isboolean(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  monk_armour_aux = ((bool)  tolua_toboolean(tolua_S,2,0));
 return 0;
}

/* get function: monk_notify_aux */
static int tolua_get_monk_notify_aux(lua_State* tolua_S)
{
 tolua_pushboolean(tolua_S,(bool)monk_notify_aux);
 return 1;
}

/* set function: monk_notify_aux */
static int tolua_set_monk_notify_aux(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!tolua_isboolean(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  monk_notify_aux = ((bool)  tolua_toboolean(tolua_S,2,0));
 return 0;
}

/* get function: max_r_idx */
static int tolua_get_max_r_idx(lua_State* tolua_S)
{
 tolua_pushnumber(tolua_S,(long)max_r_idx);
 return 1;
}

/* set function: max_r_idx */
static int tolua_set_max_r_idx(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  max_r_idx = ((u16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: max_k_idx */
static int tolua_get_max_k_idx(lua_State* tolua_S)
{
 tolua_pushnumber(tolua_S,(long)max_k_idx);
 return 1;
}

/* set function: max_k_idx */
static int tolua_set_max_k_idx(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  max_k_idx = ((u16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: max_v_idx */
static int tolua_get_max_v_idx(lua_State* tolua_S)
{
 tolua_pushnumber(tolua_S,(long)max_v_idx);
 return 1;
}

/* set function: max_v_idx */
static int tolua_set_max_v_idx(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  max_v_idx = ((u16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: max_f_idx */
static int tolua_get_max_f_idx(lua_State* tolua_S)
{
 tolua_pushnumber(tolua_S,(long)max_f_idx);
 return 1;
}

/* set function: max_f_idx */
static int tolua_set_max_f_idx(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  max_f_idx = ((u16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: max_a_idx */
static int tolua_get_max_a_idx(lua_State* tolua_S)
{
 tolua_pushnumber(tolua_S,(long)max_a_idx);
 return 1;
}

/* set function: max_a_idx */
static int tolua_set_max_a_idx(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  max_a_idx = ((u16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: max_e_idx */
static int tolua_get_max_e_idx(lua_State* tolua_S)
{
 tolua_pushnumber(tolua_S,(long)max_e_idx);
 return 1;
}

/* set function: max_e_idx */
static int tolua_set_max_e_idx(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  max_e_idx = ((u16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: max_d_idx */
static int tolua_get_max_d_idx(lua_State* tolua_S)
{
 tolua_pushnumber(tolua_S,(long)max_d_idx);
 return 1;
}

/* set function: max_d_idx */
static int tolua_set_max_d_idx(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  max_d_idx = ((u16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: max_o_idx */
static int tolua_get_max_o_idx(lua_State* tolua_S)
{
 tolua_pushnumber(tolua_S,(long)max_o_idx);
 return 1;
}

/* set function: max_o_idx */
static int tolua_set_max_o_idx(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  max_o_idx = ((u16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: max_m_idx */
static int tolua_get_max_m_idx(lua_State* tolua_S)
{
 tolua_pushnumber(tolua_S,(long)max_m_idx);
 return 1;
}

/* set function: max_m_idx */
static int tolua_set_max_m_idx(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  max_m_idx = ((u16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: max_t_idx */
static int tolua_get_max_t_idx(lua_State* tolua_S)
{
 tolua_pushnumber(tolua_S,(long)max_t_idx);
 return 1;
}

/* set function: max_t_idx */
static int tolua_set_max_t_idx(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  max_t_idx = ((u16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: max_wf_idx */
static int tolua_get_max_wf_idx(lua_State* tolua_S)
{
 tolua_pushnumber(tolua_S,(long)max_wf_idx);
 return 1;
}

/* set function: max_wf_idx */
static int tolua_set_max_wf_idx(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  max_wf_idx = ((u16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: init_flags */
static int tolua_get_init_flags(lua_State* tolua_S)
{
 tolua_pushnumber(tolua_S,(long)init_flags);
 return 1;
}

/* set function: init_flags */
static int tolua_set_init_flags(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  init_flags = ((int)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: special_flag */
static int tolua_get_special_flag(lua_State* tolua_S)
{
 tolua_pushboolean(tolua_S,(bool)special_flag);
 return 1;
}

/* set function: special_flag */
static int tolua_set_special_flag(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!tolua_isboolean(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  special_flag = ((bool)  tolua_toboolean(tolua_S,2,0));
 return 0;
}

/* get function: ambush_flag */
static int tolua_get_ambush_flag(lua_State* tolua_S)
{
 tolua_pushboolean(tolua_S,(bool)ambush_flag);
 return 1;
}

/* set function: ambush_flag */
static int tolua_set_ambush_flag(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!tolua_isboolean(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  ambush_flag = ((bool)  tolua_toboolean(tolua_S,2,0));
 return 0;
}

/* get function: no_breeds */
static int tolua_get_no_breeds(lua_State* tolua_S)
{
 tolua_pushnumber(tolua_S,(long)no_breeds);
 return 1;
}

/* set function: no_breeds */
static int tolua_set_no_breeds(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  no_breeds = ((u16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: spell_num */
static int tolua_get_spell_num(lua_State* tolua_S)
{
 tolua_pushnumber(tolua_S,(long)spell_num);
 return 1;
}

/* set function: spell_num */
static int tolua_set_spell_num(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  spell_num = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: dungeon_type */
static int tolua_get_dungeon_type(lua_State* tolua_S)
{
 tolua_pushnumber(tolua_S,(long)dungeon_type);
 return 1;
}

/* set function: dungeon_type */
static int tolua_set_dungeon_type(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  dungeon_type = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: max_dlv */
static int tolua_get_max_dlv(lua_State* tolua_S)
{
 tolua_pushnumber(tolua_S,(long)max_dlv);
 return 1;
}

/* set function: max_dlv */
static int tolua_set_max_dlv(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  max_dlv = ((s16b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: total_bounties */
static int tolua_get_total_bounties(lua_State* tolua_S)
{
 tolua_pushnumber(tolua_S,(long)total_bounties);
 return 1;
}

/* set function: total_bounties */
static int tolua_set_total_bounties(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  total_bounties = ((u32b)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: generate_encounter */
static int tolua_get_generate_encounter(lua_State* tolua_S)
{
 tolua_pushboolean(tolua_S,(bool)generate_encounter);
 return 1;
}

/* set function: generate_encounter */
static int tolua_set_generate_encounter(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!tolua_isboolean(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  generate_encounter = ((bool)  tolua_toboolean(tolua_S,2,0));
 return 0;
}

/* get function: permanent_levels */
static int tolua_get_permanent_levels(lua_State* tolua_S)
{
 tolua_pushboolean(tolua_S,(bool)permanent_levels);
 return 1;
}

/* set function: permanent_levels */
static int tolua_set_permanent_levels(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!tolua_isboolean(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  permanent_levels = ((bool)  tolua_toboolean(tolua_S,2,0));
 return 0;
}

/* get function: autoroll */
static int tolua_get_autoroll(lua_State* tolua_S)
{
 tolua_pushboolean(tolua_S,(bool)autoroll);
 return 1;
}

/* set function: autoroll */
static int tolua_set_autoroll(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!tolua_isboolean(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  autoroll = ((bool)  tolua_toboolean(tolua_S,2,0));
 return 0;
}

/* get function: hack_allow_special */
static int tolua_get_hack_allow_special(lua_State* tolua_S)
{
 tolua_pushboolean(tolua_S,(bool)hack_allow_special);
 return 1;
}

/* set function: hack_allow_special */
static int tolua_set_hack_allow_special(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!tolua_isboolean(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  hack_allow_special = ((bool)  tolua_toboolean(tolua_S,2,0));
 return 0;
}

/* get function: magic_spell */
static int tolua_get_init_magic_spell(lua_State* tolua_S)
{
 int tolua_index;
#ifndef TOLUA_RELEASE
 {
 tolua_Error tolua_err;
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in array indexing.",&tolua_err);
 }
#endif
 tolua_index = (int)tolua_tonumber(tolua_S,2,0)-1;
#ifndef TOLUA_RELEASE
 if (tolua_index<0 || tolua_index>=30)
 tolua_error(tolua_S,"array indexing out of range.",NULL);
#endif
 tolua_pushusertype(tolua_S,(void*)&magic_spell[tolua_index],"magic_spells");
 return 1;
}

/* set function: magic_spell */
static int tolua_set_init_magic_spell(lua_State* tolua_S)
{
 int tolua_index;
#ifndef TOLUA_RELEASE
 {
 tolua_Error tolua_err;
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in array indexing.",&tolua_err);
 }
#endif
 tolua_index = (int)tolua_tonumber(tolua_S,2,0)-1;
#ifndef TOLUA_RELEASE
 if (tolua_index<0 || tolua_index>=30)
 tolua_error(tolua_S,"array indexing out of range.",NULL);
#endif
  magic_spell[tolua_index] = *((magic_spells*)  tolua_tousertype(tolua_S,3,0));
 return 0;
}

/* get function: monster_magic */
static int tolua_get_init_monster_magic(lua_State* tolua_S)
{
 int tolua_index;
#ifndef TOLUA_RELEASE
 {
 tolua_Error tolua_err;
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in array indexing.",&tolua_err);
 }
#endif
 tolua_index = (int)tolua_tonumber(tolua_S,2,0)-1;
#ifndef TOLUA_RELEASE
 if (tolua_index<0 || tolua_index>=15)
 tolua_error(tolua_S,"array indexing out of range.",NULL);
#endif
 tolua_pushusertype(tolua_S,(void*)&monster_magic[tolua_index],"monster_magics");
 return 1;
}

/* set function: monster_magic */
static int tolua_set_init_monster_magic(lua_State* tolua_S)
{
 int tolua_index;
#ifndef TOLUA_RELEASE
 {
 tolua_Error tolua_err;
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in array indexing.",&tolua_err);
 }
#endif
 tolua_index = (int)tolua_tonumber(tolua_S,2,0)-1;
#ifndef TOLUA_RELEASE
 if (tolua_index<0 || tolua_index>=15)
 tolua_error(tolua_S,"array indexing out of range.",NULL);
#endif
  monster_magic[tolua_index] = *((monster_magics*)  tolua_tousertype(tolua_S,3,0));
 return 0;
}

/* get function: music_song */
static int tolua_get_init_music_song(lua_State* tolua_S)
{
 int tolua_index;
#ifndef TOLUA_RELEASE
 {
 tolua_Error tolua_err;
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in array indexing.",&tolua_err);
 }
#endif
 tolua_index = (int)tolua_tonumber(tolua_S,2,0)-1;
#ifndef TOLUA_RELEASE
 if (tolua_index<0 || tolua_index>=15)
 tolua_error(tolua_S,"array indexing out of range.",NULL);
#endif
 tolua_pushusertype(tolua_S,(void*)&music_song[tolua_index],"music_songs");
 return 1;
}

/* set function: music_song */
static int tolua_set_init_music_song(lua_State* tolua_S)
{
 int tolua_index;
#ifndef TOLUA_RELEASE
 {
 tolua_Error tolua_err;
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in array indexing.",&tolua_err);
 }
#endif
 tolua_index = (int)tolua_tonumber(tolua_S,2,0)-1;
#ifndef TOLUA_RELEASE
 if (tolua_index<0 || tolua_index>=15)
 tolua_error(tolua_S,"array indexing out of range.",NULL);
#endif
  music_song[tolua_index] = *((music_songs*)  tolua_tousertype(tolua_S,3,0));
 return 0;
}

/* get function: stores */
static int tolua_get_init_stores(lua_State* tolua_S)
{
 int tolua_index;
#ifndef TOLUA_RELEASE
 {
 tolua_Error tolua_err;
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in array indexing.",&tolua_err);
 }
#endif
 tolua_index = (int)tolua_tonumber(tolua_S,2,0)-1;
#ifndef TOLUA_RELEASE
 if (tolua_index<0 || tolua_index>=13)
 tolua_error(tolua_S,"array indexing out of range.",NULL);
#endif
 tolua_pushusertype(tolua_S,(void*)&stores[tolua_index],"store_type");
 return 1;
}

/* set function: stores */
static int tolua_set_init_stores(lua_State* tolua_S)
{
 int tolua_index;
#ifndef TOLUA_RELEASE
 {
 tolua_Error tolua_err;
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in array indexing.",&tolua_err);
 }
#endif
 tolua_index = (int)tolua_tonumber(tolua_S,2,0)-1;
#ifndef TOLUA_RELEASE
 if (tolua_index<0 || tolua_index>=13)
 tolua_error(tolua_S,"array indexing out of range.",NULL);
#endif
  stores[tolua_index] = *((store_type*)  tolua_tousertype(tolua_S,3,0));
 return 0;
}

/* get function: current_weapon */
static int tolua_get_current_weapon_ptr(lua_State* tolua_S)
{
 tolua_pushusertype(tolua_S,(void*)current_weapon,"object_type");
 return 1;
}

/* set function: current_weapon */
static int tolua_set_current_weapon_ptr(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!tolua_isusertype(tolua_S,2,"object_type",0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  current_weapon = ((object_type*)  tolua_tousertype(tolua_S,2,0));
 return 0;
}

/* get function: drop_ranged */
static int tolua_get_drop_ranged_ptr(lua_State* tolua_S)
{
 tolua_pushusertype(tolua_S,(void*)drop_ranged,"object_type");
 return 1;
}

/* set function: drop_ranged */
static int tolua_set_drop_ranged_ptr(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!tolua_isusertype(tolua_S,2,"object_type",0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  drop_ranged = ((object_type*)  tolua_tousertype(tolua_S,2,0));
 return 0;
}

/* get function: current_item */
static int tolua_get_current_item_ptr(lua_State* tolua_S)
{
 tolua_pushusertype(tolua_S,(void*)current_item,"object_type");
 return 1;
}

/* set function: current_item */
static int tolua_set_current_item_ptr(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!tolua_isusertype(tolua_S,2,"object_type",0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  current_item = ((object_type*)  tolua_tousertype(tolua_S,2,0));
 return 0;
}

/* get function: combatfeat */
static int tolua_get_combatfeat(lua_State* tolua_S)
{
 tolua_pushboolean(tolua_S,(bool)combatfeat);
 return 1;
}

/* set function: combatfeat */
static int tolua_set_combatfeat(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!tolua_isboolean(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  combatfeat = ((bool)  tolua_toboolean(tolua_S,2,0));
 return 0;
}

/* get function: center_player */
static int tolua_get_center_player(lua_State* tolua_S)
{
 tolua_pushboolean(tolua_S,(bool)center_player);
 return 1;
}

/* set function: center_player */
static int tolua_set_center_player(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!tolua_isboolean(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  center_player = ((bool)  tolua_toboolean(tolua_S,2,0));
 return 0;
}

/* get function: fate_item_modifier */
static int tolua_get_fate_item_modifier(lua_State* tolua_S)
{
 tolua_pushnumber(tolua_S,(long)fate_item_modifier);
 return 1;
}

/* set function: fate_item_modifier */
static int tolua_set_fate_item_modifier(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  fate_item_modifier = ((int)  tolua_tonumber(tolua_S,2,0));
 return 0;
}

/* get function: global_object */
static int tolua_get_global_object(lua_State* tolua_S)
{
 tolua_pushusertype(tolua_S,(void*)&global_object,"object_type");
 return 1;
}

/* set function: global_object */
static int tolua_set_global_object(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (!tolua_isusertype(tolua_S,2,"object_type",0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in variable assignment.",&tolua_err);
#endif
  global_object = *((object_type*)  tolua_tousertype(tolua_S,2,0));
 return 0;
}

/* function: lua_cave */
static int tolua_init_lua_cave00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnumber(tolua_S,1,0,&tolua_err) ||
 !tolua_isnumber(tolua_S,2,0,&tolua_err) ||
 !tolua_isnoobj(tolua_S,3,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
  int y = ((int)  tolua_tonumber(tolua_S,1,0));
  int x = ((int)  tolua_tonumber(tolua_S,2,0));
 {
  cave_type* tolua_ret = (cave_type*)  lua_cave(y,x);
 tolua_pushusertype(tolua_S,(void*)tolua_ret,"cave_type");
 }
 }
 return 1;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'cave'.",&tolua_err);
 return 0;
#endif
}

/* function: lua_monster */
static int tolua_init_lua_monster00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnumber(tolua_S,1,0,&tolua_err) ||
 !tolua_isnoobj(tolua_S,2,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
  int m_idx = ((int)  tolua_tonumber(tolua_S,1,0));
 {
  monster_type* tolua_ret = (monster_type*)  lua_monster(m_idx);
 tolua_pushusertype(tolua_S,(void*)tolua_ret,"monster_type");
 }
 }
 return 1;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'monster'.",&tolua_err);
 return 0;
#endif
}

/* function: lua_r_info */
static int tolua_init_lua_r_info00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnumber(tolua_S,1,0,&tolua_err) ||
 !tolua_isnoobj(tolua_S,2,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
  int r_idx = ((int)  tolua_tonumber(tolua_S,1,0));
 {
  monster_race* tolua_ret = (monster_race*)  lua_r_info(r_idx);
 tolua_pushusertype(tolua_S,(void*)tolua_ret,"monster_race");
 }
 }
 return 1;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'm_race'.",&tolua_err);
 return 0;
#endif
}

/* function: lua_inven */
static int tolua_init_lua_inven00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnumber(tolua_S,1,0,&tolua_err) ||
 !tolua_isnoobj(tolua_S,2,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
  int slot = ((int)  tolua_tonumber(tolua_S,1,0));
 {
  object_type* tolua_ret = (object_type*)  lua_inven(slot);
 tolua_pushusertype(tolua_S,(void*)tolua_ret,"object_type");
 }
 }
 return 1;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'inven'.",&tolua_err);
 return 0;
#endif
}

/* function: lua_kind */
static int tolua_init_lua_kind00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isusertype(tolua_S,1,"object_type",0,&tolua_err) ||
 !tolua_isnoobj(tolua_S,2,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
  object_type* o_ptr = ((object_type*)  tolua_tousertype(tolua_S,1,0));
 {
  object_kind* tolua_ret = (object_kind*)  lua_kind(o_ptr);
 tolua_pushusertype(tolua_S,(void*)tolua_ret,"object_kind");
 }
 }
 return 1;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'kind'.",&tolua_err);
 return 0;
#endif
}

/* function: lua_object */
static int tolua_init_lua_object00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnumber(tolua_S,1,0,&tolua_err) ||
 !tolua_isnoobj(tolua_S,2,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
  int oidx = ((int)  tolua_tonumber(tolua_S,1,0));
 {
  object_type* tolua_ret = (object_type*)  lua_object(oidx);
 tolua_pushusertype(tolua_S,(void*)tolua_ret,"object_type");
 }
 }
 return 1;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'object'.",&tolua_err);
 return 0;
#endif
}

/* function: lua_dungeon */
static int tolua_init_lua_dungeon00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnumber(tolua_S,1,0,&tolua_err) ||
 !tolua_isnoobj(tolua_S,2,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
  int which = ((int)  tolua_tonumber(tolua_S,1,0));
 {
  dungeon_info_type* tolua_ret = (dungeon_info_type*)  lua_dungeon(which);
 tolua_pushusertype(tolua_S,(void*)tolua_ret,"dungeon_info_type");
 }
 }
 return 1;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'dungeon'.",&tolua_err);
 return 0;
#endif
}

/* function: get_monster_flag1 */
static int tolua_init_get_monster_flag100(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnumber(tolua_S,1,0,&tolua_err) ||
 !tolua_isnumber(tolua_S,2,0,&tolua_err) ||
 !tolua_isnoobj(tolua_S,3,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
  int r_idx = ((int)  tolua_tonumber(tolua_S,1,0));
  u32b flag = ((u32b)  tolua_tonumber(tolua_S,2,0));
 {
  bool tolua_ret = (bool)  get_monster_flag1(r_idx,flag);
 tolua_pushboolean(tolua_S,(bool)tolua_ret);
 }
 }
 return 1;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'get_monster_flag1'.",&tolua_err);
 return 0;
#endif
}

/* function: get_monster_flag2 */
static int tolua_init_get_monster_flag200(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnumber(tolua_S,1,0,&tolua_err) ||
 !tolua_isnumber(tolua_S,2,0,&tolua_err) ||
 !tolua_isnoobj(tolua_S,3,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
  int r_idx = ((int)  tolua_tonumber(tolua_S,1,0));
  u32b flag = ((u32b)  tolua_tonumber(tolua_S,2,0));
 {
  bool tolua_ret = (bool)  get_monster_flag2(r_idx,flag);
 tolua_pushboolean(tolua_S,(bool)tolua_ret);
 }
 }
 return 1;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'get_monster_flag2'.",&tolua_err);
 return 0;
#endif
}

/* function: get_monster_flag3 */
static int tolua_init_get_monster_flag300(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnumber(tolua_S,1,0,&tolua_err) ||
 !tolua_isnumber(tolua_S,2,0,&tolua_err) ||
 !tolua_isnoobj(tolua_S,3,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
  int r_idx = ((int)  tolua_tonumber(tolua_S,1,0));
  u32b flag = ((u32b)  tolua_tonumber(tolua_S,2,0));
 {
  bool tolua_ret = (bool)  get_monster_flag3(r_idx,flag);
 tolua_pushboolean(tolua_S,(bool)tolua_ret);
 }
 }
 return 1;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'get_monster_flag3'.",&tolua_err);
 return 0;
#endif
}

/* function: get_monster_flag4 */
static int tolua_init_get_monster_flag400(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnumber(tolua_S,1,0,&tolua_err) ||
 !tolua_isnumber(tolua_S,2,0,&tolua_err) ||
 !tolua_isnoobj(tolua_S,3,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
  int r_idx = ((int)  tolua_tonumber(tolua_S,1,0));
  u32b flag = ((u32b)  tolua_tonumber(tolua_S,2,0));
 {
  bool tolua_ret = (bool)  get_monster_flag4(r_idx,flag);
 tolua_pushboolean(tolua_S,(bool)tolua_ret);
 }
 }
 return 1;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'get_monster_flag4'.",&tolua_err);
 return 0;
#endif
}

/* function: get_monster_flag5 */
static int tolua_init_get_monster_flag500(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnumber(tolua_S,1,0,&tolua_err) ||
 !tolua_isnumber(tolua_S,2,0,&tolua_err) ||
 !tolua_isnoobj(tolua_S,3,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
  int r_idx = ((int)  tolua_tonumber(tolua_S,1,0));
  u32b flag = ((u32b)  tolua_tonumber(tolua_S,2,0));
 {
  bool tolua_ret = (bool)  get_monster_flag5(r_idx,flag);
 tolua_pushboolean(tolua_S,(bool)tolua_ret);
 }
 }
 return 1;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'get_monster_flag5'.",&tolua_err);
 return 0;
#endif
}

/* function: get_monster_flag6 */
static int tolua_init_get_monster_flag600(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnumber(tolua_S,1,0,&tolua_err) ||
 !tolua_isnumber(tolua_S,2,0,&tolua_err) ||
 !tolua_isnoobj(tolua_S,3,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
  int r_idx = ((int)  tolua_tonumber(tolua_S,1,0));
  u32b flag = ((u32b)  tolua_tonumber(tolua_S,2,0));
 {
  bool tolua_ret = (bool)  get_monster_flag6(r_idx,flag);
 tolua_pushboolean(tolua_S,(bool)tolua_ret);
 }
 }
 return 1;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'get_monster_flag6'.",&tolua_err);
 return 0;
#endif
}

/* function: get_monster_flag7 */
static int tolua_init_get_monster_flag700(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnumber(tolua_S,1,0,&tolua_err) ||
 !tolua_isnumber(tolua_S,2,0,&tolua_err) ||
 !tolua_isnoobj(tolua_S,3,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
  int r_idx = ((int)  tolua_tonumber(tolua_S,1,0));
  u32b flag = ((u32b)  tolua_tonumber(tolua_S,2,0));
 {
  bool tolua_ret = (bool)  get_monster_flag7(r_idx,flag);
 tolua_pushboolean(tolua_S,(bool)tolua_ret);
 }
 }
 return 1;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'get_monster_flag7'.",&tolua_err);
 return 0;
#endif
}

/* function: get_monster_flag8 */
static int tolua_init_get_monster_flag800(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnumber(tolua_S,1,0,&tolua_err) ||
 !tolua_isnumber(tolua_S,2,0,&tolua_err) ||
 !tolua_isnoobj(tolua_S,3,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
  int r_idx = ((int)  tolua_tonumber(tolua_S,1,0));
  u32b flag = ((u32b)  tolua_tonumber(tolua_S,2,0));
 {
  bool tolua_ret = (bool)  get_monster_flag8(r_idx,flag);
 tolua_pushboolean(tolua_S,(bool)tolua_ret);
 }
 }
 return 1;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'get_monster_flag8'.",&tolua_err);
 return 0;
#endif
}

/* function: get_monster_flag9 */
static int tolua_init_get_monster_flag900(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnumber(tolua_S,1,0,&tolua_err) ||
 !tolua_isnumber(tolua_S,2,0,&tolua_err) ||
 !tolua_isnoobj(tolua_S,3,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
  int r_idx = ((int)  tolua_tonumber(tolua_S,1,0));
  u32b flag = ((u32b)  tolua_tonumber(tolua_S,2,0));
 {
  bool tolua_ret = (bool)  get_monster_flag9(r_idx,flag);
 tolua_pushboolean(tolua_S,(bool)tolua_ret);
 }
 }
 return 1;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'get_monster_flag9'.",&tolua_err);
 return 0;
#endif
}

/* function: get_object_flag1 */
static int tolua_init_get_object_flag100(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isusertype(tolua_S,1,"object_type",0,&tolua_err) ||
 !tolua_isnumber(tolua_S,2,0,&tolua_err) ||
 !tolua_isnoobj(tolua_S,3,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
  object_type* o_ptr = ((object_type*)  tolua_tousertype(tolua_S,1,0));
  u32b flag = ((u32b)  tolua_tonumber(tolua_S,2,0));
 {
  bool tolua_ret = (bool)  get_object_flag1(o_ptr,flag);
 tolua_pushboolean(tolua_S,(bool)tolua_ret);
 }
 }
 return 1;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'get_object_flag1'.",&tolua_err);
 return 0;
#endif
}

/* function: get_object_flag2 */
static int tolua_init_get_object_flag200(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isusertype(tolua_S,1,"object_type",0,&tolua_err) ||
 !tolua_isnumber(tolua_S,2,0,&tolua_err) ||
 !tolua_isnoobj(tolua_S,3,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
  object_type* o_ptr = ((object_type*)  tolua_tousertype(tolua_S,1,0));
  u32b flag = ((u32b)  tolua_tonumber(tolua_S,2,0));
 {
  bool tolua_ret = (bool)  get_object_flag2(o_ptr,flag);
 tolua_pushboolean(tolua_S,(bool)tolua_ret);
 }
 }
 return 1;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'get_object_flag2'.",&tolua_err);
 return 0;
#endif
}

/* function: get_object_flag3 */
static int tolua_init_get_object_flag300(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isusertype(tolua_S,1,"object_type",0,&tolua_err) ||
 !tolua_isnumber(tolua_S,2,0,&tolua_err) ||
 !tolua_isnoobj(tolua_S,3,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
  object_type* o_ptr = ((object_type*)  tolua_tousertype(tolua_S,1,0));
  u32b flag = ((u32b)  tolua_tonumber(tolua_S,2,0));
 {
  bool tolua_ret = (bool)  get_object_flag3(o_ptr,flag);
 tolua_pushboolean(tolua_S,(bool)tolua_ret);
 }
 }
 return 1;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'get_object_flag3'.",&tolua_err);
 return 0;
#endif
}

/* function: get_object_flag4 */
static int tolua_init_get_object_flag400(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isusertype(tolua_S,1,"object_type",0,&tolua_err) ||
 !tolua_isnumber(tolua_S,2,0,&tolua_err) ||
 !tolua_isnoobj(tolua_S,3,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
  object_type* o_ptr = ((object_type*)  tolua_tousertype(tolua_S,1,0));
  u32b flag = ((u32b)  tolua_tonumber(tolua_S,2,0));
 {
  bool tolua_ret = (bool)  get_object_flag4(o_ptr,flag);
 tolua_pushboolean(tolua_S,(bool)tolua_ret);
 }
 }
 return 1;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'get_object_flag4'.",&tolua_err);
 return 0;
#endif
}

/* function: get_monster_ability */
static int tolua_init_get_monster_ability00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isusertype(tolua_S,1,"monster_type",0,&tolua_err) ||
 !tolua_isnumber(tolua_S,2,0,&tolua_err) ||
 !tolua_isnoobj(tolua_S,3,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
  monster_type* m_ptr = ((monster_type*)  tolua_tousertype(tolua_S,1,0));
  u32b flag = ((u32b)  tolua_tonumber(tolua_S,2,0));
 {
  bool tolua_ret = (bool)  get_monster_ability(m_ptr,flag);
 tolua_pushboolean(tolua_S,(bool)tolua_ret);
 }
 }
 return 1;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'get_monster_ability'.",&tolua_err);
 return 0;
#endif
}

/* function: get_monster_desc */
static int tolua_init_get_monster_desc00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isusertype(tolua_S,1,"monster_type",0,&tolua_err) ||
 !tolua_isnumber(tolua_S,2,0,&tolua_err) ||
 !tolua_isnoobj(tolua_S,3,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
  monster_type* m_ptr = ((monster_type*)  tolua_tousertype(tolua_S,1,0));
  int mode = ((int)  tolua_tonumber(tolua_S,2,0));
 {
  char* tolua_ret = (char*)  get_monster_desc(m_ptr,mode);
 tolua_pushstring(tolua_S,(const char*)tolua_ret);
 }
 }
 return 1;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'get_monster_desc'.",&tolua_err);
 return 0;
#endif
}

/* function: plog */
static int tolua_init_plog00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isstring(tolua_S,1,0,&tolua_err) ||
 !tolua_isnoobj(tolua_S,2,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
  cptr str = ((cptr)  tolua_tostring(tolua_S,1,0));
 {
  plog(str);
 }
 }
 return 0;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'plog'.",&tolua_err);
 return 0;
#endif
}

/* function: always_hit_check */
static int tolua_init_always_hit_check00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnoobj(tolua_S,1,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
 {
  bool tolua_ret = (bool)  always_hit_check();
 tolua_pushboolean(tolua_S,(bool)tolua_ret);
 }
 }
 return 1;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'always_hit_check'.",&tolua_err);
 return 0;
#endif
}

/* function: verify_panel */
static int tolua_init_verify_panel00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnoobj(tolua_S,1,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
 {
  verify_panel();
 }
 }
 return 0;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'verify_panel'.",&tolua_err);
 return 0;
#endif
}

/* function: move_monster_spot */
static int tolua_init_move_monster_spot00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnumber(tolua_S,1,0,&tolua_err) ||
 !tolua_isnumber(tolua_S,2,0,&tolua_err) ||
 !tolua_isnumber(tolua_S,3,0,&tolua_err) ||
 !tolua_isnoobj(tolua_S,4,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
  int m_idx = ((int)  tolua_tonumber(tolua_S,1,0));
  int xspot = ((int)  tolua_tonumber(tolua_S,2,0));
  int yspot = ((int)  tolua_tonumber(tolua_S,3,0));
 {
  move_monster_spot(m_idx,xspot,yspot);
 }
 }
 return 0;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'move_monster_spot'.",&tolua_err);
 return 0;
#endif
}

/* function: anihilate_monsters */
static int tolua_init_anihilate_monsters00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnoobj(tolua_S,1,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
 {
  anihilate_monsters();
 }
 }
 return 0;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'anihilate_monsters'.",&tolua_err);
 return 0;
#endif
}

/* function: generate_cave */
static int tolua_init_generate_cave00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnoobj(tolua_S,1,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
 {
  generate_cave();
 }
 }
 return 0;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'generate_cave'.",&tolua_err);
 return 0;
#endif
}

/* function: verify_panel_always_update */
static int tolua_init_verify_panel_always_update00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnoobj(tolua_S,1,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
 {
  verify_panel_always_update();
 }
 }
 return 0;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'verify_panel_always_update'.",&tolua_err);
 return 0;
#endif
}

/* function: check_experience */
static int tolua_init_check_experience00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnoobj(tolua_S,1,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
 {
  check_experience();
 }
 }
 return 0;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'check_experience'.",&tolua_err);
 return 0;
#endif
}

/* function: gain_exp */
static int tolua_init_gain_exp00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnumber(tolua_S,1,0,&tolua_err) ||
 !tolua_isnoobj(tolua_S,2,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
  s32b amount = ((s32b)  tolua_tonumber(tolua_S,1,0));
 {
  gain_exp(amount);
 }
 }
 return 0;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'gain_exp'.",&tolua_err);
 return 0;
#endif
}

/* function: lose_exp */
static int tolua_init_lose_exp00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnumber(tolua_S,1,0,&tolua_err) ||
 !tolua_isnoobj(tolua_S,2,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
  s32b amount = ((s32b)  tolua_tonumber(tolua_S,1,0));
 {
  lose_exp(amount);
 }
 }
 return 0;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'lose_exp'.",&tolua_err);
 return 0;
#endif
}

/* function: attack_aura */
static int tolua_init_attack_aura00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnumber(tolua_S,1,0,&tolua_err) ||
 !tolua_isnumber(tolua_S,2,0,&tolua_err) ||
 !tolua_isnumber(tolua_S,3,0,&tolua_err) ||
 !tolua_isnoobj(tolua_S,4,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
  int typ = ((int)  tolua_tonumber(tolua_S,1,0));
  s32b dam = ((s32b)  tolua_tonumber(tolua_S,2,0));
  int rad = ((int)  tolua_tonumber(tolua_S,3,0));
 {
  bool tolua_ret = (bool)  attack_aura(typ,dam,rad);
 tolua_pushboolean(tolua_S,(bool)tolua_ret);
 }
 }
 return 1;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'attack_aura'.",&tolua_err);
 return 0;
#endif
}

/* function: set_str_boost */
static int tolua_init_set_str_boost00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnumber(tolua_S,1,0,&tolua_err) ||
 !tolua_isnoobj(tolua_S,2,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
  int v = ((int)  tolua_tonumber(tolua_S,1,0));
 {
  bool tolua_ret = (bool)  set_str_boost(v);
 tolua_pushboolean(tolua_S,(bool)tolua_ret);
 }
 }
 return 1;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'set_str_boost'.",&tolua_err);
 return 0;
#endif
}

/* function: set_int_boost */
static int tolua_init_set_int_boost00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnumber(tolua_S,1,0,&tolua_err) ||
 !tolua_isnoobj(tolua_S,2,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
  int v = ((int)  tolua_tonumber(tolua_S,1,0));
 {
  bool tolua_ret = (bool)  set_int_boost(v);
 tolua_pushboolean(tolua_S,(bool)tolua_ret);
 }
 }
 return 1;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'set_int_boost'.",&tolua_err);
 return 0;
#endif
}

/* function: set_wis_boost */
static int tolua_init_set_wis_boost00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnumber(tolua_S,1,0,&tolua_err) ||
 !tolua_isnoobj(tolua_S,2,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
  int v = ((int)  tolua_tonumber(tolua_S,1,0));
 {
  bool tolua_ret = (bool)  set_wis_boost(v);
 tolua_pushboolean(tolua_S,(bool)tolua_ret);
 }
 }
 return 1;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'set_wis_boost'.",&tolua_err);
 return 0;
#endif
}

/* function: set_dex_boost */
static int tolua_init_set_dex_boost00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnumber(tolua_S,1,0,&tolua_err) ||
 !tolua_isnoobj(tolua_S,2,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
  int v = ((int)  tolua_tonumber(tolua_S,1,0));
 {
  bool tolua_ret = (bool)  set_dex_boost(v);
 tolua_pushboolean(tolua_S,(bool)tolua_ret);
 }
 }
 return 1;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'set_dex_boost'.",&tolua_err);
 return 0;
#endif
}

/* function: set_con_boost */
static int tolua_init_set_con_boost00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnumber(tolua_S,1,0,&tolua_err) ||
 !tolua_isnoobj(tolua_S,2,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
  int v = ((int)  tolua_tonumber(tolua_S,1,0));
 {
  bool tolua_ret = (bool)  set_con_boost(v);
 tolua_pushboolean(tolua_S,(bool)tolua_ret);
 }
 }
 return 1;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'set_con_boost'.",&tolua_err);
 return 0;
#endif
}

/* function: set_chr_boost */
static int tolua_init_set_chr_boost00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnumber(tolua_S,1,0,&tolua_err) ||
 !tolua_isnoobj(tolua_S,2,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
  int v = ((int)  tolua_tonumber(tolua_S,1,0));
 {
  bool tolua_ret = (bool)  set_chr_boost(v);
 tolua_pushboolean(tolua_S,(bool)tolua_ret);
 }
 }
 return 1;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'set_chr_boost'.",&tolua_err);
 return 0;
#endif
}

/* function: set_pres */
static int tolua_init_set_pres00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnumber(tolua_S,1,0,&tolua_err) ||
 !tolua_isnoobj(tolua_S,2,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
  int v = ((int)  tolua_tonumber(tolua_S,1,0));
 {
  bool tolua_ret = (bool)  set_pres(v);
 tolua_pushboolean(tolua_S,(bool)tolua_ret);
 }
 }
 return 1;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'set_pres'.",&tolua_err);
 return 0;
#endif
}

/* function: set_mres */
static int tolua_init_set_mres00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnumber(tolua_S,1,0,&tolua_err) ||
 !tolua_isnoobj(tolua_S,2,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
  int v = ((int)  tolua_tonumber(tolua_S,1,0));
 {
  bool tolua_ret = (bool)  set_mres(v);
 tolua_pushboolean(tolua_S,(bool)tolua_ret);
 }
 }
 return 1;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'set_mres'.",&tolua_err);
 return 0;
#endif
}

/* function: set_ac_boost */
static int tolua_init_set_ac_boost00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnumber(tolua_S,1,0,&tolua_err) ||
 !tolua_isnoobj(tolua_S,2,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
  int v = ((int)  tolua_tonumber(tolua_S,1,0));
 {
  bool tolua_ret = (bool)  set_ac_boost(v);
 tolua_pushboolean(tolua_S,(bool)tolua_ret);
 }
 }
 return 1;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'set_ac_boost'.",&tolua_err);
 return 0;
#endif
}

/* function: set_elem_shield */
static int tolua_init_set_elem_shield00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnumber(tolua_S,1,0,&tolua_err) ||
 !tolua_isnoobj(tolua_S,2,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
  int v = ((int)  tolua_tonumber(tolua_S,1,0));
 {
  bool tolua_ret = (bool)  set_elem_shield(v);
 tolua_pushboolean(tolua_S,(bool)tolua_ret);
 }
 }
 return 1;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'set_elem_shield'.",&tolua_err);
 return 0;
#endif
}

/* function: set_powerattack */
static int tolua_init_set_powerattack01(lua_State* tolua_S)
{
 tolua_Error tolua_err;
 if (
 !tolua_isnumber(tolua_S,1,0,&tolua_err) ||
 !tolua_isnoobj(tolua_S,2,&tolua_err)
 )
 goto tolua_lerror;
 else
 {
  int v = ((int)  tolua_tonumber(tolua_S,1,0));
 {
  bool tolua_ret = (bool)  set_powerattack(v);
 tolua_pushboolean(tolua_S,(bool)tolua_ret);
 }
 }
 return 1;
tolua_lerror:
 return tolua_init_set_powerattack00(tolua_S);
}

/* function: set_invis */
static int tolua_init_set_invis00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnumber(tolua_S,1,0,&tolua_err) ||
 !tolua_isnumber(tolua_S,2,0,&tolua_err) ||
 !tolua_isnoobj(tolua_S,3,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
  int v = ((int)  tolua_tonumber(tolua_S,1,0));
  int p = ((int)  tolua_tonumber(tolua_S,2,0));
 {
  bool tolua_ret = (bool)  set_invis(v,p);
 tolua_pushboolean(tolua_S,(bool)tolua_ret);
 }
 }
 return 1;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'set_invis'.",&tolua_err);
 return 0;
#endif
}

/* function: set_blind */
static int tolua_init_set_blind00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnumber(tolua_S,1,0,&tolua_err) ||
 !tolua_isnoobj(tolua_S,2,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
  int v = ((int)  tolua_tonumber(tolua_S,1,0));
 {
  bool tolua_ret = (bool)  set_blind(v);
 tolua_pushboolean(tolua_S,(bool)tolua_ret);
 }
 }
 return 1;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'set_blind'.",&tolua_err);
 return 0;
#endif
}

/* function: set_confused */
static int tolua_init_set_confused00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnumber(tolua_S,1,0,&tolua_err) ||
 !tolua_isnoobj(tolua_S,2,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
  int v = ((int)  tolua_tonumber(tolua_S,1,0));
 {
  bool tolua_ret = (bool)  set_confused(v);
 tolua_pushboolean(tolua_S,(bool)tolua_ret);
 }
 }
 return 1;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'set_confused'.",&tolua_err);
 return 0;
#endif
}

/* function: set_poisoned */
static int tolua_init_set_poisoned00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnumber(tolua_S,1,0,&tolua_err) ||
 !tolua_isnoobj(tolua_S,2,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
  int v = ((int)  tolua_tonumber(tolua_S,1,0));
 {
  bool tolua_ret = (bool)  set_poisoned(v);
 tolua_pushboolean(tolua_S,(bool)tolua_ret);
 }
 }
 return 1;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'set_poisoned'.",&tolua_err);
 return 0;
#endif
}

/* function: set_afraid */
static int tolua_init_set_afraid00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnumber(tolua_S,1,0,&tolua_err) ||
 !tolua_isnoobj(tolua_S,2,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
  int v = ((int)  tolua_tonumber(tolua_S,1,0));
 {
  bool tolua_ret = (bool)  set_afraid(v);
 tolua_pushboolean(tolua_S,(bool)tolua_ret);
 }
 }
 return 1;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'set_afraid'.",&tolua_err);
 return 0;
#endif
}

/* function: set_paralyzed */
static int tolua_init_set_paralyzed00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnumber(tolua_S,1,0,&tolua_err) ||
 !tolua_isnoobj(tolua_S,2,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
  int v = ((int)  tolua_tonumber(tolua_S,1,0));
 {
  bool tolua_ret = (bool)  set_paralyzed(v);
 tolua_pushboolean(tolua_S,(bool)tolua_ret);
 }
 }
 return 1;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'set_paralyzed'.",&tolua_err);
 return 0;
#endif
}

/* function: set_image */
static int tolua_init_set_image00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnumber(tolua_S,1,0,&tolua_err) ||
 !tolua_isnoobj(tolua_S,2,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
  int v = ((int)  tolua_tonumber(tolua_S,1,0));
 {
  bool tolua_ret = (bool)  set_image(v);
 tolua_pushboolean(tolua_S,(bool)tolua_ret);
 }
 }
 return 1;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'set_image'.",&tolua_err);
 return 0;
#endif
}

/* function: set_fast */
static int tolua_init_set_fast00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnumber(tolua_S,1,0,&tolua_err) ||
 !tolua_isnoobj(tolua_S,2,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
  int v = ((int)  tolua_tonumber(tolua_S,1,0));
 {
  bool tolua_ret = (bool)  set_fast(v);
 tolua_pushboolean(tolua_S,(bool)tolua_ret);
 }
 }
 return 1;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'set_fast'.",&tolua_err);
 return 0;
#endif
}

/* function: set_slow */
static int tolua_init_set_slow00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnumber(tolua_S,1,0,&tolua_err) ||
 !tolua_isnoobj(tolua_S,2,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
  int v = ((int)  tolua_tonumber(tolua_S,1,0));
 {
  bool tolua_ret = (bool)  set_slow(v);
 tolua_pushboolean(tolua_S,(bool)tolua_ret);
 }
 }
 return 1;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'set_slow'.",&tolua_err);
 return 0;
#endif
}

/* function: set_shield */
static int tolua_init_set_shield00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnumber(tolua_S,1,0,&tolua_err) ||
 !tolua_isnumber(tolua_S,2,0,&tolua_err) ||
 !tolua_isnoobj(tolua_S,3,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
  int v = ((int)  tolua_tonumber(tolua_S,1,0));
  int p = ((int)  tolua_tonumber(tolua_S,2,0));
 {
  bool tolua_ret = (bool)  set_shield(v,p);
 tolua_pushboolean(tolua_S,(bool)tolua_ret);
 }
 }
 return 1;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'set_shield'.",&tolua_err);
 return 0;
#endif
}

/* function: set_blessed */
static int tolua_init_set_blessed00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnumber(tolua_S,1,0,&tolua_err) ||
 !tolua_isnoobj(tolua_S,2,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
  int v = ((int)  tolua_tonumber(tolua_S,1,0));
 {
  bool tolua_ret = (bool)  set_blessed(v);
 tolua_pushboolean(tolua_S,(bool)tolua_ret);
 }
 }
 return 1;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'set_blessed'.",&tolua_err);
 return 0;
#endif
}

/* function: set_hero */
static int tolua_init_set_hero00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnumber(tolua_S,1,0,&tolua_err) ||
 !tolua_isnoobj(tolua_S,2,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
  int v = ((int)  tolua_tonumber(tolua_S,1,0));
 {
  bool tolua_ret = (bool)  set_hero(v);
 tolua_pushboolean(tolua_S,(bool)tolua_ret);
 }
 }
 return 1;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'set_hero'.",&tolua_err);
 return 0;
#endif
}

/* function: set_shero */
static int tolua_init_set_shero00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnumber(tolua_S,1,0,&tolua_err) ||
 !tolua_isnoobj(tolua_S,2,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
  int v = ((int)  tolua_tonumber(tolua_S,1,0));
 {
  bool tolua_ret = (bool)  set_shero(v);
 tolua_pushboolean(tolua_S,(bool)tolua_ret);
 }
 }
 return 1;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'set_shero'.",&tolua_err);
 return 0;
#endif
}

/* function: set_tim_invis */
static int tolua_init_set_tim_invis00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnumber(tolua_S,1,0,&tolua_err) ||
 !tolua_isnoobj(tolua_S,2,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
  int v = ((int)  tolua_tonumber(tolua_S,1,0));
 {
  bool tolua_ret = (bool)  set_tim_invis(v);
 tolua_pushboolean(tolua_S,(bool)tolua_ret);
 }
 }
 return 1;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'set_tim_invis'.",&tolua_err);
 return 0;
#endif
}

/* function: set_tim_infra */
static int tolua_init_set_tim_infra00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnumber(tolua_S,1,0,&tolua_err) ||
 !tolua_isnoobj(tolua_S,2,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
  int v = ((int)  tolua_tonumber(tolua_S,1,0));
 {
  bool tolua_ret = (bool)  set_tim_infra(v);
 tolua_pushboolean(tolua_S,(bool)tolua_ret);
 }
 }
 return 1;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'set_tim_infra'.",&tolua_err);
 return 0;
#endif
}

/* function: set_stun */
static int tolua_init_set_stun00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnumber(tolua_S,1,0,&tolua_err) ||
 !tolua_isnoobj(tolua_S,2,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
  int v = ((int)  tolua_tonumber(tolua_S,1,0));
 {
  bool tolua_ret = (bool)  set_stun(v);
 tolua_pushboolean(tolua_S,(bool)tolua_ret);
 }
 }
 return 1;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'set_stun'.",&tolua_err);
 return 0;
#endif
}

/* function: set_cut */
static int tolua_init_set_cut00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnumber(tolua_S,1,0,&tolua_err) ||
 !tolua_isnoobj(tolua_S,2,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
  int v = ((int)  tolua_tonumber(tolua_S,1,0));
 {
  bool tolua_ret = (bool)  set_cut(v);
 tolua_pushboolean(tolua_S,(bool)tolua_ret);
 }
 }
 return 1;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'set_cut'.",&tolua_err);
 return 0;
#endif
}

/* function: quark_add */
static int tolua_init_quark_add00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isstring(tolua_S,1,0,&tolua_err) ||
 !tolua_isnoobj(tolua_S,2,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
  cptr str = ((cptr)  tolua_tostring(tolua_S,1,0));
 {
  s16b tolua_ret = (s16b)  quark_add(str);
 tolua_pushnumber(tolua_S,(long)tolua_ret);
 }
 }
 return 1;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'quark_add'.",&tolua_err);
 return 0;
#endif
}

/* function: inven_item_describe */
static int tolua_init_inven_item_describe00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnumber(tolua_S,1,0,&tolua_err) ||
 !tolua_isnoobj(tolua_S,2,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
  int item = ((int)  tolua_tonumber(tolua_S,1,0));
 {
  inven_item_describe(item);
 }
 }
 return 0;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'inven_item_describe'.",&tolua_err);
 return 0;
#endif
}

/* function: inven_item_increase */
static int tolua_init_inven_item_increase00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnumber(tolua_S,1,0,&tolua_err) ||
 !tolua_isnumber(tolua_S,2,0,&tolua_err) ||
 !tolua_isnoobj(tolua_S,3,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
  int item = ((int)  tolua_tonumber(tolua_S,1,0));
  int num = ((int)  tolua_tonumber(tolua_S,2,0));
 {
  inven_item_increase(item,num);
 }
 }
 return 0;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'inven_item_increase'.",&tolua_err);
 return 0;
#endif
}

/* function: inven_item_optimize */
static int tolua_init_inven_item_optimize00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnumber(tolua_S,1,0,&tolua_err) ||
 !tolua_isnoobj(tolua_S,2,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
  int item = ((int)  tolua_tonumber(tolua_S,1,0));
 {
  inven_item_optimize(item);
 }
 }
 return 0;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'inven_item_optimize'.",&tolua_err);
 return 0;
#endif
}

/* function: floor_item_describe */
static int tolua_init_floor_item_describe00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnumber(tolua_S,1,0,&tolua_err) ||
 !tolua_isnoobj(tolua_S,2,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
  int item = ((int)  tolua_tonumber(tolua_S,1,0));
 {
  floor_item_describe(item);
 }
 }
 return 0;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'floor_item_describe'.",&tolua_err);
 return 0;
#endif
}

/* function: floor_item_increase */
static int tolua_init_floor_item_increase00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnumber(tolua_S,1,0,&tolua_err) ||
 !tolua_isnumber(tolua_S,2,0,&tolua_err) ||
 !tolua_isnoobj(tolua_S,3,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
  int item = ((int)  tolua_tonumber(tolua_S,1,0));
  int num = ((int)  tolua_tonumber(tolua_S,2,0));
 {
  floor_item_increase(item,num);
 }
 }
 return 0;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'floor_item_increase'.",&tolua_err);
 return 0;
#endif
}

/* function: floor_item_optimize */
static int tolua_init_floor_item_optimize00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnumber(tolua_S,1,0,&tolua_err) ||
 !tolua_isnoobj(tolua_S,2,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
  int item = ((int)  tolua_tonumber(tolua_S,1,0));
 {
  floor_item_optimize(item);
 }
 }
 return 0;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'floor_item_optimize'.",&tolua_err);
 return 0;
#endif
}

/* function: drop_near */
static int tolua_init_drop_near00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isusertype(tolua_S,1,"object_type",0,&tolua_err) ||
 !tolua_isnumber(tolua_S,2,0,&tolua_err) ||
 !tolua_isnumber(tolua_S,3,0,&tolua_err) ||
 !tolua_isnumber(tolua_S,4,0,&tolua_err) ||
 !tolua_isnoobj(tolua_S,5,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
  object_type* o_ptr = ((object_type*)  tolua_tousertype(tolua_S,1,0));
  int chance = ((int)  tolua_tonumber(tolua_S,2,0));
  int y = ((int)  tolua_tonumber(tolua_S,3,0));
  int x = ((int)  tolua_tonumber(tolua_S,4,0));
 {
  s16b tolua_ret = (s16b)  drop_near(o_ptr,chance,y,x);
 tolua_pushnumber(tolua_S,(long)tolua_ret);
 }
 }
 return 1;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'drop_near'.",&tolua_err);
 return 0;
#endif
}

/* function: drop_near_ammo */
static int tolua_init_drop_near_ammo00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isusertype(tolua_S,1,"object_type",0,&tolua_err) ||
 !tolua_isnumber(tolua_S,2,0,&tolua_err) ||
 !tolua_isnumber(tolua_S,3,0,&tolua_err) ||
 !tolua_isnumber(tolua_S,4,0,&tolua_err) ||
 !tolua_isnoobj(tolua_S,5,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
  object_type* o_ptr = ((object_type*)  tolua_tousertype(tolua_S,1,0));
  int number = ((int)  tolua_tonumber(tolua_S,2,0));
  int y = ((int)  tolua_tonumber(tolua_S,3,0));
  int x = ((int)  tolua_tonumber(tolua_S,4,0));
 {
  s16b tolua_ret = (s16b)  drop_near_ammo(o_ptr,number,y,x);
 tolua_pushnumber(tolua_S,(long)tolua_ret);
 }
 }
 return 1;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'drop_near_ammo'.",&tolua_err);
 return 0;
#endif
}

/* function: reveal_spell */
static int tolua_init_reveal_spell00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnumber(tolua_S,1,0,&tolua_err) ||
 !tolua_isnumber(tolua_S,2,0,&tolua_err) ||
 !tolua_isnumber(tolua_S,3,0,&tolua_err) ||
 !tolua_isnoobj(tolua_S,4,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
  int x = ((int)  tolua_tonumber(tolua_S,1,0));
  int y = ((int)  tolua_tonumber(tolua_S,2,0));
  byte rad = ((byte)  tolua_tonumber(tolua_S,3,0));
 {
  reveal_spell(x,y,rad);
 }
 }
 return 0;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'reveal_spell'.",&tolua_err);
 return 0;
#endif
}

/* function: show_file */
static int tolua_init_show_file00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isstring(tolua_S,1,0,&tolua_err) ||
 !tolua_isstring(tolua_S,2,0,&tolua_err) ||
 !tolua_isnumber(tolua_S,3,0,&tolua_err) ||
 !tolua_isnumber(tolua_S,4,0,&tolua_err) ||
 !tolua_isnoobj(tolua_S,5,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
  cptr name = ((cptr)  tolua_tostring(tolua_S,1,0));
  cptr what = ((cptr)  tolua_tostring(tolua_S,2,0));
  int line = ((int)  tolua_tonumber(tolua_S,3,0));
  int mode = ((int)  tolua_tonumber(tolua_S,4,0));
 {
  bool tolua_ret = (bool)  show_file(name,what,line,mode);
 tolua_pushboolean(tolua_S,(bool)tolua_ret);
 }
 }
 return 1;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'show_file'.",&tolua_err);
 return 0;
#endif
}

/* function: Term_fresh */
static int tolua_init_Term_fresh00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnoobj(tolua_S,1,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
 {
  errr tolua_ret = (errr)  Term_fresh();
 tolua_pushnumber(tolua_S,(long)tolua_ret);
 }
 }
 return 1;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'Term_fresh'.",&tolua_err);
 return 0;
#endif
}

/* function: Term_set_cursor */
static int tolua_init_Term_set_cursor00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnumber(tolua_S,1,0,&tolua_err) ||
 !tolua_isnoobj(tolua_S,2,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
  int v = ((int)  tolua_tonumber(tolua_S,1,0));
 {
  errr tolua_ret = (errr)  Term_set_cursor(v);
 tolua_pushnumber(tolua_S,(long)tolua_ret);
 }
 }
 return 1;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'Term_set_cursor'.",&tolua_err);
 return 0;
#endif
}

/* function: Term_gotoxy */
static int tolua_init_Term_gotoxy00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnumber(tolua_S,1,0,&tolua_err) ||
 !tolua_isnumber(tolua_S,2,0,&tolua_err) ||
 !tolua_isnoobj(tolua_S,3,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
  int x = ((int)  tolua_tonumber(tolua_S,1,0));
  int y = ((int)  tolua_tonumber(tolua_S,2,0));
 {
  errr tolua_ret = (errr)  Term_gotoxy(x,y);
 tolua_pushnumber(tolua_S,(long)tolua_ret);
 }
 }
 return 1;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'Term_gotoxy'.",&tolua_err);
 return 0;
#endif
}

/* function: Term_draw */
static int tolua_init_Term_draw00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnumber(tolua_S,1,0,&tolua_err) ||
 !tolua_isnumber(tolua_S,2,0,&tolua_err) ||
 !tolua_isnumber(tolua_S,3,0,&tolua_err) ||
 !tolua_isnumber(tolua_S,4,0,&tolua_err) ||
 !tolua_isnoobj(tolua_S,5,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
  int x = ((int)  tolua_tonumber(tolua_S,1,0));
  int y = ((int)  tolua_tonumber(tolua_S,2,0));
  byte a = ((byte)  tolua_tonumber(tolua_S,3,0));
  char c = ((char)  tolua_tonumber(tolua_S,4,0));
 {
  errr tolua_ret = (errr)  Term_draw(x,y,a,c);
 tolua_pushnumber(tolua_S,(long)tolua_ret);
 }
 }
 return 1;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'Term_draw'.",&tolua_err);
 return 0;
#endif
}

/* function: Term_addch */
static int tolua_init_Term_addch00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnumber(tolua_S,1,0,&tolua_err) ||
 !tolua_isnumber(tolua_S,2,0,&tolua_err) ||
 !tolua_isnoobj(tolua_S,3,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
  byte a = ((byte)  tolua_tonumber(tolua_S,1,0));
  char c = ((char)  tolua_tonumber(tolua_S,2,0));
 {
  errr tolua_ret = (errr)  Term_addch(a,c);
 tolua_pushnumber(tolua_S,(long)tolua_ret);
 }
 }
 return 1;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'Term_addch'.",&tolua_err);
 return 0;
#endif
}

/* function: Term_addstr */
static int tolua_init_Term_addstr00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnumber(tolua_S,1,0,&tolua_err) ||
 !tolua_isnumber(tolua_S,2,0,&tolua_err) ||
 !tolua_isstring(tolua_S,3,0,&tolua_err) ||
 !tolua_isnoobj(tolua_S,4,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
  int n = ((int)  tolua_tonumber(tolua_S,1,0));
  byte a = ((byte)  tolua_tonumber(tolua_S,2,0));
  cptr s = ((cptr)  tolua_tostring(tolua_S,3,0));
 {
  errr tolua_ret = (errr)  Term_addstr(n,a,s);
 tolua_pushnumber(tolua_S,(long)tolua_ret);
 }
 }
 return 1;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'Term_addstr'.",&tolua_err);
 return 0;
#endif
}

/* function: Term_putch */
static int tolua_init_Term_putch00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnumber(tolua_S,1,0,&tolua_err) ||
 !tolua_isnumber(tolua_S,2,0,&tolua_err) ||
 !tolua_isnumber(tolua_S,3,0,&tolua_err) ||
 !tolua_isnumber(tolua_S,4,0,&tolua_err) ||
 !tolua_isnoobj(tolua_S,5,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
  int x = ((int)  tolua_tonumber(tolua_S,1,0));
  int y = ((int)  tolua_tonumber(tolua_S,2,0));
  byte a = ((byte)  tolua_tonumber(tolua_S,3,0));
  char c = ((char)  tolua_tonumber(tolua_S,4,0));
 {
  errr tolua_ret = (errr)  Term_putch(x,y,a,c);
 tolua_pushnumber(tolua_S,(long)tolua_ret);
 }
 }
 return 1;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'Term_putch'.",&tolua_err);
 return 0;
#endif
}

/* function: Term_putstr */
static int tolua_init_Term_putstr00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnumber(tolua_S,1,0,&tolua_err) ||
 !tolua_isnumber(tolua_S,2,0,&tolua_err) ||
 !tolua_isnumber(tolua_S,3,0,&tolua_err) ||
 !tolua_isnumber(tolua_S,4,0,&tolua_err) ||
 !tolua_isstring(tolua_S,5,0,&tolua_err) ||
 !tolua_isnoobj(tolua_S,6,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
  int x = ((int)  tolua_tonumber(tolua_S,1,0));
  int y = ((int)  tolua_tonumber(tolua_S,2,0));
  int n = ((int)  tolua_tonumber(tolua_S,3,0));
  byte a = ((byte)  tolua_tonumber(tolua_S,4,0));
  cptr s = ((cptr)  tolua_tostring(tolua_S,5,0));
 {
  errr tolua_ret = (errr)  Term_putstr(x,y,n,a,s);
 tolua_pushnumber(tolua_S,(long)tolua_ret);
 }
 }
 return 1;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'Term_putstr'.",&tolua_err);
 return 0;
#endif
}

/* function: Term_erase */
static int tolua_init_Term_erase00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnumber(tolua_S,1,0,&tolua_err) ||
 !tolua_isnumber(tolua_S,2,0,&tolua_err) ||
 !tolua_isnumber(tolua_S,3,0,&tolua_err) ||
 !tolua_isnoobj(tolua_S,4,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
  int x = ((int)  tolua_tonumber(tolua_S,1,0));
  int y = ((int)  tolua_tonumber(tolua_S,2,0));
  int n = ((int)  tolua_tonumber(tolua_S,3,0));
 {
  errr tolua_ret = (errr)  Term_erase(x,y,n);
 tolua_pushnumber(tolua_S,(long)tolua_ret);
 }
 }
 return 1;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'Term_erase'.",&tolua_err);
 return 0;
#endif
}

/* function: Term_clear */
static int tolua_init_Term_clear01(lua_State* tolua_S)
{
 tolua_Error tolua_err;
 if (
 !tolua_isnoobj(tolua_S,1,&tolua_err)
 )
 goto tolua_lerror;
 else
 {
 {
  errr tolua_ret = (errr)  Term_clear();
 tolua_pushnumber(tolua_S,(long)tolua_ret);
 }
 }
 return 1;
tolua_lerror:
 return tolua_init_Term_clear00(tolua_S);
}

/* function: Term_redraw */
static int tolua_init_Term_redraw00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnoobj(tolua_S,1,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
 {
  errr tolua_ret = (errr)  Term_redraw();
 tolua_pushnumber(tolua_S,(long)tolua_ret);
 }
 }
 return 1;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'Term_redraw'.",&tolua_err);
 return 0;
#endif
}

/* function: Term_get_cursor */
static int tolua_init_Term_get_cursor00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnumber(tolua_S,1,0,&tolua_err) ||
 !tolua_isnoobj(tolua_S,2,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
  int v = ((int)  tolua_tonumber(tolua_S,1,0));
 {
  errr tolua_ret = (errr)  Term_get_cursor(&v);
 tolua_pushnumber(tolua_S,(long)tolua_ret);
 tolua_pushnumber(tolua_S,(long)v);
 }
 }
 return 2;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'Term_get_cursor'.",&tolua_err);
 return 0;
#endif
}

/* function: Term_get_size */
static int tolua_init_Term_get_size00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnumber(tolua_S,1,0,&tolua_err) ||
 !tolua_isnumber(tolua_S,2,0,&tolua_err) ||
 !tolua_isnoobj(tolua_S,3,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
  int w = ((int)  tolua_tonumber(tolua_S,1,0));
  int h = ((int)  tolua_tonumber(tolua_S,2,0));
 {
  errr tolua_ret = (errr)  Term_get_size(&w,&h);
 tolua_pushnumber(tolua_S,(long)tolua_ret);
 tolua_pushnumber(tolua_S,(long)w);
 tolua_pushnumber(tolua_S,(long)h);
 }
 }
 return 3;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'Term_get_size'.",&tolua_err);
 return 0;
#endif
}

/* function: Term_locate */
static int tolua_init_Term_locate00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnumber(tolua_S,1,0,&tolua_err) ||
 !tolua_isnumber(tolua_S,2,0,&tolua_err) ||
 !tolua_isnoobj(tolua_S,3,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
  int x = ((int)  tolua_tonumber(tolua_S,1,0));
  int y = ((int)  tolua_tonumber(tolua_S,2,0));
 {
  errr tolua_ret = (errr)  Term_locate(&x,&y);
 tolua_pushnumber(tolua_S,(long)tolua_ret);
 tolua_pushnumber(tolua_S,(long)x);
 tolua_pushnumber(tolua_S,(long)y);
 }
 }
 return 3;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'Term_locate'.",&tolua_err);
 return 0;
#endif
}

/* function: Term_what */
static int tolua_init_Term_what00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnumber(tolua_S,1,0,&tolua_err) ||
 !tolua_isnumber(tolua_S,2,0,&tolua_err) ||
 !tolua_isnumber(tolua_S,3,0,&tolua_err) ||
 !tolua_isstring(tolua_S,4,0,&tolua_err) ||
 !tolua_isnoobj(tolua_S,5,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
  int x = ((int)  tolua_tonumber(tolua_S,1,0));
  int y = ((int)  tolua_tonumber(tolua_S,2,0));
  byte a = ((byte)  tolua_tonumber(tolua_S,3,0));
  char* c = ((char*)  tolua_tostring(tolua_S,4,0));
 {
  errr tolua_ret = (errr)  Term_what(x,y,&a,c);
 tolua_pushnumber(tolua_S,(long)tolua_ret);
 tolua_pushnumber(tolua_S,(long)a);
 }
 }
 return 2;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'Term_what'.",&tolua_err);
 return 0;
#endif
}

/* function: Term_flush */
static int tolua_init_Term_flush00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnoobj(tolua_S,1,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
 {
  errr tolua_ret = (errr)  Term_flush();
 tolua_pushnumber(tolua_S,(long)tolua_ret);
 }
 }
 return 1;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'Term_flush'.",&tolua_err);
 return 0;
#endif
}

/* function: Term_keypress */
static int tolua_init_Term_keypress00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnumber(tolua_S,1,0,&tolua_err) ||
 !tolua_isnoobj(tolua_S,2,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
  int k = ((int)  tolua_tonumber(tolua_S,1,0));
 {
  errr tolua_ret = (errr)  Term_keypress(k);
 tolua_pushnumber(tolua_S,(long)tolua_ret);
 }
 }
 return 1;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'Term_keypress'.",&tolua_err);
 return 0;
#endif
}

/* function: Term_key_push */
static int tolua_init_Term_key_push00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnumber(tolua_S,1,0,&tolua_err) ||
 !tolua_isnoobj(tolua_S,2,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
  int k = ((int)  tolua_tonumber(tolua_S,1,0));
 {
  errr tolua_ret = (errr)  Term_key_push(k);
 tolua_pushnumber(tolua_S,(long)tolua_ret);
 }
 }
 return 1;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'Term_key_push'.",&tolua_err);
 return 0;
#endif
}

/* function: Term_inkey */
static int tolua_init_Term_inkey00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isstring(tolua_S,1,0,&tolua_err) ||
 !tolua_isboolean(tolua_S,2,0,&tolua_err) ||
 !tolua_isboolean(tolua_S,3,0,&tolua_err) ||
 !tolua_isnoobj(tolua_S,4,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
  char* ch = ((char*)  tolua_tostring(tolua_S,1,0));
  bool wait = ((bool)  tolua_toboolean(tolua_S,2,0));
  bool take = ((bool)  tolua_toboolean(tolua_S,3,0));
 {
  errr tolua_ret = (errr)  Term_inkey(ch,wait,take);
 tolua_pushnumber(tolua_S,(long)tolua_ret);
 }
 }
 return 1;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'Term_inkey'.",&tolua_err);
 return 0;
#endif
}

/* function: Term_save */
static int tolua_init_Term_save00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnoobj(tolua_S,1,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
 {
  errr tolua_ret = (errr)  Term_save();
 tolua_pushnumber(tolua_S,(long)tolua_ret);
 }
 }
 return 1;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'Term_save'.",&tolua_err);
 return 0;
#endif
}

/* function: Term_load */
static int tolua_init_Term_load00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnoobj(tolua_S,1,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
 {
  errr tolua_ret = (errr)  Term_load();
 tolua_pushnumber(tolua_S,(long)tolua_ret);
 }
 }
 return 1;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'Term_load'.",&tolua_err);
 return 0;
#endif
}

/* function: Term_exchange */
static int tolua_init_Term_exchange00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnoobj(tolua_S,1,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
 {
  errr tolua_ret = (errr)  Term_exchange();
 tolua_pushnumber(tolua_S,(long)tolua_ret);
 }
 }
 return 1;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'Term_exchange'.",&tolua_err);
 return 0;
#endif
}

/* function: Term_resize */
static int tolua_init_Term_resize00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnumber(tolua_S,1,0,&tolua_err) ||
 !tolua_isnumber(tolua_S,2,0,&tolua_err) ||
 !tolua_isnoobj(tolua_S,3,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
  int w = ((int)  tolua_tonumber(tolua_S,1,0));
  int h = ((int)  tolua_tonumber(tolua_S,2,0));
 {
  errr tolua_ret = (errr)  Term_resize(w,h);
 tolua_pushnumber(tolua_S,(long)tolua_ret);
 }
 }
 return 1;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'Term_resize'.",&tolua_err);
 return 0;
#endif
}

/* function: Term_activate */
static int tolua_init_Term_activate00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isusertype(tolua_S,1,"term",0,&tolua_err) ||
 !tolua_isnoobj(tolua_S,2,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
  term* t = ((term*)  tolua_tousertype(tolua_S,1,0));
 {
  errr tolua_ret = (errr)  Term_activate(t);
 tolua_pushnumber(tolua_S,(long)tolua_ret);
 }
 }
 return 1;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'Term_activate'.",&tolua_err);
 return 0;
#endif
}

/* function: term_nuke */
static int tolua_init_term_nuke00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isusertype(tolua_S,1,"term",0,&tolua_err) ||
 !tolua_isnoobj(tolua_S,2,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
  term* t = ((term*)  tolua_tousertype(tolua_S,1,0));
 {
  errr tolua_ret = (errr)  term_nuke(t);
 tolua_pushnumber(tolua_S,(long)tolua_ret);
 }
 }
 return 1;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'term_nuke'.",&tolua_err);
 return 0;
#endif
}

/* function: term_init */
static int tolua_init_term_init00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isusertype(tolua_S,1,"term",0,&tolua_err) ||
 !tolua_isnumber(tolua_S,2,0,&tolua_err) ||
 !tolua_isnumber(tolua_S,3,0,&tolua_err) ||
 !tolua_isnumber(tolua_S,4,0,&tolua_err) ||
 !tolua_isnoobj(tolua_S,5,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
  term* t = ((term*)  tolua_tousertype(tolua_S,1,0));
  int w = ((int)  tolua_tonumber(tolua_S,2,0));
  int h = ((int)  tolua_tonumber(tolua_S,3,0));
  int k = ((int)  tolua_tonumber(tolua_S,4,0));
 {
  errr tolua_ret = (errr)  term_init(t,w,h,k);
 tolua_pushnumber(tolua_S,(long)tolua_ret);
 }
 }
 return 1;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'term_init'.",&tolua_err);
 return 0;
#endif
}

/* function: delete_object_idx */
static int tolua_init_delete_object_idx00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnumber(tolua_S,1,0,&tolua_err) ||
 !tolua_isnoobj(tolua_S,2,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
  int o_idx = ((int)  tolua_tonumber(tolua_S,1,0));
 {
  delete_object_idx(o_idx);
 }
 }
 return 0;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'delete_object_idx'.",&tolua_err);
 return 0;
#endif
}

/* function: delete_object */
static int tolua_init_delete_object00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
 !tolua_isnumber(tolua_S,1,0,&tolua_err) ||
 !tolua_isnumber(tolua_S,2,0,&tolua_err) ||
 !tolua_isnoobj(tolua_S,3,&tolua_err)
 )
 goto tolua_lerror;
 else
#endif
 {
  int y = ((int)  tolua_tonumber(tolua_S,1,0));
  int x = ((int)  tolua_tonumber(tolua_S,2,0));
 {
  delete_object(y,x);
 }
 }
 return 0;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'delete_object'.",&tolua_err);
 return 0;
#endif
}

/* get function: classes_def */
static int tolua_get_init_classes_def(lua_State* tolua_S)
{
 int tolua_index;
#ifndef TOLUA_RELEASE
 {
 tolua_Error tolua_err;
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in array indexing.",&tolua_err);
 }
#endif
 tolua_index = (int)tolua_tonumber(tolua_S,2,0)-1;
#ifndef TOLUA_RELEASE
 if (tolua_index<0 || tolua_index>=50)
 tolua_error(tolua_S,"array indexing out of range.",NULL);
#endif
 tolua_pushusertype(tolua_S,(void*)&classes_def[tolua_index],"class_def");
 return 1;
}

/* set function: classes_def */
static int tolua_set_init_classes_def(lua_State* tolua_S)
{
 int tolua_index;
#ifndef TOLUA_RELEASE
 {
 tolua_Error tolua_err;
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in array indexing.",&tolua_err);
 }
#endif
 tolua_index = (int)tolua_tonumber(tolua_S,2,0)-1;
#ifndef TOLUA_RELEASE
 if (tolua_index<0 || tolua_index>=50)
 tolua_error(tolua_S,"array indexing out of range.",NULL);
#endif
  classes_def[tolua_index] = *((class_def*)  tolua_tousertype(tolua_S,3,0));
 return 0;
}

/* get function: abilities_def */
static int tolua_get_init_abilities_def(lua_State* tolua_S)
{
 int tolua_index;
#ifndef TOLUA_RELEASE
 {
 tolua_Error tolua_err;
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in array indexing.",&tolua_err);
 }
#endif
 tolua_index = (int)tolua_tonumber(tolua_S,2,0)-1;
#ifndef TOLUA_RELEASE
 if (tolua_index<0 || tolua_index>=500)
 tolua_error(tolua_S,"array indexing out of range.",NULL);
#endif
 tolua_pushusertype(tolua_S,(void*)&abilities_def[tolua_index],"ability_def");
 return 1;
}

/* set function: abilities_def */
static int tolua_set_init_abilities_def(lua_State* tolua_S)
{
 int tolua_index;
#ifndef TOLUA_RELEASE
 {
 tolua_Error tolua_err;
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in array indexing.",&tolua_err);
 }
#endif
 tolua_index = (int)tolua_tonumber(tolua_S,2,0)-1;
#ifndef TOLUA_RELEASE
 if (tolua_index<0 || tolua_index>=500)
 tolua_error(tolua_S,"array indexing out of range.",NULL);
#endif
  abilities_def[tolua_index] = *((ability_def*)  tolua_tousertype(tolua_S,3,0));
 return 0;
}

/* get function: feats_def */
static int tolua_get_init_feats_def(lua_State* tolua_S)
{
 int tolua_index;
#ifndef TOLUA_RELEASE
 {
 tolua_Error tolua_err;
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in array indexing.",&tolua_err);
 }
#endif
 tolua_index = (int)tolua_tonumber(tolua_S,2,0)-1;
#ifndef TOLUA_RELEASE
 if (tolua_index<0 || tolua_index>=1000)
 tolua_error(tolua_S,"array indexing out of range.",NULL);
#endif
 tolua_pushusertype(tolua_S,(void*)&feats_def[tolua_index],"ability_def");
 return 1;
}

/* set function: feats_def */
static int tolua_set_init_feats_def(lua_State* tolua_S)
{
 int tolua_index;
#ifndef TOLUA_RELEASE
 {
 tolua_Error tolua_err;
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in array indexing.",&tolua_err);
 }
#endif
 tolua_index = (int)tolua_tonumber(tolua_S,2,0)-1;
#ifndef TOLUA_RELEASE
 if (tolua_index<0 || tolua_index>=1000)
 tolua_error(tolua_S,"array indexing out of range.",NULL);
#endif
  feats_def[tolua_index] = *((ability_def*)  tolua_tousertype(tolua_S,3,0));
 return 0;
}

/* get function: vaults_def */
static int tolua_get_init_vaults_def(lua_State* tolua_S)
{
 int tolua_index;
#ifndef TOLUA_RELEASE
 {
 tolua_Error tolua_err;
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in array indexing.",&tolua_err);
 }
#endif
 tolua_index = (int)tolua_tonumber(tolua_S,2,0)-1;
#ifndef TOLUA_RELEASE
 if (tolua_index<0 || tolua_index>=200)
 tolua_error(tolua_S,"array indexing out of range.",NULL);
#endif
 tolua_pushusertype(tolua_S,(void*)&vaults_def[tolua_index],"vault_def");
 return 1;
}

/* set function: vaults_def */
static int tolua_set_init_vaults_def(lua_State* tolua_S)
{
 int tolua_index;
#ifndef TOLUA_RELEASE
 {
 tolua_Error tolua_err;
 if (!tolua_isnumber(tolua_S,2,0,&tolua_err))
 tolua_error(tolua_S,"#vinvalid type in array indexing.",&tolua_err);
 }
#endif
 tolua_index = (int)tolua_tonumber(tolua_S,2,0)-1;
#ifndef TOLUA_RELEASE
 if (tolua_index<0 || tolua_index>=200)
 tolua_error(tolua_S,"array indexing out of range.",NULL);
#endif
  vaults_def[tolua_index] = *((vault_def*)  tolua_tousertype(tolua_S,3,0));
 return 0;
}

/* Open function */
TOLUA_API int tolua_init_open (lua_State* tolua_S)
{
 tolua_open(tolua_S);
 tolua_reg_types(tolua_S);
 tolua_module(tolua_S,NULL,1);
 tolua_beginmodule(tolua_S,NULL);
 tolua_constant(tolua_S,"TERM_DARK",TERM_DARK);
 tolua_constant(tolua_S,"TERM_WHITE",TERM_WHITE);
 tolua_constant(tolua_S,"TERM_SLATE",TERM_SLATE);
 tolua_constant(tolua_S,"TERM_ORANGE",TERM_ORANGE);
 tolua_constant(tolua_S,"TERM_RED",TERM_RED);
 tolua_constant(tolua_S,"TERM_GREEN",TERM_GREEN);
 tolua_constant(tolua_S,"TERM_BLUE",TERM_BLUE);
 tolua_constant(tolua_S,"TERM_UMBER",TERM_UMBER);
 tolua_constant(tolua_S,"TERM_L_DARK",TERM_L_DARK);
 tolua_constant(tolua_S,"TERM_L_WHITE",TERM_L_WHITE);
 tolua_constant(tolua_S,"TERM_VIOLET",TERM_VIOLET);
 tolua_constant(tolua_S,"TERM_YELLOW",TERM_YELLOW);
 tolua_constant(tolua_S,"TERM_L_RED",TERM_L_RED);
 tolua_constant(tolua_S,"TERM_L_GREEN",TERM_L_GREEN);
 tolua_constant(tolua_S,"TERM_L_BLUE",TERM_L_BLUE);
 tolua_constant(tolua_S,"TERM_L_UMBER",TERM_L_UMBER);
 tolua_constant(tolua_S,"TERM_INDIGO",TERM_INDIGO);
 tolua_constant(tolua_S,"CAVE_MARK",CAVE_MARK);
 tolua_constant(tolua_S,"CAVE_GLOW",CAVE_GLOW);
 tolua_constant(tolua_S,"CAVE_ICKY",CAVE_ICKY);
 tolua_constant(tolua_S,"CAVE_ROOM",CAVE_ROOM);
 tolua_constant(tolua_S,"CAVE_LITE",CAVE_LITE);
 tolua_constant(tolua_S,"CAVE_VIEW",CAVE_VIEW);
 tolua_constant(tolua_S,"CAVE_TEMP",CAVE_TEMP);
 tolua_constant(tolua_S,"CAVE_XTRA",CAVE_XTRA);
 tolua_constant(tolua_S,"CAVE_TRDT",CAVE_TRDT);
 tolua_constant(tolua_S,"INVEN_PACK",INVEN_PACK);
 tolua_constant(tolua_S,"BODY_WEAPON",BODY_WEAPON);
 tolua_constant(tolua_S,"BODY_TORSO",BODY_TORSO);
 tolua_constant(tolua_S,"BODY_ARMS",BODY_ARMS);
 tolua_constant(tolua_S,"BODY_FINGER",BODY_FINGER);
 tolua_constant(tolua_S,"BODY_HEAD",BODY_HEAD);
 tolua_constant(tolua_S,"BODY_LEGS",BODY_LEGS);
 tolua_constant(tolua_S,"BODY_MAX",BODY_MAX);
 tolua_constant(tolua_S,"INVEN_WIELD",INVEN_WIELD);
 tolua_constant(tolua_S,"INVEN_BOW",INVEN_BOW);
 tolua_constant(tolua_S,"INVEN_RING",INVEN_RING);
 tolua_constant(tolua_S,"INVEN_NECK",INVEN_NECK);
 tolua_constant(tolua_S,"INVEN_LITE",INVEN_LITE);
 tolua_constant(tolua_S,"INVEN_BODY",INVEN_BODY);
 tolua_constant(tolua_S,"INVEN_OUTER",INVEN_OUTER);
 tolua_constant(tolua_S,"INVEN_ARM",INVEN_ARM);
 tolua_constant(tolua_S,"INVEN_HEAD",INVEN_HEAD);
 tolua_constant(tolua_S,"INVEN_HANDS",INVEN_HANDS);
 tolua_constant(tolua_S,"INVEN_FEET",INVEN_FEET);
 tolua_constant(tolua_S,"INVEN_AMMO",INVEN_AMMO);
 tolua_constant(tolua_S,"INVEN_TOOL",INVEN_TOOL);
 tolua_constant(tolua_S,"INVEN_ESSENCE",INVEN_ESSENCE);
 tolua_constant(tolua_S,"INVEN_TOTAL",INVEN_TOTAL);
 tolua_constant(tolua_S,"TV_SKELETON",TV_SKELETON);
 tolua_constant(tolua_S,"TV_BOTTLE",TV_BOTTLE);
 tolua_constant(tolua_S,"TV_BATERIE",TV_BATERIE);
 tolua_constant(tolua_S,"TV_SPIKE",TV_SPIKE);
 tolua_constant(tolua_S,"TV_MSTAFF",TV_MSTAFF);
 tolua_constant(tolua_S,"TV_CHEST",TV_CHEST);
 tolua_constant(tolua_S,"TV_PARCHEMENT",TV_PARCHEMENT);
 tolua_constant(tolua_S,"TV_CORPSE",TV_CORPSE);
 tolua_constant(tolua_S,"TV_EGG",TV_EGG);
 tolua_constant(tolua_S,"TV_JUNK",TV_JUNK);
 tolua_constant(tolua_S,"TV_TOOL",TV_TOOL);
 tolua_constant(tolua_S,"TV_INSTRUMENT",TV_INSTRUMENT);
 tolua_constant(tolua_S,"TV_THROWING",TV_THROWING);
 tolua_constant(tolua_S,"TV_AMMO",TV_AMMO);
 tolua_constant(tolua_S,"TV_RANGED",TV_RANGED);
 tolua_constant(tolua_S,"TV_DIGGING",TV_DIGGING);
 tolua_constant(tolua_S,"TV_WEAPON",TV_WEAPON);
 tolua_constant(tolua_S,"TV_BOOTS",TV_BOOTS);
 tolua_constant(tolua_S,"TV_GLOVES",TV_GLOVES);
 tolua_constant(tolua_S,"TV_HELM",TV_HELM);
 tolua_constant(tolua_S,"TV_CROWN",TV_CROWN);
 tolua_constant(tolua_S,"TV_SHIELD",TV_SHIELD);
 tolua_constant(tolua_S,"TV_CLOAK",TV_CLOAK);
 tolua_constant(tolua_S,"TV_SOFT_ARMOR",TV_SOFT_ARMOR);
 tolua_constant(tolua_S,"TV_HARD_ARMOR",TV_HARD_ARMOR);
 tolua_constant(tolua_S,"TV_DRAG_ARMOR",TV_DRAG_ARMOR);
 tolua_constant(tolua_S,"TV_LITE",TV_LITE);
 tolua_constant(tolua_S,"TV_AMULET",TV_AMULET);
 tolua_constant(tolua_S,"TV_ARM_BAND",TV_ARM_BAND);
 tolua_constant(tolua_S,"TV_RING",TV_RING);
 tolua_constant(tolua_S,"TV_STAFF",TV_STAFF);
 tolua_constant(tolua_S,"TV_WAND",TV_WAND);
 tolua_constant(tolua_S,"TV_ROD",TV_ROD);
 tolua_constant(tolua_S,"TV_SOUL",TV_SOUL);
 tolua_constant(tolua_S,"TV_ESSENCE",TV_ESSENCE);
 tolua_constant(tolua_S,"TV_SCROLL",TV_SCROLL);
 tolua_constant(tolua_S,"TV_POTION",TV_POTION);
 tolua_constant(tolua_S,"TV_FLASK",TV_FLASK);
 tolua_constant(tolua_S,"TV_LICIALHYD",TV_LICIALHYD);
 tolua_constant(tolua_S,"TV_BOOK_ELEMENTAL",TV_BOOK_ELEMENTAL);
 tolua_constant(tolua_S,"TV_BOOK_ALTERATION",TV_BOOK_ALTERATION);
 tolua_constant(tolua_S,"TV_BOOK_MYSTICISM",TV_BOOK_MYSTICISM);
 tolua_constant(tolua_S,"TV_BOOK_CONJURATION",TV_BOOK_CONJURATION);
 tolua_constant(tolua_S,"TV_BOOK_DIVINATION",TV_BOOK_DIVINATION);
 tolua_constant(tolua_S,"TV_RHYTHM",TV_RHYTHM);
 tolua_constant(tolua_S,"TV_HARMONY",TV_HARMONY);
 tolua_constant(tolua_S,"TV_MELODY",TV_MELODY);
 tolua_constant(tolua_S,"TV_HYPNOS",TV_HYPNOS);
 tolua_constant(tolua_S,"TV_GOLD",TV_GOLD);
 tolua_constant(tolua_S,"TV_RANDART",TV_RANDART);
 tolua_constant(tolua_S,"TV_CRYSTAL",TV_CRYSTAL);
 tolua_constant(tolua_S,"GF_FIRE",GF_FIRE);
 tolua_constant(tolua_S,"GF_COLD",GF_COLD);
 tolua_constant(tolua_S,"GF_ELEC",GF_ELEC);
 tolua_constant(tolua_S,"GF_ACID",GF_ACID);
 tolua_constant(tolua_S,"GF_POIS",GF_POIS);
 tolua_constant(tolua_S,"GF_LITE",GF_LITE);
 tolua_constant(tolua_S,"GF_DARK",GF_DARK);
 tolua_constant(tolua_S,"GF_WARP",GF_WARP);
 tolua_constant(tolua_S,"GF_WATER",GF_WATER);
 tolua_constant(tolua_S,"GF_WIND",GF_WIND);
 tolua_constant(tolua_S,"GF_EARTH",GF_EARTH);
 tolua_constant(tolua_S,"GF_SOUND",GF_SOUND);
 tolua_constant(tolua_S,"GF_RADIO",GF_RADIO);
 tolua_constant(tolua_S,"GF_CHAOS",GF_CHAOS);
 tolua_constant(tolua_S,"GF_PHYSICAL",GF_PHYSICAL);
 tolua_constant(tolua_S,"GF_MANA",GF_MANA);
 tolua_constant(tolua_S,"GF_MISSILE",GF_MISSILE);
 tolua_constant(tolua_S,"GF_FROSTFIRE",GF_FROSTFIRE);
 tolua_constant(tolua_S,"GF_GREY",GF_GREY);
 tolua_constant(tolua_S,"GF_TOXIC",GF_TOXIC);
 tolua_constant(tolua_S,"GF_MUD",GF_MUD);
 tolua_constant(tolua_S,"GF_CONFUSION",GF_CONFUSION);
 tolua_constant(tolua_S,"GF_ICE",GF_ICE);
 tolua_constant(tolua_S,"GF_KILL_WALL",GF_KILL_WALL);
 tolua_constant(tolua_S,"GF_KILL_DOOR",GF_KILL_DOOR);
 tolua_constant(tolua_S,"GF_KILL_TRAP",GF_KILL_TRAP);
 tolua_constant(tolua_S,"GF_MAKE_WALL",GF_MAKE_WALL);
 tolua_constant(tolua_S,"GF_MAKE_DOOR",GF_MAKE_DOOR);
 tolua_constant(tolua_S,"GF_MAKE_TRAP",GF_MAKE_TRAP);
 tolua_constant(tolua_S,"GF_STONE_TO_MUD",GF_STONE_TO_MUD);
 tolua_constant(tolua_S,"GF_OLD_CLONE",GF_OLD_CLONE);
 tolua_constant(tolua_S,"GF_OLD_POLY",GF_OLD_POLY);
 tolua_constant(tolua_S,"GF_OLD_HEAL",GF_OLD_HEAL);
 tolua_constant(tolua_S,"GF_OLD_SPEED",GF_OLD_SPEED);
 tolua_constant(tolua_S,"GF_OLD_SLOW",GF_OLD_SLOW);
 tolua_constant(tolua_S,"GF_OLD_CONF",GF_OLD_CONF);
 tolua_constant(tolua_S,"GF_OLD_SLEEP",GF_OLD_SLEEP);
 tolua_constant(tolua_S,"GF_OLD_DRAIN",GF_OLD_DRAIN);
 tolua_constant(tolua_S,"GF_AWAY_UNDEAD",GF_AWAY_UNDEAD);
 tolua_constant(tolua_S,"GF_AWAY_EVIL",GF_AWAY_EVIL);
 tolua_constant(tolua_S,"GF_TURN_UNDEAD",GF_TURN_UNDEAD);
 tolua_constant(tolua_S,"GF_TURN_EVIL",GF_TURN_EVIL);
 tolua_constant(tolua_S,"GF_TURN_ALL",GF_TURN_ALL);
 tolua_constant(tolua_S,"GF_DISP_UNDEAD",GF_DISP_UNDEAD);
 tolua_constant(tolua_S,"GF_DISP_EVIL",GF_DISP_EVIL);
 tolua_constant(tolua_S,"GF_DISP_ALL",GF_DISP_ALL);
 tolua_constant(tolua_S,"GF_DISP_DEMON",GF_DISP_DEMON);
 tolua_constant(tolua_S,"GF_DISP_LIVING",GF_DISP_LIVING);
 tolua_constant(tolua_S,"GF_ROCKET",GF_ROCKET);
 tolua_constant(tolua_S,"GF_MAKE_GLYPH",GF_MAKE_GLYPH);
 tolua_constant(tolua_S,"GF_STONE_WALL",GF_STONE_WALL);
 tolua_constant(tolua_S,"GF_DEATH_RAY",GF_DEATH_RAY);
 tolua_constant(tolua_S,"GF_STUN",GF_STUN);
 tolua_constant(tolua_S,"GF_CONTROL_UNDEAD",GF_CONTROL_UNDEAD);
 tolua_constant(tolua_S,"GF_CONTROL_ANIMAL",GF_CONTROL_ANIMAL);
 tolua_constant(tolua_S,"GF_TELEKINESIS",GF_TELEKINESIS);
 tolua_constant(tolua_S,"GF_JAM_DOOR",GF_JAM_DOOR);
 tolua_constant(tolua_S,"GF_DOMINATION",GF_DOMINATION);
 tolua_constant(tolua_S,"GF_DISP_GOOD",GF_DISP_GOOD);
 tolua_constant(tolua_S,"GF_IDENTIFY",GF_IDENTIFY);
 tolua_constant(tolua_S,"GF_RAISE",GF_RAISE);
 tolua_constant(tolua_S,"GF_STAR_IDENTIFY",GF_STAR_IDENTIFY);
 tolua_constant(tolua_S,"GF_DESTRUCTION",GF_DESTRUCTION);
 tolua_constant(tolua_S,"GF_STUN_CONF",GF_STUN_CONF);
 tolua_constant(tolua_S,"GF_STUN_DAM",GF_STUN_DAM);
 tolua_constant(tolua_S,"GF_CONF_DAM",GF_CONF_DAM);
 tolua_constant(tolua_S,"GF_IMPLOSION",GF_IMPLOSION);
 tolua_constant(tolua_S,"GF_LAVA_FLOW",GF_LAVA_FLOW);
 tolua_constant(tolua_S,"GF_FEAR",GF_FEAR);
 tolua_constant(tolua_S,"GF_BETWEEN_GATE",GF_BETWEEN_GATE);
 tolua_constant(tolua_S,"GF_WINDS_MANA",GF_WINDS_MANA);
 tolua_constant(tolua_S,"GF_DEATH",GF_DEATH);
 tolua_constant(tolua_S,"GF_WEAKEN",GF_WEAKEN);
 tolua_constant(tolua_S,"GF_LOWER_POWER",GF_LOWER_POWER);
 tolua_constant(tolua_S,"GF_LOWER_MAGIC",GF_LOWER_MAGIC);
 tolua_constant(tolua_S,"GF_SLOW_DOWN",GF_SLOW_DOWN);
 tolua_constant(tolua_S,"GF_LIFE_BLAST",GF_LIFE_BLAST);
 tolua_constant(tolua_S,"GF_HALVE_DAMAGES",GF_HALVE_DAMAGES);
 tolua_constant(tolua_S,"GF_HALVE_MAGIC",GF_HALVE_MAGIC);
 tolua_constant(tolua_S,"GF_HALVE_SPEED",GF_HALVE_SPEED);
 tolua_constant(tolua_S,"GF_HALVE_LEVEL",GF_HALVE_LEVEL);
 tolua_constant(tolua_S,"GF_LOCK",GF_LOCK);
 tolua_constant(tolua_S,"GF_DAMAGES_CURSE",GF_DAMAGES_CURSE);
 tolua_constant(tolua_S,"GF_INCOMPETENCE",GF_INCOMPETENCE);
 tolua_constant(tolua_S,"GF_RETROGRADE",GF_RETROGRADE);
 tolua_constant(tolua_S,"GF_DOMINATION_CURSE",GF_DOMINATION_CURSE);
 tolua_constant(tolua_S,"GF_SLEEP_POLLEN",GF_SLEEP_POLLEN);
 tolua_constant(tolua_S,"GF_WAR_BLESSING",GF_WAR_BLESSING);
 tolua_constant(tolua_S,"GF_FRAILNESS",GF_FRAILNESS);
 tolua_constant(tolua_S,"GF_INEPTITUDE",GF_INEPTITUDE);
 tolua_constant(tolua_S,"GF_FEAR_CURSE",GF_FEAR_CURSE);
 tolua_constant(tolua_S,"GF_PSI_HITRATE",GF_PSI_HITRATE);
 tolua_constant(tolua_S,"GF_PSI_FEAR",GF_PSI_FEAR);
 tolua_constant(tolua_S,"GF_REDUCE_DEF",GF_REDUCE_DEF);
 tolua_constant(tolua_S,"GF_REDUCE_HIT",GF_REDUCE_HIT);
 tolua_constant(tolua_S,"GF_REDUCE_SPEED",GF_REDUCE_SPEED);
 tolua_constant(tolua_S,"GF_MORALE_BOOST",GF_MORALE_BOOST);
 tolua_constant(tolua_S,"GF_EVOLVE",GF_EVOLVE);
 tolua_constant(tolua_S,"GF_UNEVOLVE",GF_UNEVOLVE);
 tolua_constant(tolua_S,"GF_UNSUMMON",GF_UNSUMMON);
 tolua_constant(tolua_S,"GF_SLEEP_GAS",GF_SLEEP_GAS);
 tolua_constant(tolua_S,"GF_ANIMAL_EMPATHY",GF_ANIMAL_EMPATHY);
 tolua_constant(tolua_S,"GF_AURA_LIFE",GF_AURA_LIFE);
 tolua_constant(tolua_S,"GF_SMITE_EVIL",GF_SMITE_EVIL);
 tolua_constant(tolua_S,"GF_RETROGRADE_DARKNESS",GF_RETROGRADE_DARKNESS);
 tolua_constant(tolua_S,"GF_DOMINATE_MONSTER",GF_DOMINATE_MONSTER);
 tolua_constant(tolua_S,"GF_SHATTER_EVIL",GF_SHATTER_EVIL);
 tolua_constant(tolua_S,"GF_ANGELIC_VOICE",GF_ANGELIC_VOICE);
 tolua_constant(tolua_S,"GF_REPULSE_EVIL",GF_REPULSE_EVIL);
 tolua_constant(tolua_S,"GF_SLAY_EVIL",GF_SLAY_EVIL);
 tolua_constant(tolua_S,"GF_SEAL_LIGHT",GF_SEAL_LIGHT);
 tolua_constant(tolua_S,"GF_PARALYZE",GF_PARALYZE);
 tolua_constant(tolua_S,"GF_STEALTH_ATTACK",GF_STEALTH_ATTACK);
 tolua_constant(tolua_S,"GF_WARCRY",GF_WARCRY);
 tolua_constant(tolua_S,"GF_LOSE_STR",GF_LOSE_STR);
 tolua_constant(tolua_S,"GF_LOSE_INT",GF_LOSE_INT);
 tolua_constant(tolua_S,"GF_LOSE_WIS",GF_LOSE_WIS);
 tolua_constant(tolua_S,"GF_LOSE_DEX",GF_LOSE_DEX);
 tolua_constant(tolua_S,"GF_LOSE_CON",GF_LOSE_CON);
 tolua_constant(tolua_S,"GF_LOSE_CHR",GF_LOSE_CHR);
 tolua_constant(tolua_S,"GF_LOSE_ALL",GF_LOSE_ALL);
 tolua_constant(tolua_S,"GF_LOSE_EXP",GF_LOSE_EXP);
 tolua_constant(tolua_S,"GF_TAUNT",GF_TAUNT);
 tolua_constant(tolua_S,"GF_DIVINATION",GF_DIVINATION);
 tolua_constant(tolua_S,"GF_HARM",GF_HARM);
 tolua_constant(tolua_S,"GF_UNDEAD_SMITE",GF_UNDEAD_SMITE);
 tolua_constant(tolua_S,"GF_DEMON_SMITE",GF_DEMON_SMITE);
 tolua_constant(tolua_S,"GF_EVIL_SMITE",GF_EVIL_SMITE);
 tolua_constant(tolua_S,"GF_COMMAND_ELEMENT",GF_COMMAND_ELEMENT);
 tolua_constant(tolua_S,"PANEL_HGT",PANEL_HGT);
 tolua_constant(tolua_S,"PANEL_WID",PANEL_WID);
 tolua_constant(tolua_S,"SCREEN_HGT",SCREEN_HGT);
 tolua_constant(tolua_S,"SCREEN_WID",SCREEN_WID);
 tolua_constant(tolua_S,"MAX_HGT",MAX_HGT);
 tolua_constant(tolua_S,"MAX_WID",MAX_WID);
 tolua_constant(tolua_S,"KEYMAP_MODES",KEYMAP_MODES);
 tolua_constant(tolua_S,"KEYMAP_MODE_ORIG",KEYMAP_MODE_ORIG);
 tolua_constant(tolua_S,"KEYMAP_MODE_ROGUE",KEYMAP_MODE_ROGUE);
 tolua_constant(tolua_S,"MAX_HISCORES",MAX_HISCORES);
 tolua_constant(tolua_S,"MAX_DEPTH",MAX_DEPTH);
 tolua_constant(tolua_S,"LITE_MAX",LITE_MAX);
 tolua_constant(tolua_S,"VIEW_MAX",VIEW_MAX);
 tolua_constant(tolua_S,"TEMP_MAX",TEMP_MAX);
 tolua_constant(tolua_S,"MAX_OWNERS",MAX_OWNERS);
 tolua_constant(tolua_S,"MAX_SEXES",MAX_SEXES);
 tolua_constant(tolua_S,"MAX_RACES",MAX_RACES);
 tolua_constant(tolua_S,"MAX_CLASS",MAX_CLASS);
 tolua_constant(tolua_S,"MAX_ABILITIES",MAX_ABILITIES);
 tolua_constant(tolua_S,"MAX_SIGHT",MAX_SIGHT);
 tolua_constant(tolua_S,"MAX_RANGE",MAX_RANGE);
 tolua_constant(tolua_S,"MAX_WILD_X",MAX_WILD_X);
 tolua_constant(tolua_S,"MAX_WILD_Y",MAX_WILD_Y);
 tolua_constant(tolua_S,"SOUND_HIT",SOUND_HIT);
 tolua_constant(tolua_S,"SOUND_MISS",SOUND_MISS);
 tolua_constant(tolua_S,"SOUND_FLEE",SOUND_FLEE);
 tolua_constant(tolua_S,"SOUND_DROP",SOUND_DROP);
 tolua_constant(tolua_S,"SOUND_KILL",SOUND_KILL);
 tolua_constant(tolua_S,"SOUND_LEVEL",SOUND_LEVEL);
 tolua_constant(tolua_S,"SOUND_DEATH",SOUND_DEATH);
 tolua_constant(tolua_S,"SOUND_STUDY",SOUND_STUDY);
 tolua_constant(tolua_S,"SOUND_TELEPORT",SOUND_TELEPORT);
 tolua_constant(tolua_S,"SOUND_SHOOT",SOUND_SHOOT);
 tolua_constant(tolua_S,"SOUND_QUAFF",SOUND_QUAFF);
 tolua_constant(tolua_S,"SOUND_ZAP",SOUND_ZAP);
 tolua_constant(tolua_S,"SOUND_WALK",SOUND_WALK);
 tolua_constant(tolua_S,"SOUND_TPOTHER",SOUND_TPOTHER);
 tolua_constant(tolua_S,"SOUND_HITWALL",SOUND_HITWALL);
 tolua_constant(tolua_S,"SOUND_EAT",SOUND_EAT);
 tolua_constant(tolua_S,"SOUND_STORE1",SOUND_STORE1);
 tolua_constant(tolua_S,"SOUND_STORE2",SOUND_STORE2);
 tolua_constant(tolua_S,"SOUND_STORE3",SOUND_STORE3);
 tolua_constant(tolua_S,"SOUND_STORE4",SOUND_STORE4);
 tolua_constant(tolua_S,"SOUND_DIG",SOUND_DIG);
 tolua_constant(tolua_S,"SOUND_OPENDOOR",SOUND_OPENDOOR);
 tolua_constant(tolua_S,"SOUND_SHUTDOOR",SOUND_SHUTDOOR);
 tolua_constant(tolua_S,"SOUND_TPLEVEL",SOUND_TPLEVEL);
 tolua_constant(tolua_S,"SOUND_SCROLL",SOUND_SCROLL);
 tolua_constant(tolua_S,"SOUND_BUY",SOUND_BUY);
 tolua_constant(tolua_S,"SOUND_SELL",SOUND_SELL);
 tolua_constant(tolua_S,"SOUND_WARN",SOUND_WARN);
 tolua_constant(tolua_S,"SOUND_ROCKET",SOUND_ROCKET);
 tolua_constant(tolua_S,"SOUND_N_KILL",SOUND_N_KILL);
 tolua_constant(tolua_S,"SOUND_U_KILL",SOUND_U_KILL);
 tolua_constant(tolua_S,"SOUND_QUEST",SOUND_QUEST);
 tolua_constant(tolua_S,"SOUND_HEAL",SOUND_HEAL);
 tolua_constant(tolua_S,"SOUND_X_HEAL",SOUND_X_HEAL);
 tolua_constant(tolua_S,"SOUND_BITE",SOUND_BITE);
 tolua_constant(tolua_S,"SOUND_CLAW",SOUND_CLAW);
 tolua_constant(tolua_S,"SOUND_M_SPELL",SOUND_M_SPELL);
 tolua_constant(tolua_S,"SOUND_SUMMON",SOUND_SUMMON);
 tolua_constant(tolua_S,"SOUND_BREATH",SOUND_BREATH);
 tolua_constant(tolua_S,"SOUND_BALL",SOUND_BALL);
 tolua_constant(tolua_S,"SOUND_M_HEAL",SOUND_M_HEAL);
 tolua_constant(tolua_S,"SOUND_ATK_SPELL",SOUND_ATK_SPELL);
 tolua_constant(tolua_S,"SOUND_EVIL",SOUND_EVIL);
 tolua_constant(tolua_S,"SOUND_TOUCH",SOUND_TOUCH);
 tolua_constant(tolua_S,"SOUND_STING",SOUND_STING);
 tolua_constant(tolua_S,"SOUND_CRUSH",SOUND_CRUSH);
 tolua_constant(tolua_S,"SOUND_SLIME",SOUND_SLIME);
 tolua_constant(tolua_S,"SOUND_WAIL",SOUND_WAIL);
 tolua_constant(tolua_S,"SOUND_WINNER",SOUND_WINNER);
 tolua_constant(tolua_S,"SOUND_FIRE",SOUND_FIRE);
 tolua_constant(tolua_S,"SOUND_ACID",SOUND_ACID);
 tolua_constant(tolua_S,"SOUND_ELEC",SOUND_ELEC);
 tolua_constant(tolua_S,"SOUND_COLD",SOUND_COLD);
 tolua_constant(tolua_S,"SOUND_ILLEGAL",SOUND_ILLEGAL);
 tolua_constant(tolua_S,"SOUND_FAIL",SOUND_FAIL);
 tolua_constant(tolua_S,"SOUND_WAKEUP",SOUND_WAKEUP);
 tolua_constant(tolua_S,"SOUND_INVULN",SOUND_INVULN);
 tolua_constant(tolua_S,"SOUND_FALL",SOUND_FALL);
 tolua_constant(tolua_S,"SOUND_PAIN",SOUND_PAIN);
 tolua_constant(tolua_S,"SOUND_DESTITEM",SOUND_DESTITEM);
 tolua_constant(tolua_S,"SOUND_MOAN",SOUND_MOAN);
 tolua_constant(tolua_S,"SOUND_SHOW",SOUND_SHOW);
 tolua_constant(tolua_S,"SOUND_UNUSED",SOUND_UNUSED);
 tolua_constant(tolua_S,"SOUND_EXPLODE",SOUND_EXPLODE);
 tolua_constant(tolua_S,"SOUND_MAX",SOUND_MAX);
 tolua_constant(tolua_S,"FEAT_NONE",FEAT_NONE);
 tolua_constant(tolua_S,"FEAT_FLOOR",FEAT_FLOOR);
 tolua_constant(tolua_S,"FEAT_GLYPH",FEAT_GLYPH);
 tolua_constant(tolua_S,"FEAT_OPEN",FEAT_OPEN);
 tolua_constant(tolua_S,"FEAT_BROKEN",FEAT_BROKEN);
 tolua_constant(tolua_S,"FEAT_LESS",FEAT_LESS);
 tolua_constant(tolua_S,"FEAT_MORE",FEAT_MORE);
 tolua_constant(tolua_S,"FEAT_QUEST_ENTER",FEAT_QUEST_ENTER);
 tolua_constant(tolua_S,"FEAT_QUEST_EXIT",FEAT_QUEST_EXIT);
 tolua_constant(tolua_S,"FEAT_QUEST_DOWN",FEAT_QUEST_DOWN);
 tolua_constant(tolua_S,"FEAT_QUEST_UP",FEAT_QUEST_UP);
 tolua_constant(tolua_S,"FEAT_SHAFT_DOWN",FEAT_SHAFT_DOWN);
 tolua_constant(tolua_S,"FEAT_SHAFT_UP",FEAT_SHAFT_UP);
 tolua_constant(tolua_S,"FEAT_ICE_OPEN",FEAT_ICE_OPEN);
 tolua_constant(tolua_S,"FEAT_ICE_BROKEN",FEAT_ICE_BROKEN);
 tolua_constant(tolua_S,"FEAT_DOOR_HEAD",FEAT_DOOR_HEAD);
 tolua_constant(tolua_S,"FEAT_DOOR_TAIL",FEAT_DOOR_TAIL);
 tolua_constant(tolua_S,"FEAT_ICE_DOOR_HEAD",FEAT_ICE_DOOR_HEAD);
 tolua_constant(tolua_S,"FEAT_ICE_DOOR_TAIL",FEAT_ICE_DOOR_TAIL);
 tolua_constant(tolua_S,"FEAT_SECRET",FEAT_SECRET);
 tolua_constant(tolua_S,"FEAT_RUBBLE",FEAT_RUBBLE);
 tolua_constant(tolua_S,"FEAT_ICE_SECRET",FEAT_ICE_SECRET);
 tolua_constant(tolua_S,"FEAT_MAGMA",FEAT_MAGMA);
 tolua_constant(tolua_S,"FEAT_QUARTZ",FEAT_QUARTZ);
 tolua_constant(tolua_S,"FEAT_MAGMA_H",FEAT_MAGMA_H);
 tolua_constant(tolua_S,"FEAT_QUARTZ_H",FEAT_QUARTZ_H);
 tolua_constant(tolua_S,"FEAT_MAGMA_K",FEAT_MAGMA_K);
 tolua_constant(tolua_S,"FEAT_QUARTZ_K",FEAT_QUARTZ_K);
 tolua_constant(tolua_S,"FEAT_WALL_EXTRA",FEAT_WALL_EXTRA);
 tolua_constant(tolua_S,"FEAT_WALL_INNER",FEAT_WALL_INNER);
 tolua_constant(tolua_S,"FEAT_WALL_OUTER",FEAT_WALL_OUTER);
 tolua_constant(tolua_S,"FEAT_WALL_SOLID",FEAT_WALL_SOLID);
 tolua_constant(tolua_S,"FEAT_PERM_EXTRA",FEAT_PERM_EXTRA);
 tolua_constant(tolua_S,"FEAT_PERM_INNER",FEAT_PERM_INNER);
 tolua_constant(tolua_S,"FEAT_PERM_OUTER",FEAT_PERM_OUTER);
 tolua_constant(tolua_S,"FEAT_PERM_SOLID",FEAT_PERM_SOLID);
 tolua_constant(tolua_S,"FEAT_MINOR_GLYPH",FEAT_MINOR_GLYPH);
 tolua_constant(tolua_S,"FEAT_PATTERN_START",FEAT_PATTERN_START);
 tolua_constant(tolua_S,"FEAT_PATTERN_1",FEAT_PATTERN_1);
 tolua_constant(tolua_S,"FEAT_PATTERN_2",FEAT_PATTERN_2);
 tolua_constant(tolua_S,"FEAT_PATTERN_3",FEAT_PATTERN_3);
 tolua_constant(tolua_S,"FEAT_PATTERN_4",FEAT_PATTERN_4);
 tolua_constant(tolua_S,"FEAT_PATTERN_END",FEAT_PATTERN_END);
 tolua_constant(tolua_S,"FEAT_PATTERN_OLD",FEAT_PATTERN_OLD);
 tolua_constant(tolua_S,"FEAT_PATTERN_XTRA1",FEAT_PATTERN_XTRA1);
 tolua_constant(tolua_S,"FEAT_PATTERN_XTRA2",FEAT_PATTERN_XTRA2);
 tolua_constant(tolua_S,"FEAT_SHOP_HEAD",FEAT_SHOP_HEAD);
 tolua_constant(tolua_S,"FEAT_SHOP_TAIL",FEAT_SHOP_TAIL);
 tolua_constant(tolua_S,"FEAT_DEEP_WATER",FEAT_DEEP_WATER);
 tolua_constant(tolua_S,"FEAT_SHAL_WATER",FEAT_SHAL_WATER);
 tolua_constant(tolua_S,"FEAT_DEEP_LAVA",FEAT_DEEP_LAVA);
 tolua_constant(tolua_S,"FEAT_SHAL_LAVA",FEAT_SHAL_LAVA);
 tolua_constant(tolua_S,"FEAT_DARK_PIT",FEAT_DARK_PIT);
 tolua_constant(tolua_S,"FEAT_DIRT",FEAT_DIRT);
 tolua_constant(tolua_S,"FEAT_GRASS",FEAT_GRASS);
 tolua_constant(tolua_S,"FEAT_TREES",FEAT_TREES);
 tolua_constant(tolua_S,"FEAT_MOUNTAIN",FEAT_MOUNTAIN);
 tolua_constant(tolua_S,"FEAT_SNOW",FEAT_SNOW);
 tolua_constant(tolua_S,"FEAT_SNOW_TREES",FEAT_SNOW_TREES);
 tolua_constant(tolua_S,"FEAT_GLACIER",FEAT_GLACIER);
 tolua_constant(tolua_S,"FEAT_ICE_WALL",FEAT_ICE_WALL);
 tolua_constant(tolua_S,"FEAT_PERM_ICE_WALL",FEAT_PERM_ICE_WALL);
 tolua_constant(tolua_S,"FEAT_BETWEEN",FEAT_BETWEEN);
 tolua_constant(tolua_S,"FEAT_GLASS_WALL",FEAT_GLASS_WALL);
 tolua_constant(tolua_S,"FEAT_ILLUS_WALL",FEAT_ILLUS_WALL);
 tolua_constant(tolua_S,"FEAT_TOWN",FEAT_TOWN);
 tolua_constant(tolua_S,"FEAT_ENCOUNTER",FEAT_ENCOUNTER);
 tolua_constant(tolua_S,"FEAT_FIRE_FIELD",FEAT_FIRE_FIELD);
 tolua_constant(tolua_S,"FEAT_COLD_FIELD",FEAT_COLD_FIELD);
 tolua_constant(tolua_S,"FEAT_ELEC_FIELD",FEAT_ELEC_FIELD);
 tolua_constant(tolua_S,"FEAT_WEBS",FEAT_WEBS);
 tolua_constant(tolua_S,"FEAT_SPIKE_TRAP",FEAT_SPIKE_TRAP);
 tolua_constant(tolua_S,"FEAT_GAS_TRAP",FEAT_GAS_TRAP);
 tolua_constant(tolua_S,"FEAT_POISON_TRAP",FEAT_POISON_TRAP);
 tolua_constant(tolua_S,"FEAT_VINE_FIELD",FEAT_VINE_FIELD);
 tolua_constant(tolua_S,"FEAT_THORNED_VINES",FEAT_THORNED_VINES);
 tolua_constant(tolua_S,"FEAT_STORMS",FEAT_STORMS);
 tolua_constant(tolua_S,"FEAT_DARK_MIST",FEAT_DARK_MIST);
 tolua_constant(tolua_S,"CLASS_APPRENTICE",CLASS_APPRENTICE);
 tolua_constant(tolua_S,"CLASS_WARRIOR",CLASS_WARRIOR);
 tolua_constant(tolua_S,"CLASS_FIGHTER",CLASS_FIGHTER);
 tolua_constant(tolua_S,"CLASS_MAGE",CLASS_MAGE);
 tolua_constant(tolua_S,"CLASS_PRIEST",CLASS_PRIEST);
 tolua_constant(tolua_S,"CLASS_ROGUE",CLASS_ROGUE);
 tolua_constant(tolua_S,"CLASS_RANGER",CLASS_RANGER);
 tolua_constant(tolua_S,"CLASS_PALADIN",CLASS_PALADIN);
 tolua_constant(tolua_S,"CLASS_MONK",CLASS_MONK);
 tolua_constant(tolua_S,"CLASS_MARKSMAN",CLASS_MARKSMAN);
 tolua_constant(tolua_S,"CLASS_HIGH_MAGE",CLASS_HIGH_MAGE);
 tolua_constant(tolua_S,"CLASS_ELEM_LORD",CLASS_ELEM_LORD);
 tolua_constant(tolua_S,"CLASS_MONSTER_MAGE",CLASS_MONSTER_MAGE);
 tolua_constant(tolua_S,"CLASS_DEFENDER",CLASS_DEFENDER);
 tolua_constant(tolua_S,"CLASS_JUSTICE_WARRIOR",CLASS_JUSTICE_WARRIOR);
 tolua_constant(tolua_S,"CLASS_ZELAR",CLASS_ZELAR);
 tolua_constant(tolua_S,"CLASS_SOUL_GUARDIAN",CLASS_SOUL_GUARDIAN);
 tolua_constant(tolua_S,"CLASS_SHADOW",CLASS_SHADOW);
 tolua_constant(tolua_S,"CLASS_ENCHANTER",CLASS_ENCHANTER);
 tolua_constant(tolua_S,"CLASS_DIVINER",CLASS_DIVINER);
 tolua_constant(tolua_S,"CLASS_MONSTER",CLASS_MONSTER);
 tolua_constant(tolua_S,"CLASS_BARD",CLASS_BARD);
 tolua_constant(tolua_S,"RACE_HUMAN",RACE_HUMAN);
 tolua_constant(tolua_S,"RACE_HALF_ELF",RACE_HALF_ELF);
 tolua_constant(tolua_S,"RACE_ELF",RACE_ELF);
 tolua_constant(tolua_S,"RACE_DWARF",RACE_DWARF);
 tolua_constant(tolua_S,"RACE_GNOME",RACE_GNOME);
 tolua_constant(tolua_S,"RACE_KOBOLD",RACE_KOBOLD);
 tolua_constant(tolua_S,"RACE_DEVLING",RACE_DEVLING);
 tolua_constant(tolua_S,"RACE_CELESTIAL",RACE_CELESTIAL);
 tolua_constant(tolua_S,"RACE_DEMON",RACE_DEMON);
 tolua_constant(tolua_S,"RACE_ZULGOR",RACE_ZULGOR);
 tolua_constant(tolua_S,"RACE_MONSTER",RACE_MONSTER);
 tolua_constant(tolua_S,"SEX_FEMALE",SEX_FEMALE);
 tolua_constant(tolua_S,"SEX_MALE",SEX_MALE);
 tolua_constant(tolua_S,"SEX_NEUTER",SEX_NEUTER);
 tolua_constant(tolua_S,"A_STR",A_STR);
 tolua_constant(tolua_S,"A_INT",A_INT);
 tolua_constant(tolua_S,"A_WIS",A_WIS);
 tolua_constant(tolua_S,"A_DEX",A_DEX);
 tolua_constant(tolua_S,"A_CON",A_CON);
 tolua_constant(tolua_S,"A_CHR",A_CHR);
 tolua_constant(tolua_S,"SKILL_MAX",SKILL_MAX);
 tolua_constant(tolua_S,"MAX_RESIST",MAX_RESIST);
 tolua_constant(tolua_S,"RF1_UNIQUE",RF1_UNIQUE);
 tolua_constant(tolua_S,"RF1_QUESTOR",RF1_QUESTOR);
 tolua_constant(tolua_S,"RF1_MALE",RF1_MALE);
 tolua_constant(tolua_S,"RF1_FEMALE",RF1_FEMALE);
 tolua_constant(tolua_S,"RF1_CHAR_CLEAR",RF1_CHAR_CLEAR);
 tolua_constant(tolua_S,"RF1_CHAR_MULTI",RF1_CHAR_MULTI);
 tolua_constant(tolua_S,"RF1_ATTR_CLEAR",RF1_ATTR_CLEAR);
 tolua_constant(tolua_S,"RF1_ATTR_MULTI",RF1_ATTR_MULTI);
 tolua_constant(tolua_S,"RF1_FORCE_DEPTH",RF1_FORCE_DEPTH);
 tolua_constant(tolua_S,"RF1_FORCE_MAXHP",RF1_FORCE_MAXHP);
 tolua_constant(tolua_S,"RF1_FORCE_SLEEP",RF1_FORCE_SLEEP);
 tolua_constant(tolua_S,"RF1_FORCE_EXTRA",RF1_FORCE_EXTRA);
 tolua_constant(tolua_S,"RF1_FRIEND",RF1_FRIEND);
 tolua_constant(tolua_S,"RF1_FRIENDS",RF1_FRIENDS);
 tolua_constant(tolua_S,"RF1_ESCORT",RF1_ESCORT);
 tolua_constant(tolua_S,"RF1_ESCORTS",RF1_ESCORTS);
 tolua_constant(tolua_S,"RF1_NEVER_BLOW",RF1_NEVER_BLOW);
 tolua_constant(tolua_S,"RF1_NEVER_MOVE",RF1_NEVER_MOVE);
 tolua_constant(tolua_S,"RF1_RAND_25",RF1_RAND_25);
 tolua_constant(tolua_S,"RF1_RAND_50",RF1_RAND_50);
 tolua_constant(tolua_S,"RF1_ONLY_GOLD",RF1_ONLY_GOLD);
 tolua_constant(tolua_S,"RF1_ONLY_ITEM",RF1_ONLY_ITEM);
 tolua_constant(tolua_S,"RF1_DROP_60",RF1_DROP_60);
 tolua_constant(tolua_S,"RF1_DROP_90",RF1_DROP_90);
 tolua_constant(tolua_S,"RF1_DROP_1D2",RF1_DROP_1D2);
 tolua_constant(tolua_S,"RF1_DROP_2D2",RF1_DROP_2D2);
 tolua_constant(tolua_S,"RF1_DROP_3D2",RF1_DROP_3D2);
 tolua_constant(tolua_S,"RF1_DROP_4D2",RF1_DROP_4D2);
 tolua_constant(tolua_S,"RF1_DROP_GOOD",RF1_DROP_GOOD);
 tolua_constant(tolua_S,"RF1_DROP_GREAT",RF1_DROP_GREAT);
 tolua_constant(tolua_S,"RF1_DROP_USEFUL",RF1_DROP_USEFUL);
 tolua_constant(tolua_S,"RF1_DROP_CHOSEN",RF1_DROP_CHOSEN);
 tolua_constant(tolua_S,"RF2_STUPID",RF2_STUPID);
 tolua_constant(tolua_S,"RF2_SMART",RF2_SMART);
 tolua_constant(tolua_S,"RF2_CAN_SPEAK",RF2_CAN_SPEAK);
 tolua_constant(tolua_S,"RF2_REFLECTING",RF2_REFLECTING);
 tolua_constant(tolua_S,"RF2_INVISIBLE",RF2_INVISIBLE);
 tolua_constant(tolua_S,"RF2_COLD_BLOOD",RF2_COLD_BLOOD);
 tolua_constant(tolua_S,"RF2_EMPTY_MIND",RF2_EMPTY_MIND);
 tolua_constant(tolua_S,"RF2_WEIRD_MIND",RF2_WEIRD_MIND);
 tolua_constant(tolua_S,"RF2_MULTIPLY",RF2_MULTIPLY);
 tolua_constant(tolua_S,"RF2_REGENERATE",RF2_REGENERATE);
 tolua_constant(tolua_S,"RF2_SHAPECHANGER",RF2_SHAPECHANGER);
 tolua_constant(tolua_S,"RF2_ATTR_ANY",RF2_ATTR_ANY);
 tolua_constant(tolua_S,"RF2_POWERFUL",RF2_POWERFUL);
 tolua_constant(tolua_S,"RF2_ELDRITCH_HORROR",RF2_ELDRITCH_HORROR);
 tolua_constant(tolua_S,"RF2_AURA_FIRE",RF2_AURA_FIRE);
 tolua_constant(tolua_S,"RF2_AURA_ELEC",RF2_AURA_ELEC);
 tolua_constant(tolua_S,"RF2_OPEN_DOOR",RF2_OPEN_DOOR);
 tolua_constant(tolua_S,"RF2_BASH_DOOR",RF2_BASH_DOOR);
 tolua_constant(tolua_S,"RF2_PASS_WALL",RF2_PASS_WALL);
 tolua_constant(tolua_S,"RF2_KILL_WALL",RF2_KILL_WALL);
 tolua_constant(tolua_S,"RF2_MOVE_BODY",RF2_MOVE_BODY);
 tolua_constant(tolua_S,"RF2_KILL_BODY",RF2_KILL_BODY);
 tolua_constant(tolua_S,"RF2_TAKE_ITEM",RF2_TAKE_ITEM);
 tolua_constant(tolua_S,"RF2_KILL_ITEM",RF2_KILL_ITEM);
 tolua_constant(tolua_S,"RF2_BRAIN_1",RF2_BRAIN_1);
 tolua_constant(tolua_S,"RF2_BRAIN_2",RF2_BRAIN_2);
 tolua_constant(tolua_S,"RF2_BRAIN_3",RF2_BRAIN_3);
 tolua_constant(tolua_S,"RF2_BRAIN_4",RF2_BRAIN_4);
 tolua_constant(tolua_S,"RF2_BRAIN_5",RF2_BRAIN_5);
 tolua_constant(tolua_S,"RF2_BRAIN_6",RF2_BRAIN_6);
 tolua_constant(tolua_S,"RF2_BRAIN_7",RF2_BRAIN_7);
 tolua_constant(tolua_S,"RF2_BRAIN_8",RF2_BRAIN_8);
 tolua_constant(tolua_S,"RF3_ORC",RF3_ORC);
 tolua_constant(tolua_S,"RF3_TROLL",RF3_TROLL);
 tolua_constant(tolua_S,"RF3_GIANT",RF3_GIANT);
 tolua_constant(tolua_S,"RF3_DRAGON",RF3_DRAGON);
 tolua_constant(tolua_S,"RF3_DEMON",RF3_DEMON);
 tolua_constant(tolua_S,"RF3_UNDEAD",RF3_UNDEAD);
 tolua_constant(tolua_S,"RF3_EVIL",RF3_EVIL);
 tolua_constant(tolua_S,"RF3_ANIMAL",RF3_ANIMAL);
 tolua_constant(tolua_S,"RF3_GOOD",RF3_GOOD);
 tolua_constant(tolua_S,"RF3_AURA_COLD",RF3_AURA_COLD);
 tolua_constant(tolua_S,"RF3_NONLIVING",RF3_NONLIVING);
 tolua_constant(tolua_S,"RF3_NO_FEAR",RF3_NO_FEAR);
 tolua_constant(tolua_S,"RF3_NO_STUN",RF3_NO_STUN);
 tolua_constant(tolua_S,"RF3_NO_CONF",RF3_NO_CONF);
 tolua_constant(tolua_S,"RF3_NO_SLEEP",RF3_NO_SLEEP);
 tolua_constant(tolua_S,"RF7_AQUATIC",RF7_AQUATIC);
 tolua_constant(tolua_S,"RF7_CAN_SWIM",RF7_CAN_SWIM);
 tolua_constant(tolua_S,"RF7_CAN_FLY",RF7_CAN_FLY);
 tolua_constant(tolua_S,"RF7_FRIENDLY",RF7_FRIENDLY);
 tolua_constant(tolua_S,"RF7_PET",RF7_PET);
 tolua_constant(tolua_S,"RF7_MORTAL",RF7_MORTAL);
 tolua_constant(tolua_S,"RF7_PLAYER_MONSTER",RF7_PLAYER_MONSTER);
 tolua_constant(tolua_S,"RF7_NAZGUL",RF7_NAZGUL);
 tolua_constant(tolua_S,"RF7_LEVEL_30",RF7_LEVEL_30);
 tolua_constant(tolua_S,"RF7_LEVEL_60",RF7_LEVEL_60);
 tolua_constant(tolua_S,"RF7_LEVEL_100",RF7_LEVEL_100);
 tolua_constant(tolua_S,"RF7_VARIAZ",RF7_VARIAZ);
 tolua_constant(tolua_S,"RF7_SEDUCE_MALES",RF7_SEDUCE_MALES);
 tolua_constant(tolua_S,"RF7_SEDUCE_FEMALES",RF7_SEDUCE_FEMALES);
 tolua_constant(tolua_S,"RF7_TOWNSFOLK",RF7_TOWNSFOLK);
 tolua_constant(tolua_S,"RF7_NEVER_MOVE_FRIENDLY",RF7_NEVER_MOVE_FRIENDLY);
 tolua_constant(tolua_S,"RF7_GUARD",RF7_GUARD);
 tolua_constant(tolua_S,"RF7_NEVER_BOSS",RF7_NEVER_BOSS);
 tolua_constant(tolua_S,"RF7_NEVER_ATTACKED",RF7_NEVER_ATTACKED);
 tolua_constant(tolua_S,"RF7_DEATH_DIALOG",RF7_DEATH_DIALOG);
 tolua_constant(tolua_S,"RF7_ICE",RF7_ICE);
 tolua_constant(tolua_S,"RF7_IMMORTAL",RF7_IMMORTAL);
 tolua_constant(tolua_S,"RF7_RANDOM",RF7_RANDOM);
 tolua_constant(tolua_S,"RF7_UNPLAYABLE",RF7_UNPLAYABLE);
 tolua_constant(tolua_S,"RF8_DUNGEON",RF8_DUNGEON);
 tolua_constant(tolua_S,"RF8_WILD_TOWN",RF8_WILD_TOWN);
 tolua_constant(tolua_S,"RF8_XXX8X02",RF8_XXX8X02);
 tolua_constant(tolua_S,"RF8_WILD_SHORE",RF8_WILD_SHORE);
 tolua_constant(tolua_S,"RF8_WILD_OCEAN",RF8_WILD_OCEAN);
 tolua_constant(tolua_S,"RF8_WILD_WASTE",RF8_WILD_WASTE);
 tolua_constant(tolua_S,"RF8_WILD_WOOD",RF8_WILD_WOOD);
 tolua_constant(tolua_S,"RF8_WILD_VOLCANO",RF8_WILD_VOLCANO);
 tolua_constant(tolua_S,"RF8_XXX8X08",RF8_XXX8X08);
 tolua_constant(tolua_S,"RF8_WILD_MOUNTAIN",RF8_WILD_MOUNTAIN);
 tolua_constant(tolua_S,"RF8_WILD_GRASS",RF8_WILD_GRASS);
 tolua_constant(tolua_S,"RF8_CTHANGBAND",RF8_CTHANGBAND);
 tolua_constant(tolua_S,"RF8_PERNANGBAND",RF8_PERNANGBAND);
 tolua_constant(tolua_S,"RF8_ZANGBAND",RF8_ZANGBAND);
 tolua_constant(tolua_S,"RF8_JOKEANGBAND",RF8_JOKEANGBAND);
 tolua_constant(tolua_S,"RF8_ANGBAND",RF8_ANGBAND);
 tolua_constant(tolua_S,"RF8_WILD_TOO",RF8_WILD_TOO);
 tolua_constant(tolua_S,"RF9_DROP_CORPSE",RF9_DROP_CORPSE);
 tolua_constant(tolua_S,"RF9_DROP_SKELETON",RF9_DROP_SKELETON);
 tolua_constant(tolua_S,"RF9_HAS_EGG",RF9_HAS_EGG);
 tolua_constant(tolua_S,"RF9_IMPRESED",RF9_IMPRESED);
 tolua_constant(tolua_S,"RF9_SUSCEP_ACID",RF9_SUSCEP_ACID);
 tolua_constant(tolua_S,"RF9_SUSCEP_ELEC",RF9_SUSCEP_ELEC);
 tolua_constant(tolua_S,"RF9_SUSCEP_POIS",RF9_SUSCEP_POIS);
 tolua_constant(tolua_S,"RF9_KILL_TREES",RF9_KILL_TREES);
 tolua_constant(tolua_S,"RF9_WYRM_PROTECT",RF9_WYRM_PROTECT);
 tolua_constant(tolua_S,"RF9_DOPPLEGANGER",RF9_DOPPLEGANGER);
 tolua_constant(tolua_S,"RF9_ONLY_DEPTH",RF9_ONLY_DEPTH);
 tolua_constant(tolua_S,"RF9_SPECIAL_GENE",RF9_SPECIAL_GENE);
 tolua_constant(tolua_S,"TR1_STR",TR1_STR);
 tolua_constant(tolua_S,"TR1_INT",TR1_INT);
 tolua_constant(tolua_S,"TR1_WIS",TR1_WIS);
 tolua_constant(tolua_S,"TR1_DEX",TR1_DEX);
 tolua_constant(tolua_S,"TR1_CON",TR1_CON);
 tolua_constant(tolua_S,"TR1_CHR",TR1_CHR);
 tolua_constant(tolua_S,"TR1_MANA",TR1_MANA);
 tolua_constant(tolua_S,"TR1_SPELL",TR1_SPELL);
 tolua_constant(tolua_S,"TR1_STEALTH",TR1_STEALTH);
 tolua_constant(tolua_S,"TR1_SHARPENED",TR1_SHARPENED);
 tolua_constant(tolua_S,"TR1_INFRA",TR1_INFRA);
 tolua_constant(tolua_S,"TR1_ENCHANTED",TR1_ENCHANTED);
 tolua_constant(tolua_S,"TR1_SPEED",TR1_SPEED);
 tolua_constant(tolua_S,"TR1_BLOWS",TR1_BLOWS);
 tolua_constant(tolua_S,"TR1_CHAOTIC",TR1_CHAOTIC);
 tolua_constant(tolua_S,"TR1_VAMPIRIC",TR1_VAMPIRIC);
 tolua_constant(tolua_S,"TR1_SLAY_ANIMAL",TR1_SLAY_ANIMAL);
 tolua_constant(tolua_S,"TR1_SLAY_EVIL",TR1_SLAY_EVIL);
 tolua_constant(tolua_S,"TR1_SLAY_UNDEAD",TR1_SLAY_UNDEAD);
 tolua_constant(tolua_S,"TR1_SLAY_DEMON",TR1_SLAY_DEMON);
 tolua_constant(tolua_S,"TR1_SLAY_ORC",TR1_SLAY_ORC);
 tolua_constant(tolua_S,"TR1_SLAY_TROLL",TR1_SLAY_TROLL);
 tolua_constant(tolua_S,"TR1_SLAY_GIANT",TR1_SLAY_GIANT);
 tolua_constant(tolua_S,"TR1_SLAY_DRAGON",TR1_SLAY_DRAGON);
 tolua_constant(tolua_S,"TR1_KILL_DRAGON",TR1_KILL_DRAGON);
 tolua_constant(tolua_S,"TR1_VORPAL",TR1_VORPAL);
 tolua_constant(tolua_S,"TR1_IMPACT",TR1_IMPACT);
 tolua_constant(tolua_S,"TR1_BRAND_POIS",TR1_BRAND_POIS);
 tolua_constant(tolua_S,"TR1_BRAND_ACID",TR1_BRAND_ACID);
 tolua_constant(tolua_S,"TR1_BRAND_ELEC",TR1_BRAND_ELEC);
 tolua_constant(tolua_S,"TR1_BRAND_FIRE",TR1_BRAND_FIRE);
 tolua_constant(tolua_S,"TR1_BRAND_COLD",TR1_BRAND_COLD);
 tolua_constant(tolua_S,"TR1_NULL_MASK",TR1_NULL_MASK);
 tolua_constant(tolua_S,"TR2_SUST_STR",TR2_SUST_STR);
 tolua_constant(tolua_S,"TR2_SUST_INT",TR2_SUST_INT);
 tolua_constant(tolua_S,"TR2_SUST_WIS",TR2_SUST_WIS);
 tolua_constant(tolua_S,"TR2_SUST_DEX",TR2_SUST_DEX);
 tolua_constant(tolua_S,"TR2_SUST_CON",TR2_SUST_CON);
 tolua_constant(tolua_S,"TR2_SUST_CHR",TR2_SUST_CHR);
 tolua_constant(tolua_S,"TR2_INVIS",TR2_INVIS);
 tolua_constant(tolua_S,"TR2_LIFE",TR2_LIFE);
 tolua_constant(tolua_S,"TR2_IM_ACID",TR2_IM_ACID);
 tolua_constant(tolua_S,"TR2_IM_ELEC",TR2_IM_ELEC);
 tolua_constant(tolua_S,"TR2_IM_FIRE",TR2_IM_FIRE);
 tolua_constant(tolua_S,"TR2_IM_COLD",TR2_IM_COLD);
 tolua_constant(tolua_S,"TR2_RES_WATER",TR2_RES_WATER);
 tolua_constant(tolua_S,"TR2_REFLECT",TR2_REFLECT);
 tolua_constant(tolua_S,"TR2_FREE_ACT",TR2_FREE_ACT);
 tolua_constant(tolua_S,"TR2_HOLD_LIFE",TR2_HOLD_LIFE);
 tolua_constant(tolua_S,"TR2_RES_ACID",TR2_RES_ACID);
 tolua_constant(tolua_S,"TR2_RES_ELEC",TR2_RES_ELEC);
 tolua_constant(tolua_S,"TR2_RES_FIRE",TR2_RES_FIRE);
 tolua_constant(tolua_S,"TR2_RES_COLD",TR2_RES_COLD);
 tolua_constant(tolua_S,"TR2_RES_POIS",TR2_RES_POIS);
 tolua_constant(tolua_S,"TR2_RES_FEAR",TR2_RES_FEAR);
 tolua_constant(tolua_S,"TR2_RES_LITE",TR2_RES_LITE);
 tolua_constant(tolua_S,"TR2_RES_DARK",TR2_RES_DARK);
 tolua_constant(tolua_S,"TR2_RES_BLIND",TR2_RES_BLIND);
 tolua_constant(tolua_S,"TR2_RES_CONF",TR2_RES_CONF);
 tolua_constant(tolua_S,"TR2_RES_SOUND",TR2_RES_SOUND);
 tolua_constant(tolua_S,"TR2_RES_EARTH",TR2_RES_EARTH);
 tolua_constant(tolua_S,"TR2_RES_RADIO",TR2_RES_RADIO);
 tolua_constant(tolua_S,"TR2_RES_WIND",TR2_RES_WIND);
 tolua_constant(tolua_S,"TR2_RES_CHAOS",TR2_RES_CHAOS);
 tolua_constant(tolua_S,"TR2_RES_WARP",TR2_RES_WARP);
 tolua_constant(tolua_S,"TR2_NULL_MASK",TR2_NULL_MASK);
 tolua_constant(tolua_S,"TR3_SH_FIRE",TR3_SH_FIRE);
 tolua_constant(tolua_S,"TR3_SH_ELEC",TR3_SH_ELEC);
 tolua_constant(tolua_S,"TR3_QUESTITEM",TR3_QUESTITEM);
 tolua_constant(tolua_S,"TR3_DECAY",TR3_DECAY);
 tolua_constant(tolua_S,"TR3_NO_TELE",TR3_NO_TELE);
 tolua_constant(tolua_S,"TR3_NO_MAGIC",TR3_NO_MAGIC);
 tolua_constant(tolua_S,"TR3_WRAITH",TR3_WRAITH);
 tolua_constant(tolua_S,"TR3_EASY_KNOW",TR3_EASY_KNOW);
 tolua_constant(tolua_S,"TR3_HIDE_TYPE",TR3_HIDE_TYPE);
 tolua_constant(tolua_S,"TR3_SHOW_MODS",TR3_SHOW_MODS);
 tolua_constant(tolua_S,"TR3_INSTA_ART",TR3_INSTA_ART);
 tolua_constant(tolua_S,"TR3_FEATHER",TR3_FEATHER);
 tolua_constant(tolua_S,"TR3_LITE",TR3_LITE);
 tolua_constant(tolua_S,"TR3_SEE_INVIS",TR3_SEE_INVIS);
 tolua_constant(tolua_S,"TR3_TELEPATHY",TR3_TELEPATHY);
 tolua_constant(tolua_S,"TR3_SLOW_DIGEST",TR3_SLOW_DIGEST);
 tolua_constant(tolua_S,"TR3_REGEN",TR3_REGEN);
 tolua_constant(tolua_S,"TR3_XTRA_MIGHT",TR3_XTRA_MIGHT);
 tolua_constant(tolua_S,"TR3_XTRA_SHOTS",TR3_XTRA_SHOTS);
 tolua_constant(tolua_S,"TR3_IGNORE_ACID",TR3_IGNORE_ACID);
 tolua_constant(tolua_S,"TR3_IGNORE_ELEC",TR3_IGNORE_ELEC);
 tolua_constant(tolua_S,"TR3_IGNORE_FIRE",TR3_IGNORE_FIRE);
 tolua_constant(tolua_S,"TR3_IGNORE_COLD",TR3_IGNORE_COLD);
 tolua_constant(tolua_S,"TR3_ACTIVATE",TR3_ACTIVATE);
 tolua_constant(tolua_S,"TR3_DRAIN_EXP",TR3_DRAIN_EXP);
 tolua_constant(tolua_S,"TR3_TELEPORT",TR3_TELEPORT);
 tolua_constant(tolua_S,"TR3_AGGRAVATE",TR3_AGGRAVATE);
 tolua_constant(tolua_S,"TR3_BLESSED",TR3_BLESSED);
 tolua_constant(tolua_S,"TR3_CURSED",TR3_CURSED);
 tolua_constant(tolua_S,"TR3_HEAVY_CURSE",TR3_HEAVY_CURSE);
 tolua_constant(tolua_S,"TR3_PERMA_CURSE",TR3_PERMA_CURSE);
 tolua_constant(tolua_S,"TR3_NULL_MASK",TR3_NULL_MASK);
 tolua_constant(tolua_S,"TR4_NEVER_BLOW",TR4_NEVER_BLOW);
 tolua_constant(tolua_S,"TR4_ICE",TR4_ICE);
 tolua_constant(tolua_S,"TR4_RECHARGE",TR4_RECHARGE);
 tolua_constant(tolua_S,"TR4_FLY",TR4_FLY);
 tolua_constant(tolua_S,"TR4_COULD2H",TR4_COULD2H);
 tolua_constant(tolua_S,"TR4_MUST2H",TR4_MUST2H);
 tolua_constant(tolua_S,"TR4_LEVELS",TR4_LEVELS);
 tolua_constant(tolua_S,"TR4_CLONE",TR4_CLONE);
 tolua_constant(tolua_S,"TR4_SPECIAL_GENE",TR4_SPECIAL_GENE);
 tolua_constant(tolua_S,"TR4_CLIMB",TR4_CLIMB);
 tolua_constant(tolua_S,"TR4_CRAFTED",TR4_CRAFTED);
 tolua_constant(tolua_S,"TR4_MODERATE_POWER",TR4_MODERATE_POWER);
 tolua_constant(tolua_S,"TR4_ONLY_MALE",TR4_ONLY_MALE);
 tolua_constant(tolua_S,"TR4_ONLY_FEMALE",TR4_ONLY_FEMALE);
 tolua_constant(tolua_S,"TR4_ENHANCED",TR4_ENHANCED);
 tolua_constant(tolua_S,"TR4_CHARGEABLE",TR4_CHARGEABLE);
 tolua_constant(tolua_S,"TR4_INDESTRUCTIBLE",TR4_INDESTRUCTIBLE);
 tolua_constant(tolua_S,"TR4_ETERNAL",TR4_ETERNAL);
 tolua_constant(tolua_S,"TR4_SLAY_MALE",TR4_SLAY_MALE);
 tolua_constant(tolua_S,"TR4_SLAY_FEMALE",TR4_SLAY_FEMALE);
 tolua_constant(tolua_S,"TR4_ALWAYS_HIT",TR4_ALWAYS_HIT);
 tolua_constant(tolua_S,"TR4_LOWER_DEF",TR4_LOWER_DEF);
 tolua_constant(tolua_S,"TR4_LOWER_HIT",TR4_LOWER_HIT);
 tolua_constant(tolua_S,"TR4_RETURNING",TR4_RETURNING);
 tolua_constant(tolua_S,"TR4_SAFETY",TR4_SAFETY);
 tolua_constant(tolua_S,"TR4_PROTECTION",TR4_PROTECTION);
 tolua_constant(tolua_S,"TR4_ENCHANTED",TR4_ENCHANTED);
 tolua_constant(tolua_S,"TR4_PARRY",TR4_PARRY);
 tolua_constant(tolua_S,"TR4_NULL_MASK",TR4_NULL_MASK);
 tolua_constant(tolua_S,"BOSS_IMMUNE_WEAPONS",BOSS_IMMUNE_WEAPONS);
 tolua_constant(tolua_S,"BOSS_IMMUNE_MAGIC",BOSS_IMMUNE_MAGIC);
 tolua_constant(tolua_S,"BOSS_DOUBLE_DAMAGES",BOSS_DOUBLE_DAMAGES);
 tolua_constant(tolua_S,"BOSS_HALVE_DAMAGES",BOSS_HALVE_DAMAGES);
 tolua_constant(tolua_S,"BOSS_CURSED_HITS",BOSS_CURSED_HITS);
 tolua_constant(tolua_S,"BOSS_DOUBLE_MAGIC",BOSS_DOUBLE_MAGIC);
 tolua_constant(tolua_S,"BOSS_RETURNING",BOSS_RETURNING);
 tolua_constant(tolua_S,"CURSE_LOWER_POWER",CURSE_LOWER_POWER);
 tolua_constant(tolua_S,"CURSE_LOWER_MAGIC",CURSE_LOWER_MAGIC);
 tolua_constant(tolua_S,"CURSE_HALVE_HP",CURSE_HALVE_HP);
 tolua_constant(tolua_S,"CURSE_LOCK",CURSE_LOCK);
 tolua_constant(tolua_S,"CURSE_RETURNING",CURSE_RETURNING);
 tolua_constant(tolua_S,"CURSE_SLOW_DOWN",CURSE_SLOW_DOWN);
 tolua_constant(tolua_S,"CURSE_HALVE_SPEED",CURSE_HALVE_SPEED);
 tolua_constant(tolua_S,"CURSE_LIFE_BLAST",CURSE_LIFE_BLAST);
 tolua_constant(tolua_S,"CURSE_HALVE_DAMAGES",CURSE_HALVE_DAMAGES);
 tolua_constant(tolua_S,"CURSE_HALVE_MAGIC",CURSE_HALVE_MAGIC);
 tolua_constant(tolua_S,"CURSE_HALVE_LEVEL",CURSE_HALVE_LEVEL);
 tolua_constant(tolua_S,"CURSE_DAMAGES_CURSE",CURSE_DAMAGES_CURSE);
 tolua_constant(tolua_S,"WAR_BLESSED",WAR_BLESSED);
 tolua_constant(tolua_S,"CURSE_FRAILNESS",CURSE_FRAILNESS);
 tolua_constant(tolua_S,"CURSE_INEPTITUDE",CURSE_INEPTITUDE);
 tolua_constant(tolua_S,"CURSE_FEAR",CURSE_FEAR);
 tolua_constant(tolua_S,"EYE_STABBED",EYE_STABBED);
 tolua_constant(tolua_S,"MUTILATE_LEGS",MUTILATE_LEGS);
 tolua_constant(tolua_S,"MUTILATE_ARMS",MUTILATE_ARMS);
 tolua_constant(tolua_S,"PSYCHIC_HITRATE",PSYCHIC_HITRATE);
 tolua_constant(tolua_S,"MORALE_BOOST",MORALE_BOOST);
 tolua_constant(tolua_S,"BOSS_MAGIC_RETURNING",BOSS_MAGIC_RETURNING);
 tolua_constant(tolua_S,"TAUNTED",TAUNTED);
 tolua_constant(tolua_S,"PIERCING_SPELLS",PIERCING_SPELLS);
 tolua_constant(tolua_S,"FF1_NO_WALK",FF1_NO_WALK);
 tolua_constant(tolua_S,"FF1_NO_VISION",FF1_NO_VISION);
 tolua_constant(tolua_S,"FF1_CAN_LEVITATE",FF1_CAN_LEVITATE);
 tolua_constant(tolua_S,"FF1_CAN_PASS",FF1_CAN_PASS);
 tolua_constant(tolua_S,"FF1_FLOOR",FF1_FLOOR);
 tolua_constant(tolua_S,"FF1_WALL",FF1_WALL);
 tolua_constant(tolua_S,"FF1_PERMANENT",FF1_PERMANENT);
 tolua_constant(tolua_S,"FF1_CAN_FLY",FF1_CAN_FLY);
 tolua_constant(tolua_S,"FF1_REMEMBER",FF1_REMEMBER);
 tolua_constant(tolua_S,"FF1_NOTICE",FF1_NOTICE);
 tolua_constant(tolua_S,"FF1_DONT_NOTICE_RUNNING",FF1_DONT_NOTICE_RUNNING);
 tolua_constant(tolua_S,"FF1_CAN_RUN",FF1_CAN_RUN);
 tolua_constant(tolua_S,"FF1_DOOR",FF1_DOOR);
 tolua_constant(tolua_S,"FF1_SUPPORT_LIGHT",FF1_SUPPORT_LIGHT);
 tolua_constant(tolua_S,"FF1_CAN_CLIMB",FF1_CAN_CLIMB);
 tolua_constant(tolua_S,"DF1_PRINCIPAL",DF1_PRINCIPAL);
 tolua_constant(tolua_S,"DF1_MAZE",DF1_MAZE);
 tolua_constant(tolua_S,"DF1_SMALLEST",DF1_SMALLEST);
 tolua_constant(tolua_S,"DF1_SMALL",DF1_SMALL);
 tolua_constant(tolua_S,"DF1_BIG",DF1_BIG);
 tolua_constant(tolua_S,"DF1_NO_DOORS",DF1_NO_DOORS);
 tolua_constant(tolua_S,"DF1_WATER_RIVER",DF1_WATER_RIVER);
 tolua_constant(tolua_S,"DF1_LAVA_RIVER",DF1_LAVA_RIVER);
 tolua_constant(tolua_S,"DF1_WATER_RIVERS",DF1_WATER_RIVERS);
 tolua_constant(tolua_S,"DF1_LAVA_RIVERS",DF1_LAVA_RIVERS);
 tolua_constant(tolua_S,"DF1_CAVE",DF1_CAVE);
 tolua_constant(tolua_S,"DF1_CAVERN",DF1_CAVERN);
 tolua_constant(tolua_S,"DF1_NO_UP",DF1_NO_UP);
 tolua_constant(tolua_S,"DF1_HOT",DF1_HOT);
 tolua_constant(tolua_S,"DF1_COLD",DF1_COLD);
 tolua_constant(tolua_S,"DF1_NO_DOWN",DF1_NO_DOWN);
 tolua_constant(tolua_S,"DF1_FORGET",DF1_FORGET);
 tolua_constant(tolua_S,"DF1_UNDEAD",DF1_UNDEAD);
 tolua_constant(tolua_S,"DF1_DEMON",DF1_DEMON);
 tolua_constant(tolua_S,"DF1_DRAGON",DF1_DRAGON);
 tolua_constant(tolua_S,"DF1_NO_GENERIC",DF1_NO_GENERIC);
 tolua_constant(tolua_S,"DF1_ICE",DF1_ICE);
 tolua_constant(tolua_S,"DF1_WEIRD",DF1_WEIRD);
 tolua_constant(tolua_S,"DF1_RANDOM_ONLY",DF1_RANDOM_ONLY);
 tolua_constant(tolua_S,"IDENT_SENSE",IDENT_SENSE);
 tolua_constant(tolua_S,"IDENT_FIXED",IDENT_FIXED);
 tolua_constant(tolua_S,"IDENT_EMPTY",IDENT_EMPTY);
 tolua_constant(tolua_S,"IDENT_KNOWN",IDENT_KNOWN);
 tolua_constant(tolua_S,"IDENT_STOREB",IDENT_STOREB);
 tolua_constant(tolua_S,"IDENT_MENTAL",IDENT_MENTAL);
 tolua_constant(tolua_S,"IDENT_CURSED",IDENT_CURSED);
 tolua_constant(tolua_S,"IDENT_BROKEN",IDENT_BROKEN);
 tolua_constant(tolua_S,"MAX_VAULTS",MAX_VAULTS);
 tolua_function(tolua_S,"notice_stuff",tolua_init_notice_stuff00);
 tolua_function(tolua_S,"update_stuff",tolua_init_update_stuff00);
 tolua_function(tolua_S,"redraw_stuff",tolua_init_redraw_stuff00);
 tolua_function(tolua_S,"window_stuff",tolua_init_window_stuff00);
 tolua_function(tolua_S,"handle_stuff",tolua_init_handle_stuff00);
 tolua_function(tolua_S,"update_and_handle",tolua_init_update_and_handle00);
 tolua_function(tolua_S,"inkey",tolua_init_inkey00);
 tolua_function(tolua_S,"bell",tolua_init_bell00);
 tolua_function(tolua_S,"sound",tolua_init_sound00);
 tolua_function(tolua_S,"msg_print",tolua_init_msg_print00);
 tolua_function(tolua_S,"screen_save",tolua_init_screen_save00);
 tolua_function(tolua_S,"screen_load",tolua_init_screen_load00);
 tolua_function(tolua_S,"c_put_str",tolua_init_c_put_str00);
 tolua_function(tolua_S,"put_str",tolua_init_put_str00);
 tolua_function(tolua_S,"c_prt",tolua_init_c_prt00);
 tolua_function(tolua_S,"prt",tolua_init_prt00);
 tolua_function(tolua_S,"clear_from",tolua_init_clear_from00);
 tolua_function(tolua_S,"pause_line",tolua_init_pause_line00);
 tolua_function(tolua_S,"request_command",tolua_init_request_command00);
 tolua_function(tolua_S,"askfor_aux",tolua_init_askfor_aux00);
 tolua_function(tolua_S,"get_string",tolua_init_get_string00);
 tolua_function(tolua_S,"get_check",tolua_init_get_check00);
 tolua_function(tolua_S,"get_com",tolua_init_get_com00);
 tolua_function(tolua_S,"get_quantity",tolua_init_get_quantity00);
 tolua_function(tolua_S,"Term_clear",tolua_init_Term_clear00);
 tolua_function(tolua_S,"flush",tolua_init_flush00);
 tolua_function(tolua_S,"process_dialog",tolua_init_process_dialog00);
 tolua_function(tolua_S,"show_dialog",tolua_init_show_dialog00);
 tolua_function(tolua_S,"get_quantity_s32b",tolua_init_get_quantity_s32b00);
 tolua_function(tolua_S,"script_do_file",tolua_init_script_do_file00);
 tolua_function(tolua_S,"lua_randint",tolua_init_lua_randint00);
 tolua_function(tolua_S,"distance",tolua_init_distance00);
 tolua_function(tolua_S,"los",tolua_init_los00);
 tolua_function(tolua_S,"player_can_see_bold",tolua_init_player_can_see_bold00);
 tolua_function(tolua_S,"cave_valid_bold",tolua_init_cave_valid_bold00);
 tolua_function(tolua_S,"no_lite",tolua_init_no_lite00);
 tolua_function(tolua_S,"move_cursor_relative",tolua_init_move_cursor_relative00);
 tolua_function(tolua_S,"print_rel",tolua_init_print_rel00);
 tolua_function(tolua_S,"note_spot",tolua_init_note_spot00);
 tolua_function(tolua_S,"lite_spot",tolua_init_lite_spot00);
 tolua_function(tolua_S,"prt_map",tolua_init_prt_map00);
 tolua_function(tolua_S,"display_map",tolua_init_display_map00);
 tolua_function(tolua_S,"do_cmd_view_map",tolua_init_do_cmd_view_map00);
 tolua_function(tolua_S,"forget_lite",tolua_init_forget_lite00);
 tolua_function(tolua_S,"update_lite",tolua_init_update_lite00);
 tolua_function(tolua_S,"forget_view",tolua_init_forget_view00);
 tolua_function(tolua_S,"update_view",tolua_init_update_view00);
 tolua_function(tolua_S,"forget_flow",tolua_init_forget_flow00);
 tolua_function(tolua_S,"update_flow",tolua_init_update_flow00);
 tolua_function(tolua_S,"map_area",tolua_init_map_area00);
 tolua_function(tolua_S,"wiz_lite",tolua_init_wiz_lite00);
 tolua_function(tolua_S,"wiz_lite_extra",tolua_init_wiz_lite_extra00);
 tolua_function(tolua_S,"wiz_dark",tolua_init_wiz_dark00);
 tolua_function(tolua_S,"cave_set_feat",tolua_init_cave_set_feat00);
 tolua_function(tolua_S,"mmove2",tolua_init_mmove200);
 tolua_function(tolua_S,"projectable",tolua_init_projectable00);
 tolua_function(tolua_S,"scatter",tolua_init_scatter00);
 tolua_function(tolua_S,"health_track",tolua_init_health_track00);
 tolua_function(tolua_S,"monster_race_track",tolua_init_monster_race_track00);
 tolua_function(tolua_S,"object_kind_track",tolua_init_object_kind_track00);
 tolua_function(tolua_S,"disturb",tolua_init_disturb00);
 tolua_function(tolua_S,"is_quest",tolua_init_is_quest00);
 tolua_function(tolua_S,"tot_dam_aux",tolua_init_tot_dam_aux00);
 tolua_function(tolua_S,"search",tolua_init_search00);
 tolua_function(tolua_S,"carry",tolua_init_carry00);
 tolua_function(tolua_S,"py_attack",tolua_init_py_attack00);
 tolua_function(tolua_S,"player_can_enter",tolua_init_player_can_enter00);
 tolua_function(tolua_S,"move_player",tolua_init_move_player00);
 tolua_function(tolua_S,"run_step",tolua_init_run_step00);
 tolua_function(tolua_S,"step_effects",tolua_init_step_effects00);
 tolua_function(tolua_S,"do_cmd_pet",tolua_init_do_cmd_pet00);
 tolua_function(tolua_S,"incarnate_monster_attack",tolua_init_incarnate_monster_attack00);
 tolua_function(tolua_S,"critical_hits",tolua_init_critical_hits00);
 tolua_function(tolua_S,"standing_on_forest",tolua_init_standing_on_forest00);
 tolua_function(tolua_S,"do_cmd_go_up",tolua_init_do_cmd_go_up00);
 tolua_function(tolua_S,"do_cmd_go_down",tolua_init_do_cmd_go_down00);
 tolua_function(tolua_S,"do_cmd_search",tolua_init_do_cmd_search00);
 tolua_function(tolua_S,"do_cmd_toggle_search",tolua_init_do_cmd_toggle_search00);
 tolua_function(tolua_S,"do_cmd_open",tolua_init_do_cmd_open00);
 tolua_function(tolua_S,"do_cmd_close",tolua_init_do_cmd_close00);
 tolua_function(tolua_S,"do_cmd_tunnel",tolua_init_do_cmd_tunnel00);
 tolua_function(tolua_S,"do_cmd_tunnel_aux",tolua_init_do_cmd_tunnel_aux00);
 tolua_function(tolua_S,"do_cmd_disarm",tolua_init_do_cmd_disarm00);
 tolua_function(tolua_S,"do_cmd_bash",tolua_init_do_cmd_bash00);
 tolua_function(tolua_S,"do_cmd_alter",tolua_init_do_cmd_alter00);
 tolua_function(tolua_S,"do_cmd_spike",tolua_init_do_cmd_spike00);
 tolua_function(tolua_S,"do_cmd_walk",tolua_init_do_cmd_walk00);
 tolua_function(tolua_S,"do_cmd_stay",tolua_init_do_cmd_stay00);
 tolua_function(tolua_S,"do_cmd_run",tolua_init_do_cmd_run00);
 tolua_function(tolua_S,"do_cmd_rest",tolua_init_do_cmd_rest00);
 tolua_function(tolua_S,"do_cmd_fire",tolua_init_do_cmd_fire00);
 tolua_function(tolua_S,"do_cmd_steal",tolua_init_do_cmd_steal00);
 tolua_function(tolua_S,"use_monster_ranged_attack",tolua_init_use_monster_ranged_attack00);
 tolua_function(tolua_S,"choose_current_weapon",tolua_init_choose_current_weapon00);
 tolua_function(tolua_S,"use_hardcode_ability",tolua_init_use_hardcode_ability00);
 tolua_function(tolua_S,"reload_ranged",tolua_init_reload_ranged00);
 tolua_function(tolua_S,"throw_select",tolua_init_throw_select00);
 tolua_function(tolua_S,"alchemy_brand",tolua_init_alchemy_brand00);
 tolua_function(tolua_S,"alchemy_resist",tolua_init_alchemy_resist00);
 tolua_function(tolua_S,"essence_transfer",tolua_init_essence_transfer00);
 tolua_function(tolua_S,"defense_transfer",tolua_init_defense_transfer00);
 tolua_function(tolua_S,"diviner_wish",tolua_init_diviner_wish00);
 tolua_function(tolua_S,"do_cmd_inven",tolua_init_do_cmd_inven00);
 tolua_function(tolua_S,"do_cmd_equip",tolua_init_do_cmd_equip00);
 tolua_function(tolua_S,"do_cmd_wield",tolua_init_do_cmd_wield00);
 tolua_function(tolua_S,"do_cmd_takeoff",tolua_init_do_cmd_takeoff00);
 tolua_function(tolua_S,"do_cmd_drop",tolua_init_do_cmd_drop00);
 tolua_function(tolua_S,"do_cmd_destroy",tolua_init_do_cmd_destroy00);
 tolua_function(tolua_S,"do_cmd_observe",tolua_init_do_cmd_observe00);
 tolua_function(tolua_S,"do_cmd_uninscribe",tolua_init_do_cmd_uninscribe00);
 tolua_function(tolua_S,"do_cmd_inscribe",tolua_init_do_cmd_inscribe00);
 tolua_function(tolua_S,"do_cmd_refill",tolua_init_do_cmd_refill00);
 tolua_function(tolua_S,"do_cmd_target",tolua_init_do_cmd_target00);
 tolua_function(tolua_S,"do_cmd_look",tolua_init_do_cmd_look00);
 tolua_function(tolua_S,"do_cmd_locate",tolua_init_do_cmd_locate00);
 tolua_function(tolua_S,"do_cmd_locate_center",tolua_init_do_cmd_locate_center00);
 tolua_function(tolua_S,"do_cmd_query_symbol",tolua_init_do_cmd_query_symbol00);
 tolua_function(tolua_S,"do_cmd_racial_power",tolua_init_do_cmd_racial_power00);
 tolua_function(tolua_S,"research_mon",tolua_init_research_mon00);
 tolua_function(tolua_S,"do_cmd_sense_grid_mana",tolua_init_do_cmd_sense_grid_mana00);
 tolua_function(tolua_S,"max_carry",tolua_init_max_carry00);
 tolua_function(tolua_S,"summoned_item",tolua_init_summoned_item00);
 tolua_function(tolua_S,"one_weapon_wield",tolua_init_one_weapon_wield00);
 tolua_function(tolua_S,"two_weapon_wield",tolua_init_two_weapon_wield00);
 tolua_function(tolua_S,"do_cmd_redraw",tolua_init_do_cmd_redraw00);
 tolua_function(tolua_S,"do_cmd_change_name",tolua_init_do_cmd_change_name00);
 tolua_function(tolua_S,"do_cmd_message_one",tolua_init_do_cmd_message_one00);
 tolua_function(tolua_S,"do_cmd_messages",tolua_init_do_cmd_messages00);
 tolua_function(tolua_S,"do_cmd_options",tolua_init_do_cmd_options00);
 tolua_function(tolua_S,"do_cmd_pref",tolua_init_do_cmd_pref00);
 tolua_function(tolua_S,"do_cmd_macros",tolua_init_do_cmd_macros00);
 tolua_function(tolua_S,"do_cmd_visuals",tolua_init_do_cmd_visuals00);
 tolua_function(tolua_S,"do_cmd_colors",tolua_init_do_cmd_colors00);
 tolua_function(tolua_S,"do_cmd_note",tolua_init_do_cmd_note00);
 tolua_function(tolua_S,"do_cmd_version",tolua_init_do_cmd_version00);
 tolua_function(tolua_S,"do_cmd_feeling",tolua_init_do_cmd_feeling00);
 tolua_function(tolua_S,"do_cmd_load_screen",tolua_init_do_cmd_load_screen00);
 tolua_function(tolua_S,"do_cmd_save_screen",tolua_init_do_cmd_save_screen00);
 tolua_function(tolua_S,"do_cmd_knowledge",tolua_init_do_cmd_knowledge00);
 tolua_function(tolua_S,"plural_aux",tolua_init_plural_aux00);
 tolua_function(tolua_S,"do_cmd_time",tolua_init_do_cmd_time00);
 tolua_function(tolua_S,"do_cmd_options_aux",tolua_init_do_cmd_options_aux00);
 tolua_function(tolua_S,"do_cmd_study",tolua_init_do_cmd_study00);
 tolua_function(tolua_S,"mutate_player",tolua_init_mutate_player00);
 tolua_function(tolua_S,"item_tester_hook_armour",tolua_init_item_tester_hook_armour00);
 tolua_function(tolua_S,"use_body_power",tolua_init_use_body_power00);
 tolua_function(tolua_S,"conjure_item",tolua_init_conjure_item00);
 tolua_function(tolua_S,"conjure_item_any",tolua_init_conjure_item_any00);
 tolua_function(tolua_S,"place_field",tolua_init_place_field00);
 tolua_function(tolua_S,"recharge_crystal",tolua_init_recharge_crystal00);
 tolua_function(tolua_S,"object_eternality",tolua_init_object_eternality00);
 tolua_function(tolua_S,"make_item_magic",tolua_init_make_item_magic00);
 tolua_function(tolua_S,"make_item_levelable",tolua_init_make_item_levelable00);
 tolua_function(tolua_S,"place_field_monsters",tolua_init_place_field_monsters00);
 tolua_function(tolua_S,"do_cmd_quaff_potion",tolua_init_do_cmd_quaff_potion00);
 tolua_function(tolua_S,"do_cmd_read_scroll",tolua_init_do_cmd_read_scroll00);
 tolua_function(tolua_S,"do_cmd_aim_wand",tolua_init_do_cmd_aim_wand00);
 tolua_function(tolua_S,"do_cmd_use_staff",tolua_init_do_cmd_use_staff00);
 tolua_function(tolua_S,"do_cmd_zap_rod",tolua_init_do_cmd_zap_rod00);
 tolua_function(tolua_S,"do_cmd_activate",tolua_init_do_cmd_activate00);
 tolua_function(tolua_S,"place_object",tolua_init_place_object00);
 tolua_function(tolua_S,"place_gold",tolua_init_place_gold00);
 tolua_function(tolua_S,"object_value",tolua_init_object_value00);
 tolua_function(tolua_S,"object_value_real",tolua_init_object_value_real00);
 tolua_function(tolua_S,"is_identified",tolua_init_is_identified00);
 tolua_function(tolua_S,"lua_pick_item",tolua_init_lua_pick_item00);
 tolua_function(tolua_S,"lua_get_item",tolua_init_lua_get_item00);
 tolua_function(tolua_S,"get_obj_num_tval",tolua_init_get_obj_num_tval00);
 tolua_function(tolua_S,"make_object_tval",tolua_init_make_object_tval00);
 tolua_function(tolua_S,"place_object_tval",tolua_init_place_object_tval00);
 tolua_function(tolua_S,"drop_global_object",tolua_init_drop_global_object00);
 tolua_function(tolua_S,"drop_object_specific",tolua_init_drop_object_specific00);
 tolua_variable(tolua_S,"summoner_monster",tolua_get_summoner_monster_ptr,tolua_set_summoner_monster_ptr);
 tolua_function(tolua_S,"get_wilderness_flag",tolua_init_get_wilderness_flag00);
 tolua_function(tolua_S,"delete_monster_idx",tolua_init_delete_monster_idx00);
 tolua_function(tolua_S,"delete_monster",tolua_init_delete_monster00);
 tolua_function(tolua_S,"compact_monsters",tolua_init_compact_monsters00);
 tolua_function(tolua_S,"wipe_m_list",tolua_init_wipe_m_list00);
 tolua_function(tolua_S,"m_pop",tolua_init_m_pop00);
 tolua_function(tolua_S,"get_mon_num_prep",tolua_init_get_mon_num_prep00);
 tolua_function(tolua_S,"get_mon_num",tolua_init_get_mon_num00);
 tolua_function(tolua_S,"monster_desc",tolua_init_monster_desc00);
 tolua_function(tolua_S,"monster_race_desc",tolua_init_monster_race_desc00);
 tolua_function(tolua_S,"lore_do_probe",tolua_init_lore_do_probe00);
 tolua_function(tolua_S,"lore_treasure",tolua_init_lore_treasure00);
 tolua_function(tolua_S,"update_mon",tolua_init_update_mon00);
 tolua_function(tolua_S,"update_monsters",tolua_init_update_monsters00);
 tolua_function(tolua_S,"place_monster_aux",tolua_init_place_monster_aux00);
 tolua_function(tolua_S,"place_monster",tolua_init_place_monster00);
 tolua_function(tolua_S,"alloc_horde",tolua_init_alloc_horde00);
 tolua_function(tolua_S,"alloc_monster",tolua_init_alloc_monster00);
 tolua_function(tolua_S,"summon_specific",tolua_init_summon_specific00);
 tolua_function(tolua_S,"monster_swap",tolua_init_monster_swap00);
 tolua_function(tolua_S,"multiply_monster",tolua_init_multiply_monster00);
 tolua_function(tolua_S,"update_smart_learn",tolua_init_update_smart_learn00);
 tolua_function(tolua_S,"summon_specific_friendly",tolua_init_summon_specific_friendly00);
 tolua_function(tolua_S,"place_monster_one",tolua_init_place_monster_one00);
 tolua_function(tolua_S,"place_monster_one_return",tolua_init_place_monster_one_return00);
 tolua_function(tolua_S,"player_place",tolua_init_player_place00);
 tolua_function(tolua_S,"monster_drop_carried_objects",tolua_init_monster_drop_carried_objects00);
 tolua_function(tolua_S,"apply_monster_level_hp",tolua_init_apply_monster_level_hp00);
 tolua_function(tolua_S,"get_boss_ability",tolua_init_get_boss_ability00);
 tolua_function(tolua_S,"place_monster_one_no_boss",tolua_init_place_monster_one_no_boss00);
 tolua_function(tolua_S,"place_monster_one_return_no_boss",tolua_init_place_monster_one_return_no_boss00);
 tolua_function(tolua_S,"place_monster_aux_no_boss",tolua_init_place_monster_aux_no_boss00);
 tolua_function(tolua_S,"place_monster_one_simulacrum",tolua_init_place_monster_one_simulacrum00);
 tolua_function(tolua_S,"summon_specific_friendly_kind",tolua_init_summon_specific_friendly_kind00);
 tolua_function(tolua_S,"get_mon_num_kind",tolua_init_get_mon_num_kind00);
 tolua_function(tolua_S,"summon_specific_friendly_name",tolua_init_summon_specific_friendly_name00);
 tolua_function(tolua_S,"get_mon_num_name",tolua_init_get_mon_num_name00);
 tolua_function(tolua_S,"place_monster_one_image",tolua_init_place_monster_one_image00);
 tolua_function(tolua_S,"place_monster_animated",tolua_init_place_monster_animated00);
 tolua_function(tolua_S,"summon_specific_kind",tolua_init_summon_specific_kind00);
 tolua_function(tolua_S,"summon_specific_ridx",tolua_init_summon_specific_ridx00);
 tolua_function(tolua_S,"apply_monster_level_stats",tolua_init_apply_monster_level_stats00);
 tolua_function(tolua_S,"is_pet",tolua_init_is_pet00);
 tolua_function(tolua_S,"set_pet",tolua_init_set_pet00);
 tolua_function(tolua_S,"boss_of_global_object",tolua_init_boss_of_global_object00);
 tolua_function(tolua_S,"get_mon_num_rflag",tolua_init_get_mon_num_rflag00);
 tolua_function(tolua_S,"summon_specific_rflag",tolua_init_summon_specific_rflag00);
 tolua_function(tolua_S,"get_race_kills",tolua_init_get_race_kills00);
 tolua_function(tolua_S,"shield_has",tolua_init_shield_has00);
 tolua_function(tolua_S,"sword_has",tolua_init_sword_has00);
 tolua_function(tolua_S,"hafted_has",tolua_init_hafted_has00);
 tolua_function(tolua_S,"polearm_has",tolua_init_polearm_has00);
 tolua_function(tolua_S,"rod_has",tolua_init_rod_has00);
 tolua_function(tolua_S,"unarmed",tolua_init_unarmed00);
 tolua_function(tolua_S,"heavy_armor",tolua_init_heavy_armor00);
 tolua_function(tolua_S,"player_invis",tolua_init_player_invis00);
 tolua_function(tolua_S,"lua_bolt",tolua_init_lua_bolt00);
 tolua_function(tolua_S,"lua_ball",tolua_init_lua_ball00);
 tolua_function(tolua_S,"poly_r_idx",tolua_init_poly_r_idx00);
 tolua_function(tolua_S,"get_pos_player",tolua_init_get_pos_player00);
 tolua_function(tolua_S,"teleport_to_player",tolua_init_teleport_to_player00);
 tolua_function(tolua_S,"teleport_player_directed",tolua_init_teleport_player_directed00);
 tolua_function(tolua_S,"teleport_away",tolua_init_teleport_away00);
 tolua_function(tolua_S,"teleport_player",tolua_init_teleport_player00);
 tolua_function(tolua_S,"teleport_player_to",tolua_init_teleport_player_to00);
 tolua_function(tolua_S,"teleport_monster_to",tolua_init_teleport_monster_to00);
 tolua_function(tolua_S,"teleport_player_level",tolua_init_teleport_player_level00);
 tolua_function(tolua_S,"recall_player",tolua_init_recall_player00);
 tolua_function(tolua_S,"take_hit",tolua_init_take_hit00);
 tolua_function(tolua_S,"acid_dam",tolua_init_acid_dam00);
 tolua_function(tolua_S,"elec_dam",tolua_init_elec_dam00);
 tolua_function(tolua_S,"fire_dam",tolua_init_fire_dam00);
 tolua_function(tolua_S,"cold_dam",tolua_init_cold_dam00);
 tolua_function(tolua_S,"inc_stat",tolua_init_inc_stat00);
 tolua_function(tolua_S,"dec_stat",tolua_init_dec_stat00);
 tolua_function(tolua_S,"res_stat",tolua_init_res_stat00);
 tolua_function(tolua_S,"apply_disenchant",tolua_init_apply_disenchant00);
 tolua_function(tolua_S,"project_m",tolua_init_project_m00);
 tolua_function(tolua_S,"project_path",tolua_init_project_path00);
 tolua_function(tolua_S,"project",tolua_init_project00);
 tolua_function(tolua_S,"mutate_player",tolua_init_mutate_player01);
 tolua_function(tolua_S,"generate_spell",tolua_init_generate_spell00);
 tolua_variable(tolua_S,"unsafe",tolua_get_unsafe,tolua_set_unsafe);
 tolua_function(tolua_S,"describe_attack_fully",tolua_init_describe_attack_fully00);
 tolua_function(tolua_S,"lord_piercing",tolua_init_lord_piercing00);
 tolua_function(tolua_S,"grow_trees",tolua_init_grow_trees00);
 tolua_function(tolua_S,"hp_player",tolua_init_hp_player00);
 tolua_function(tolua_S,"warding_glyph",tolua_init_warding_glyph00);
 tolua_function(tolua_S,"explosive_rune",tolua_init_explosive_rune00);
 tolua_function(tolua_S,"do_dec_stat",tolua_init_do_dec_stat00);
 tolua_function(tolua_S,"do_res_stat",tolua_init_do_res_stat00);
 tolua_function(tolua_S,"do_inc_stat",tolua_init_do_inc_stat00);
 tolua_function(tolua_S,"identify_pack",tolua_init_identify_pack00);
 tolua_function(tolua_S,"message_pain",tolua_init_message_pain00);
 tolua_function(tolua_S,"remove_curse",tolua_init_remove_curse00);
 tolua_function(tolua_S,"remove_all_curse",tolua_init_remove_all_curse00);
 tolua_function(tolua_S,"restore_level",tolua_init_restore_level00);
 tolua_function(tolua_S,"self_knowledge",tolua_init_self_knowledge00);
 tolua_function(tolua_S,"lose_all_info",tolua_init_lose_all_info00);
 tolua_function(tolua_S,"detect_traps",tolua_init_detect_traps00);
 tolua_function(tolua_S,"detect_doors",tolua_init_detect_doors00);
 tolua_function(tolua_S,"detect_stairs",tolua_init_detect_stairs00);
 tolua_function(tolua_S,"detect_treasure",tolua_init_detect_treasure00);
 tolua_function(tolua_S,"detect_objects_gold",tolua_init_detect_objects_gold00);
 tolua_function(tolua_S,"detect_objects_normal",tolua_init_detect_objects_normal00);
 tolua_function(tolua_S,"detect_objects_magic",tolua_init_detect_objects_magic00);
 tolua_function(tolua_S,"detect_monsters_normal",tolua_init_detect_monsters_normal00);
 tolua_function(tolua_S,"detect_monsters_invis",tolua_init_detect_monsters_invis00);
 tolua_function(tolua_S,"detect_monsters_evil",tolua_init_detect_monsters_evil00);
 tolua_function(tolua_S,"detect_monsters_xxx",tolua_init_detect_monsters_xxx00);
 tolua_function(tolua_S,"detect_monsters_string",tolua_init_detect_monsters_string00);
 tolua_function(tolua_S,"detect_monsters_nonliving",tolua_init_detect_monsters_nonliving00);
 tolua_function(tolua_S,"detect_all",tolua_init_detect_all00);
 tolua_function(tolua_S,"stair_creation",tolua_init_stair_creation00);
 tolua_function(tolua_S,"wall_stone",tolua_init_wall_stone00);
 tolua_function(tolua_S,"ident_spell",tolua_init_ident_spell00);
 tolua_function(tolua_S,"identify_fully",tolua_init_identify_fully00);
 tolua_function(tolua_S,"recharge",tolua_init_recharge00);
 tolua_function(tolua_S,"speed_monsters",tolua_init_speed_monsters00);
 tolua_function(tolua_S,"slow_monsters",tolua_init_slow_monsters00);
 tolua_function(tolua_S,"sleep_monsters",tolua_init_sleep_monsters00);
 tolua_function(tolua_S,"conf_monsters",tolua_init_conf_monsters00);
 tolua_function(tolua_S,"aggravate_monsters",tolua_init_aggravate_monsters00);
 tolua_function(tolua_S,"genocide",tolua_init_genocide00);
 tolua_function(tolua_S,"mass_genocide",tolua_init_mass_genocide00);
 tolua_function(tolua_S,"probing",tolua_init_probing00);
 tolua_function(tolua_S,"change_wild_mode",tolua_init_change_wild_mode00);
 tolua_function(tolua_S,"banish_evil",tolua_init_banish_evil00);
 tolua_function(tolua_S,"dispel_evil",tolua_init_dispel_evil00);
 tolua_function(tolua_S,"dispel_good",tolua_init_dispel_good00);
 tolua_function(tolua_S,"dispel_undead",tolua_init_dispel_undead00);
 tolua_function(tolua_S,"dispel_monsters",tolua_init_dispel_monsters00);
 tolua_function(tolua_S,"dispel_living",tolua_init_dispel_living00);
 tolua_function(tolua_S,"dispel_demons",tolua_init_dispel_demons00);
 tolua_function(tolua_S,"turn_undead",tolua_init_turn_undead00);
 tolua_function(tolua_S,"destroy_area",tolua_init_destroy_area00);
 tolua_function(tolua_S,"earthquake",tolua_init_earthquake00);
 tolua_function(tolua_S,"lite_room",tolua_init_lite_room00);
 tolua_function(tolua_S,"unlite_room",tolua_init_unlite_room00);
 tolua_function(tolua_S,"lite_area",tolua_init_lite_area00);
 tolua_function(tolua_S,"unlite_area",tolua_init_unlite_area00);
 tolua_function(tolua_S,"fire_ball_beam",tolua_init_fire_ball_beam00);
 tolua_function(tolua_S,"fire_ball",tolua_init_fire_ball00);
 tolua_function(tolua_S,"fire_bolt",tolua_init_fire_bolt00);
 tolua_function(tolua_S,"fire_beam",tolua_init_fire_beam00);
 tolua_function(tolua_S,"fire_bolt_or_beam",tolua_init_fire_bolt_or_beam00);
 tolua_function(tolua_S,"lite_line",tolua_init_lite_line00);
 tolua_function(tolua_S,"drain_life",tolua_init_drain_life00);
 tolua_function(tolua_S,"death_ray",tolua_init_death_ray00);
 tolua_function(tolua_S,"wall_to_mud",tolua_init_wall_to_mud00);
 tolua_function(tolua_S,"destroy_door",tolua_init_destroy_door00);
 tolua_function(tolua_S,"disarm_trap",tolua_init_disarm_trap00);
 tolua_function(tolua_S,"wizard_lock",tolua_init_wizard_lock00);
 tolua_function(tolua_S,"heal_monster",tolua_init_heal_monster00);
 tolua_function(tolua_S,"speed_monster",tolua_init_speed_monster00);
 tolua_function(tolua_S,"slow_monster",tolua_init_slow_monster00);
 tolua_function(tolua_S,"sleep_monster",tolua_init_sleep_monster00);
 tolua_function(tolua_S,"confuse_monster",tolua_init_confuse_monster00);
 tolua_function(tolua_S,"stun_monster",tolua_init_stun_monster00);
 tolua_function(tolua_S,"fear_monster",tolua_init_fear_monster00);
 tolua_function(tolua_S,"scare_monsters",tolua_init_scare_monsters00);
 tolua_function(tolua_S,"poly_monster",tolua_init_poly_monster00);
 tolua_function(tolua_S,"clone_monster",tolua_init_clone_monster00);
 tolua_function(tolua_S,"teleport_monster",tolua_init_teleport_monster00);
 tolua_function(tolua_S,"door_creation",tolua_init_door_creation00);
 tolua_function(tolua_S,"trap_creation",tolua_init_trap_creation00);
 tolua_function(tolua_S,"glyph_creation",tolua_init_glyph_creation00);
 tolua_function(tolua_S,"destroy_doors_touch",tolua_init_destroy_doors_touch00);
 tolua_function(tolua_S,"sleep_monsters_touch",tolua_init_sleep_monsters_touch00);
 tolua_function(tolua_S,"alchemy",tolua_init_alchemy00);
 tolua_function(tolua_S,"wall_breaker",tolua_init_wall_breaker00);
 tolua_function(tolua_S,"bless_weapon",tolua_init_bless_weapon00);
 tolua_function(tolua_S,"confuse_monsters",tolua_init_confuse_monsters00);
 tolua_function(tolua_S,"charm_animals",tolua_init_charm_animals00);
 tolua_function(tolua_S,"stun_monsters",tolua_init_stun_monsters00);
 tolua_function(tolua_S,"banish_monsters",tolua_init_banish_monsters00);
 tolua_function(tolua_S,"turn_monsters",tolua_init_turn_monsters00);
 tolua_function(tolua_S,"turn_evil",tolua_init_turn_evil00);
 tolua_function(tolua_S,"deathray_monsters",tolua_init_deathray_monsters00);
 tolua_function(tolua_S,"control_one_undead",tolua_init_control_one_undead00);
 tolua_function(tolua_S,"charm_animal",tolua_init_charm_animal00);
 tolua_function(tolua_S,"get_table_name",tolua_init_get_table_name00);
 tolua_function(tolua_S,"flag_cost",tolua_init_flag_cost00);
 tolua_function(tolua_S,"alter_reality",tolua_init_alter_reality00);
 tolua_function(tolua_S,"report_magics",tolua_init_report_magics00);
 tolua_function(tolua_S,"teleport_swap",tolua_init_teleport_swap00);
 tolua_function(tolua_S,"swap_position",tolua_init_swap_position00);
 tolua_function(tolua_S,"item_tester_hook_recharge",tolua_init_item_tester_hook_recharge00);
 tolua_function(tolua_S,"get_activation_power",tolua_init_get_activation_power00);
 tolua_function(tolua_S,"invoke",tolua_init_invoke00);
 tolua_function(tolua_S,"project_hack",tolua_init_project_hack00);
 tolua_function(tolua_S,"project_meteor",tolua_init_project_meteor00);
 tolua_function(tolua_S,"item_tester_hook_artifactable",tolua_init_item_tester_hook_artifactable00);
 tolua_function(tolua_S,"project_hook",tolua_init_project_hook00);
 tolua_function(tolua_S,"random_misc",tolua_init_random_misc00);
 tolua_function(tolua_S,"random_plus",tolua_init_random_plus00);
 tolua_function(tolua_S,"fire_jump_ball",tolua_init_fire_jump_ball00);
 tolua_function(tolua_S,"mass_change_allegiance",tolua_init_mass_change_allegiance00);
 tolua_function(tolua_S,"chain_attack",tolua_init_chain_attack00);
 tolua_function(tolua_S,"chain_attack_fields",tolua_init_chain_attack_fields00);
 tolua_function(tolua_S,"fire_ball_specific_grid",tolua_init_fire_ball_specific_grid00);
 tolua_function(tolua_S,"hard_kick",tolua_init_hard_kick00);
 tolua_function(tolua_S,"identify_fully_specific",tolua_init_identify_fully_specific00);
 tolua_function(tolua_S,"multiply_divide",tolua_init_multiply_divide00);
 tolua_function(tolua_S,"safety_check",tolua_init_safety_check00);
 tolua_function(tolua_S,"modify_stat_value",tolua_init_modify_stat_value00);
 tolua_function(tolua_S,"weight_limit",tolua_init_weight_limit00);
 tolua_function(tolua_S,"calc_skills",tolua_init_calc_skills00);
 tolua_function(tolua_S,"calc_stats",tolua_init_calc_stats00);
 tolua_function(tolua_S,"calc_equipment",tolua_init_calc_equipment00);
 tolua_function(tolua_S,"calc_resistances",tolua_init_calc_resistances00);
 tolua_function(tolua_S,"calc_cursed",tolua_init_calc_cursed00);
 tolua_function(tolua_S,"set_powerattack",tolua_init_set_powerattack00);
 tolua_function(tolua_S,"mon_take_hit",tolua_init_mon_take_hit00);
 tolua_function(tolua_S,"tgt_pt",tolua_init_tgt_pt00);
 tolua_function(tolua_S,"dagger_check",tolua_init_dagger_check00);
 tolua_function(tolua_S,"axe_check",tolua_init_axe_check00);
 tolua_function(tolua_S,"activate_item",tolua_init_activate_item00);
 tolua_function(tolua_S,"pick_spell",tolua_init_pick_spell00);
 tolua_function(tolua_S,"fate_monsters",tolua_init_fate_monsters00);
 tolua_function(tolua_S,"fate_items",tolua_init_fate_items00);
 tolua_function(tolua_S,"pick_song",tolua_init_pick_song00);
 tolua_function(tolua_S,"is_elemental",tolua_init_is_elemental00);
 tolua_function(tolua_S,"is_alteration",tolua_init_is_alteration00);
 tolua_function(tolua_S,"is_mysticism",tolua_init_is_mysticism00);
 tolua_function(tolua_S,"is_divination",tolua_init_is_divination00);
 tolua_function(tolua_S,"generate_town",tolua_init_generate_town00);
 tolua_function(tolua_S,"generate_quest",tolua_init_generate_quest00);
 tolua_function(tolua_S,"generate_wilderness",tolua_init_generate_wilderness00);
 tolua_function(tolua_S,"generate_vault",tolua_init_generate_vault00);
 tolua_function(tolua_S,"quest_artifact_prep",tolua_init_quest_artifact_prep00);
 tolua_function(tolua_S,"lua_update_monsters",tolua_init_lua_update_monsters00);
 tolua_function(tolua_S,"lua_update_stuff",tolua_init_lua_update_stuff00);
 tolua_function(tolua_S,"lua_get_aim_dir",tolua_init_lua_get_aim_dir00);
 tolua_function(tolua_S,"lua_get_rep_dir",tolua_init_lua_get_rep_dir00);
 tolua_function(tolua_S,"lua_cave_empty_bold",tolua_init_lua_cave_empty_bold00);
 tolua_function(tolua_S,"get_cave_info_flag",tolua_init_get_cave_info_flag00);
 tolua_function(tolua_S,"lua_tgt_pt",tolua_init_lua_tgt_pt00);
 tolua_function(tolua_S,"lua_in_bounds",tolua_init_lua_in_bounds00);
 tolua_function(tolua_S,"lua_in_bounds2",tolua_init_lua_in_bounds200);
 tolua_function(tolua_S,"lua_player_has_los_bold",tolua_init_lua_player_has_los_bold00);
 tolua_function(tolua_S,"lua_project",tolua_init_lua_project00);
 tolua_function(tolua_S,"memorize_race_flag1",tolua_init_memorize_race_flag100);
 tolua_function(tolua_S,"memorize_race_flag2",tolua_init_memorize_race_flag200);
 tolua_function(tolua_S,"memorize_race_flag3",tolua_init_memorize_race_flag300);
 tolua_function(tolua_S,"memorize_race_flag4",tolua_init_memorize_race_flag400);
 tolua_function(tolua_S,"memorize_race_flag5",tolua_init_memorize_race_flag500);
 tolua_function(tolua_S,"memorize_race_flag6",tolua_init_memorize_race_flag600);
 tolua_function(tolua_S,"memorize_race_flag7",tolua_init_memorize_race_flag700);
 tolua_function(tolua_S,"memorize_race_flag8",tolua_init_memorize_race_flag800);
 tolua_function(tolua_S,"memorize_race_flag9",tolua_init_memorize_race_flag900);
 tolua_function(tolua_S,"give_monster_ability",tolua_init_give_monster_ability00);
 tolua_function(tolua_S,"remove_monster_ability",tolua_init_remove_monster_ability00);
 tolua_function(tolua_S,"give_object_flag1",tolua_init_give_object_flag100);
 tolua_function(tolua_S,"give_object_flag2",tolua_init_give_object_flag200);
 tolua_function(tolua_S,"give_object_flag3",tolua_init_give_object_flag300);
 tolua_function(tolua_S,"give_object_flag4",tolua_init_give_object_flag400);
 tolua_function(tolua_S,"remove_object_flag1",tolua_init_remove_object_flag100);
 tolua_function(tolua_S,"remove_object_flag2",tolua_init_remove_object_flag200);
 tolua_function(tolua_S,"remove_object_flag3",tolua_init_remove_object_flag300);
 tolua_function(tolua_S,"remove_object_flag4",tolua_init_remove_object_flag400);
 tolua_function(tolua_S,"lua_mod",tolua_init_lua_mod00);
 tolua_function(tolua_S,"get_player_monster_ability",tolua_init_get_player_monster_ability00);
 tolua_function(tolua_S,"give_monster_race_flag1",tolua_init_give_monster_race_flag100);
 tolua_function(tolua_S,"give_monster_race_flag2",tolua_init_give_monster_race_flag200);
 tolua_function(tolua_S,"give_monster_race_flag3",tolua_init_give_monster_race_flag300);
 tolua_function(tolua_S,"give_monster_race_flag4",tolua_init_give_monster_race_flag400);
 tolua_function(tolua_S,"give_monster_race_flag5",tolua_init_give_monster_race_flag500);
 tolua_function(tolua_S,"give_monster_race_flag6",tolua_init_give_monster_race_flag600);
 tolua_function(tolua_S,"give_monster_race_flag7",tolua_init_give_monster_race_flag700);
 tolua_function(tolua_S,"give_monster_race_flag8",tolua_init_give_monster_race_flag800);
 tolua_function(tolua_S,"give_monster_race_flag9",tolua_init_give_monster_race_flag900);
 tolua_function(tolua_S,"give_dungeon_flag1",tolua_init_give_dungeon_flag100);
 tolua_function(tolua_S,"remove_dungeon_flag1",tolua_init_remove_dungeon_flag100);
 tolua_function(tolua_S,"lua_cave_mark",tolua_init_lua_cave_mark00);
 tolua_function(tolua_S,"lua_get_string",tolua_init_lua_get_string00);
 tolua_function(tolua_S,"get_feat_flag1",tolua_init_get_feat_flag100);
 tolua_array(tolua_S,"copyright",tolua_get_init_copyright,tolua_set_init_copyright);
 tolua_variable(tolua_S,"version_major",tolua_get_version_major,tolua_set_version_major);
 tolua_variable(tolua_S,"version_minor",tolua_get_version_minor,tolua_set_version_minor);
 tolua_variable(tolua_S,"version_patch",tolua_get_version_patch,tolua_set_version_patch);
 tolua_variable(tolua_S,"version_extra",tolua_get_version_extra,tolua_set_version_extra);
 tolua_variable(tolua_S,"sf_major",tolua_get_sf_major,tolua_set_sf_major);
 tolua_variable(tolua_S,"sf_minor",tolua_get_sf_minor,tolua_set_sf_minor);
 tolua_variable(tolua_S,"sf_patch",tolua_get_sf_patch,tolua_set_sf_patch);
 tolua_variable(tolua_S,"sf_extra",tolua_get_sf_extra,tolua_set_sf_extra);
 tolua_variable(tolua_S,"sf_xtra",tolua_get_sf_xtra,tolua_set_sf_xtra);
 tolua_variable(tolua_S,"z_major",tolua_get_z_major,tolua_set_z_major);
 tolua_variable(tolua_S,"z_minor",tolua_get_z_minor,tolua_set_z_minor);
 tolua_variable(tolua_S,"z_patch",tolua_get_z_patch,tolua_set_z_patch);
 tolua_variable(tolua_S,"sf_when",tolua_get_sf_when,tolua_set_sf_when);
 tolua_variable(tolua_S,"sf_lives",tolua_get_sf_lives,tolua_set_sf_lives);
 tolua_variable(tolua_S,"sf_saves",tolua_get_sf_saves,tolua_set_sf_saves);
 tolua_variable(tolua_S,"arg_fiddle",tolua_get_arg_fiddle,tolua_set_arg_fiddle);
 tolua_variable(tolua_S,"arg_wizard",tolua_get_arg_wizard,tolua_set_arg_wizard);
 tolua_variable(tolua_S,"arg_sound",tolua_get_arg_sound,tolua_set_arg_sound);
 tolua_variable(tolua_S,"arg_graphics",tolua_get_arg_graphics,tolua_set_arg_graphics);
 tolua_variable(tolua_S,"arg_force_original",tolua_get_arg_force_original,tolua_set_arg_force_original);
 tolua_variable(tolua_S,"arg_force_roguelike",tolua_get_arg_force_roguelike,tolua_set_arg_force_roguelike);
 tolua_variable(tolua_S,"character_generated",tolua_get_character_generated,tolua_set_character_generated);
 tolua_variable(tolua_S,"character_dungeon",tolua_get_character_dungeon,tolua_set_character_dungeon);
 tolua_variable(tolua_S,"character_loaded",tolua_get_character_loaded,tolua_set_character_loaded);
 tolua_variable(tolua_S,"character_saved",tolua_get_character_saved,tolua_set_character_saved);
 tolua_variable(tolua_S,"character_icky",tolua_get_character_icky,tolua_set_character_icky);
 tolua_variable(tolua_S,"character_xtra",tolua_get_character_xtra,tolua_set_character_xtra);
 tolua_variable(tolua_S,"seed_flavor",tolua_get_seed_flavor,tolua_set_seed_flavor);
 tolua_variable(tolua_S,"seed_town",tolua_get_seed_town,tolua_set_seed_town);
 tolua_variable(tolua_S,"seed_dungeon",tolua_get_seed_dungeon,tolua_set_seed_dungeon);
 tolua_variable(tolua_S,"command_cmd",tolua_get_command_cmd,tolua_set_command_cmd);
 tolua_variable(tolua_S,"command_arg",tolua_get_command_arg,tolua_set_command_arg);
 tolua_variable(tolua_S,"command_rep",tolua_get_command_rep,tolua_set_command_rep);
 tolua_variable(tolua_S,"command_dir",tolua_get_command_dir,tolua_set_command_dir);
 tolua_variable(tolua_S,"command_see",tolua_get_command_see,tolua_set_command_see);
 tolua_variable(tolua_S,"command_gap",tolua_get_command_gap,tolua_set_command_gap);
 tolua_variable(tolua_S,"command_wrk",tolua_get_command_wrk,tolua_set_command_wrk);
 tolua_variable(tolua_S,"command_new",tolua_get_command_new,tolua_set_command_new);
 tolua_variable(tolua_S,"energy_use",tolua_get_energy_use,tolua_set_energy_use);
 tolua_variable(tolua_S,"create_up_stair",tolua_get_create_up_stair,tolua_set_create_up_stair);
 tolua_variable(tolua_S,"create_down_stair",tolua_get_create_down_stair,tolua_set_create_down_stair);
 tolua_variable(tolua_S,"create_up_shaft",tolua_get_create_up_shaft,tolua_set_create_up_shaft);
 tolua_variable(tolua_S,"create_down_shaft",tolua_get_create_down_shaft,tolua_set_create_down_shaft);
 tolua_variable(tolua_S,"msg_flag",tolua_get_msg_flag,tolua_set_msg_flag);
 tolua_variable(tolua_S,"alive",tolua_get_alive,tolua_set_alive);
 tolua_variable(tolua_S,"death",tolua_get_death,tolua_set_death);
 tolua_variable(tolua_S,"running",tolua_get_running,tolua_set_running);
 tolua_variable(tolua_S,"resting",tolua_get_resting,tolua_set_resting);
 tolua_variable(tolua_S,"cur_hgt",tolua_get_cur_hgt,tolua_set_cur_hgt);
 tolua_variable(tolua_S,"cur_wid",tolua_get_cur_wid,tolua_set_cur_wid);
 tolua_variable(tolua_S,"dun_level",tolua_get_dun_level,tolua_set_dun_level);
 tolua_variable(tolua_S,"old_dun_level",tolua_get_old_dun_level,tolua_set_old_dun_level);
 tolua_variable(tolua_S,"num_repro",tolua_get_num_repro,tolua_set_num_repro);
 tolua_variable(tolua_S,"object_level",tolua_get_object_level,tolua_set_object_level);
 tolua_variable(tolua_S,"monster_level",tolua_get_monster_level,tolua_set_monster_level);
 tolua_variable(tolua_S,"turn",tolua_get_turn,tolua_set_turn);
 tolua_variable(tolua_S,"old_turn",tolua_get_old_turn,tolua_set_old_turn);
 tolua_variable(tolua_S,"wizard",tolua_get_wizard,tolua_set_wizard);
 tolua_variable(tolua_S,"use_sound",tolua_get_use_sound,tolua_set_use_sound);
 tolua_variable(tolua_S,"use_graphics",tolua_get_use_graphics,tolua_set_use_graphics);
 tolua_variable(tolua_S,"total_winner",tolua_get_total_winner,tolua_set_total_winner);
 tolua_variable(tolua_S,"panic_save",tolua_get_panic_save,tolua_set_panic_save);
 tolua_variable(tolua_S,"noscore",tolua_get_noscore,tolua_set_noscore);
 tolua_variable(tolua_S,"signal_count",tolua_get_signal_count,tolua_set_signal_count);
 tolua_variable(tolua_S,"inkey_base",tolua_get_inkey_base,tolua_set_inkey_base);
 tolua_variable(tolua_S,"inkey_xtra",tolua_get_inkey_xtra,tolua_set_inkey_xtra);
 tolua_variable(tolua_S,"inkey_scan",tolua_get_inkey_scan,tolua_set_inkey_scan);
 tolua_variable(tolua_S,"inkey_flag",tolua_get_inkey_flag,tolua_set_inkey_flag);
 tolua_variable(tolua_S,"coin_type",tolua_get_coin_type,tolua_set_coin_type);
 tolua_variable(tolua_S,"opening_chest",tolua_get_opening_chest,tolua_set_opening_chest);
 tolua_variable(tolua_S,"shimmer_monsters",tolua_get_shimmer_monsters,tolua_set_shimmer_monsters);
 tolua_variable(tolua_S,"shimmer_objects",tolua_get_shimmer_objects,tolua_set_shimmer_objects);
 tolua_variable(tolua_S,"repair_monsters",tolua_get_repair_monsters,tolua_set_repair_monsters);
 tolua_variable(tolua_S,"repair_objects",tolua_get_repair_objects,tolua_set_repair_objects);
 tolua_variable(tolua_S,"total_weight",tolua_get_total_weight,tolua_set_total_weight);
 tolua_variable(tolua_S,"inven_nxt",tolua_get_inven_nxt,tolua_set_inven_nxt);
 tolua_variable(tolua_S,"inven_cnt",tolua_get_inven_cnt,tolua_set_inven_cnt);
 tolua_variable(tolua_S,"equip_cnt",tolua_get_equip_cnt,tolua_set_equip_cnt);
 tolua_variable(tolua_S,"o_max",tolua_get_o_max,tolua_set_o_max);
 tolua_variable(tolua_S,"o_cnt",tolua_get_o_cnt,tolua_set_o_cnt);
 tolua_variable(tolua_S,"m_max",tolua_get_m_max,tolua_set_m_max);
 tolua_variable(tolua_S,"m_cnt",tolua_get_m_cnt,tolua_set_m_cnt);
 tolua_variable(tolua_S,"hack_m_idx",tolua_get_hack_m_idx,tolua_set_hack_m_idx);
 tolua_variable(tolua_S,"hack_m_idx_ii",tolua_get_hack_m_idx_ii,tolua_set_hack_m_idx_ii);
 tolua_variable(tolua_S,"total_friends",tolua_get_total_friends,tolua_set_total_friends);
 tolua_variable(tolua_S,"total_friend_levels",tolua_get_total_friend_levels,tolua_set_total_friend_levels);
 tolua_variable(tolua_S,"leaving_quest",tolua_get_leaving_quest,tolua_set_leaving_quest);
 tolua_variable(tolua_S,"multi_rew",tolua_get_multi_rew,tolua_set_multi_rew);
 tolua_variable(tolua_S,"summon_kin_type",tolua_get_summon_kin_type,tolua_set_summon_kin_type);
 tolua_variable(tolua_S,"hack_mind",tolua_get_hack_mind,tolua_set_hack_mind);
 tolua_variable(tolua_S,"hack_mutation",tolua_get_hack_mutation,tolua_set_hack_mutation);
 tolua_variable(tolua_S,"is_autosave",tolua_get_is_autosave,tolua_set_is_autosave);
 tolua_variable(tolua_S,"artifact_bias",tolua_get_artifact_bias,tolua_set_artifact_bias);
 tolua_variable(tolua_S,"show_inven_graph",tolua_get_show_inven_graph,tolua_set_show_inven_graph);
 tolua_variable(tolua_S,"show_store_graph",tolua_get_show_store_graph,tolua_set_show_store_graph);
 tolua_variable(tolua_S,"show_equip_graph",tolua_get_show_equip_graph,tolua_set_show_equip_graph);
 tolua_variable(tolua_S,"rogue_like_commands",tolua_get_rogue_like_commands,tolua_set_rogue_like_commands);
 tolua_variable(tolua_S,"quick_messages",tolua_get_quick_messages,tolua_set_quick_messages);
 tolua_variable(tolua_S,"other_query_flag",tolua_get_other_query_flag,tolua_set_other_query_flag);
 tolua_variable(tolua_S,"carry_query_flag",tolua_get_carry_query_flag,tolua_set_carry_query_flag);
 tolua_variable(tolua_S,"always_pickup",tolua_get_always_pickup,tolua_set_always_pickup);
 tolua_variable(tolua_S,"no_pickup_corpse",tolua_get_no_pickup_corpse,tolua_set_no_pickup_corpse);
 tolua_variable(tolua_S,"always_repeat",tolua_get_always_repeat,tolua_set_always_repeat);
 tolua_variable(tolua_S,"use_old_target",tolua_get_use_old_target,tolua_set_use_old_target);
 tolua_variable(tolua_S,"depth_in_feet",tolua_get_depth_in_feet,tolua_set_depth_in_feet);
 tolua_variable(tolua_S,"use_color",tolua_get_use_color,tolua_set_use_color);
 tolua_variable(tolua_S,"compress_savefile",tolua_get_compress_savefile,tolua_set_compress_savefile);
 tolua_variable(tolua_S,"hilite_player",tolua_get_hilite_player,tolua_set_hilite_player);
 tolua_variable(tolua_S,"ring_bell",tolua_get_ring_bell,tolua_set_ring_bell);
 tolua_variable(tolua_S,"find_ignore_stairs",tolua_get_find_ignore_stairs,tolua_set_find_ignore_stairs);
 tolua_variable(tolua_S,"find_ignore_doors",tolua_get_find_ignore_doors,tolua_set_find_ignore_doors);
 tolua_variable(tolua_S,"find_cut",tolua_get_find_cut,tolua_set_find_cut);
 tolua_variable(tolua_S,"find_examine",tolua_get_find_examine,tolua_set_find_examine);
 tolua_variable(tolua_S,"disturb_near",tolua_get_disturb_near,tolua_set_disturb_near);
 tolua_variable(tolua_S,"disturb_move",tolua_get_disturb_move,tolua_set_disturb_move);
 tolua_variable(tolua_S,"disturb_panel",tolua_get_disturb_panel,tolua_set_disturb_panel);
 tolua_variable(tolua_S,"disturb_state",tolua_get_disturb_state,tolua_set_disturb_state);
 tolua_variable(tolua_S,"disturb_minor",tolua_get_disturb_minor,tolua_set_disturb_minor);
 tolua_variable(tolua_S,"disturb_other",tolua_get_disturb_other,tolua_set_disturb_other);
 tolua_variable(tolua_S,"avoid_abort",tolua_get_avoid_abort,tolua_set_avoid_abort);
 tolua_variable(tolua_S,"avoid_other",tolua_get_avoid_other,tolua_set_avoid_other);
 tolua_variable(tolua_S,"flush_disturb",tolua_get_flush_disturb,tolua_set_flush_disturb);
 tolua_variable(tolua_S,"flush_failure",tolua_get_flush_failure,tolua_set_flush_failure);
 tolua_variable(tolua_S,"flush_command",tolua_get_flush_command,tolua_set_flush_command);
 tolua_variable(tolua_S,"fresh_before",tolua_get_fresh_before,tolua_set_fresh_before);
 tolua_variable(tolua_S,"fresh_after",tolua_get_fresh_after,tolua_set_fresh_after);
 tolua_variable(tolua_S,"fresh_message",tolua_get_fresh_message,tolua_set_fresh_message);
 tolua_variable(tolua_S,"alert_hitpoint",tolua_get_alert_hitpoint,tolua_set_alert_hitpoint);
 tolua_variable(tolua_S,"alert_failure",tolua_get_alert_failure,tolua_set_alert_failure);
 tolua_variable(tolua_S,"view_yellow_lite",tolua_get_view_yellow_lite,tolua_set_view_yellow_lite);
 tolua_variable(tolua_S,"view_bright_lite",tolua_get_view_bright_lite,tolua_set_view_bright_lite);
 tolua_variable(tolua_S,"view_granite_lite",tolua_get_view_granite_lite,tolua_set_view_granite_lite);
 tolua_variable(tolua_S,"view_special_lite",tolua_get_view_special_lite,tolua_set_view_special_lite);
 tolua_variable(tolua_S,"skip_mutations",tolua_get_skip_mutations,tolua_set_skip_mutations);
 tolua_variable(tolua_S,"plain_descriptions",tolua_get_plain_descriptions,tolua_set_plain_descriptions);
 tolua_variable(tolua_S,"stupid_monsters",tolua_get_stupid_monsters,tolua_set_stupid_monsters);
 tolua_variable(tolua_S,"auto_destroy",tolua_get_auto_destroy,tolua_set_auto_destroy);
 tolua_variable(tolua_S,"wear_confirm",tolua_get_wear_confirm,tolua_set_wear_confirm);
 tolua_variable(tolua_S,"confirm_stairs",tolua_get_confirm_stairs,tolua_set_confirm_stairs);
 tolua_variable(tolua_S,"disturb_pets",tolua_get_disturb_pets,tolua_set_disturb_pets);
 tolua_variable(tolua_S,"view_perma_grids",tolua_get_view_perma_grids,tolua_set_view_perma_grids);
 tolua_variable(tolua_S,"view_torch_grids",tolua_get_view_torch_grids,tolua_set_view_torch_grids);
 tolua_variable(tolua_S,"flow_by_sound",tolua_get_flow_by_sound,tolua_set_flow_by_sound);
 tolua_variable(tolua_S,"flow_by_smell",tolua_get_flow_by_smell,tolua_set_flow_by_smell);
 tolua_variable(tolua_S,"track_follow",tolua_get_track_follow,tolua_set_track_follow);
 tolua_variable(tolua_S,"track_target",tolua_get_track_target,tolua_set_track_target);
 tolua_variable(tolua_S,"stack_allow_items",tolua_get_stack_allow_items,tolua_set_stack_allow_items);
 tolua_variable(tolua_S,"stack_allow_wands",tolua_get_stack_allow_wands,tolua_set_stack_allow_wands);
 tolua_variable(tolua_S,"stack_force_notes",tolua_get_stack_force_notes,tolua_set_stack_force_notes);
 tolua_variable(tolua_S,"stack_force_costs",tolua_get_stack_force_costs,tolua_set_stack_force_costs);
 tolua_variable(tolua_S,"view_reduce_lite",tolua_get_view_reduce_lite,tolua_set_view_reduce_lite);
 tolua_variable(tolua_S,"view_reduce_view",tolua_get_view_reduce_view,tolua_set_view_reduce_view);
 tolua_variable(tolua_S,"auto_haggle",tolua_get_auto_haggle,tolua_set_auto_haggle);
 tolua_variable(tolua_S,"auto_scum",tolua_get_auto_scum,tolua_set_auto_scum);
 tolua_variable(tolua_S,"expand_look",tolua_get_expand_look,tolua_set_expand_look);
 tolua_variable(tolua_S,"expand_list",tolua_get_expand_list,tolua_set_expand_list);
 tolua_variable(tolua_S,"dungeon_align",tolua_get_dungeon_align,tolua_set_dungeon_align);
 tolua_variable(tolua_S,"dungeon_stair",tolua_get_dungeon_stair,tolua_set_dungeon_stair);
 tolua_variable(tolua_S,"smart_learn",tolua_get_smart_learn,tolua_set_smart_learn);
 tolua_variable(tolua_S,"smart_cheat",tolua_get_smart_cheat,tolua_set_smart_cheat);
 tolua_variable(tolua_S,"show_labels",tolua_get_show_labels,tolua_set_show_labels);
 tolua_variable(tolua_S,"show_weights",tolua_get_show_weights,tolua_set_show_weights);
 tolua_variable(tolua_S,"show_choices",tolua_get_show_choices,tolua_set_show_choices);
 tolua_variable(tolua_S,"show_details",tolua_get_show_details,tolua_set_show_details);
 tolua_variable(tolua_S,"testing_stack",tolua_get_testing_stack,tolua_set_testing_stack);
 tolua_variable(tolua_S,"testing_carry",tolua_get_testing_carry,tolua_set_testing_carry);
 tolua_variable(tolua_S,"cheat_peek",tolua_get_cheat_peek,tolua_set_cheat_peek);
 tolua_variable(tolua_S,"cheat_hear",tolua_get_cheat_hear,tolua_set_cheat_hear);
 tolua_variable(tolua_S,"cheat_room",tolua_get_cheat_room,tolua_set_cheat_room);
 tolua_variable(tolua_S,"cheat_xtra",tolua_get_cheat_xtra,tolua_set_cheat_xtra);
 tolua_variable(tolua_S,"cheat_know",tolua_get_cheat_know,tolua_set_cheat_know);
 tolua_variable(tolua_S,"cheat_live",tolua_get_cheat_live,tolua_set_cheat_live);
 tolua_variable(tolua_S,"last_words",tolua_get_last_words,tolua_set_last_words);
 tolua_variable(tolua_S,"speak_unique",tolua_get_speak_unique,tolua_set_speak_unique);
 tolua_variable(tolua_S,"small_levels",tolua_get_small_levels,tolua_set_small_levels);
 tolua_variable(tolua_S,"empty_levels",tolua_get_empty_levels,tolua_set_empty_levels);
 tolua_variable(tolua_S,"water_levels",tolua_get_water_levels,tolua_set_water_levels);
 tolua_variable(tolua_S,"always_small_level",tolua_get_always_small_level,tolua_set_always_small_level);
 tolua_variable(tolua_S,"flavored_attacks",tolua_get_flavored_attacks,tolua_set_flavored_attacks);
 tolua_variable(tolua_S,"player_symbols",tolua_get_player_symbols,tolua_set_player_symbols);
 tolua_variable(tolua_S,"hitpoint_warn",tolua_get_hitpoint_warn,tolua_set_hitpoint_warn);
 tolua_variable(tolua_S,"delay_factor",tolua_get_delay_factor,tolua_set_delay_factor);
 tolua_variable(tolua_S,"autosave_freq",tolua_get_autosave_freq,tolua_set_autosave_freq);
 tolua_variable(tolua_S,"autosave_t",tolua_get_autosave_t,tolua_set_autosave_t);
 tolua_variable(tolua_S,"autosave_l",tolua_get_autosave_l,tolua_set_autosave_l);
 tolua_variable(tolua_S,"feeling",tolua_get_feeling,tolua_set_feeling);
 tolua_variable(tolua_S,"rating",tolua_get_rating,tolua_set_rating);
 tolua_variable(tolua_S,"good_item_flag",tolua_get_good_item_flag,tolua_set_good_item_flag);
 tolua_variable(tolua_S,"closing_flag",tolua_get_closing_flag,tolua_set_closing_flag);
 tolua_variable(tolua_S,"max_panel_rows",tolua_get_max_panel_rows,tolua_set_max_panel_rows);
 tolua_variable(tolua_S,"max_panel_cols",tolua_get_max_panel_cols,tolua_set_max_panel_cols);
 tolua_variable(tolua_S,"panel_row",tolua_get_panel_row,tolua_set_panel_row);
 tolua_variable(tolua_S,"panel_col",tolua_get_panel_col,tolua_set_panel_col);
 tolua_variable(tolua_S,"panel_row_min",tolua_get_panel_row_min,tolua_set_panel_row_min);
 tolua_variable(tolua_S,"panel_row_max",tolua_get_panel_row_max,tolua_set_panel_row_max);
 tolua_variable(tolua_S,"panel_col_min",tolua_get_panel_col_min,tolua_set_panel_col_min);
 tolua_variable(tolua_S,"panel_col_max",tolua_get_panel_col_max,tolua_set_panel_col_max);
 tolua_variable(tolua_S,"panel_col_prt",tolua_get_panel_col_prt,tolua_set_panel_col_prt);
 tolua_variable(tolua_S,"panel_row_prt",tolua_get_panel_row_prt,tolua_set_panel_row_prt);
 tolua_variable(tolua_S,"py",tolua_get_py,tolua_set_py);
 tolua_variable(tolua_S,"px",tolua_get_px,tolua_set_px);
 tolua_variable(tolua_S,"global_x",tolua_get_global_x,tolua_set_global_x);
 tolua_variable(tolua_S,"global_y",tolua_get_global_y,tolua_set_global_y);
 tolua_variable(tolua_S,"nevermiss",tolua_get_nevermiss,tolua_set_nevermiss);
 tolua_variable(tolua_S,"no_magic_return",tolua_get_no_magic_return,tolua_set_no_magic_return);
 tolua_variable(tolua_S,"monster_physical",tolua_get_monster_physical,tolua_set_monster_physical);
 tolua_variable(tolua_S,"monster_ranged",tolua_get_monster_ranged,tolua_set_monster_ranged);
 tolua_variable(tolua_S,"ranged_attack",tolua_get_ranged_attack,tolua_set_ranged_attack);
 tolua_variable(tolua_S,"melee_attack",tolua_get_melee_attack,tolua_set_melee_attack);
 tolua_variable(tolua_S,"throw_attack",tolua_get_throw_attack,tolua_set_throw_attack);
 tolua_variable(tolua_S,"monster_died",tolua_get_monster_died,tolua_set_monster_died);
 tolua_variable(tolua_S,"ignore_spellcraft",tolua_get_ignore_spellcraft,tolua_set_ignore_spellcraft);
 tolua_variable(tolua_S,"damages_counter",tolua_get_damages_counter,tolua_set_damages_counter);
 tolua_variable(tolua_S,"damages_counter_player_damages",tolua_get_damages_counter_player_damages,tolua_set_damages_counter_player_damages);
 tolua_variable(tolua_S,"damages_counter_duration",tolua_get_damages_counter_duration,tolua_set_damages_counter_duration);
 tolua_variable(tolua_S,"stormshadow",tolua_get_stormshadow,tolua_set_stormshadow);
 tolua_variable(tolua_S,"enemy_immortality",tolua_get_enemy_immortality,tolua_set_enemy_immortality);
 tolua_variable(tolua_S,"tmpluastring",tolua_get_tmpluastring,tolua_set_tmpluastring);
 tolua_variable(tolua_S,"red_roff",tolua_get_red_roff,tolua_set_red_roff);
 tolua_variable(tolua_S,"term_saved",tolua_get_term_saved,tolua_set_term_saved);
 tolua_variable(tolua_S,"dying",tolua_get_dying,tolua_set_dying);
 tolua_variable(tolua_S,"exblows",tolua_get_exblows,tolua_set_exblows);
 tolua_variable(tolua_S,"exshots",tolua_get_exshots,tolua_set_exshots);
 tolua_variable(tolua_S,"dropshots",tolua_get_dropshots,tolua_set_dropshots);
 tolua_variable(tolua_S,"dropnum",tolua_get_dropnum,tolua_set_dropnum);
 tolua_variable(tolua_S,"throw_item",tolua_get_throw_item,tolua_set_throw_item);
 tolua_variable(tolua_S,"throw_floorpack",tolua_get_throw_floorpack,tolua_set_throw_floorpack);
 tolua_variable(tolua_S,"testop",tolua_get_testop,tolua_set_testop);
 tolua_variable(tolua_S,"target_who",tolua_get_target_who,tolua_set_target_who);
 tolua_variable(tolua_S,"target_col",tolua_get_target_col,tolua_set_target_col);
 tolua_variable(tolua_S,"target_row",tolua_get_target_row,tolua_set_target_row);
 tolua_variable(tolua_S,"health_who",tolua_get_health_who,tolua_set_health_who);
 tolua_variable(tolua_S,"monster_race_idx",tolua_get_monster_race_idx,tolua_set_monster_race_idx);
 tolua_variable(tolua_S,"monster_type_idx",tolua_get_monster_type_idx,tolua_set_monster_type_idx);
 tolua_variable(tolua_S,"object_kind_idx",tolua_get_object_kind_idx,tolua_set_object_kind_idx);
 tolua_variable(tolua_S,"player_uid",tolua_get_player_uid,tolua_set_player_uid);
 tolua_variable(tolua_S,"player_euid",tolua_get_player_euid,tolua_set_player_euid);
 tolua_variable(tolua_S,"player_egid",tolua_get_player_egid,tolua_set_player_egid);
 tolua_variable(tolua_S,"player_name",tolua_get_player_name,tolua_set_player_name);
 tolua_variable(tolua_S,"player_base",tolua_get_player_base,tolua_set_player_base);
 tolua_variable(tolua_S,"died_from",tolua_get_died_from,tolua_set_died_from);
 tolua_variable(tolua_S,"history",tolua_get_history,tolua_set_history);
 tolua_variable(tolua_S,"savefile",tolua_get_savefile,tolua_set_savefile);
 tolua_variable(tolua_S,"lite_n",tolua_get_lite_n,tolua_set_lite_n);
 tolua_array(tolua_S,"lite_y",tolua_get_init_lite_y,tolua_set_init_lite_y);
 tolua_array(tolua_S,"lite_x",tolua_get_init_lite_x,tolua_set_init_lite_x);
 tolua_variable(tolua_S,"view_n",tolua_get_view_n,tolua_set_view_n);
 tolua_array(tolua_S,"view_y",tolua_get_init_view_y,tolua_set_init_view_y);
 tolua_array(tolua_S,"view_x",tolua_get_init_view_x,tolua_set_init_view_x);
 tolua_variable(tolua_S,"temp_n",tolua_get_temp_n,tolua_set_temp_n);
 tolua_array(tolua_S,"temp_y",tolua_get_init_temp_y,tolua_set_init_temp_y);
 tolua_array(tolua_S,"temp_x",tolua_get_init_temp_x,tolua_set_init_temp_x);
 tolua_variable(tolua_S,"macro__num",tolua_get_macro__num,tolua_set_macro__num);
 tolua_variable(tolua_S,"macro__pat",tolua_get_macro__pat,tolua_set_macro__pat);
 tolua_variable(tolua_S,"macro__act",tolua_get_macro__act,tolua_set_macro__act);
 tolua_variable(tolua_S,"macro__cmd",tolua_get_macro__cmd,tolua_set_macro__cmd);
 tolua_variable(tolua_S,"macro__buf",tolua_get_macro__buf,tolua_set_macro__buf);
 tolua_variable(tolua_S,"quark__num",tolua_get_quark__num,tolua_set_quark__num);
 tolua_variable(tolua_S,"quark__str",tolua_get_quark__str,tolua_set_quark__str);
 tolua_variable(tolua_S,"message__next",tolua_get_message__next,tolua_set_message__next);
 tolua_variable(tolua_S,"message__last",tolua_get_message__last,tolua_set_message__last);
 tolua_variable(tolua_S,"message__head",tolua_get_message__head,tolua_set_message__head);
 tolua_variable(tolua_S,"message__tail",tolua_get_message__tail,tolua_set_message__tail);
 tolua_variable(tolua_S,"message__ptr",tolua_get_message__ptr,tolua_set_message__ptr);
 tolua_variable(tolua_S,"message__buf",tolua_get_message__buf,tolua_set_message__buf);
 tolua_array(tolua_S,"option_flag",tolua_get_init_option_flag,tolua_set_init_option_flag);
 tolua_array(tolua_S,"option_mask",tolua_get_init_option_mask,tolua_set_init_option_mask);
 tolua_array(tolua_S,"window_flag",tolua_get_init_window_flag,tolua_set_init_window_flag);
 tolua_array(tolua_S,"window_mask",tolua_get_init_window_mask,tolua_set_init_window_mask);
 tolua_array(tolua_S,"angband_term",tolua_get_init_angband_term,tolua_set_init_angband_term);
 tolua_variable(tolua_S,"o_list",tolua_get_o_list_ptr,tolua_set_o_list_ptr);
 tolua_variable(tolua_S,"m_list",tolua_get_m_list_ptr,tolua_set_m_list_ptr);
 tolua_variable(tolua_S,"inventory",tolua_get_inventory_ptr,tolua_set_inventory_ptr);
 tolua_variable(tolua_S,"alloc_kind_size",tolua_get_alloc_kind_size,tolua_set_alloc_kind_size);
 tolua_variable(tolua_S,"alloc_kind_table",tolua_get_alloc_kind_table_ptr,tolua_set_alloc_kind_table_ptr);
 tolua_variable(tolua_S,"alloc_race_size",tolua_get_alloc_race_size,tolua_set_alloc_race_size);
 tolua_variable(tolua_S,"alloc_race_table",tolua_get_alloc_race_table_ptr,tolua_set_alloc_race_table_ptr);
 tolua_array(tolua_S,"misc_to_attr",tolua_get_init_misc_to_attr,tolua_set_init_misc_to_attr);
 tolua_variable(tolua_S,"misc_to_char",tolua_get_misc_to_char,tolua_set_misc_to_char);
 tolua_array(tolua_S,"tval_to_attr",tolua_get_init_tval_to_attr,tolua_set_init_tval_to_attr);
 tolua_variable(tolua_S,"tval_to_char",tolua_get_tval_to_char,tolua_set_tval_to_char);
 tolua_variable(tolua_S,"p_body",tolua_get_p_body,tolua_set_p_body);
 tolua_variable(tolua_S,"p_ptr",tolua_get_p_ptr_ptr,tolua_set_p_ptr_ptr);
 tolua_variable(tolua_S,"v_head",tolua_get_v_head_ptr,tolua_set_v_head_ptr);
 tolua_variable(tolua_S,"v_info",tolua_get_v_info_ptr,tolua_set_v_info_ptr);
 tolua_variable(tolua_S,"v_name",tolua_get_v_name,tolua_set_v_name);
 tolua_variable(tolua_S,"v_text",tolua_get_v_text,tolua_set_v_text);
 tolua_variable(tolua_S,"f_head",tolua_get_f_head_ptr,tolua_set_f_head_ptr);
 tolua_variable(tolua_S,"f_info",tolua_get_f_info_ptr,tolua_set_f_info_ptr);
 tolua_variable(tolua_S,"f_name",tolua_get_f_name,tolua_set_f_name);
 tolua_variable(tolua_S,"f_text",tolua_get_f_text,tolua_set_f_text);
 tolua_variable(tolua_S,"k_head",tolua_get_k_head_ptr,tolua_set_k_head_ptr);
 tolua_variable(tolua_S,"k_info",tolua_get_k_info_ptr,tolua_set_k_info_ptr);
 tolua_variable(tolua_S,"k_name",tolua_get_k_name,tolua_set_k_name);
 tolua_variable(tolua_S,"k_text",tolua_get_k_text,tolua_set_k_text);
 tolua_variable(tolua_S,"a_head",tolua_get_a_head_ptr,tolua_set_a_head_ptr);
 tolua_variable(tolua_S,"a_info",tolua_get_a_info_ptr,tolua_set_a_info_ptr);
 tolua_variable(tolua_S,"a_name",tolua_get_a_name,tolua_set_a_name);
 tolua_variable(tolua_S,"a_text",tolua_get_a_text,tolua_set_a_text);
 tolua_variable(tolua_S,"e_head",tolua_get_e_head_ptr,tolua_set_e_head_ptr);
 tolua_variable(tolua_S,"e_info",tolua_get_e_info_ptr,tolua_set_e_info_ptr);
 tolua_variable(tolua_S,"e_name",tolua_get_e_name,tolua_set_e_name);
 tolua_variable(tolua_S,"e_text",tolua_get_e_text,tolua_set_e_text);
 tolua_variable(tolua_S,"r_head",tolua_get_r_head_ptr,tolua_set_r_head_ptr);
 tolua_variable(tolua_S,"r_info",tolua_get_r_info_ptr,tolua_set_r_info_ptr);
 tolua_variable(tolua_S,"r_name",tolua_get_r_name,tolua_set_r_name);
 tolua_variable(tolua_S,"r_text",tolua_get_r_text,tolua_set_r_text);
 tolua_variable(tolua_S,"d_head",tolua_get_d_head_ptr,tolua_set_d_head_ptr);
 tolua_variable(tolua_S,"d_info",tolua_get_d_info_ptr,tolua_set_d_info_ptr);
 tolua_variable(tolua_S,"d_name",tolua_get_d_name,tolua_set_d_name);
 tolua_variable(tolua_S,"d_text",tolua_get_d_text,tolua_set_d_text);
 tolua_variable(tolua_S,"t_head",tolua_get_t_head_ptr,tolua_set_t_head_ptr);
 tolua_variable(tolua_S,"t_info",tolua_get_t_info_ptr,tolua_set_t_info_ptr);
 tolua_variable(tolua_S,"t_name",tolua_get_t_name,tolua_set_t_name);
 tolua_variable(tolua_S,"t_text",tolua_get_t_text,tolua_set_t_text);
 tolua_variable(tolua_S,"ANGBAND_SYS",tolua_get_ANGBAND_SYS,tolua_set_ANGBAND_SYS);
 tolua_variable(tolua_S,"ANGBAND_GRAF",tolua_get_ANGBAND_GRAF,tolua_set_ANGBAND_GRAF);
 tolua_variable(tolua_S,"ANGBAND_DIR",tolua_get_ANGBAND_DIR,tolua_set_ANGBAND_DIR);
 tolua_variable(tolua_S,"ANGBAND_DIR_APEX",tolua_get_ANGBAND_DIR_APEX,tolua_set_ANGBAND_DIR_APEX);
 tolua_variable(tolua_S,"ANGBAND_DIR_BONE",tolua_get_ANGBAND_DIR_BONE,tolua_set_ANGBAND_DIR_BONE);
 tolua_variable(tolua_S,"ANGBAND_DIR_DATA",tolua_get_ANGBAND_DIR_DATA,tolua_set_ANGBAND_DIR_DATA);
 tolua_variable(tolua_S,"ANGBAND_DIR_EDIT",tolua_get_ANGBAND_DIR_EDIT,tolua_set_ANGBAND_DIR_EDIT);
 tolua_variable(tolua_S,"ANGBAND_DIR_FILE",tolua_get_ANGBAND_DIR_FILE,tolua_set_ANGBAND_DIR_FILE);
 tolua_variable(tolua_S,"ANGBAND_DIR_HELP",tolua_get_ANGBAND_DIR_HELP,tolua_set_ANGBAND_DIR_HELP);
 tolua_variable(tolua_S,"ANGBAND_DIR_INFO",tolua_get_ANGBAND_DIR_INFO,tolua_set_ANGBAND_DIR_INFO);
 tolua_variable(tolua_S,"ANGBAND_DIR_SAVE",tolua_get_ANGBAND_DIR_SAVE,tolua_set_ANGBAND_DIR_SAVE);
 tolua_variable(tolua_S,"ANGBAND_DIR_USER",tolua_get_ANGBAND_DIR_USER,tolua_set_ANGBAND_DIR_USER);
 tolua_variable(tolua_S,"ANGBAND_DIR_XTRA",tolua_get_ANGBAND_DIR_XTRA,tolua_set_ANGBAND_DIR_XTRA);
 tolua_variable(tolua_S,"ANGBAND_DIR_PREF",tolua_get_ANGBAND_DIR_PREF,tolua_set_ANGBAND_DIR_PREF);
 tolua_variable(tolua_S,"ANGBAND_DIR_SCRIPT",tolua_get_ANGBAND_DIR_SCRIPT,tolua_set_ANGBAND_DIR_SCRIPT);
 tolua_variable(tolua_S,"item_tester_full",tolua_get_item_tester_full,tolua_set_item_tester_full);
 tolua_variable(tolua_S,"item_tester_tval",tolua_get_item_tester_tval,tolua_set_item_tester_tval);
 tolua_variable(tolua_S,"monk_armour_aux",tolua_get_monk_armour_aux,tolua_set_monk_armour_aux);
 tolua_variable(tolua_S,"monk_notify_aux",tolua_get_monk_notify_aux,tolua_set_monk_notify_aux);
 tolua_variable(tolua_S,"max_r_idx",tolua_get_max_r_idx,tolua_set_max_r_idx);
 tolua_variable(tolua_S,"max_k_idx",tolua_get_max_k_idx,tolua_set_max_k_idx);
 tolua_variable(tolua_S,"max_v_idx",tolua_get_max_v_idx,tolua_set_max_v_idx);
 tolua_variable(tolua_S,"max_f_idx",tolua_get_max_f_idx,tolua_set_max_f_idx);
 tolua_variable(tolua_S,"max_a_idx",tolua_get_max_a_idx,tolua_set_max_a_idx);
 tolua_variable(tolua_S,"max_e_idx",tolua_get_max_e_idx,tolua_set_max_e_idx);
 tolua_variable(tolua_S,"max_d_idx",tolua_get_max_d_idx,tolua_set_max_d_idx);
 tolua_variable(tolua_S,"max_o_idx",tolua_get_max_o_idx,tolua_set_max_o_idx);
 tolua_variable(tolua_S,"max_m_idx",tolua_get_max_m_idx,tolua_set_max_m_idx);
 tolua_variable(tolua_S,"max_t_idx",tolua_get_max_t_idx,tolua_set_max_t_idx);
 tolua_variable(tolua_S,"max_wf_idx",tolua_get_max_wf_idx,tolua_set_max_wf_idx);
 tolua_variable(tolua_S,"init_flags",tolua_get_init_flags,tolua_set_init_flags);
 tolua_variable(tolua_S,"special_flag",tolua_get_special_flag,tolua_set_special_flag);
 tolua_variable(tolua_S,"ambush_flag",tolua_get_ambush_flag,tolua_set_ambush_flag);
 tolua_variable(tolua_S,"no_breeds",tolua_get_no_breeds,tolua_set_no_breeds);
 tolua_variable(tolua_S,"spell_num",tolua_get_spell_num,tolua_set_spell_num);
 tolua_variable(tolua_S,"dungeon_type",tolua_get_dungeon_type,tolua_set_dungeon_type);
 tolua_variable(tolua_S,"max_dlv",tolua_get_max_dlv,tolua_set_max_dlv);
 tolua_variable(tolua_S,"total_bounties",tolua_get_total_bounties,tolua_set_total_bounties);
 tolua_variable(tolua_S,"generate_encounter",tolua_get_generate_encounter,tolua_set_generate_encounter);
 tolua_variable(tolua_S,"permanent_levels",tolua_get_permanent_levels,tolua_set_permanent_levels);
 tolua_variable(tolua_S,"autoroll",tolua_get_autoroll,tolua_set_autoroll);
 tolua_variable(tolua_S,"hack_allow_special",tolua_get_hack_allow_special,tolua_set_hack_allow_special);
 tolua_array(tolua_S,"magic_spell",tolua_get_init_magic_spell,tolua_set_init_magic_spell);
 tolua_array(tolua_S,"monster_magic",tolua_get_init_monster_magic,tolua_set_init_monster_magic);
 tolua_array(tolua_S,"music_song",tolua_get_init_music_song,tolua_set_init_music_song);
 tolua_array(tolua_S,"stores",tolua_get_init_stores,tolua_set_init_stores);
 tolua_variable(tolua_S,"current_weapon",tolua_get_current_weapon_ptr,tolua_set_current_weapon_ptr);
 tolua_variable(tolua_S,"drop_ranged",tolua_get_drop_ranged_ptr,tolua_set_drop_ranged_ptr);
 tolua_variable(tolua_S,"current_item",tolua_get_current_item_ptr,tolua_set_current_item_ptr);
 tolua_variable(tolua_S,"combatfeat",tolua_get_combatfeat,tolua_set_combatfeat);
 tolua_variable(tolua_S,"center_player",tolua_get_center_player,tolua_set_center_player);
 tolua_variable(tolua_S,"fate_item_modifier",tolua_get_fate_item_modifier,tolua_set_fate_item_modifier);
 tolua_variable(tolua_S,"global_object",tolua_get_global_object,tolua_set_global_object);
 tolua_function(tolua_S,"cave",tolua_init_lua_cave00);
 tolua_function(tolua_S,"monster",tolua_init_lua_monster00);
 tolua_function(tolua_S,"m_race",tolua_init_lua_r_info00);
 tolua_function(tolua_S,"inven",tolua_init_lua_inven00);
 tolua_function(tolua_S,"kind",tolua_init_lua_kind00);
 tolua_function(tolua_S,"object",tolua_init_lua_object00);
 tolua_function(tolua_S,"dungeon",tolua_init_lua_dungeon00);
 tolua_function(tolua_S,"get_monster_flag1",tolua_init_get_monster_flag100);
 tolua_function(tolua_S,"get_monster_flag2",tolua_init_get_monster_flag200);
 tolua_function(tolua_S,"get_monster_flag3",tolua_init_get_monster_flag300);
 tolua_function(tolua_S,"get_monster_flag4",tolua_init_get_monster_flag400);
 tolua_function(tolua_S,"get_monster_flag5",tolua_init_get_monster_flag500);
 tolua_function(tolua_S,"get_monster_flag6",tolua_init_get_monster_flag600);
 tolua_function(tolua_S,"get_monster_flag7",tolua_init_get_monster_flag700);
 tolua_function(tolua_S,"get_monster_flag8",tolua_init_get_monster_flag800);
 tolua_function(tolua_S,"get_monster_flag9",tolua_init_get_monster_flag900);
 tolua_function(tolua_S,"get_object_flag1",tolua_init_get_object_flag100);
 tolua_function(tolua_S,"get_object_flag2",tolua_init_get_object_flag200);
 tolua_function(tolua_S,"get_object_flag3",tolua_init_get_object_flag300);
 tolua_function(tolua_S,"get_object_flag4",tolua_init_get_object_flag400);
 tolua_function(tolua_S,"get_monster_ability",tolua_init_get_monster_ability00);
 tolua_function(tolua_S,"get_monster_desc",tolua_init_get_monster_desc00);
 tolua_function(tolua_S,"plog",tolua_init_plog00);
 tolua_function(tolua_S,"always_hit_check",tolua_init_always_hit_check00);
 tolua_function(tolua_S,"verify_panel",tolua_init_verify_panel00);
 tolua_function(tolua_S,"move_monster_spot",tolua_init_move_monster_spot00);
 tolua_function(tolua_S,"anihilate_monsters",tolua_init_anihilate_monsters00);
 tolua_function(tolua_S,"generate_cave",tolua_init_generate_cave00);
 tolua_function(tolua_S,"verify_panel_always_update",tolua_init_verify_panel_always_update00);
 tolua_function(tolua_S,"check_experience",tolua_init_check_experience00);
 tolua_function(tolua_S,"gain_exp",tolua_init_gain_exp00);
 tolua_function(tolua_S,"lose_exp",tolua_init_lose_exp00);
 tolua_function(tolua_S,"attack_aura",tolua_init_attack_aura00);
 tolua_function(tolua_S,"set_str_boost",tolua_init_set_str_boost00);
 tolua_function(tolua_S,"set_int_boost",tolua_init_set_int_boost00);
 tolua_function(tolua_S,"set_wis_boost",tolua_init_set_wis_boost00);
 tolua_function(tolua_S,"set_dex_boost",tolua_init_set_dex_boost00);
 tolua_function(tolua_S,"set_con_boost",tolua_init_set_con_boost00);
 tolua_function(tolua_S,"set_chr_boost",tolua_init_set_chr_boost00);
 tolua_function(tolua_S,"set_pres",tolua_init_set_pres00);
 tolua_function(tolua_S,"set_mres",tolua_init_set_mres00);
 tolua_function(tolua_S,"set_ac_boost",tolua_init_set_ac_boost00);
 tolua_function(tolua_S,"set_elem_shield",tolua_init_set_elem_shield00);
 tolua_function(tolua_S,"set_powerattack",tolua_init_set_powerattack01);
 tolua_function(tolua_S,"set_invis",tolua_init_set_invis00);
 tolua_function(tolua_S,"set_blind",tolua_init_set_blind00);
 tolua_function(tolua_S,"set_confused",tolua_init_set_confused00);
 tolua_function(tolua_S,"set_poisoned",tolua_init_set_poisoned00);
 tolua_function(tolua_S,"set_afraid",tolua_init_set_afraid00);
 tolua_function(tolua_S,"set_paralyzed",tolua_init_set_paralyzed00);
 tolua_function(tolua_S,"set_image",tolua_init_set_image00);
 tolua_function(tolua_S,"set_fast",tolua_init_set_fast00);
 tolua_function(tolua_S,"set_slow",tolua_init_set_slow00);
 tolua_function(tolua_S,"set_shield",tolua_init_set_shield00);
 tolua_function(tolua_S,"set_blessed",tolua_init_set_blessed00);
 tolua_function(tolua_S,"set_hero",tolua_init_set_hero00);
 tolua_function(tolua_S,"set_shero",tolua_init_set_shero00);
 tolua_function(tolua_S,"set_tim_invis",tolua_init_set_tim_invis00);
 tolua_function(tolua_S,"set_tim_infra",tolua_init_set_tim_infra00);
 tolua_function(tolua_S,"set_stun",tolua_init_set_stun00);
 tolua_function(tolua_S,"set_cut",tolua_init_set_cut00);
 tolua_function(tolua_S,"quark_add",tolua_init_quark_add00);
 tolua_function(tolua_S,"inven_item_describe",tolua_init_inven_item_describe00);
 tolua_function(tolua_S,"inven_item_increase",tolua_init_inven_item_increase00);
 tolua_function(tolua_S,"inven_item_optimize",tolua_init_inven_item_optimize00);
 tolua_function(tolua_S,"floor_item_describe",tolua_init_floor_item_describe00);
 tolua_function(tolua_S,"floor_item_increase",tolua_init_floor_item_increase00);
 tolua_function(tolua_S,"floor_item_optimize",tolua_init_floor_item_optimize00);
 tolua_function(tolua_S,"drop_near",tolua_init_drop_near00);
 tolua_function(tolua_S,"drop_near_ammo",tolua_init_drop_near_ammo00);
 tolua_function(tolua_S,"reveal_spell",tolua_init_reveal_spell00);
 tolua_function(tolua_S,"show_file",tolua_init_show_file00);
 tolua_function(tolua_S,"Term_fresh",tolua_init_Term_fresh00);
 tolua_function(tolua_S,"Term_set_cursor",tolua_init_Term_set_cursor00);
 tolua_function(tolua_S,"Term_gotoxy",tolua_init_Term_gotoxy00);
 tolua_function(tolua_S,"Term_draw",tolua_init_Term_draw00);
 tolua_function(tolua_S,"Term_addch",tolua_init_Term_addch00);
 tolua_function(tolua_S,"Term_addstr",tolua_init_Term_addstr00);
 tolua_function(tolua_S,"Term_putch",tolua_init_Term_putch00);
 tolua_function(tolua_S,"Term_putstr",tolua_init_Term_putstr00);
 tolua_function(tolua_S,"Term_erase",tolua_init_Term_erase00);
 tolua_function(tolua_S,"Term_clear",tolua_init_Term_clear01);
 tolua_function(tolua_S,"Term_redraw",tolua_init_Term_redraw00);
 tolua_function(tolua_S,"Term_get_cursor",tolua_init_Term_get_cursor00);
 tolua_function(tolua_S,"Term_get_size",tolua_init_Term_get_size00);
 tolua_function(tolua_S,"Term_locate",tolua_init_Term_locate00);
 tolua_function(tolua_S,"Term_what",tolua_init_Term_what00);
 tolua_function(tolua_S,"Term_flush",tolua_init_Term_flush00);
 tolua_function(tolua_S,"Term_keypress",tolua_init_Term_keypress00);
 tolua_function(tolua_S,"Term_key_push",tolua_init_Term_key_push00);
 tolua_function(tolua_S,"Term_inkey",tolua_init_Term_inkey00);
 tolua_function(tolua_S,"Term_save",tolua_init_Term_save00);
 tolua_function(tolua_S,"Term_load",tolua_init_Term_load00);
 tolua_function(tolua_S,"Term_exchange",tolua_init_Term_exchange00);
 tolua_function(tolua_S,"Term_resize",tolua_init_Term_resize00);
 tolua_function(tolua_S,"Term_activate",tolua_init_Term_activate00);
 tolua_function(tolua_S,"term_nuke",tolua_init_term_nuke00);
 tolua_function(tolua_S,"term_init",tolua_init_term_init00);
 tolua_function(tolua_S,"delete_object_idx",tolua_init_delete_object_idx00);
 tolua_function(tolua_S,"delete_object",tolua_init_delete_object00);
 tolua_array(tolua_S,"classes_def",tolua_get_init_classes_def,tolua_set_init_classes_def);
 tolua_array(tolua_S,"abilities_def",tolua_get_init_abilities_def,tolua_set_init_abilities_def);
 tolua_array(tolua_S,"feats_def",tolua_get_init_feats_def,tolua_set_init_feats_def);
 tolua_array(tolua_S,"vaults_def",tolua_get_init_vaults_def,tolua_set_init_vaults_def);
 tolua_endmodule(tolua_S);
 return 1;
}
