/*
** Lua binding: util
** Generated automatically by tolua 4.0a - angband on Sun Jul 14 13:51:26 2002.
*/

#include "lua/tolua.h"

/* Exported function */
int tolua_util_open (lua_State* tolua_S);
void tolua_util_close (lua_State* tolua_S);

#include "angband.h"
#include "plots.h"
static bool lua_cave_is(cave_type *c_ptr, s32b flag) { return (f_info[c_ptr->feat].flags1 & flag); }

/* function to register type */
static void toluaI_reg_types (lua_State* tolua_S)
{
 tolua_usertype(tolua_S,"cave_type");
}

/* error messages */
#define TOLUA_ERR_SELF tolua_error(tolua_S,"invalid 'self'")
#define TOLUA_ERR_ASSIGN tolua_error(tolua_S,"#vinvalid type in variable assignment.")

/* get function: turn */
static int toluaI_get_util_turn(lua_State* tolua_S)
{
 tolua_pushnumber(tolua_S,(long)turn);
 return 1;
}

/* set function: turn */
static int toluaI_set_util_turn(lua_State* tolua_S)
{
 if (!tolua_istype(tolua_S,1,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  turn = ((s32b)  tolua_getnumber(tolua_S,1,0));
 return 0;
}

/* get function: old_turn */
static int toluaI_get_util_old_turn(lua_State* tolua_S)
{
 tolua_pushnumber(tolua_S,(long)old_turn);
 return 1;
}

/* set function: old_turn */
static int toluaI_set_util_old_turn(lua_State* tolua_S)
{
 if (!tolua_istype(tolua_S,1,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  old_turn = ((s32b)  tolua_getnumber(tolua_S,1,0));
 return 0;
}

/* get function: cur_wid */
static int toluaI_get_util_cur_wid(lua_State* tolua_S)
{
 tolua_pushnumber(tolua_S,(long)cur_wid);
 return 1;
}

/* set function: cur_wid */
static int toluaI_set_util_cur_wid(lua_State* tolua_S)
{
 if (!tolua_istype(tolua_S,1,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  cur_wid = ((s16b)  tolua_getnumber(tolua_S,1,0));
 return 0;
}

/* get function: cur_hgt */
static int toluaI_get_util_cur_hgt(lua_State* tolua_S)
{
 tolua_pushnumber(tolua_S,(long)cur_hgt);
 return 1;
}

/* set function: cur_hgt */
static int toluaI_set_util_cur_hgt(lua_State* tolua_S)
{
 if (!tolua_istype(tolua_S,1,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  cur_hgt = ((s16b)  tolua_getnumber(tolua_S,1,0));
 return 0;
}

/* function: bst */
static int toluaI_util_bst00(lua_State* tolua_S)
{
 if (
 !tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
 !tolua_istype(tolua_S,2,LUA_TNUMBER,0) ||
 !tolua_isnoobj(tolua_S,3)
 )
 goto tolua_lerror;
 else
 {
  s32b what = ((s32b)  tolua_getnumber(tolua_S,1,0));
  s32b t = ((s32b)  tolua_getnumber(tolua_S,2,0));
 {
  s32b toluaI_ret = (s32b)  bst(what,t);
 tolua_pushnumber(tolua_S,(long)toluaI_ret);
 }
 }
 return 1;
tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'bst'.");
 return 0;
}

/* function: path_build */
static int toluaI_util_path_build00(lua_State* tolua_S)
{
 if (
 !tolua_istype(tolua_S,1,LUA_TSTRING,0) ||
 !tolua_istype(tolua_S,2,LUA_TNUMBER,0) ||
 !tolua_istype(tolua_S,3,LUA_TSTRING,0) ||
 !tolua_istype(tolua_S,4,LUA_TSTRING,0) ||
 !tolua_isnoobj(tolua_S,5)
 )
 goto tolua_lerror;
 else
 {
  char* buf = ((char*)  tolua_getstring(tolua_S,1,0));
  int max = ((int)  tolua_getnumber(tolua_S,2,0));
  cptr path = ((cptr)  tolua_getstring(tolua_S,3,0));
  cptr file = ((cptr)  tolua_getstring(tolua_S,4,0));
 {
  errr toluaI_ret = (errr)  path_build(buf,max,path,file);
 tolua_pushnumber(tolua_S,(long)toluaI_ret);
 }
 }
 return 1;
tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'path_build'.");
 return 0;
}

/* function: move_cursor */
static int toluaI_util_move_cursor00(lua_State* tolua_S)
{
 if (
 !tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
 !tolua_istype(tolua_S,2,LUA_TNUMBER,0) ||
 !tolua_isnoobj(tolua_S,3)
 )
 goto tolua_lerror;
 else
 {
  int row = ((int)  tolua_getnumber(tolua_S,1,0));
  int col = ((int)  tolua_getnumber(tolua_S,2,0));
 {
  move_cursor(row,col);
 }
 }
 return 0;
tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'move_cursor'.");
 return 0;
}

/* function: flush */
static int toluaI_util_flush00(lua_State* tolua_S)
{
 if (
 !tolua_isnoobj(tolua_S,1)
 )
 goto tolua_lerror;
 else
 {
 {
  flush();
 }
 }
 return 0;
tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'flush'.");
 return 0;
}

/* get function: inkey_scan */
static int toluaI_get_util_inkey_scan(lua_State* tolua_S)
{
 tolua_pushnumber(tolua_S,(long)inkey_scan);
 return 1;
}

/* set function: inkey_scan */
static int toluaI_set_util_inkey_scan(lua_State* tolua_S)
{
 if (!tolua_istype(tolua_S,1,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  inkey_scan = ((bool)  tolua_getnumber(tolua_S,1,0));
 return 0;
}

/* function: inkey */
static int toluaI_util_inkey00(lua_State* tolua_S)
{
 if (
 !tolua_isnoobj(tolua_S,1)
 )
 goto tolua_lerror;
 else
 {
 {
  char toluaI_ret = (char)  inkey();
 tolua_pushnumber(tolua_S,(long)toluaI_ret);
 }
 }
 return 1;
tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'inkey'.");
 return 0;
}

/* function: cmsg_print */
static int toluaI_util_cmsg_print00(lua_State* tolua_S)
{
 if (
 !tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
 !tolua_istype(tolua_S,2,LUA_TSTRING,0) ||
 !tolua_isnoobj(tolua_S,3)
 )
 goto tolua_lerror;
 else
 {
  byte color = ((byte)  tolua_getnumber(tolua_S,1,0));
  cptr msg = ((cptr)  tolua_getstring(tolua_S,2,0));
 {
  cmsg_print(color,msg);
 }
 }
 return 0;
tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'cmsg_print'.");
 return 0;
}

/* function: msg_print */
static int toluaI_util_msg_print00(lua_State* tolua_S)
{
 if (
 !tolua_istype(tolua_S,1,LUA_TSTRING,0) ||
 !tolua_isnoobj(tolua_S,2)
 )
 goto tolua_lerror;
 else
 {
  cptr msg = ((cptr)  tolua_getstring(tolua_S,1,0));
 {
  msg_print(msg);
 }
 }
 return 0;
tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'msg_print'.");
 return 0;
}

/* function: screen_save */
static int toluaI_util_screen_save00(lua_State* tolua_S)
{
 if (
 !tolua_isnoobj(tolua_S,1)
 )
 goto tolua_lerror;
 else
 {
 {
  screen_save();
 }
 }
 return 0;
tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'screen_save'.");
 return 0;
}

/* function: screen_load */
static int toluaI_util_screen_load00(lua_State* tolua_S)
{
 if (
 !tolua_isnoobj(tolua_S,1)
 )
 goto tolua_lerror;
 else
 {
 {
  screen_load();
 }
 }
 return 0;
tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'screen_load'.");
 return 0;
}

/* function: Term_save */
static int toluaI_util_Term_save00(lua_State* tolua_S)
{
 if (
 !tolua_isnoobj(tolua_S,1)
 )
 goto tolua_lerror;
 else
 {
 {
  errr toluaI_ret = (errr)  Term_save();
 tolua_pushnumber(tolua_S,(long)toluaI_ret);
 }
 }
 return 1;
tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'Term_save'.");
 return 0;
}

/* function: Term_load */
static int toluaI_util_Term_load00(lua_State* tolua_S)
{
 if (
 !tolua_isnoobj(tolua_S,1)
 )
 goto tolua_lerror;
 else
 {
 {
  errr toluaI_ret = (errr)  Term_load();
 tolua_pushnumber(tolua_S,(long)toluaI_ret);
 }
 }
 return 1;
tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'Term_load'.");
 return 0;
}

/* function: c_put_str */
static int toluaI_util_c_put_str00(lua_State* tolua_S)
{
 if (
 !tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
 !tolua_istype(tolua_S,2,LUA_TSTRING,0) ||
 !tolua_istype(tolua_S,3,LUA_TNUMBER,0) ||
 !tolua_istype(tolua_S,4,LUA_TNUMBER,0) ||
 !tolua_isnoobj(tolua_S,5)
 )
 goto tolua_lerror;
 else
 {
  byte attr = ((byte)  tolua_getnumber(tolua_S,1,0));
  cptr str = ((cptr)  tolua_getstring(tolua_S,2,0));
  int row = ((int)  tolua_getnumber(tolua_S,3,0));
  int col = ((int)  tolua_getnumber(tolua_S,4,0));
 {
  c_put_str(attr,str,row,col);
 }
 }
 return 0;
tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'c_put_str'.");
 return 0;
}

/* function: c_prt */
static int toluaI_util_c_prt00(lua_State* tolua_S)
{
 if (
 !tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
 !tolua_istype(tolua_S,2,LUA_TSTRING,0) ||
 !tolua_istype(tolua_S,3,LUA_TNUMBER,0) ||
 !tolua_istype(tolua_S,4,LUA_TNUMBER,0) ||
 !tolua_isnoobj(tolua_S,5)
 )
 goto tolua_lerror;
 else
 {
  byte attr = ((byte)  tolua_getnumber(tolua_S,1,0));
  cptr str = ((cptr)  tolua_getstring(tolua_S,2,0));
  int row = ((int)  tolua_getnumber(tolua_S,3,0));
  int col = ((int)  tolua_getnumber(tolua_S,4,0));
 {
  c_prt(attr,str,row,col);
 }
 }
 return 0;
tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'c_prt'.");
 return 0;
}

/* function: prt */
static int toluaI_util_prt00(lua_State* tolua_S)
{
 if (
 !tolua_istype(tolua_S,1,LUA_TSTRING,0) ||
 !tolua_istype(tolua_S,2,LUA_TNUMBER,0) ||
 !tolua_istype(tolua_S,3,LUA_TNUMBER,0) ||
 !tolua_isnoobj(tolua_S,4)
 )
 goto tolua_lerror;
 else
 {
  cptr str = ((cptr)  tolua_getstring(tolua_S,1,0));
  int row = ((int)  tolua_getnumber(tolua_S,2,0));
  int col = ((int)  tolua_getnumber(tolua_S,3,0));
 {
  prt(str,row,col);
 }
 }
 return 0;
tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'prt'.");
 return 0;
}

/* function: clear_from */
static int toluaI_util_clear_from00(lua_State* tolua_S)
{
 if (
 !tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
 !tolua_isnoobj(tolua_S,2)
 )
 goto tolua_lerror;
 else
 {
  int row = ((int)  tolua_getnumber(tolua_S,1,0));
 {
  clear_from(row);
 }
 }
 return 0;
tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'clear_from'.");
 return 0;
}

/* function: askfor_aux */
static int toluaI_util_askfor_aux00(lua_State* tolua_S)
{
 if (
 !tolua_istype(tolua_S,1,LUA_TSTRING,0) ||
 !tolua_istype(tolua_S,2,LUA_TNUMBER,0) ||
 !tolua_isnoobj(tolua_S,3)
 )
 goto tolua_lerror;
 else
 {
  char* buf = ((char*)  tolua_getstring(tolua_S,1,0));
  int len = ((int)  tolua_getnumber(tolua_S,2,0));
 {
  bool toluaI_ret = (bool)  askfor_aux(buf,len);
 tolua_pushnumber(tolua_S,(long)toluaI_ret);
 }
 }
 return 1;
tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'askfor_aux'.");
 return 0;
}

/* function: get_string */
static int toluaI_util_get_string00(lua_State* tolua_S)
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
  cptr prompt = ((cptr)  tolua_getstring(tolua_S,1,0));
  char* buf = ((char*)  tolua_getstring(tolua_S,2,0));
  int len = ((int)  tolua_getnumber(tolua_S,3,0));
 {
  bool toluaI_ret = (bool)  get_string(prompt,buf,len);
 tolua_pushnumber(tolua_S,(long)toluaI_ret);
 }
 }
 return 1;
tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'get_string'.");
 return 0;
}

/* function: get_check */
static int toluaI_util_get_check00(lua_State* tolua_S)
{
 if (
 !tolua_istype(tolua_S,1,LUA_TSTRING,0) ||
 !tolua_isnoobj(tolua_S,2)
 )
 goto tolua_lerror;
 else
 {
  cptr prompt = ((cptr)  tolua_getstring(tolua_S,1,0));
 {
  bool toluaI_ret = (bool)  get_check(prompt);
 tolua_pushnumber(tolua_S,(long)toluaI_ret);
 }
 }
 return 1;
tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'get_check'.");
 return 0;
}

/* function: get_com_lua */
static int toluaI_util_get_com00(lua_State* tolua_S)
{
 if (
 !tolua_istype(tolua_S,1,LUA_TSTRING,0) ||
 !tolua_istype(tolua_S,2,LUA_TNUMBER,0) ||
 !tolua_isnoobj(tolua_S,3)
 )
 goto tolua_lerror;
 else
 {
  cptr promtp = ((cptr)  tolua_getstring(tolua_S,1,0));
  int com = ((int)  tolua_getnumber(tolua_S,2,0));
 {
  bool toluaI_ret = (bool)  get_com_lua(promtp,&com);
 tolua_pushnumber(tolua_S,(long)toluaI_ret);
 tolua_pushnumber(tolua_S,(long)com);
 }
 }
 return 2;
tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'get_com'.");
 return 0;
}

/* function: get_quantity */
static int toluaI_util_get_quantity00(lua_State* tolua_S)
{
 if (
 !tolua_istype(tolua_S,1,LUA_TSTRING,0) ||
 !tolua_istype(tolua_S,2,LUA_TNUMBER,0) ||
 !tolua_isnoobj(tolua_S,3)
 )
 goto tolua_lerror;
 else
 {
  cptr prompt = ((cptr)  tolua_getstring(tolua_S,1,0));
  s32b max = ((s32b)  tolua_getnumber(tolua_S,2,0));
 {
  s32b toluaI_ret = (s32b)  get_quantity(prompt,max);
 tolua_pushnumber(tolua_S,(long)toluaI_ret);
 }
 }
 return 1;
tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'get_quantity'.");
 return 0;
}

/* function: test_monster_name */
static int toluaI_util_test_monster_name00(lua_State* tolua_S)
{
 if (
 !tolua_istype(tolua_S,1,LUA_TSTRING,0) ||
 !tolua_isnoobj(tolua_S,2)
 )
 goto tolua_lerror;
 else
 {
  cptr name = ((cptr)  tolua_getstring(tolua_S,1,0));
 {
  int toluaI_ret = (int)  test_monster_name(name);
 tolua_pushnumber(tolua_S,(long)toluaI_ret);
 }
 }
 return 1;
tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'test_monster_name'.");
 return 0;
}

/* function: test_item_name */
static int toluaI_util_test_item_name00(lua_State* tolua_S)
{
 if (
 !tolua_istype(tolua_S,1,LUA_TSTRING,0) ||
 !tolua_isnoobj(tolua_S,2)
 )
 goto tolua_lerror;
 else
 {
  cptr name = ((cptr)  tolua_getstring(tolua_S,1,0));
 {
  int toluaI_ret = (int)  test_item_name(name);
 tolua_pushnumber(tolua_S,(long)toluaI_ret);
 }
 }
 return 1;
tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'test_item_name'.");
 return 0;
}

/* function: luck */
static int toluaI_util_luck00(lua_State* tolua_S)
{
 if (
 !tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
 !tolua_istype(tolua_S,2,LUA_TNUMBER,0) ||
 !tolua_isnoobj(tolua_S,3)
 )
 goto tolua_lerror;
 else
 {
  int min = ((int)  tolua_getnumber(tolua_S,1,0));
  int max = ((int)  tolua_getnumber(tolua_S,2,0));
 {
  int toluaI_ret = (int)  luck(min,max);
 tolua_pushnumber(tolua_S,(long)toluaI_ret);
 }
 }
 return 1;
tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'luck'.");
 return 0;
}

/* function: get_player_race_name */
static int toluaI_util_get_player_race_name00(lua_State* tolua_S)
{
 if (
 !tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
 !tolua_istype(tolua_S,2,LUA_TNUMBER,0) ||
 !tolua_isnoobj(tolua_S,3)
 )
 goto tolua_lerror;
 else
 {
  int pr = ((int)  tolua_getnumber(tolua_S,1,0));
  int ps = ((int)  tolua_getnumber(tolua_S,2,0));
 {
  cptr toluaI_ret = (cptr)  get_player_race_name(pr,ps);
 tolua_pushstring(tolua_S,(const char*)toluaI_ret);
 }
 }
 return 1;
tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'get_player_race_name'.");
 return 0;
}

/* function: quit */
static int toluaI_util_quit00(lua_State* tolua_S)
{
 if (
 !tolua_istype(tolua_S,1,LUA_TSTRING,0) ||
 !tolua_isnoobj(tolua_S,2)
 )
 goto tolua_lerror;
 else
 {
  cptr str = ((cptr)  tolua_getstring(tolua_S,1,0));
 {
  quit(str);
 }
 }
 return 0;
tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'quit'.");
 return 0;
}

/* function: value_scale */
static int toluaI_util_value_scale00(lua_State* tolua_S)
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
  int value = ((int)  tolua_getnumber(tolua_S,1,0));
  int vmax = ((int)  tolua_getnumber(tolua_S,2,0));
  int max = ((int)  tolua_getnumber(tolua_S,3,0));
  int min = ((int)  tolua_getnumber(tolua_S,4,0));
 {
  s32b toluaI_ret = (s32b)  value_scale(value,vmax,max,min);
 tolua_pushnumber(tolua_S,(long)toluaI_ret);
 }
 }
 return 1;
tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'value_scale'.");
 return 0;
}

/* get function: process_hooks_restart */
static int toluaI_get_util_process_hooks_restart(lua_State* tolua_S)
{
 tolua_pushnumber(tolua_S,(long)process_hooks_restart);
 return 1;
}

/* set function: process_hooks_restart */
static int toluaI_set_util_process_hooks_restart(lua_State* tolua_S)
{
 if (!tolua_istype(tolua_S,1,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  process_hooks_restart = ((int)  tolua_getnumber(tolua_S,1,0));
 return 0;
}

/* function: dump_hooks */
static int toluaI_util_dump_hooks00(lua_State* tolua_S)
{
 if (
 !tolua_isnoobj(tolua_S,1)
 )
 goto tolua_lerror;
 else
 {
 {
  dump_hooks();
 }
 }
 return 0;
tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'dump_hooks'.");
 return 0;
}

/* function: add_hook_script */
static int toluaI_util_add_hook_script00(lua_State* tolua_S)
{
 if (
 !tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
 !tolua_istype(tolua_S,2,LUA_TSTRING,0) ||
 !tolua_istype(tolua_S,3,LUA_TSTRING,0) ||
 !tolua_isnoobj(tolua_S,4)
 )
 goto tolua_lerror;
 else
 {
  int h_idx = ((int)  tolua_getnumber(tolua_S,1,0));
  char* script = ((char*)  tolua_getstring(tolua_S,2,0));
  cptr name = ((cptr)  tolua_getstring(tolua_S,3,0));
 {
  add_hook_script(h_idx,script,name);
 }
 }
 return 0;
tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'add_hook_script'.");
 return 0;
}

/* function: del_hook_name */
static int toluaI_util_del_hook_name00(lua_State* tolua_S)
{
 if (
 !tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
 !tolua_istype(tolua_S,2,LUA_TSTRING,0) ||
 !tolua_isnoobj(tolua_S,3)
 )
 goto tolua_lerror;
 else
 {
  int h_idx = ((int)  tolua_getnumber(tolua_S,1,0));
  cptr name = ((cptr)  tolua_getstring(tolua_S,2,0));
 {
  del_hook_name(h_idx,name);
 }
 }
 return 0;
tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'del_hook_name'.");
 return 0;
}

/* function: tome_dofile */
static int toluaI_util_tome_dofile00(lua_State* tolua_S)
{
 if (
 !tolua_istype(tolua_S,1,LUA_TSTRING,0) ||
 !tolua_isnoobj(tolua_S,2)
 )
 goto tolua_lerror;
 else
 {
  char* file = ((char*)  tolua_getstring(tolua_S,1,0));
 {
  bool toluaI_ret = (bool)  tome_dofile(file);
 tolua_pushnumber(tolua_S,(long)toluaI_ret);
 }
 }
 return 1;
tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'tome_dofile'.");
 return 0;
}

/* function: exec_lua */
static int toluaI_util_exec_lua00(lua_State* tolua_S)
{
 if (
 !tolua_istype(tolua_S,1,LUA_TSTRING,0) ||
 !tolua_isnoobj(tolua_S,2)
 )
 goto tolua_lerror;
 else
 {
  char* file = ((char*)  tolua_getstring(tolua_S,1,0));
 {
  int toluaI_ret = (int)  exec_lua(file);
 tolua_pushnumber(tolua_S,(long)toluaI_ret);
 }
 }
 return 1;
tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'exec_lua'.");
 return 0;
}

/* function: lua_print_hook */
static int toluaI_util_print_hook00(lua_State* tolua_S)
{
 if (
 !tolua_istype(tolua_S,1,LUA_TSTRING,0) ||
 !tolua_isnoobj(tolua_S,2)
 )
 goto tolua_lerror;
 else
 {
  cptr str = ((cptr)  tolua_getstring(tolua_S,1,0));
 {
  lua_print_hook(str);
 }
 }
 return 0;
tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'print_hook'.");
 return 0;
}

/* function: register_savefile */
static int toluaI_util_register_savefile00(lua_State* tolua_S)
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
  register_savefile(num);
 }
 }
 return 0;
tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'register_savefile'.");
 return 0;
}

/* function: save_number_key */
static int toluaI_util_save_number_key00(lua_State* tolua_S)
{
 if (
 !tolua_istype(tolua_S,1,LUA_TSTRING,0) ||
 !tolua_istype(tolua_S,2,LUA_TNUMBER,0) ||
 !tolua_isnoobj(tolua_S,3)
 )
 goto tolua_lerror;
 else
 {
  char* key = ((char*)  tolua_getstring(tolua_S,1,0));
  s32b val = ((s32b)  tolua_getnumber(tolua_S,2,0));
 {
  save_number_key(key,val);
 }
 }
 return 0;
tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'save_number_key'.");
 return 0;
}

/* get function: adj_mag_study */
static int toluaI_get_util_adj_mag_study(lua_State* tolua_S)
{
 int toluaI_index;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 tolua_error(tolua_S,"invalid type in array indexing.");
 toluaI_index = (int)tolua_getnumber(tolua_S,2,0)-1;
 if (toluaI_index<0 || toluaI_index>=100)
 tolua_error(tolua_S,"array indexing out of range.");
 tolua_pushnumber(tolua_S,(long)adj_mag_study[toluaI_index]);
 return 1;
}

/* set function: adj_mag_study */
static int toluaI_set_util_adj_mag_study(lua_State* tolua_S)
{
 int toluaI_index;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 tolua_error(tolua_S,"invalid type in array indexing.");
 toluaI_index = (int)tolua_getnumber(tolua_S,2,0)-1;
 if (toluaI_index<0 || toluaI_index>=100)
 tolua_error(tolua_S,"array indexing out of range.");
  adj_mag_study[toluaI_index] = ((byte)  tolua_getnumber(tolua_S,3,0));
 return 0;
}

/* get function: adj_mag_mana */
static int toluaI_get_util_adj_mag_mana(lua_State* tolua_S)
{
 int toluaI_index;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 tolua_error(tolua_S,"invalid type in array indexing.");
 toluaI_index = (int)tolua_getnumber(tolua_S,2,0)-1;
 if (toluaI_index<0 || toluaI_index>=100)
 tolua_error(tolua_S,"array indexing out of range.");
 tolua_pushnumber(tolua_S,(long)adj_mag_mana[toluaI_index]);
 return 1;
}

/* set function: adj_mag_mana */
static int toluaI_set_util_adj_mag_mana(lua_State* tolua_S)
{
 int toluaI_index;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 tolua_error(tolua_S,"invalid type in array indexing.");
 toluaI_index = (int)tolua_getnumber(tolua_S,2,0)-1;
 if (toluaI_index<0 || toluaI_index>=100)
 tolua_error(tolua_S,"array indexing out of range.");
  adj_mag_mana[toluaI_index] = ((byte)  tolua_getnumber(tolua_S,3,0));
 return 0;
}

/* get function: adj_mag_fail */
static int toluaI_get_util_adj_mag_fail(lua_State* tolua_S)
{
 int toluaI_index;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 tolua_error(tolua_S,"invalid type in array indexing.");
 toluaI_index = (int)tolua_getnumber(tolua_S,2,0)-1;
 if (toluaI_index<0 || toluaI_index>=100)
 tolua_error(tolua_S,"array indexing out of range.");
 tolua_pushnumber(tolua_S,(long)adj_mag_fail[toluaI_index]);
 return 1;
}

/* set function: adj_mag_fail */
static int toluaI_set_util_adj_mag_fail(lua_State* tolua_S)
{
 int toluaI_index;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 tolua_error(tolua_S,"invalid type in array indexing.");
 toluaI_index = (int)tolua_getnumber(tolua_S,2,0)-1;
 if (toluaI_index<0 || toluaI_index>=100)
 tolua_error(tolua_S,"array indexing out of range.");
  adj_mag_fail[toluaI_index] = ((byte)  tolua_getnumber(tolua_S,3,0));
 return 0;
}

/* get function: adj_mag_stat */
static int toluaI_get_util_adj_mag_stat(lua_State* tolua_S)
{
 int toluaI_index;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 tolua_error(tolua_S,"invalid type in array indexing.");
 toluaI_index = (int)tolua_getnumber(tolua_S,2,0)-1;
 if (toluaI_index<0 || toluaI_index>=100)
 tolua_error(tolua_S,"array indexing out of range.");
 tolua_pushnumber(tolua_S,(long)adj_mag_stat[toluaI_index]);
 return 1;
}

/* set function: adj_mag_stat */
static int toluaI_set_util_adj_mag_stat(lua_State* tolua_S)
{
 int toluaI_index;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 tolua_error(tolua_S,"invalid type in array indexing.");
 toluaI_index = (int)tolua_getnumber(tolua_S,2,0)-1;
 if (toluaI_index<0 || toluaI_index>=100)
 tolua_error(tolua_S,"array indexing out of range.");
  adj_mag_stat[toluaI_index] = ((byte)  tolua_getnumber(tolua_S,3,0));
 return 0;
}

/* get function: adj_chr_gold */
static int toluaI_get_util_adj_chr_gold(lua_State* tolua_S)
{
 int toluaI_index;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 tolua_error(tolua_S,"invalid type in array indexing.");
 toluaI_index = (int)tolua_getnumber(tolua_S,2,0)-1;
 if (toluaI_index<0 || toluaI_index>=100)
 tolua_error(tolua_S,"array indexing out of range.");
 tolua_pushnumber(tolua_S,(long)adj_chr_gold[toluaI_index]);
 return 1;
}

/* set function: adj_chr_gold */
static int toluaI_set_util_adj_chr_gold(lua_State* tolua_S)
{
 int toluaI_index;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 tolua_error(tolua_S,"invalid type in array indexing.");
 toluaI_index = (int)tolua_getnumber(tolua_S,2,0)-1;
 if (toluaI_index<0 || toluaI_index>=100)
 tolua_error(tolua_S,"array indexing out of range.");
  adj_chr_gold[toluaI_index] = ((byte)  tolua_getnumber(tolua_S,3,0));
 return 0;
}

/* get function: adj_int_dev */
static int toluaI_get_util_adj_int_dev(lua_State* tolua_S)
{
 int toluaI_index;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 tolua_error(tolua_S,"invalid type in array indexing.");
 toluaI_index = (int)tolua_getnumber(tolua_S,2,0)-1;
 if (toluaI_index<0 || toluaI_index>=100)
 tolua_error(tolua_S,"array indexing out of range.");
 tolua_pushnumber(tolua_S,(long)adj_int_dev[toluaI_index]);
 return 1;
}

/* set function: adj_int_dev */
static int toluaI_set_util_adj_int_dev(lua_State* tolua_S)
{
 int toluaI_index;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 tolua_error(tolua_S,"invalid type in array indexing.");
 toluaI_index = (int)tolua_getnumber(tolua_S,2,0)-1;
 if (toluaI_index<0 || toluaI_index>=100)
 tolua_error(tolua_S,"array indexing out of range.");
  adj_int_dev[toluaI_index] = ((byte)  tolua_getnumber(tolua_S,3,0));
 return 0;
}

/* get function: adj_wis_sav */
static int toluaI_get_util_adj_wis_sav(lua_State* tolua_S)
{
 int toluaI_index;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 tolua_error(tolua_S,"invalid type in array indexing.");
 toluaI_index = (int)tolua_getnumber(tolua_S,2,0)-1;
 if (toluaI_index<0 || toluaI_index>=100)
 tolua_error(tolua_S,"array indexing out of range.");
 tolua_pushnumber(tolua_S,(long)adj_wis_sav[toluaI_index]);
 return 1;
}

/* set function: adj_wis_sav */
static int toluaI_set_util_adj_wis_sav(lua_State* tolua_S)
{
 int toluaI_index;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 tolua_error(tolua_S,"invalid type in array indexing.");
 toluaI_index = (int)tolua_getnumber(tolua_S,2,0)-1;
 if (toluaI_index<0 || toluaI_index>=100)
 tolua_error(tolua_S,"array indexing out of range.");
  adj_wis_sav[toluaI_index] = ((byte)  tolua_getnumber(tolua_S,3,0));
 return 0;
}

/* get function: adj_dex_dis */
static int toluaI_get_util_adj_dex_dis(lua_State* tolua_S)
{
 int toluaI_index;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 tolua_error(tolua_S,"invalid type in array indexing.");
 toluaI_index = (int)tolua_getnumber(tolua_S,2,0)-1;
 if (toluaI_index<0 || toluaI_index>=100)
 tolua_error(tolua_S,"array indexing out of range.");
 tolua_pushnumber(tolua_S,(long)adj_dex_dis[toluaI_index]);
 return 1;
}

/* set function: adj_dex_dis */
static int toluaI_set_util_adj_dex_dis(lua_State* tolua_S)
{
 int toluaI_index;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 tolua_error(tolua_S,"invalid type in array indexing.");
 toluaI_index = (int)tolua_getnumber(tolua_S,2,0)-1;
 if (toluaI_index<0 || toluaI_index>=100)
 tolua_error(tolua_S,"array indexing out of range.");
  adj_dex_dis[toluaI_index] = ((byte)  tolua_getnumber(tolua_S,3,0));
 return 0;
}

/* get function: adj_int_dis */
static int toluaI_get_util_adj_int_dis(lua_State* tolua_S)
{
 int toluaI_index;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 tolua_error(tolua_S,"invalid type in array indexing.");
 toluaI_index = (int)tolua_getnumber(tolua_S,2,0)-1;
 if (toluaI_index<0 || toluaI_index>=100)
 tolua_error(tolua_S,"array indexing out of range.");
 tolua_pushnumber(tolua_S,(long)adj_int_dis[toluaI_index]);
 return 1;
}

/* set function: adj_int_dis */
static int toluaI_set_util_adj_int_dis(lua_State* tolua_S)
{
 int toluaI_index;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 tolua_error(tolua_S,"invalid type in array indexing.");
 toluaI_index = (int)tolua_getnumber(tolua_S,2,0)-1;
 if (toluaI_index<0 || toluaI_index>=100)
 tolua_error(tolua_S,"array indexing out of range.");
  adj_int_dis[toluaI_index] = ((byte)  tolua_getnumber(tolua_S,3,0));
 return 0;
}

/* get function: adj_dex_ta */
static int toluaI_get_util_adj_dex_ta(lua_State* tolua_S)
{
 int toluaI_index;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 tolua_error(tolua_S,"invalid type in array indexing.");
 toluaI_index = (int)tolua_getnumber(tolua_S,2,0)-1;
 if (toluaI_index<0 || toluaI_index>=100)
 tolua_error(tolua_S,"array indexing out of range.");
 tolua_pushnumber(tolua_S,(long)adj_dex_ta[toluaI_index]);
 return 1;
}

/* set function: adj_dex_ta */
static int toluaI_set_util_adj_dex_ta(lua_State* tolua_S)
{
 int toluaI_index;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 tolua_error(tolua_S,"invalid type in array indexing.");
 toluaI_index = (int)tolua_getnumber(tolua_S,2,0)-1;
 if (toluaI_index<0 || toluaI_index>=100)
 tolua_error(tolua_S,"array indexing out of range.");
  adj_dex_ta[toluaI_index] = ((byte)  tolua_getnumber(tolua_S,3,0));
 return 0;
}

/* get function: adj_str_td */
static int toluaI_get_util_adj_str_td(lua_State* tolua_S)
{
 int toluaI_index;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 tolua_error(tolua_S,"invalid type in array indexing.");
 toluaI_index = (int)tolua_getnumber(tolua_S,2,0)-1;
 if (toluaI_index<0 || toluaI_index>=100)
 tolua_error(tolua_S,"array indexing out of range.");
 tolua_pushnumber(tolua_S,(long)adj_str_td[toluaI_index]);
 return 1;
}

/* set function: adj_str_td */
static int toluaI_set_util_adj_str_td(lua_State* tolua_S)
{
 int toluaI_index;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 tolua_error(tolua_S,"invalid type in array indexing.");
 toluaI_index = (int)tolua_getnumber(tolua_S,2,0)-1;
 if (toluaI_index<0 || toluaI_index>=100)
 tolua_error(tolua_S,"array indexing out of range.");
  adj_str_td[toluaI_index] = ((byte)  tolua_getnumber(tolua_S,3,0));
 return 0;
}

/* get function: adj_dex_th */
static int toluaI_get_util_adj_dex_th(lua_State* tolua_S)
{
 int toluaI_index;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 tolua_error(tolua_S,"invalid type in array indexing.");
 toluaI_index = (int)tolua_getnumber(tolua_S,2,0)-1;
 if (toluaI_index<0 || toluaI_index>=100)
 tolua_error(tolua_S,"array indexing out of range.");
 tolua_pushnumber(tolua_S,(long)adj_dex_th[toluaI_index]);
 return 1;
}

/* set function: adj_dex_th */
static int toluaI_set_util_adj_dex_th(lua_State* tolua_S)
{
 int toluaI_index;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 tolua_error(tolua_S,"invalid type in array indexing.");
 toluaI_index = (int)tolua_getnumber(tolua_S,2,0)-1;
 if (toluaI_index<0 || toluaI_index>=100)
 tolua_error(tolua_S,"array indexing out of range.");
  adj_dex_th[toluaI_index] = ((byte)  tolua_getnumber(tolua_S,3,0));
 return 0;
}

/* get function: adj_str_th */
static int toluaI_get_util_adj_str_th(lua_State* tolua_S)
{
 int toluaI_index;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 tolua_error(tolua_S,"invalid type in array indexing.");
 toluaI_index = (int)tolua_getnumber(tolua_S,2,0)-1;
 if (toluaI_index<0 || toluaI_index>=100)
 tolua_error(tolua_S,"array indexing out of range.");
 tolua_pushnumber(tolua_S,(long)adj_str_th[toluaI_index]);
 return 1;
}

/* set function: adj_str_th */
static int toluaI_set_util_adj_str_th(lua_State* tolua_S)
{
 int toluaI_index;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 tolua_error(tolua_S,"invalid type in array indexing.");
 toluaI_index = (int)tolua_getnumber(tolua_S,2,0)-1;
 if (toluaI_index<0 || toluaI_index>=100)
 tolua_error(tolua_S,"array indexing out of range.");
  adj_str_th[toluaI_index] = ((byte)  tolua_getnumber(tolua_S,3,0));
 return 0;
}

/* get function: adj_str_wgt */
static int toluaI_get_util_adj_str_wgt(lua_State* tolua_S)
{
 int toluaI_index;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 tolua_error(tolua_S,"invalid type in array indexing.");
 toluaI_index = (int)tolua_getnumber(tolua_S,2,0)-1;
 if (toluaI_index<0 || toluaI_index>=100)
 tolua_error(tolua_S,"array indexing out of range.");
 tolua_pushnumber(tolua_S,(long)adj_str_wgt[toluaI_index]);
 return 1;
}

/* set function: adj_str_wgt */
static int toluaI_set_util_adj_str_wgt(lua_State* tolua_S)
{
 int toluaI_index;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 tolua_error(tolua_S,"invalid type in array indexing.");
 toluaI_index = (int)tolua_getnumber(tolua_S,2,0)-1;
 if (toluaI_index<0 || toluaI_index>=100)
 tolua_error(tolua_S,"array indexing out of range.");
  adj_str_wgt[toluaI_index] = ((byte)  tolua_getnumber(tolua_S,3,0));
 return 0;
}

/* get function: adj_str_hold */
static int toluaI_get_util_adj_str_hold(lua_State* tolua_S)
{
 int toluaI_index;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 tolua_error(tolua_S,"invalid type in array indexing.");
 toluaI_index = (int)tolua_getnumber(tolua_S,2,0)-1;
 if (toluaI_index<0 || toluaI_index>=100)
 tolua_error(tolua_S,"array indexing out of range.");
 tolua_pushnumber(tolua_S,(long)adj_str_hold[toluaI_index]);
 return 1;
}

/* set function: adj_str_hold */
static int toluaI_set_util_adj_str_hold(lua_State* tolua_S)
{
 int toluaI_index;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 tolua_error(tolua_S,"invalid type in array indexing.");
 toluaI_index = (int)tolua_getnumber(tolua_S,2,0)-1;
 if (toluaI_index<0 || toluaI_index>=100)
 tolua_error(tolua_S,"array indexing out of range.");
  adj_str_hold[toluaI_index] = ((byte)  tolua_getnumber(tolua_S,3,0));
 return 0;
}

/* get function: adj_str_dig */
static int toluaI_get_util_adj_str_dig(lua_State* tolua_S)
{
 int toluaI_index;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 tolua_error(tolua_S,"invalid type in array indexing.");
 toluaI_index = (int)tolua_getnumber(tolua_S,2,0)-1;
 if (toluaI_index<0 || toluaI_index>=100)
 tolua_error(tolua_S,"array indexing out of range.");
 tolua_pushnumber(tolua_S,(long)adj_str_dig[toluaI_index]);
 return 1;
}

/* set function: adj_str_dig */
static int toluaI_set_util_adj_str_dig(lua_State* tolua_S)
{
 int toluaI_index;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 tolua_error(tolua_S,"invalid type in array indexing.");
 toluaI_index = (int)tolua_getnumber(tolua_S,2,0)-1;
 if (toluaI_index<0 || toluaI_index>=100)
 tolua_error(tolua_S,"array indexing out of range.");
  adj_str_dig[toluaI_index] = ((byte)  tolua_getnumber(tolua_S,3,0));
 return 0;
}

/* get function: adj_str_blow */
static int toluaI_get_util_adj_str_blow(lua_State* tolua_S)
{
 int toluaI_index;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 tolua_error(tolua_S,"invalid type in array indexing.");
 toluaI_index = (int)tolua_getnumber(tolua_S,2,0)-1;
 if (toluaI_index<0 || toluaI_index>=100)
 tolua_error(tolua_S,"array indexing out of range.");
 tolua_pushnumber(tolua_S,(long)adj_str_blow[toluaI_index]);
 return 1;
}

/* set function: adj_str_blow */
static int toluaI_set_util_adj_str_blow(lua_State* tolua_S)
{
 int toluaI_index;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 tolua_error(tolua_S,"invalid type in array indexing.");
 toluaI_index = (int)tolua_getnumber(tolua_S,2,0)-1;
 if (toluaI_index<0 || toluaI_index>=100)
 tolua_error(tolua_S,"array indexing out of range.");
  adj_str_blow[toluaI_index] = ((byte)  tolua_getnumber(tolua_S,3,0));
 return 0;
}

/* get function: adj_dex_blow */
static int toluaI_get_util_adj_dex_blow(lua_State* tolua_S)
{
 int toluaI_index;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 tolua_error(tolua_S,"invalid type in array indexing.");
 toluaI_index = (int)tolua_getnumber(tolua_S,2,0)-1;
 if (toluaI_index<0 || toluaI_index>=100)
 tolua_error(tolua_S,"array indexing out of range.");
 tolua_pushnumber(tolua_S,(long)adj_dex_blow[toluaI_index]);
 return 1;
}

/* set function: adj_dex_blow */
static int toluaI_set_util_adj_dex_blow(lua_State* tolua_S)
{
 int toluaI_index;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 tolua_error(tolua_S,"invalid type in array indexing.");
 toluaI_index = (int)tolua_getnumber(tolua_S,2,0)-1;
 if (toluaI_index<0 || toluaI_index>=100)
 tolua_error(tolua_S,"array indexing out of range.");
  adj_dex_blow[toluaI_index] = ((byte)  tolua_getnumber(tolua_S,3,0));
 return 0;
}

/* get function: adj_dex_safe */
static int toluaI_get_util_adj_dex_safe(lua_State* tolua_S)
{
 int toluaI_index;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 tolua_error(tolua_S,"invalid type in array indexing.");
 toluaI_index = (int)tolua_getnumber(tolua_S,2,0)-1;
 if (toluaI_index<0 || toluaI_index>=100)
 tolua_error(tolua_S,"array indexing out of range.");
 tolua_pushnumber(tolua_S,(long)adj_dex_safe[toluaI_index]);
 return 1;
}

/* set function: adj_dex_safe */
static int toluaI_set_util_adj_dex_safe(lua_State* tolua_S)
{
 int toluaI_index;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 tolua_error(tolua_S,"invalid type in array indexing.");
 toluaI_index = (int)tolua_getnumber(tolua_S,2,0)-1;
 if (toluaI_index<0 || toluaI_index>=100)
 tolua_error(tolua_S,"array indexing out of range.");
  adj_dex_safe[toluaI_index] = ((byte)  tolua_getnumber(tolua_S,3,0));
 return 0;
}

/* get function: adj_con_fix */
static int toluaI_get_util_adj_con_fix(lua_State* tolua_S)
{
 int toluaI_index;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 tolua_error(tolua_S,"invalid type in array indexing.");
 toluaI_index = (int)tolua_getnumber(tolua_S,2,0)-1;
 if (toluaI_index<0 || toluaI_index>=100)
 tolua_error(tolua_S,"array indexing out of range.");
 tolua_pushnumber(tolua_S,(long)adj_con_fix[toluaI_index]);
 return 1;
}

/* set function: adj_con_fix */
static int toluaI_set_util_adj_con_fix(lua_State* tolua_S)
{
 int toluaI_index;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 tolua_error(tolua_S,"invalid type in array indexing.");
 toluaI_index = (int)tolua_getnumber(tolua_S,2,0)-1;
 if (toluaI_index<0 || toluaI_index>=100)
 tolua_error(tolua_S,"array indexing out of range.");
  adj_con_fix[toluaI_index] = ((byte)  tolua_getnumber(tolua_S,3,0));
 return 0;
}

/* get function: adj_con_mhp */
static int toluaI_get_util_adj_con_mhp(lua_State* tolua_S)
{
 int toluaI_index;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 tolua_error(tolua_S,"invalid type in array indexing.");
 toluaI_index = (int)tolua_getnumber(tolua_S,2,0)-1;
 if (toluaI_index<0 || toluaI_index>=100)
 tolua_error(tolua_S,"array indexing out of range.");
 tolua_pushnumber(tolua_S,(long)adj_con_mhp[toluaI_index]);
 return 1;
}

/* set function: adj_con_mhp */
static int toluaI_set_util_adj_con_mhp(lua_State* tolua_S)
{
 int toluaI_index;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 tolua_error(tolua_S,"invalid type in array indexing.");
 toluaI_index = (int)tolua_getnumber(tolua_S,2,0)-1;
 if (toluaI_index<0 || toluaI_index>=100)
 tolua_error(tolua_S,"array indexing out of range.");
  adj_con_mhp[toluaI_index] = ((byte)  tolua_getnumber(tolua_S,3,0));
 return 0;
}

/* function: repeat_push */
static int toluaI_util_repeat_push00(lua_State* tolua_S)
{
 if (
 !tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
 !tolua_isnoobj(tolua_S,2)
 )
 goto tolua_lerror;
 else
 {
  int what = ((int)  tolua_getnumber(tolua_S,1,0));
 {
  repeat_push(what);
 }
 }
 return 0;
tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'repeat_push'.");
 return 0;
}

/* function: repeat_pull */
static int toluaI_util_repeat_pull00(lua_State* tolua_S)
{
 if (
 !tolua_istype(tolua_S,1,LUA_TNUMBER,1) ||
 !tolua_isnoobj(tolua_S,2)
 )
 goto tolua_lerror;
 else
 {
  int what = ((int)  tolua_getnumber(tolua_S,1,0));
 {
  bool toluaI_ret = (bool)  repeat_pull(&what);
 tolua_pushnumber(tolua_S,(long)toluaI_ret);
 tolua_pushnumber(tolua_S,(long)what);
 }
 }
 return 2;
tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'repeat_pull'.");
 return 0;
}

/* function: repeat_check */
static int toluaI_util_repeat_check00(lua_State* tolua_S)
{
 if (
 !tolua_isnoobj(tolua_S,1)
 )
 goto tolua_lerror;
 else
 {
 {
  repeat_check();
 }
 }
 return 0;
tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'repeat_check'.");
 return 0;
}

/* function: get_count */
static int toluaI_util_get_count00(lua_State* tolua_S)
{
 if (
 !tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
 !tolua_istype(tolua_S,2,LUA_TNUMBER,0) ||
 !tolua_isnoobj(tolua_S,3)
 )
 goto tolua_lerror;
 else
 {
  int number = ((int)  tolua_getnumber(tolua_S,1,0));
  int max = ((int)  tolua_getnumber(tolua_S,2,0));
 {
  get_count(number,max);
 }
 }
 return 0;
tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'get_count'.");
 return 0;
}

/* get function: info of class  cave_type */
static int toluaI_get_util_cave_type_info(lua_State* tolua_S)
{
  cave_type* self = (cave_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->info);
 return 1;
}

/* set function: info of class  cave_type */
static int toluaI_set_util_cave_type_info(lua_State* tolua_S)
{
  cave_type* self = (cave_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->info = ((u16b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: feat of class  cave_type */
static int toluaI_get_util_cave_type_feat(lua_State* tolua_S)
{
  cave_type* self = (cave_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->feat);
 return 1;
}

/* set function: feat of class  cave_type */
static int toluaI_set_util_cave_type_feat(lua_State* tolua_S)
{
  cave_type* self = (cave_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->feat = ((byte)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: o_idx of class  cave_type */
static int toluaI_get_util_cave_type_o_idx(lua_State* tolua_S)
{
  cave_type* self = (cave_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->o_idx);
 return 1;
}

/* set function: o_idx of class  cave_type */
static int toluaI_set_util_cave_type_o_idx(lua_State* tolua_S)
{
  cave_type* self = (cave_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->o_idx = ((s16b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: m_idx of class  cave_type */
static int toluaI_get_util_cave_type_m_idx(lua_State* tolua_S)
{
  cave_type* self = (cave_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->m_idx);
 return 1;
}

/* set function: m_idx of class  cave_type */
static int toluaI_set_util_cave_type_m_idx(lua_State* tolua_S)
{
  cave_type* self = (cave_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->m_idx = ((s16b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: t_idx of class  cave_type */
static int toluaI_get_util_cave_type_t_idx(lua_State* tolua_S)
{
  cave_type* self = (cave_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->t_idx);
 return 1;
}

/* set function: t_idx of class  cave_type */
static int toluaI_set_util_cave_type_t_idx(lua_State* tolua_S)
{
  cave_type* self = (cave_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->t_idx = ((s16b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: special of class  cave_type */
static int toluaI_get_util_cave_type_special(lua_State* tolua_S)
{
  cave_type* self = (cave_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->special);
 return 1;
}

/* set function: special of class  cave_type */
static int toluaI_set_util_cave_type_special(lua_State* tolua_S)
{
  cave_type* self = (cave_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->special = ((s16b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: special2 of class  cave_type */
static int toluaI_get_util_cave_type_special2(lua_State* tolua_S)
{
  cave_type* self = (cave_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->special2);
 return 1;
}

/* set function: special2 of class  cave_type */
static int toluaI_set_util_cave_type_special2(lua_State* tolua_S)
{
  cave_type* self = (cave_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->special2 = ((s16b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: inscription of class  cave_type */
static int toluaI_get_util_cave_type_inscription(lua_State* tolua_S)
{
  cave_type* self = (cave_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->inscription);
 return 1;
}

/* set function: inscription of class  cave_type */
static int toluaI_set_util_cave_type_inscription(lua_State* tolua_S)
{
  cave_type* self = (cave_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->inscription = ((s16b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: mana of class  cave_type */
static int toluaI_get_util_cave_type_mana(lua_State* tolua_S)
{
  cave_type* self = (cave_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->mana);
 return 1;
}

/* set function: mana of class  cave_type */
static int toluaI_set_util_cave_type_mana(lua_State* tolua_S)
{
  cave_type* self = (cave_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->mana = ((byte)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: mimic of class  cave_type */
static int toluaI_get_util_cave_type_mimic(lua_State* tolua_S)
{
  cave_type* self = (cave_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->mimic);
 return 1;
}

/* set function: mimic of class  cave_type */
static int toluaI_set_util_cave_type_mimic(lua_State* tolua_S)
{
  cave_type* self = (cave_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->mimic = ((byte)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: effect of class  cave_type */
static int toluaI_get_util_cave_type_effect(lua_State* tolua_S)
{
  cave_type* self = (cave_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->effect);
 return 1;
}

/* set function: effect of class  cave_type */
static int toluaI_set_util_cave_type_effect(lua_State* tolua_S)
{
  cave_type* self = (cave_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->effect = ((s16b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* function: lua_cave_is */
static int toluaI_util_cave_is00(lua_State* tolua_S)
{
 if (
 !tolua_istype(tolua_S,1,tolua_tag(tolua_S,"cave_type"),0) ||
 !tolua_istype(tolua_S,2,LUA_TNUMBER,0) ||
 !tolua_isnoobj(tolua_S,3)
 )
 goto tolua_lerror;
 else
 {
  cave_type* c_ptr = ((cave_type*)  tolua_getusertype(tolua_S,1,0));
  s32b flag = ((s32b)  tolua_getnumber(tolua_S,2,0));
 {
  bool toluaI_ret = (bool)  lua_cave_is(c_ptr,flag);
 tolua_pushnumber(tolua_S,(long)toluaI_ret);
 }
 }
 return 1;
tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'cave_is'.");
 return 0;
}

/* function: lua_get_cave */
static int toluaI_util_cave00(lua_State* tolua_S)
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
  cave_type* toluaI_ret = (cave_type*)  lua_get_cave(y,x);
 tolua_pushusertype(tolua_S,(void*)toluaI_ret,tolua_tag(tolua_S,"cave_type"));
 }
 }
 return 1;
tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'cave'.");
 return 0;
}

/* function: set_target */
static int toluaI_util_set_target00(lua_State* tolua_S)
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
  set_target(y,x);
 }
 }
 return 0;
tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'set_target'.");
 return 0;
}

/* get function: m_allow_special */
static int toluaI_get_util_m_allow_special(lua_State* tolua_S)
{
 int toluaI_index;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 tolua_error(tolua_S,"invalid type in array indexing.");
 toluaI_index = (int)tolua_getnumber(tolua_S,2,0)-1;
 if (toluaI_index<0 || toluaI_index>=max_r_idx)
 tolua_error(tolua_S,"array indexing out of range.");
 tolua_pushnumber(tolua_S,(long)m_allow_special[toluaI_index]);
 return 1;
}

/* set function: m_allow_special */
static int toluaI_set_util_m_allow_special(lua_State* tolua_S)
{
 int toluaI_index;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 tolua_error(tolua_S,"invalid type in array indexing.");
 toluaI_index = (int)tolua_getnumber(tolua_S,2,0)-1;
 if (toluaI_index<0 || toluaI_index>=max_r_idx)
 tolua_error(tolua_S,"array indexing out of range.");
  m_allow_special[toluaI_index] = ((bool)  tolua_getnumber(tolua_S,3,0));
 return 0;
}

/* get function: k_allow_special */
static int toluaI_get_util_k_allow_special(lua_State* tolua_S)
{
 int toluaI_index;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 tolua_error(tolua_S,"invalid type in array indexing.");
 toluaI_index = (int)tolua_getnumber(tolua_S,2,0)-1;
 if (toluaI_index<0 || toluaI_index>=max_k_idx)
 tolua_error(tolua_S,"array indexing out of range.");
 tolua_pushnumber(tolua_S,(long)k_allow_special[toluaI_index]);
 return 1;
}

/* set function: k_allow_special */
static int toluaI_set_util_k_allow_special(lua_State* tolua_S)
{
 int toluaI_index;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 tolua_error(tolua_S,"invalid type in array indexing.");
 toluaI_index = (int)tolua_getnumber(tolua_S,2,0)-1;
 if (toluaI_index<0 || toluaI_index>=max_k_idx)
 tolua_error(tolua_S,"array indexing out of range.");
  k_allow_special[toluaI_index] = ((bool)  tolua_getnumber(tolua_S,3,0));
 return 0;
}

/* get function: a_allow_special */
static int toluaI_get_util_a_allow_special(lua_State* tolua_S)
{
 int toluaI_index;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 tolua_error(tolua_S,"invalid type in array indexing.");
 toluaI_index = (int)tolua_getnumber(tolua_S,2,0)-1;
 if (toluaI_index<0 || toluaI_index>=max_a_idx)
 tolua_error(tolua_S,"array indexing out of range.");
 tolua_pushnumber(tolua_S,(long)a_allow_special[toluaI_index]);
 return 1;
}

/* set function: a_allow_special */
static int toluaI_set_util_a_allow_special(lua_State* tolua_S)
{
 int toluaI_index;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 tolua_error(tolua_S,"invalid type in array indexing.");
 toluaI_index = (int)tolua_getnumber(tolua_S,2,0)-1;
 if (toluaI_index<0 || toluaI_index>=max_a_idx)
 tolua_error(tolua_S,"array indexing out of range.");
  a_allow_special[toluaI_index] = ((bool)  tolua_getnumber(tolua_S,3,0));
 return 0;
}

/* function: cave_set_feat */
static int toluaI_util_cave_set_feat00(lua_State* tolua_S)
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
  int y = ((int)  tolua_getnumber(tolua_S,1,0));
  int x = ((int)  tolua_getnumber(tolua_S,2,0));
  int feat = ((int)  tolua_getnumber(tolua_S,3,0));
 {
  cave_set_feat(y,x,feat);
 }
 }
 return 0;
tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'cave_set_feat'.");
 return 0;
}

/* function: show_file */
static int toluaI_util_show_file00(lua_State* tolua_S)
{
 if (
 !tolua_istype(tolua_S,1,LUA_TSTRING,0) ||
 !tolua_istype(tolua_S,2,LUA_TSTRING,0) ||
 !tolua_istype(tolua_S,3,LUA_TNUMBER,0) ||
 !tolua_istype(tolua_S,4,LUA_TNUMBER,0) ||
 !tolua_isnoobj(tolua_S,5)
 )
 goto tolua_lerror;
 else
 {
  cptr name = ((cptr)  tolua_getstring(tolua_S,1,0));
  cptr what = ((cptr)  tolua_getstring(tolua_S,2,0));
  int line = ((int)  tolua_getnumber(tolua_S,3,0));
  int mode = ((int)  tolua_getnumber(tolua_S,4,0));
 {
  bool toluaI_ret = (bool)  show_file(name,what,line,mode);
 tolua_pushnumber(tolua_S,(long)toluaI_ret);
 }
 }
 return 1;
tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'show_file'.");
 return 0;
}

/* get function: target_who */
static int toluaI_get_util_target_who(lua_State* tolua_S)
{
 tolua_pushnumber(tolua_S,(long)target_who);
 return 1;
}

/* set function: target_who */
static int toluaI_set_util_target_who(lua_State* tolua_S)
{
 if (!tolua_istype(tolua_S,1,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  target_who = ((s16b)  tolua_getnumber(tolua_S,1,0));
 return 0;
}

/* get function: target_col */
static int toluaI_get_util_target_col(lua_State* tolua_S)
{
 tolua_pushnumber(tolua_S,(long)target_col);
 return 1;
}

/* set function: target_col */
static int toluaI_set_util_target_col(lua_State* tolua_S)
{
 if (!tolua_istype(tolua_S,1,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  target_col = ((s16b)  tolua_getnumber(tolua_S,1,0));
 return 0;
}

/* get function: target_row */
static int toluaI_get_util_target_row(lua_State* tolua_S)
{
 tolua_pushnumber(tolua_S,(long)target_row);
 return 1;
}

/* set function: target_row */
static int toluaI_set_util_target_row(lua_State* tolua_S)
{
 if (!tolua_istype(tolua_S,1,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  target_row = ((s16b)  tolua_getnumber(tolua_S,1,0));
 return 0;
}

/* get function: max_bact */
static int toluaI_get_util_max_bact(lua_State* tolua_S)
{
 tolua_pushnumber(tolua_S,(long)max_bact);
 return 1;
}

/* set function: max_bact */
static int toluaI_set_util_max_bact(lua_State* tolua_S)
{
 if (!tolua_istype(tolua_S,1,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  max_bact = ((int)  tolua_getnumber(tolua_S,1,0));
 return 0;
}

/* function: get_map_size */
static int toluaI_util_get_map_size00(lua_State* tolua_S)
{
 if (
 !tolua_istype(tolua_S,1,LUA_TSTRING,0) ||
 !tolua_istype(tolua_S,2,LUA_TNUMBER,1) ||
 !tolua_istype(tolua_S,3,LUA_TNUMBER,1) ||
 !tolua_isnoobj(tolua_S,4)
 )
 goto tolua_lerror;
 else
 {
  char* name = ((char*)  tolua_getstring(tolua_S,1,0));
  int ysize = ((int)  tolua_getnumber(tolua_S,2,0));
  int xsize = ((int)  tolua_getnumber(tolua_S,3,0));
 {
  get_map_size(name,&ysize,&xsize);
 tolua_pushnumber(tolua_S,(long)ysize);
 tolua_pushnumber(tolua_S,(long)xsize);
 }
 }
 return 2;
tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'get_map_size'.");
 return 0;
}

/* function: load_map */
static int toluaI_util_load_map00(lua_State* tolua_S)
{
 if (
 !tolua_istype(tolua_S,1,LUA_TSTRING,0) ||
 !tolua_istype(tolua_S,2,LUA_TNUMBER,1) ||
 !tolua_istype(tolua_S,3,LUA_TNUMBER,1) ||
 !tolua_isnoobj(tolua_S,4)
 )
 goto tolua_lerror;
 else
 {
  char* name = ((char*)  tolua_getstring(tolua_S,1,0));
  int y = ((int)  tolua_getnumber(tolua_S,2,2));
  int x = ((int)  tolua_getnumber(tolua_S,3,2));
 {
  load_map(name,&y,&x);
 tolua_pushnumber(tolua_S,(long)y);
 tolua_pushnumber(tolua_S,(long)x);
 }
 }
 return 2;
tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'load_map'.");
 return 0;
}

/* function: alloc_room */
static int toluaI_util_alloc_room00(lua_State* tolua_S)
{
 if (
 !tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
 !tolua_istype(tolua_S,2,LUA_TNUMBER,0) ||
 !tolua_istype(tolua_S,3,LUA_TNUMBER,0) ||
 !tolua_istype(tolua_S,4,LUA_TNUMBER,0) ||
 !tolua_istype(tolua_S,5,LUA_TNUMBER,1) ||
 !tolua_istype(tolua_S,6,LUA_TNUMBER,1) ||
 !tolua_istype(tolua_S,7,LUA_TNUMBER,1) ||
 !tolua_istype(tolua_S,8,LUA_TNUMBER,1) ||
 !tolua_isnoobj(tolua_S,9)
 )
 goto tolua_lerror;
 else
 {
  int by0 = ((int)  tolua_getnumber(tolua_S,1,0));
  int bx0 = ((int)  tolua_getnumber(tolua_S,2,0));
  int ysize = ((int)  tolua_getnumber(tolua_S,3,0));
  int xsize = ((int)  tolua_getnumber(tolua_S,4,0));
  int y1 = ((int)  tolua_getnumber(tolua_S,5,0));
  int x1 = ((int)  tolua_getnumber(tolua_S,6,0));
  int y2 = ((int)  tolua_getnumber(tolua_S,7,0));
  int x2 = ((int)  tolua_getnumber(tolua_S,8,0));
 {
  bool toluaI_ret = (bool)  alloc_room(by0,bx0,ysize,xsize,&y1,&x1,&y2,&x2);
 tolua_pushnumber(tolua_S,(long)toluaI_ret);
 tolua_pushnumber(tolua_S,(long)y1);
 tolua_pushnumber(tolua_S,(long)x1);
 tolua_pushnumber(tolua_S,(long)y2);
 tolua_pushnumber(tolua_S,(long)x2);
 }
 }
 return 5;
tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'alloc_room'.");
 return 0;
}

/* Open function */
int tolua_util_open (lua_State* tolua_S)
{
 tolua_open(tolua_S);
 toluaI_reg_types(tolua_S);
 tolua_constant(tolua_S,NULL,"TRUE",TRUE);
 tolua_constant(tolua_S,NULL,"FALSE",FALSE);
 tolua_constant(tolua_S,NULL,"ESCAPE",ESCAPE);
 tolua_constant(tolua_S,NULL,"TERM_DARK",TERM_DARK);
 tolua_constant(tolua_S,NULL,"TERM_WHITE",TERM_WHITE);
 tolua_constant(tolua_S,NULL,"TERM_SLATE",TERM_SLATE);
 tolua_constant(tolua_S,NULL,"TERM_ORANGE",TERM_ORANGE);
 tolua_constant(tolua_S,NULL,"TERM_RED",TERM_RED);
 tolua_constant(tolua_S,NULL,"TERM_GREEN",TERM_GREEN);
 tolua_constant(tolua_S,NULL,"TERM_BLUE",TERM_BLUE);
 tolua_constant(tolua_S,NULL,"TERM_UMBER",TERM_UMBER);
 tolua_constant(tolua_S,NULL,"TERM_L_DARK",TERM_L_DARK);
 tolua_constant(tolua_S,NULL,"TERM_L_WHITE",TERM_L_WHITE);
 tolua_constant(tolua_S,NULL,"TERM_VIOLET",TERM_VIOLET);
 tolua_constant(tolua_S,NULL,"TERM_YELLOW",TERM_YELLOW);
 tolua_constant(tolua_S,NULL,"TERM_L_RED",TERM_L_RED);
 tolua_constant(tolua_S,NULL,"TERM_L_GREEN",TERM_L_GREEN);
 tolua_constant(tolua_S,NULL,"TERM_L_BLUE",TERM_L_BLUE);
 tolua_constant(tolua_S,NULL,"TERM_L_UMBER",TERM_L_UMBER);
 tolua_constant(tolua_S,NULL,"HOOK_MONSTER_DEATH",HOOK_MONSTER_DEATH);
 tolua_constant(tolua_S,NULL,"HOOK_OPEN",HOOK_OPEN);
 tolua_constant(tolua_S,NULL,"HOOK_GEN_QUEST",HOOK_GEN_QUEST);
 tolua_constant(tolua_S,NULL,"HOOK_END_TURN",HOOK_END_TURN);
 tolua_constant(tolua_S,NULL,"HOOK_FEELING",HOOK_FEELING);
 tolua_constant(tolua_S,NULL,"HOOK_NEW_MONSTER",HOOK_NEW_MONSTER);
 tolua_constant(tolua_S,NULL,"HOOK_GEN_LEVEL",HOOK_GEN_LEVEL);
 tolua_constant(tolua_S,NULL,"HOOK_BUILD_ROOM1",HOOK_BUILD_ROOM1);
 tolua_constant(tolua_S,NULL,"HOOK_NEW_LEVEL",HOOK_NEW_LEVEL);
 tolua_constant(tolua_S,NULL,"HOOK_QUEST_FINISH",HOOK_QUEST_FINISH);
 tolua_constant(tolua_S,NULL,"HOOK_QUEST_FAIL",HOOK_QUEST_FAIL);
 tolua_constant(tolua_S,NULL,"HOOK_GIVE",HOOK_GIVE);
 tolua_constant(tolua_S,NULL,"HOOK_CHAR_DUMP",HOOK_CHAR_DUMP);
 tolua_constant(tolua_S,NULL,"HOOK_INIT_QUEST",HOOK_INIT_QUEST);
 tolua_constant(tolua_S,NULL,"HOOK_WILD_GEN",HOOK_WILD_GEN);
 tolua_constant(tolua_S,NULL,"HOOK_DROP",HOOK_DROP);
 tolua_constant(tolua_S,NULL,"HOOK_IDENTIFY",HOOK_IDENTIFY);
 tolua_constant(tolua_S,NULL,"HOOK_MOVE",HOOK_MOVE);
 tolua_constant(tolua_S,NULL,"HOOK_STAIR",HOOK_STAIR);
 tolua_constant(tolua_S,NULL,"HOOK_MONSTER_AI",HOOK_MONSTER_AI);
 tolua_constant(tolua_S,NULL,"HOOK_PLAYER_LEVEL",HOOK_PLAYER_LEVEL);
 tolua_constant(tolua_S,NULL,"HOOK_WIELD",HOOK_WIELD);
 tolua_constant(tolua_S,NULL,"HOOK_INIT",HOOK_INIT);
 tolua_constant(tolua_S,NULL,"HOOK_QUAFF",HOOK_QUAFF);
 tolua_constant(tolua_S,NULL,"HOOK_AIM",HOOK_AIM);
 tolua_constant(tolua_S,NULL,"HOOK_USE",HOOK_USE);
 tolua_constant(tolua_S,NULL,"HOOK_ACTIVATE",HOOK_ACTIVATE);
 tolua_constant(tolua_S,NULL,"HOOK_ZAP",HOOK_ZAP);
 tolua_constant(tolua_S,NULL,"HOOK_READ",HOOK_READ);
 tolua_constant(tolua_S,NULL,"HOOK_CALC_BONUS",HOOK_CALC_BONUS);
 tolua_constant(tolua_S,NULL,"HOOK_PLAYER_FLAGS",HOOK_PLAYER_FLAGS);
 tolua_constant(tolua_S,NULL,"HOOK_KEYPRESS",HOOK_KEYPRESS);
 tolua_constant(tolua_S,NULL,"HOOK_CHAT",HOOK_CHAT);
 tolua_constant(tolua_S,NULL,"HOOK_MON_SPEAK",HOOK_MON_SPEAK);
 tolua_constant(tolua_S,NULL,"HOOK_MKEY",HOOK_MKEY);
 tolua_constant(tolua_S,NULL,"HOOK_BIRTH_OBJECTS",HOOK_BIRTH_OBJECTS);
 tolua_constant(tolua_S,NULL,"HOOK_ACTIVATE_DESC",HOOK_ACTIVATE_DESC);
 tolua_constant(tolua_S,NULL,"HOOK_INIT_GAME",HOOK_INIT_GAME);
 tolua_constant(tolua_S,NULL,"HOOK_ACTIVATE_POWER",HOOK_ACTIVATE_POWER);
 tolua_constant(tolua_S,NULL,"HOOK_ITEM_NAME",HOOK_ITEM_NAME);
 tolua_constant(tolua_S,NULL,"HOOK_SAVE_GAME",HOOK_SAVE_GAME);
 tolua_constant(tolua_S,NULL,"HOOK_LOAD_GAME",HOOK_LOAD_GAME);
 tolua_constant(tolua_S,NULL,"HOOK_LEVEL_REGEN",HOOK_LEVEL_REGEN);
 tolua_constant(tolua_S,NULL,"HOOK_LEVEL_END_GEN",HOOK_LEVEL_END_GEN);
 tolua_constant(tolua_S,NULL,"HOOK_BUILDING_ACTION",HOOK_BUILDING_ACTION);
 tolua_globalvar(tolua_S,"turn",toluaI_get_util_turn,toluaI_set_util_turn);
 tolua_globalvar(tolua_S,"old_turn",toluaI_get_util_old_turn,toluaI_set_util_old_turn);
 tolua_globalvar(tolua_S,"cur_wid",toluaI_get_util_cur_wid,toluaI_set_util_cur_wid);
 tolua_globalvar(tolua_S,"cur_hgt",toluaI_get_util_cur_hgt,toluaI_set_util_cur_hgt);
 tolua_function(tolua_S,NULL,"bst",toluaI_util_bst00);
 tolua_function(tolua_S,NULL,"path_build",toluaI_util_path_build00);
 tolua_function(tolua_S,NULL,"move_cursor",toluaI_util_move_cursor00);
 tolua_function(tolua_S,NULL,"flush",toluaI_util_flush00);
 tolua_globalvar(tolua_S,"inkey_scan",toluaI_get_util_inkey_scan,toluaI_set_util_inkey_scan);
 tolua_function(tolua_S,NULL,"inkey",toluaI_util_inkey00);
 tolua_function(tolua_S,NULL,"cmsg_print",toluaI_util_cmsg_print00);
 tolua_function(tolua_S,NULL,"msg_print",toluaI_util_msg_print00);
 tolua_function(tolua_S,NULL,"screen_save",toluaI_util_screen_save00);
 tolua_function(tolua_S,NULL,"screen_load",toluaI_util_screen_load00);
 tolua_function(tolua_S,NULL,"Term_save",toluaI_util_Term_save00);
 tolua_function(tolua_S,NULL,"Term_load",toluaI_util_Term_load00);
 tolua_function(tolua_S,NULL,"c_put_str",toluaI_util_c_put_str00);
 tolua_function(tolua_S,NULL,"c_prt",toluaI_util_c_prt00);
 tolua_function(tolua_S,NULL,"prt",toluaI_util_prt00);
 tolua_function(tolua_S,NULL,"clear_from",toluaI_util_clear_from00);
 tolua_function(tolua_S,NULL,"askfor_aux",toluaI_util_askfor_aux00);
 tolua_function(tolua_S,NULL,"get_string",toluaI_util_get_string00);
 tolua_function(tolua_S,NULL,"get_check",toluaI_util_get_check00);
 tolua_function(tolua_S,NULL,"get_com",toluaI_util_get_com00);
 tolua_function(tolua_S,NULL,"get_quantity",toluaI_util_get_quantity00);
 tolua_function(tolua_S,NULL,"test_monster_name",toluaI_util_test_monster_name00);
 tolua_function(tolua_S,NULL,"test_item_name",toluaI_util_test_item_name00);
 tolua_function(tolua_S,NULL,"luck",toluaI_util_luck00);
 tolua_function(tolua_S,NULL,"get_player_race_name",toluaI_util_get_player_race_name00);
 tolua_function(tolua_S,NULL,"quit",toluaI_util_quit00);
 tolua_function(tolua_S,NULL,"value_scale",toluaI_util_value_scale00);
 tolua_globalvar(tolua_S,"process_hooks_restart",toluaI_get_util_process_hooks_restart,toluaI_set_util_process_hooks_restart);
 tolua_function(tolua_S,NULL,"dump_hooks",toluaI_util_dump_hooks00);
 tolua_function(tolua_S,NULL,"add_hook_script",toluaI_util_add_hook_script00);
 tolua_function(tolua_S,NULL,"del_hook_name",toluaI_util_del_hook_name00);
 tolua_function(tolua_S,NULL,"tome_dofile",toluaI_util_tome_dofile00);
 tolua_function(tolua_S,NULL,"exec_lua",toluaI_util_exec_lua00);
 tolua_function(tolua_S,NULL,"print_hook",toluaI_util_print_hook00);
 tolua_function(tolua_S,NULL,"register_savefile",toluaI_util_register_savefile00);
 tolua_function(tolua_S,NULL,"save_number_key",toluaI_util_save_number_key00);
 tolua_globalarray(tolua_S,"adj_mag_study",toluaI_get_util_adj_mag_study,toluaI_set_util_adj_mag_study);
 tolua_globalarray(tolua_S,"adj_mag_mana",toluaI_get_util_adj_mag_mana,toluaI_set_util_adj_mag_mana);
 tolua_globalarray(tolua_S,"adj_mag_fail",toluaI_get_util_adj_mag_fail,toluaI_set_util_adj_mag_fail);
 tolua_globalarray(tolua_S,"adj_mag_stat",toluaI_get_util_adj_mag_stat,toluaI_set_util_adj_mag_stat);
 tolua_globalarray(tolua_S,"adj_chr_gold",toluaI_get_util_adj_chr_gold,toluaI_set_util_adj_chr_gold);
 tolua_globalarray(tolua_S,"adj_int_dev",toluaI_get_util_adj_int_dev,toluaI_set_util_adj_int_dev);
 tolua_globalarray(tolua_S,"adj_wis_sav",toluaI_get_util_adj_wis_sav,toluaI_set_util_adj_wis_sav);
 tolua_globalarray(tolua_S,"adj_dex_dis",toluaI_get_util_adj_dex_dis,toluaI_set_util_adj_dex_dis);
 tolua_globalarray(tolua_S,"adj_int_dis",toluaI_get_util_adj_int_dis,toluaI_set_util_adj_int_dis);
 tolua_globalarray(tolua_S,"adj_dex_ta",toluaI_get_util_adj_dex_ta,toluaI_set_util_adj_dex_ta);
 tolua_globalarray(tolua_S,"adj_str_td",toluaI_get_util_adj_str_td,toluaI_set_util_adj_str_td);
 tolua_globalarray(tolua_S,"adj_dex_th",toluaI_get_util_adj_dex_th,toluaI_set_util_adj_dex_th);
 tolua_globalarray(tolua_S,"adj_str_th",toluaI_get_util_adj_str_th,toluaI_set_util_adj_str_th);
 tolua_globalarray(tolua_S,"adj_str_wgt",toluaI_get_util_adj_str_wgt,toluaI_set_util_adj_str_wgt);
 tolua_globalarray(tolua_S,"adj_str_hold",toluaI_get_util_adj_str_hold,toluaI_set_util_adj_str_hold);
 tolua_globalarray(tolua_S,"adj_str_dig",toluaI_get_util_adj_str_dig,toluaI_set_util_adj_str_dig);
 tolua_globalarray(tolua_S,"adj_str_blow",toluaI_get_util_adj_str_blow,toluaI_set_util_adj_str_blow);
 tolua_globalarray(tolua_S,"adj_dex_blow",toluaI_get_util_adj_dex_blow,toluaI_set_util_adj_dex_blow);
 tolua_globalarray(tolua_S,"adj_dex_safe",toluaI_get_util_adj_dex_safe,toluaI_set_util_adj_dex_safe);
 tolua_globalarray(tolua_S,"adj_con_fix",toluaI_get_util_adj_con_fix,toluaI_set_util_adj_con_fix);
 tolua_globalarray(tolua_S,"adj_con_mhp",toluaI_get_util_adj_con_mhp,toluaI_set_util_adj_con_mhp);
 tolua_function(tolua_S,NULL,"repeat_push",toluaI_util_repeat_push00);
 tolua_function(tolua_S,NULL,"repeat_pull",toluaI_util_repeat_pull00);
 tolua_function(tolua_S,NULL,"repeat_check",toluaI_util_repeat_check00);
 tolua_function(tolua_S,NULL,"get_count",toluaI_util_get_count00);
 tolua_constant(tolua_S,NULL,"FF1_NO_WALK",FF1_NO_WALK);
 tolua_constant(tolua_S,NULL,"FF1_NO_VISION",FF1_NO_VISION);
 tolua_constant(tolua_S,NULL,"FF1_CAN_LEVITATE",FF1_CAN_LEVITATE);
 tolua_constant(tolua_S,NULL,"FF1_CAN_PASS",FF1_CAN_PASS);
 tolua_constant(tolua_S,NULL,"FF1_FLOOR",FF1_FLOOR);
 tolua_constant(tolua_S,NULL,"FF1_WALL",FF1_WALL);
 tolua_constant(tolua_S,NULL,"FF1_PERMANENT",FF1_PERMANENT);
 tolua_constant(tolua_S,NULL,"FF1_CAN_FLY",FF1_CAN_FLY);
 tolua_constant(tolua_S,NULL,"FF1_REMEMBER",FF1_REMEMBER);
 tolua_constant(tolua_S,NULL,"FF1_NOTICE",FF1_NOTICE);
 tolua_constant(tolua_S,NULL,"FF1_DONT_NOTICE_RUNNING",FF1_DONT_NOTICE_RUNNING);
 tolua_constant(tolua_S,NULL,"FF1_CAN_RUN",FF1_CAN_RUN);
 tolua_constant(tolua_S,NULL,"FF1_DOOR",FF1_DOOR);
 tolua_constant(tolua_S,NULL,"FF1_SUPPORT_LIGHT",FF1_SUPPORT_LIGHT);
 tolua_constant(tolua_S,NULL,"FF1_CAN_CLIMB",FF1_CAN_CLIMB);
 tolua_constant(tolua_S,NULL,"FF1_TUNNELABLE",FF1_TUNNELABLE);
 tolua_constant(tolua_S,NULL,"FF1_WEB",FF1_WEB);
 tolua_constant(tolua_S,NULL,"FF1_ATTR_MULTI",FF1_ATTR_MULTI);
 tolua_cclass(tolua_S,"cave_type","");
 tolua_tablevar(tolua_S,"cave_type","info",toluaI_get_util_cave_type_info,toluaI_set_util_cave_type_info);
 tolua_tablevar(tolua_S,"cave_type","feat",toluaI_get_util_cave_type_feat,toluaI_set_util_cave_type_feat);
 tolua_tablevar(tolua_S,"cave_type","o_idx",toluaI_get_util_cave_type_o_idx,toluaI_set_util_cave_type_o_idx);
 tolua_tablevar(tolua_S,"cave_type","m_idx",toluaI_get_util_cave_type_m_idx,toluaI_set_util_cave_type_m_idx);
 tolua_tablevar(tolua_S,"cave_type","t_idx",toluaI_get_util_cave_type_t_idx,toluaI_set_util_cave_type_t_idx);
 tolua_tablevar(tolua_S,"cave_type","special",toluaI_get_util_cave_type_special,toluaI_set_util_cave_type_special);
 tolua_tablevar(tolua_S,"cave_type","special2",toluaI_get_util_cave_type_special2,toluaI_set_util_cave_type_special2);
 tolua_tablevar(tolua_S,"cave_type","inscription",toluaI_get_util_cave_type_inscription,toluaI_set_util_cave_type_inscription);
 tolua_tablevar(tolua_S,"cave_type","mana",toluaI_get_util_cave_type_mana,toluaI_set_util_cave_type_mana);
 tolua_tablevar(tolua_S,"cave_type","mimic",toluaI_get_util_cave_type_mimic,toluaI_set_util_cave_type_mimic);
 tolua_tablevar(tolua_S,"cave_type","effect",toluaI_get_util_cave_type_effect,toluaI_set_util_cave_type_effect);
 tolua_function(tolua_S,NULL,"cave_is",toluaI_util_cave_is00);
 tolua_function(tolua_S,NULL,"cave",toluaI_util_cave00);
 tolua_function(tolua_S,NULL,"set_target",toluaI_util_set_target00);
 tolua_globalarray(tolua_S,"m_allow_special",toluaI_get_util_m_allow_special,toluaI_set_util_m_allow_special);
 tolua_globalarray(tolua_S,"k_allow_special",toluaI_get_util_k_allow_special,toluaI_set_util_k_allow_special);
 tolua_globalarray(tolua_S,"a_allow_special",toluaI_get_util_a_allow_special,toluaI_set_util_a_allow_special);
 tolua_function(tolua_S,NULL,"cave_set_feat",toluaI_util_cave_set_feat00);
 tolua_function(tolua_S,NULL,"show_file",toluaI_util_show_file00);
 tolua_globalvar(tolua_S,"target_who",toluaI_get_util_target_who,toluaI_set_util_target_who);
 tolua_globalvar(tolua_S,"target_col",toluaI_get_util_target_col,toluaI_set_util_target_col);
 tolua_globalvar(tolua_S,"target_row",toluaI_get_util_target_row,toluaI_set_util_target_row);
 tolua_globalvar(tolua_S,"max_bact",toluaI_get_util_max_bact,toluaI_set_util_max_bact);
 tolua_function(tolua_S,NULL,"get_map_size",toluaI_util_get_map_size00);
 tolua_function(tolua_S,NULL,"load_map",toluaI_util_load_map00);
 tolua_function(tolua_S,NULL,"alloc_room",toluaI_util_alloc_room00);
 return 1;
}
/* Close function */
void tolua_util_close (lua_State* tolua_S)
{
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TRUE");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"FALSE");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"ESCAPE");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TERM_DARK");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TERM_WHITE");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TERM_SLATE");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TERM_ORANGE");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TERM_RED");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TERM_GREEN");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TERM_BLUE");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TERM_UMBER");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TERM_L_DARK");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TERM_L_WHITE");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TERM_VIOLET");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TERM_YELLOW");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TERM_L_RED");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TERM_L_GREEN");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TERM_L_BLUE");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TERM_L_UMBER");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"HOOK_MONSTER_DEATH");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"HOOK_OPEN");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"HOOK_GEN_QUEST");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"HOOK_END_TURN");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"HOOK_FEELING");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"HOOK_NEW_MONSTER");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"HOOK_GEN_LEVEL");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"HOOK_BUILD_ROOM1");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"HOOK_NEW_LEVEL");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"HOOK_QUEST_FINISH");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"HOOK_QUEST_FAIL");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"HOOK_GIVE");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"HOOK_CHAR_DUMP");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"HOOK_INIT_QUEST");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"HOOK_WILD_GEN");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"HOOK_DROP");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"HOOK_IDENTIFY");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"HOOK_MOVE");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"HOOK_STAIR");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"HOOK_MONSTER_AI");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"HOOK_PLAYER_LEVEL");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"HOOK_WIELD");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"HOOK_INIT");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"HOOK_QUAFF");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"HOOK_AIM");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"HOOK_USE");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"HOOK_ACTIVATE");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"HOOK_ZAP");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"HOOK_READ");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"HOOK_CALC_BONUS");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"HOOK_PLAYER_FLAGS");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"HOOK_KEYPRESS");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"HOOK_CHAT");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"HOOK_MON_SPEAK");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"HOOK_MKEY");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"HOOK_BIRTH_OBJECTS");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"HOOK_ACTIVATE_DESC");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"HOOK_INIT_GAME");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"HOOK_ACTIVATE_POWER");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"HOOK_ITEM_NAME");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"HOOK_SAVE_GAME");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"HOOK_LOAD_GAME");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"HOOK_LEVEL_REGEN");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"HOOK_LEVEL_END_GEN");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"HOOK_BUILDING_ACTION");
 lua_getglobals(tolua_S);
 lua_pushstring(tolua_S,"turn"); lua_pushnil(tolua_S); lua_rawset(tolua_S,-3);
 lua_pop(tolua_S,1);
 lua_getglobals(tolua_S);
 lua_pushstring(tolua_S,"old_turn"); lua_pushnil(tolua_S); lua_rawset(tolua_S,-3);
 lua_pop(tolua_S,1);
 lua_getglobals(tolua_S);
 lua_pushstring(tolua_S,"cur_wid"); lua_pushnil(tolua_S); lua_rawset(tolua_S,-3);
 lua_pop(tolua_S,1);
 lua_getglobals(tolua_S);
 lua_pushstring(tolua_S,"cur_hgt"); lua_pushnil(tolua_S); lua_rawset(tolua_S,-3);
 lua_pop(tolua_S,1);
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"bst");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"path_build");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"move_cursor");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"flush");
 lua_getglobals(tolua_S);
 lua_pushstring(tolua_S,"inkey_scan"); lua_pushnil(tolua_S); lua_rawset(tolua_S,-3);
 lua_pop(tolua_S,1);
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"inkey");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"cmsg_print");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"msg_print");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"screen_save");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"screen_load");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"Term_save");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"Term_load");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"c_put_str");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"c_prt");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"prt");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"clear_from");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"askfor_aux");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"get_string");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"get_check");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"get_com");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"get_quantity");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"test_monster_name");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"test_item_name");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"luck");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"get_player_race_name");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"quit");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"value_scale");
 lua_getglobals(tolua_S);
 lua_pushstring(tolua_S,"process_hooks_restart"); lua_pushnil(tolua_S); lua_rawset(tolua_S,-3);
 lua_pop(tolua_S,1);
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"dump_hooks");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"add_hook_script");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"del_hook_name");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"tome_dofile");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"exec_lua");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"print_hook");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"register_savefile");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"save_number_key");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"adj_mag_study");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"adj_mag_mana");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"adj_mag_fail");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"adj_mag_stat");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"adj_chr_gold");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"adj_int_dev");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"adj_wis_sav");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"adj_dex_dis");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"adj_int_dis");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"adj_dex_ta");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"adj_str_td");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"adj_dex_th");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"adj_str_th");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"adj_str_wgt");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"adj_str_hold");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"adj_str_dig");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"adj_str_blow");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"adj_dex_blow");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"adj_dex_safe");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"adj_con_fix");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"adj_con_mhp");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"repeat_push");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"repeat_pull");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"repeat_check");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"get_count");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"FF1_NO_WALK");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"FF1_NO_VISION");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"FF1_CAN_LEVITATE");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"FF1_CAN_PASS");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"FF1_FLOOR");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"FF1_WALL");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"FF1_PERMANENT");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"FF1_CAN_FLY");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"FF1_REMEMBER");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"FF1_NOTICE");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"FF1_DONT_NOTICE_RUNNING");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"FF1_CAN_RUN");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"FF1_DOOR");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"FF1_SUPPORT_LIGHT");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"FF1_CAN_CLIMB");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"FF1_TUNNELABLE");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"FF1_WEB");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"FF1_ATTR_MULTI");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"cave_type");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"cave_is");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"cave");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"set_target");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"m_allow_special");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"k_allow_special");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"a_allow_special");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"cave_set_feat");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"show_file");
 lua_getglobals(tolua_S);
 lua_pushstring(tolua_S,"target_who"); lua_pushnil(tolua_S); lua_rawset(tolua_S,-3);
 lua_pop(tolua_S,1);
 lua_getglobals(tolua_S);
 lua_pushstring(tolua_S,"target_col"); lua_pushnil(tolua_S); lua_rawset(tolua_S,-3);
 lua_pop(tolua_S,1);
 lua_getglobals(tolua_S);
 lua_pushstring(tolua_S,"target_row"); lua_pushnil(tolua_S); lua_rawset(tolua_S,-3);
 lua_pop(tolua_S,1);
 lua_getglobals(tolua_S);
 lua_pushstring(tolua_S,"max_bact"); lua_pushnil(tolua_S); lua_rawset(tolua_S,-3);
 lua_pop(tolua_S,1);
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"get_map_size");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"load_map");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"alloc_room");
}
