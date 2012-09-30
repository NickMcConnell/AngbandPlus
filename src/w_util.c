/*
** Lua binding: util
** Generated automatically by tolua 4.0a - angband on Sun Apr 28 23:13:34 2002.
*/

#include "lua/tolua.h"

/* Exported function */
int tolua_util_open (lua_State* tolua_S);
void tolua_util_close (lua_State* tolua_S);

#include "angband.h"
#include "plots.h"
extern s32b intMod(s32b a, s32b b);
extern s32b intAnd(s32b a, s32b b);
extern s32b intOr(s32b a, s32b b);
extern s32b intXor(s32b a, s32b b);
extern s32b intShiftl(s32b a, s32b b);
extern s32b intShiftr(s32b a, s32b b);
extern s32b intBitNot(s32b b);

/* function to register type */
static void toluaI_reg_types (lua_State* tolua_S)
{
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

/* function: pern_dofile */
static int toluaI_util_pern_dofile00(lua_State* tolua_S)
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
  bool toluaI_ret = (bool)  pern_dofile(file);
 tolua_pushnumber(tolua_S,(long)toluaI_ret);
 }
 }
 return 1;
tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'pern_dofile'.");
 return 0;
}

/* function: intMod */
static int toluaI_util_mod00(lua_State* tolua_S)
{
 if (
 !tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
 !tolua_istype(tolua_S,2,LUA_TNUMBER,0) ||
 !tolua_isnoobj(tolua_S,3)
 )
 goto tolua_lerror;
 else
 {
  s32b a = ((s32b)  tolua_getnumber(tolua_S,1,0));
  s32b b = ((s32b)  tolua_getnumber(tolua_S,2,0));
 {
  s32b toluaI_ret = (s32b)  intMod(a,b);
 tolua_pushnumber(tolua_S,(long)toluaI_ret);
 }
 }
 return 1;
tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'mod'.");
 return 0;
}

/* function: intAnd */
static int toluaI_util_band00(lua_State* tolua_S)
{
 if (
 !tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
 !tolua_istype(tolua_S,2,LUA_TNUMBER,0) ||
 !tolua_isnoobj(tolua_S,3)
 )
 goto tolua_lerror;
 else
 {
  s32b a = ((s32b)  tolua_getnumber(tolua_S,1,0));
  s32b b = ((s32b)  tolua_getnumber(tolua_S,2,0));
 {
  s32b toluaI_ret = (s32b)  intAnd(a,b);
 tolua_pushnumber(tolua_S,(long)toluaI_ret);
 }
 }
 return 1;
tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'band'.");
 return 0;
}

/* function: intOr */
static int toluaI_util_bor00(lua_State* tolua_S)
{
 if (
 !tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
 !tolua_istype(tolua_S,2,LUA_TNUMBER,0) ||
 !tolua_isnoobj(tolua_S,3)
 )
 goto tolua_lerror;
 else
 {
  s32b a = ((s32b)  tolua_getnumber(tolua_S,1,0));
  s32b b = ((s32b)  tolua_getnumber(tolua_S,2,0));
 {
  s32b toluaI_ret = (s32b)  intOr(a,b);
 tolua_pushnumber(tolua_S,(long)toluaI_ret);
 }
 }
 return 1;
tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'bor'.");
 return 0;
}

/* function: intXor */
static int toluaI_util_bxor00(lua_State* tolua_S)
{
 if (
 !tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
 !tolua_istype(tolua_S,2,LUA_TNUMBER,0) ||
 !tolua_isnoobj(tolua_S,3)
 )
 goto tolua_lerror;
 else
 {
  s32b a = ((s32b)  tolua_getnumber(tolua_S,1,0));
  s32b b = ((s32b)  tolua_getnumber(tolua_S,2,0));
 {
  s32b toluaI_ret = (s32b)  intXor(a,b);
 tolua_pushnumber(tolua_S,(long)toluaI_ret);
 }
 }
 return 1;
tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'bxor'.");
 return 0;
}

/* function: intShiftl */
static int toluaI_util_bshl00(lua_State* tolua_S)
{
 if (
 !tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
 !tolua_istype(tolua_S,2,LUA_TNUMBER,0) ||
 !tolua_isnoobj(tolua_S,3)
 )
 goto tolua_lerror;
 else
 {
  s32b a = ((s32b)  tolua_getnumber(tolua_S,1,0));
  s32b b = ((s32b)  tolua_getnumber(tolua_S,2,0));
 {
  s32b toluaI_ret = (s32b)  intShiftl(a,b);
 tolua_pushnumber(tolua_S,(long)toluaI_ret);
 }
 }
 return 1;
tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'bshl'.");
 return 0;
}

/* function: intShiftr */
static int toluaI_util_bshr00(lua_State* tolua_S)
{
 if (
 !tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
 !tolua_istype(tolua_S,2,LUA_TNUMBER,0) ||
 !tolua_isnoobj(tolua_S,3)
 )
 goto tolua_lerror;
 else
 {
  s32b a = ((s32b)  tolua_getnumber(tolua_S,1,0));
  s32b b = ((s32b)  tolua_getnumber(tolua_S,2,0));
 {
  s32b toluaI_ret = (s32b)  intShiftr(a,b);
 tolua_pushnumber(tolua_S,(long)toluaI_ret);
 }
 }
 return 1;
tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'bshr'.");
 return 0;
}

/* function: intBitNot */
static int toluaI_util_bnot00(lua_State* tolua_S)
{
 if (
 !tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
 !tolua_isnoobj(tolua_S,2)
 )
 goto tolua_lerror;
 else
 {
  s32b b = ((s32b)  tolua_getnumber(tolua_S,1,0));
 {
  s32b toluaI_ret = (s32b)  intBitNot(b);
 tolua_pushnumber(tolua_S,(long)toluaI_ret);
 }
 }
 return 1;
tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'bnot'.");
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
 tolua_globalvar(tolua_S,"turn",toluaI_get_util_turn,toluaI_set_util_turn);
 tolua_globalvar(tolua_S,"old_turn",toluaI_get_util_old_turn,toluaI_set_util_old_turn);
 tolua_function(tolua_S,NULL,"bst",toluaI_util_bst00);
 tolua_function(tolua_S,NULL,"path_build",toluaI_util_path_build00);
 tolua_function(tolua_S,NULL,"move_cursor",toluaI_util_move_cursor00);
 tolua_globalvar(tolua_S,"inkey_scan",toluaI_get_util_inkey_scan,toluaI_set_util_inkey_scan);
 tolua_function(tolua_S,NULL,"inkey",toluaI_util_inkey00);
 tolua_function(tolua_S,NULL,"cmsg_print",toluaI_util_cmsg_print00);
 tolua_function(tolua_S,NULL,"msg_print",toluaI_util_msg_print00);
 tolua_function(tolua_S,NULL,"screen_save",toluaI_util_screen_save00);
 tolua_function(tolua_S,NULL,"screen_load",toluaI_util_screen_load00);
 tolua_function(tolua_S,NULL,"c_put_str",toluaI_util_c_put_str00);
 tolua_function(tolua_S,NULL,"c_prt",toluaI_util_c_prt00);
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
 tolua_globalvar(tolua_S,"process_hooks_restart",toluaI_get_util_process_hooks_restart,toluaI_set_util_process_hooks_restart);
 tolua_function(tolua_S,NULL,"dump_hooks",toluaI_util_dump_hooks00);
 tolua_function(tolua_S,NULL,"add_hook_script",toluaI_util_add_hook_script00);
 tolua_function(tolua_S,NULL,"del_hook_name",toluaI_util_del_hook_name00);
 tolua_function(tolua_S,NULL,"pern_dofile",toluaI_util_pern_dofile00);
 tolua_function(tolua_S,NULL,"mod",toluaI_util_mod00);
 tolua_function(tolua_S,NULL,"band",toluaI_util_band00);
 tolua_function(tolua_S,NULL,"bor",toluaI_util_bor00);
 tolua_function(tolua_S,NULL,"bxor",toluaI_util_bxor00);
 tolua_function(tolua_S,NULL,"bshl",toluaI_util_bshl00);
 tolua_function(tolua_S,NULL,"bshr",toluaI_util_bshr00);
 tolua_function(tolua_S,NULL,"bnot",toluaI_util_bnot00);
 tolua_function(tolua_S,NULL,"register_savefile",toluaI_util_register_savefile00);
 tolua_function(tolua_S,NULL,"save_number_key",toluaI_util_save_number_key00);
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
 lua_getglobals(tolua_S);
 lua_pushstring(tolua_S,"turn"); lua_pushnil(tolua_S); lua_rawset(tolua_S,-3);
 lua_pop(tolua_S,1);
 lua_getglobals(tolua_S);
 lua_pushstring(tolua_S,"old_turn"); lua_pushnil(tolua_S); lua_rawset(tolua_S,-3);
 lua_pop(tolua_S,1);
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"bst");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"path_build");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"move_cursor");
 lua_getglobals(tolua_S);
 lua_pushstring(tolua_S,"inkey_scan"); lua_pushnil(tolua_S); lua_rawset(tolua_S,-3);
 lua_pop(tolua_S,1);
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"inkey");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"cmsg_print");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"msg_print");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"screen_save");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"screen_load");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"c_put_str");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"c_prt");
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
 lua_getglobals(tolua_S);
 lua_pushstring(tolua_S,"process_hooks_restart"); lua_pushnil(tolua_S); lua_rawset(tolua_S,-3);
 lua_pop(tolua_S,1);
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"dump_hooks");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"add_hook_script");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"del_hook_name");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"pern_dofile");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"mod");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"band");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"bor");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"bxor");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"bshl");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"bshr");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"bnot");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"register_savefile");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"save_number_key");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"get_map_size");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"load_map");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"alloc_room");
}
