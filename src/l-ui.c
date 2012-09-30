/*
** Lua binding: ui
** Generated automatically by tolua 4.0a - angband.
*/

#include "lua/tolua.h"

/* Exported function */
int  tolua_ui_open (lua_State* tolua_S);
void tolua_ui_close (lua_State* tolua_S);

#include "angband.h"

/* function to register type */
static void toluaI_reg_types (lua_State* tolua_S)
{
(void) tolua_S;	/* Hack - prevent compiler warnings */
}

/* function: update_stuff */
static int toluaI_ui_update_stuff00(lua_State* tolua_S)
{
 if (!tolua_isnoobj(tolua_S,1))
 {
  TOLUA_ERR_FN(update_stuff);
 } else {
  update_stuff();
 }
 return 0;
}

/* function: redraw_stuff */
static int toluaI_ui_redraw_stuff00(lua_State* tolua_S)
{
 if (!tolua_isnoobj(tolua_S,1))
 {
  TOLUA_ERR_FN(redraw_stuff);
 } else {
  redraw_stuff();
 }
 return 0;
}

/* function: window_stuff */
static int toluaI_ui_window_stuff00(lua_State* tolua_S)
{
 if (!tolua_isnoobj(tolua_S,1))
 {
  TOLUA_ERR_FN(window_stuff);
 } else {
  window_stuff();
 }
 return 0;
}

/* function: handle_stuff */
static int toluaI_ui_handle_stuff00(lua_State* tolua_S)
{
 if (!tolua_isnoobj(tolua_S,1))
 {
  TOLUA_ERR_FN(handle_stuff);
 } else {
  handle_stuff();
 }
 return 0;
}

/* function: message_flush */
static int toluaI_ui_message_flush00(lua_State* tolua_S)
{
 if (!tolua_isnoobj(tolua_S,1))
 {
  TOLUA_ERR_FN(message_flush);
 } else {
  message_flush();
 }
 return 0;
}

/* function: msgf */
static int toluaI_ui_msgf00(lua_State* tolua_S)
{
 if (!tolua_istype(tolua_S,1,LUA_TSTRING,0) ||
     !tolua_isnoobj(tolua_S,2))
 {
  TOLUA_ERR_FN(msgf);
 } else {
  cptr fmt = ((cptr)  tolua_getstring(tolua_S,1,0));
  msgf(fmt);
 }
 return 0;
}

/* function: get_check */
static int toluaI_ui_get_check00(lua_State* tolua_S)
{
 if (!tolua_istype(tolua_S,1,LUA_TSTRING,0) ||
     !tolua_isnoobj(tolua_S,2))
 {
  TOLUA_ERR_FN(get_check);
 } else {
  cptr prompt = ((cptr)  tolua_getstring(tolua_S,1,0));
  bool toluaI_ret = (bool)  get_check(prompt);
  tolua_pushbool(tolua_S,(int)toluaI_ret);
 }
 return 1;
}

/* function: get_rnd_line */
static int toluaI_ui_get_rnd_line00(lua_State* tolua_S)
{
 if (!tolua_istype(tolua_S,1,LUA_TSTRING,0) ||
     !tolua_istype(tolua_S,2,LUA_TNUMBER,0) ||
     !tolua_istype(tolua_S,3,LUA_TSTRING,0) ||
     !tolua_isnoobj(tolua_S,4))
 {
  TOLUA_ERR_FN(get_rnd_line);
 } else {
  cptr file_name = ((cptr)  tolua_getstring(tolua_S,1,0));
  int entry = ((int)  tolua_getnumber(tolua_S,2,0));
  char* output = ((char*)  tolua_getstring(tolua_S,3,0));
  errr toluaI_ret = (errr)  get_rnd_line(file_name,entry,output);
  tolua_pushnumber(tolua_S,(long)toluaI_ret);
 }
 return 1;
}

/* Open function */
int tolua_ui_open (lua_State* tolua_S)
{
 tolua_open(tolua_S);
 toluaI_reg_types(tolua_S);
 TOLUA_DEF(TERM_DARK);
 TOLUA_DEF(TERM_WHITE);
 TOLUA_DEF(TERM_SLATE);
 TOLUA_DEF(TERM_ORANGE);
 TOLUA_DEF(TERM_RED);
 TOLUA_DEF(TERM_GREEN);
 TOLUA_DEF(TERM_BLUE);
 TOLUA_DEF(TERM_UMBER);
 TOLUA_DEF(TERM_L_DARK);
 TOLUA_DEF(TERM_L_WHITE);
 TOLUA_DEF(TERM_VIOLET);
 TOLUA_DEF(TERM_YELLOW);
 TOLUA_DEF(TERM_L_RED);
 TOLUA_DEF(TERM_L_GREEN);
 TOLUA_DEF(TERM_L_BLUE);
 TOLUA_DEF(TERM_L_UMBER);
 TOLUA_DEF(PU_BONUS);
 TOLUA_DEF(PU_TORCH);
 TOLUA_DEF(PU_HP);
 TOLUA_DEF(PU_MANA);
 TOLUA_DEF(PU_SPELLS);
 TOLUA_DEF(PU_VIEW);
 TOLUA_DEF(PU_MON_LITE);
 TOLUA_DEF(PU_MONSTERS);
 TOLUA_DEF(PU_DISTANCE);
 TOLUA_DEF(PU_FLOW);
 TOLUA_DEF(PR_MISC);
 TOLUA_DEF(PR_TITLE);
 TOLUA_DEF(PR_LEV);
 TOLUA_DEF(PR_EXP);
 TOLUA_DEF(PR_STATS);
 TOLUA_DEF(PR_ARMOR);
 TOLUA_DEF(PR_HP);
 TOLUA_DEF(PR_MANA);
 TOLUA_DEF(PR_GOLD);
 TOLUA_DEF(PR_DEPTH);
 TOLUA_DEF(PR_EQUIPPY);
 TOLUA_DEF(PR_HEALTH);
 TOLUA_DEF(PR_CUT);
 TOLUA_DEF(PR_STUN);
 TOLUA_DEF(PR_HUNGER);
 TOLUA_DEF(PR_STATUS);
 TOLUA_DEF(PR_BLIND);
 TOLUA_DEF(PR_CONFUSED);
 TOLUA_DEF(PR_AFRAID);
 TOLUA_DEF(PR_POISONED);
 TOLUA_DEF(PR_STATE);
 TOLUA_DEF(PR_SPEED);
 TOLUA_DEF(PR_STUDY);
 TOLUA_DEF(PR_EXTRA);
 TOLUA_DEF(PR_BASIC);
 TOLUA_DEF(PR_MAP);
 TOLUA_DEF(PR_WIPE);
 TOLUA_DEF(PW_INVEN);
 TOLUA_DEF(PW_EQUIP);
 TOLUA_DEF(PW_SPELL);
 TOLUA_DEF(PW_PLAYER);
 TOLUA_DEF(PW_SCRIPT_VARS);
 TOLUA_DEF(PW_SCRIPT_SOURCE);
 TOLUA_DEF(PW_MESSAGE);
 TOLUA_DEF(PW_OVERHEAD);
 TOLUA_DEF(PW_MONSTER);
 TOLUA_DEF(PW_OBJECT);
 TOLUA_DEF(PW_DUNGEON);
 TOLUA_DEF(PW_SNAPSHOT);
 TOLUA_DEF(PW_VISIBLE);
 TOLUA_DEF(PW_BORG_1);
 TOLUA_DEF(PW_BORG_2);
 TOLUA_FUN(update_stuff,toluaI_ui_update_stuff00);
 TOLUA_FUN(redraw_stuff,toluaI_ui_redraw_stuff00);
 TOLUA_FUN(window_stuff,toluaI_ui_window_stuff00);
 TOLUA_FUN(handle_stuff,toluaI_ui_handle_stuff00);
 TOLUA_FUN(message_flush,toluaI_ui_message_flush00);
 TOLUA_FUN(msgf,toluaI_ui_msgf00);
 TOLUA_FUN(get_check,toluaI_ui_get_check00);
 TOLUA_FUN(get_rnd_line,toluaI_ui_get_rnd_line00);
 return 1;
}
/* Close function */
void tolua_ui_close (lua_State* tolua_S)
{
 TOLUA_UNDEF(TERM_DARK);
 TOLUA_UNDEF(TERM_WHITE);
 TOLUA_UNDEF(TERM_SLATE);
 TOLUA_UNDEF(TERM_ORANGE);
 TOLUA_UNDEF(TERM_RED);
 TOLUA_UNDEF(TERM_GREEN);
 TOLUA_UNDEF(TERM_BLUE);
 TOLUA_UNDEF(TERM_UMBER);
 TOLUA_UNDEF(TERM_L_DARK);
 TOLUA_UNDEF(TERM_L_WHITE);
 TOLUA_UNDEF(TERM_VIOLET);
 TOLUA_UNDEF(TERM_YELLOW);
 TOLUA_UNDEF(TERM_L_RED);
 TOLUA_UNDEF(TERM_L_GREEN);
 TOLUA_UNDEF(TERM_L_BLUE);
 TOLUA_UNDEF(TERM_L_UMBER);
 TOLUA_UNDEF(PU_BONUS);
 TOLUA_UNDEF(PU_TORCH);
 TOLUA_UNDEF(PU_HP);
 TOLUA_UNDEF(PU_MANA);
 TOLUA_UNDEF(PU_SPELLS);
 TOLUA_UNDEF(PU_VIEW);
 TOLUA_UNDEF(PU_MON_LITE);
 TOLUA_UNDEF(PU_MONSTERS);
 TOLUA_UNDEF(PU_DISTANCE);
 TOLUA_UNDEF(PU_FLOW);
 TOLUA_UNDEF(PR_MISC);
 TOLUA_UNDEF(PR_TITLE);
 TOLUA_UNDEF(PR_LEV);
 TOLUA_UNDEF(PR_EXP);
 TOLUA_UNDEF(PR_STATS);
 TOLUA_UNDEF(PR_ARMOR);
 TOLUA_UNDEF(PR_HP);
 TOLUA_UNDEF(PR_MANA);
 TOLUA_UNDEF(PR_GOLD);
 TOLUA_UNDEF(PR_DEPTH);
 TOLUA_UNDEF(PR_EQUIPPY);
 TOLUA_UNDEF(PR_HEALTH);
 TOLUA_UNDEF(PR_CUT);
 TOLUA_UNDEF(PR_STUN);
 TOLUA_UNDEF(PR_HUNGER);
 TOLUA_UNDEF(PR_STATUS);
 TOLUA_UNDEF(PR_BLIND);
 TOLUA_UNDEF(PR_CONFUSED);
 TOLUA_UNDEF(PR_AFRAID);
 TOLUA_UNDEF(PR_POISONED);
 TOLUA_UNDEF(PR_STATE);
 TOLUA_UNDEF(PR_SPEED);
 TOLUA_UNDEF(PR_STUDY);
 TOLUA_UNDEF(PR_EXTRA);
 TOLUA_UNDEF(PR_BASIC);
 TOLUA_UNDEF(PR_MAP);
 TOLUA_UNDEF(PR_WIPE);
 TOLUA_UNDEF(PW_INVEN);
 TOLUA_UNDEF(PW_EQUIP);
 TOLUA_UNDEF(PW_SPELL);
 TOLUA_UNDEF(PW_PLAYER);
 TOLUA_UNDEF(PW_SCRIPT_VARS);
 TOLUA_UNDEF(PW_SCRIPT_SOURCE);
 TOLUA_UNDEF(PW_MESSAGE);
 TOLUA_UNDEF(PW_OVERHEAD);
 TOLUA_UNDEF(PW_MONSTER);
 TOLUA_UNDEF(PW_OBJECT);
 TOLUA_UNDEF(PW_DUNGEON);
 TOLUA_UNDEF(PW_SNAPSHOT);
 TOLUA_UNDEF(PW_VISIBLE);
 TOLUA_UNDEF(PW_BORG_1);
 TOLUA_UNDEF(PW_BORG_2);
 TOLUA_UNDEF(update_stuff);
 TOLUA_UNDEF(redraw_stuff);
 TOLUA_UNDEF(window_stuff);
 TOLUA_UNDEF(handle_stuff);
 TOLUA_UNDEF(message_flush);
 TOLUA_UNDEF(msgf);
 TOLUA_UNDEF(get_check);
 TOLUA_UNDEF(get_rnd_line);
}
