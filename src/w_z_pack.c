/*
** Lua binding: z_pack
** Generated automatically by tolua 4.0a - angband on Sun Apr 28 20:22:08 2002.
*/

#include "lua/tolua.h"

/* Exported function */
int tolua_z_pack_open (lua_State* tolua_S);
void tolua_z_pack_close (lua_State* tolua_S);

#include "angband.h"
static s32b lua_rand_int(s32b m) {return rand_int(m);}
static s32b lua_rand_range(s32b A, s32b B) {return ((A) + (rand_int(1+(B)-(A))));}
static s32b lua_rand_spread(s32b A, s32b D) {return ((A) + (rand_int(1+(D)+(D))) - (D));}
static s32b lua_randint(s32b m) {return rand_int(m) + 1;}
static bool lua_magik(s32b P) {return (rand_int(100) < (P));}

/* function to register type */
static void toluaI_reg_types (lua_State* tolua_S)
{
}

/* error messages */
#define TOLUA_ERR_SELF tolua_error(tolua_S,"invalid 'self'")
#define TOLUA_ERR_ASSIGN tolua_error(tolua_S,"#vinvalid type in variable assignment.")

/* function: Term_xtra */
static int toluaI_z_pack_Term_xtra00(lua_State* tolua_S)
{
 if (
 !tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
 !tolua_istype(tolua_S,2,LUA_TNUMBER,0) ||
 !tolua_isnoobj(tolua_S,3)
 )
 goto tolua_lerror;
 else
 {
  int n = ((int)  tolua_getnumber(tolua_S,1,0));
  int v = ((int)  tolua_getnumber(tolua_S,2,0));
 {
  errr toluaI_ret = (errr)  Term_xtra(n,v);
 tolua_pushnumber(tolua_S,(long)toluaI_ret);
 }
 }
 return 1;
tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'Term_xtra'.");
 return 0;
}

/* function: Term_set_cursor */
static int toluaI_z_pack_Term_set_cursor00(lua_State* tolua_S)
{
 if (
 !tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
 !tolua_isnoobj(tolua_S,2)
 )
 goto tolua_lerror;
 else
 {
  int v = ((int)  tolua_getnumber(tolua_S,1,0));
 {
  errr toluaI_ret = (errr)  Term_set_cursor(v);
 tolua_pushnumber(tolua_S,(long)toluaI_ret);
 }
 }
 return 1;
tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'Term_set_cursor'.");
 return 0;
}

/* function: Term_gotoxy */
static int toluaI_z_pack_Term_gotoxy00(lua_State* tolua_S)
{
 if (
 !tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
 !tolua_istype(tolua_S,2,LUA_TNUMBER,0) ||
 !tolua_isnoobj(tolua_S,3)
 )
 goto tolua_lerror;
 else
 {
  int x = ((int)  tolua_getnumber(tolua_S,1,0));
  int y = ((int)  tolua_getnumber(tolua_S,2,0));
 {
  errr toluaI_ret = (errr)  Term_gotoxy(x,y);
 tolua_pushnumber(tolua_S,(long)toluaI_ret);
 }
 }
 return 1;
tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'Term_gotoxy'.");
 return 0;
}

/* function: Term_putch */
static int toluaI_z_pack_Term_putch00(lua_State* tolua_S)
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
  int x = ((int)  tolua_getnumber(tolua_S,1,0));
  int y = ((int)  tolua_getnumber(tolua_S,2,0));
  byte a = ((byte)  tolua_getnumber(tolua_S,3,0));
  char c = ((char)  tolua_getnumber(tolua_S,4,0));
 {
  errr toluaI_ret = (errr)  Term_putch(x,y,a,c);
 tolua_pushnumber(tolua_S,(long)toluaI_ret);
 }
 }
 return 1;
tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'Term_putch'.");
 return 0;
}

/* function: Term_putstr */
static int toluaI_z_pack_Term_putstr00(lua_State* tolua_S)
{
 if (
 !tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
 !tolua_istype(tolua_S,2,LUA_TNUMBER,0) ||
 !tolua_istype(tolua_S,3,LUA_TNUMBER,0) ||
 !tolua_istype(tolua_S,4,LUA_TNUMBER,0) ||
 !tolua_istype(tolua_S,5,LUA_TSTRING,0) ||
 !tolua_isnoobj(tolua_S,6)
 )
 goto tolua_lerror;
 else
 {
  int x = ((int)  tolua_getnumber(tolua_S,1,0));
  int y = ((int)  tolua_getnumber(tolua_S,2,0));
  int n = ((int)  tolua_getnumber(tolua_S,3,0));
  byte a = ((byte)  tolua_getnumber(tolua_S,4,0));
  cptr s = ((cptr)  tolua_getstring(tolua_S,5,0));
 {
  errr toluaI_ret = (errr)  Term_putstr(x,y,n,a,s);
 tolua_pushnumber(tolua_S,(long)toluaI_ret);
 }
 }
 return 1;
tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'Term_putstr'.");
 return 0;
}

/* function: Term_clear */
static int toluaI_z_pack_Term_clear00(lua_State* tolua_S)
{
 if (
 !tolua_isnoobj(tolua_S,1)
 )
 goto tolua_lerror;
 else
 {
 {
  errr toluaI_ret = (errr)  Term_clear();
 tolua_pushnumber(tolua_S,(long)toluaI_ret);
 }
 }
 return 1;
tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'Term_clear'.");
 return 0;
}

/* function: Term_redraw */
static int toluaI_z_pack_Term_redraw00(lua_State* tolua_S)
{
 if (
 !tolua_isnoobj(tolua_S,1)
 )
 goto tolua_lerror;
 else
 {
 {
  errr toluaI_ret = (errr)  Term_redraw();
 tolua_pushnumber(tolua_S,(long)toluaI_ret);
 }
 }
 return 1;
tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'Term_redraw'.");
 return 0;
}

/* function: Term_redraw_section */
static int toluaI_z_pack_Term_redraw_section00(lua_State* tolua_S)
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
  int x1 = ((int)  tolua_getnumber(tolua_S,1,0));
  int y1 = ((int)  tolua_getnumber(tolua_S,2,0));
  int x2 = ((int)  tolua_getnumber(tolua_S,3,0));
  int y2 = ((int)  tolua_getnumber(tolua_S,4,0));
 {
  errr toluaI_ret = (errr)  Term_redraw_section(x1,y1,x2,y2);
 tolua_pushnumber(tolua_S,(long)toluaI_ret);
 }
 }
 return 1;
tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'Term_redraw_section'.");
 return 0;
}

/* function: lua_rand_int */
static int toluaI_z_pack_rand_int00(lua_State* tolua_S)
{
 if (
 !tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
 !tolua_isnoobj(tolua_S,2)
 )
 goto tolua_lerror;
 else
 {
  s32b m = ((s32b)  tolua_getnumber(tolua_S,1,0));
 {
  s32b toluaI_ret = (s32b)  lua_rand_int(m);
 tolua_pushnumber(tolua_S,(long)toluaI_ret);
 }
 }
 return 1;
tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'rand_int'.");
 return 0;
}

/* function: lua_rand_range */
static int toluaI_z_pack_rand_range00(lua_State* tolua_S)
{
 if (
 !tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
 !tolua_istype(tolua_S,2,LUA_TNUMBER,0) ||
 !tolua_isnoobj(tolua_S,3)
 )
 goto tolua_lerror;
 else
 {
  s32b A = ((s32b)  tolua_getnumber(tolua_S,1,0));
  s32b B = ((s32b)  tolua_getnumber(tolua_S,2,0));
 {
  s32b toluaI_ret = (s32b)  lua_rand_range(A,B);
 tolua_pushnumber(tolua_S,(long)toluaI_ret);
 }
 }
 return 1;
tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'rand_range'.");
 return 0;
}

/* function: lua_rand_spread */
static int toluaI_z_pack_rand_spread00(lua_State* tolua_S)
{
 if (
 !tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
 !tolua_istype(tolua_S,2,LUA_TNUMBER,0) ||
 !tolua_isnoobj(tolua_S,3)
 )
 goto tolua_lerror;
 else
 {
  s32b A = ((s32b)  tolua_getnumber(tolua_S,1,0));
  s32b D = ((s32b)  tolua_getnumber(tolua_S,2,0));
 {
  s32b toluaI_ret = (s32b)  lua_rand_spread(A,D);
 tolua_pushnumber(tolua_S,(long)toluaI_ret);
 }
 }
 return 1;
tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'rand_spread'.");
 return 0;
}

/* function: lua_randint */
static int toluaI_z_pack_randint00(lua_State* tolua_S)
{
 if (
 !tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
 !tolua_isnoobj(tolua_S,2)
 )
 goto tolua_lerror;
 else
 {
  s32b m = ((s32b)  tolua_getnumber(tolua_S,1,0));
 {
  s32b toluaI_ret = (s32b)  lua_randint(m);
 tolua_pushnumber(tolua_S,(long)toluaI_ret);
 }
 }
 return 1;
tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'randint'.");
 return 0;
}

/* function: lua_magik */
static int toluaI_z_pack_magik00(lua_State* tolua_S)
{
 if (
 !tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
 !tolua_isnoobj(tolua_S,2)
 )
 goto tolua_lerror;
 else
 {
  s32b P = ((s32b)  tolua_getnumber(tolua_S,1,0));
 {
  bool toluaI_ret = (bool)  lua_magik(P);
 tolua_pushnumber(tolua_S,(long)toluaI_ret);
 }
 }
 return 1;
tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'magik'.");
 return 0;
}

/* get function: Rand_quick */
static int toluaI_get_z_pack_Rand_quick(lua_State* tolua_S)
{
 tolua_pushnumber(tolua_S,(long)Rand_quick);
 return 1;
}

/* set function: Rand_quick */
static int toluaI_set_z_pack_Rand_quick(lua_State* tolua_S)
{
 if (!tolua_istype(tolua_S,1,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  Rand_quick = ((bool)  tolua_getnumber(tolua_S,1,0));
 return 0;
}

/* get function: Rand_value */
static int toluaI_get_z_pack_Rand_value(lua_State* tolua_S)
{
 tolua_pushnumber(tolua_S,(long)Rand_value);
 return 1;
}

/* set function: Rand_value */
static int toluaI_set_z_pack_Rand_value(lua_State* tolua_S)
{
 if (!tolua_istype(tolua_S,1,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  Rand_value = ((u32b)  tolua_getnumber(tolua_S,1,0));
 return 0;
}

/* function: damroll */
static int toluaI_z_pack_damroll00(lua_State* tolua_S)
{
 if (
 !tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
 !tolua_istype(tolua_S,2,LUA_TNUMBER,0) ||
 !tolua_isnoobj(tolua_S,3)
 )
 goto tolua_lerror;
 else
 {
  int num = ((int)  tolua_getnumber(tolua_S,1,0));
  int sides = ((int)  tolua_getnumber(tolua_S,2,0));
 {
  s16b toluaI_ret = (s16b)  damroll(num,sides);
 tolua_pushnumber(tolua_S,(long)toluaI_ret);
 }
 }
 return 1;
tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'damroll'.");
 return 0;
}

/* function: maxroll */
static int toluaI_z_pack_maxroll00(lua_State* tolua_S)
{
 if (
 !tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
 !tolua_istype(tolua_S,2,LUA_TNUMBER,0) ||
 !tolua_isnoobj(tolua_S,3)
 )
 goto tolua_lerror;
 else
 {
  int num = ((int)  tolua_getnumber(tolua_S,1,0));
  int sides = ((int)  tolua_getnumber(tolua_S,2,0));
 {
  s16b toluaI_ret = (s16b)  maxroll(num,sides);
 tolua_pushnumber(tolua_S,(long)toluaI_ret);
 }
 }
 return 1;
tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'maxroll'.");
 return 0;
}

/* Open function */
int tolua_z_pack_open (lua_State* tolua_S)
{
 tolua_open(tolua_S);
 toluaI_reg_types(tolua_S);
 tolua_constant(tolua_S,NULL,"TERM_XTRA_EVENT",TERM_XTRA_EVENT);
 tolua_constant(tolua_S,NULL,"TERM_XTRA_FLUSH",TERM_XTRA_FLUSH);
 tolua_constant(tolua_S,NULL,"TERM_XTRA_CLEAR",TERM_XTRA_CLEAR);
 tolua_constant(tolua_S,NULL,"TERM_XTRA_SHAPE",TERM_XTRA_SHAPE);
 tolua_constant(tolua_S,NULL,"TERM_XTRA_FROSH",TERM_XTRA_FROSH);
 tolua_constant(tolua_S,NULL,"TERM_XTRA_FRESH",TERM_XTRA_FRESH);
 tolua_constant(tolua_S,NULL,"TERM_XTRA_NOISE",TERM_XTRA_NOISE);
 tolua_constant(tolua_S,NULL,"TERM_XTRA_SOUND",TERM_XTRA_SOUND);
 tolua_constant(tolua_S,NULL,"TERM_XTRA_BORED",TERM_XTRA_BORED);
 tolua_constant(tolua_S,NULL,"TERM_XTRA_REACT",TERM_XTRA_REACT);
 tolua_constant(tolua_S,NULL,"TERM_XTRA_ALIVE",TERM_XTRA_ALIVE);
 tolua_constant(tolua_S,NULL,"TERM_XTRA_LEVEL",TERM_XTRA_LEVEL);
 tolua_constant(tolua_S,NULL,"TERM_XTRA_DELAY",TERM_XTRA_DELAY);
 tolua_function(tolua_S,NULL,"Term_xtra",toluaI_z_pack_Term_xtra00);
 tolua_function(tolua_S,NULL,"Term_set_cursor",toluaI_z_pack_Term_set_cursor00);
 tolua_function(tolua_S,NULL,"Term_gotoxy",toluaI_z_pack_Term_gotoxy00);
 tolua_function(tolua_S,NULL,"Term_putch",toluaI_z_pack_Term_putch00);
 tolua_function(tolua_S,NULL,"Term_putstr",toluaI_z_pack_Term_putstr00);
 tolua_function(tolua_S,NULL,"Term_clear",toluaI_z_pack_Term_clear00);
 tolua_function(tolua_S,NULL,"Term_redraw",toluaI_z_pack_Term_redraw00);
 tolua_function(tolua_S,NULL,"Term_redraw_section",toluaI_z_pack_Term_redraw_section00);
 tolua_function(tolua_S,NULL,"rand_int",toluaI_z_pack_rand_int00);
 tolua_function(tolua_S,NULL,"rand_range",toluaI_z_pack_rand_range00);
 tolua_function(tolua_S,NULL,"rand_spread",toluaI_z_pack_rand_spread00);
 tolua_function(tolua_S,NULL,"randint",toluaI_z_pack_randint00);
 tolua_function(tolua_S,NULL,"magik",toluaI_z_pack_magik00);
 tolua_globalvar(tolua_S,"Rand_quick",toluaI_get_z_pack_Rand_quick,toluaI_set_z_pack_Rand_quick);
 tolua_globalvar(tolua_S,"Rand_value",toluaI_get_z_pack_Rand_value,toluaI_set_z_pack_Rand_value);
 tolua_function(tolua_S,NULL,"damroll",toluaI_z_pack_damroll00);
 tolua_function(tolua_S,NULL,"maxroll",toluaI_z_pack_maxroll00);
 return 1;
}
/* Close function */
void tolua_z_pack_close (lua_State* tolua_S)
{
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TERM_XTRA_EVENT");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TERM_XTRA_FLUSH");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TERM_XTRA_CLEAR");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TERM_XTRA_SHAPE");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TERM_XTRA_FROSH");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TERM_XTRA_FRESH");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TERM_XTRA_NOISE");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TERM_XTRA_SOUND");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TERM_XTRA_BORED");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TERM_XTRA_REACT");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TERM_XTRA_ALIVE");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TERM_XTRA_LEVEL");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TERM_XTRA_DELAY");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"Term_xtra");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"Term_set_cursor");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"Term_gotoxy");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"Term_putch");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"Term_putstr");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"Term_clear");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"Term_redraw");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"Term_redraw_section");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"rand_int");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"rand_range");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"rand_spread");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"randint");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"magik");
 lua_getglobals(tolua_S);
 lua_pushstring(tolua_S,"Rand_quick"); lua_pushnil(tolua_S); lua_rawset(tolua_S,-3);
 lua_pop(tolua_S,1);
 lua_getglobals(tolua_S);
 lua_pushstring(tolua_S,"Rand_value"); lua_pushnil(tolua_S); lua_rawset(tolua_S,-3);
 lua_pop(tolua_S,1);
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"damroll");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"maxroll");
}
