/*
** Lua binding: z_pack
** Generated automatically by tolua 4.0a - angband on Sun Jun 15 19:53:32 2003.
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
 tolua_usertype(tolua_S,"zsock_hooks");
 tolua_usertype(tolua_S,"timer_callback");
 tolua_usertype(tolua_S,"ip_connection");
}

/* error messages */
#define TOLUA_ERR_SELF tolua_error(tolua_S,"invalid 'self'")
#define TOLUA_ERR_ASSIGN tolua_error(tolua_S,"#vinvalid type in variable assignment.")

/* get function: Term_xtra_long */
static int toluaI_get_z_pack_Term_xtra_long(lua_State* tolua_S)
{
 tolua_pushnumber(tolua_S,(long)Term_xtra_long);
 return 1;
}

/* set function: Term_xtra_long */
static int toluaI_set_z_pack_Term_xtra_long(lua_State* tolua_S)
{
 if (!tolua_istype(tolua_S,1,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  Term_xtra_long = ((long)  tolua_getnumber(tolua_S,1,0));
 return 0;
}

/* get function: scansubdir_dir */
static int toluaI_get_z_pack_scansubdir_dir(lua_State* tolua_S)
{
 int toluaI_index;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 tolua_error(tolua_S,"invalid type in array indexing.");
 toluaI_index = (int)tolua_getnumber(tolua_S,2,0)-1;
 if (toluaI_index<0 || toluaI_index>=1024)
 tolua_error(tolua_S,"array indexing out of range.");
 tolua_pushnumber(tolua_S,(long)scansubdir_dir[toluaI_index]);
 return 1;
}

/* set function: scansubdir_dir */
static int toluaI_set_z_pack_scansubdir_dir(lua_State* tolua_S)
{
 int toluaI_index;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 tolua_error(tolua_S,"invalid type in array indexing.");
 toluaI_index = (int)tolua_getnumber(tolua_S,2,0)-1;
 if (toluaI_index<0 || toluaI_index>=1024)
 tolua_error(tolua_S,"array indexing out of range.");
  scansubdir_dir[toluaI_index] = ((char)  tolua_getnumber(tolua_S,3,0));
 return 0;
}

/* get function: scansubdir_max */
static int toluaI_get_z_pack_scansubdir_max(lua_State* tolua_S)
{
 tolua_pushnumber(tolua_S,(long)scansubdir_max);
 return 1;
}

/* set function: scansubdir_max */
static int toluaI_set_z_pack_scansubdir_max(lua_State* tolua_S)
{
 if (!tolua_istype(tolua_S,1,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  scansubdir_max = ((int)  tolua_getnumber(tolua_S,1,0));
 return 0;
}

/* get function: scansubdir_result */
static int toluaI_get_z_pack_scansubdir_result(lua_State* tolua_S)
{
 int toluaI_index;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 tolua_error(tolua_S,"invalid type in array indexing.");
 toluaI_index = (int)tolua_getnumber(tolua_S,2,0)-1;
 if (toluaI_index<0 || toluaI_index>=scansubdir_max)
 tolua_error(tolua_S,"array indexing out of range.");
 tolua_pushstring(tolua_S,(const char*)scansubdir_result[toluaI_index]);
 return 1;
}

/* set function: scansubdir_result */
static int toluaI_set_z_pack_scansubdir_result(lua_State* tolua_S)
{
 int toluaI_index;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 tolua_error(tolua_S,"invalid type in array indexing.");
 toluaI_index = (int)tolua_getnumber(tolua_S,2,0)-1;
 if (toluaI_index<0 || toluaI_index>=scansubdir_max)
 tolua_error(tolua_S,"array indexing out of range.");
  scansubdir_result[toluaI_index] = ((cptr)  tolua_getstring(tolua_S,3,0));
 return 0;
}

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

/* function: Term_get_size */
static int toluaI_z_pack_Term_get_size00(lua_State* tolua_S)
{
 if (
 !tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
 !tolua_istype(tolua_S,2,LUA_TNUMBER,0) ||
 !tolua_isnoobj(tolua_S,3)
 )
 goto tolua_lerror;
 else
 {
  int w = ((int)  tolua_getnumber(tolua_S,1,0));
  int h = ((int)  tolua_getnumber(tolua_S,2,0));
 {
  errr toluaI_ret = (errr)  Term_get_size(&w,&h);
 tolua_pushnumber(tolua_S,(long)toluaI_ret);
 tolua_pushnumber(tolua_S,(long)w);
 tolua_pushnumber(tolua_S,(long)h);
 }
 }
 return 3;
tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'Term_get_size'.");
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

/* get function: setup of class  ip_connection */
static int toluaI_get_z_pack_ip_connection_setup(lua_State* tolua_S)
{
  ip_connection* self = (ip_connection*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->setup);
 return 1;
}

/* set function: setup of class  ip_connection */
static int toluaI_set_z_pack_ip_connection_setup(lua_State* tolua_S)
{
  ip_connection* self = (ip_connection*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->setup = ((bool)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: conn_ip of class  ip_connection */
static int toluaI_get_z_pack_ip_connection_conn_ip(lua_State* tolua_S)
{
  ip_connection* self = (ip_connection*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->conn_ip);
 return 1;
}

/* set function: conn_ip of class  ip_connection */
static int toluaI_set_z_pack_ip_connection_conn_ip(lua_State* tolua_S)
{
  ip_connection* self = (ip_connection*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->conn_ip = ((long)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: conn_port of class  ip_connection */
static int toluaI_get_z_pack_ip_connection_conn_port(lua_State* tolua_S)
{
  ip_connection* self = (ip_connection*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->conn_port);
 return 1;
}

/* set function: conn_port of class  ip_connection */
static int toluaI_set_z_pack_ip_connection_conn_port(lua_State* tolua_S)
{
  ip_connection* self = (ip_connection*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->conn_port = ((int)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: conn_type of class  ip_connection */
static int toluaI_get_z_pack_ip_connection_conn_type(lua_State* tolua_S)
{
  ip_connection* self = (ip_connection*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->conn_type);
 return 1;
}

/* set function: conn_type of class  ip_connection */
static int toluaI_set_z_pack_ip_connection_conn_type(lua_State* tolua_S)
{
  ip_connection* self = (ip_connection*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->conn_type = ((byte)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: connected of class  ip_connection */
static int toluaI_get_z_pack_ip_connection_connected(lua_State* tolua_S)
{
  ip_connection* self = (ip_connection*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->connected);
 return 1;
}

/* set function: connected of class  ip_connection */
static int toluaI_set_z_pack_ip_connection_connected(lua_State* tolua_S)
{
  ip_connection* self = (ip_connection*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->connected = ((bool)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: socket of class  ip_connection */
static int toluaI_get_z_pack_ip_connection_socket(lua_State* tolua_S)
{
  ip_connection* self = (ip_connection*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushuserdata(tolua_S,(void*)self->socket);
 return 1;
}

/* set function: socket of class  ip_connection */
static int toluaI_set_z_pack_ip_connection_socket(lua_State* tolua_S)
{
  ip_connection* self = (ip_connection*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TUSERDATA,0))
 TOLUA_ERR_ASSIGN;
  self->socket = ((void*)  tolua_getuserdata(tolua_S,2,0));
 return 0;
}

/* get function: server of class  ip_connection */
static int toluaI_get_z_pack_ip_connection_server(lua_State* tolua_S)
{
  ip_connection* self = (ip_connection*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->server);
 return 1;
}

/* set function: server of class  ip_connection */
static int toluaI_set_z_pack_ip_connection_server(lua_State* tolua_S)
{
  ip_connection* self = (ip_connection*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->server = ((bool)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* method: new_connection of class  zsock_hooks */
static int toluaI_z_pack_zsock_hooks_new_connection00(lua_State* tolua_S)
{
 if (
 !tolua_istype(tolua_S,1,tolua_tag(tolua_S,"zsock_hooks"),0) ||
 !tolua_isnoobj(tolua_S,2)
 )
 goto tolua_lerror;
 else
 {
  zsock_hooks* self = (zsock_hooks*)  tolua_getusertype(tolua_S,1,0);
 if (!self) tolua_error(tolua_S,"invalid 'self' in function 'new_connection'");
 {
  ip_connection* toluaI_ret = (ip_connection*)  self->new_connection();
 tolua_pushusertype(tolua_S,(void*)toluaI_ret,tolua_tag(tolua_S,"ip_connection"));
 }
 }
 return 1;
tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'new_connection'.");
 return 0;
}

/* method: free_connection of class  zsock_hooks */
static int toluaI_z_pack_zsock_hooks_free_connection00(lua_State* tolua_S)
{
 if (
 !tolua_istype(tolua_S,1,tolua_tag(tolua_S,"zsock_hooks"),0) ||
 !tolua_istype(tolua_S,2,tolua_tag(tolua_S,"ip_connection"),0) ||
 !tolua_isnoobj(tolua_S,3)
 )
 goto tolua_lerror;
 else
 {
  zsock_hooks* self = (zsock_hooks*)  tolua_getusertype(tolua_S,1,0);
  ip_connection* c = ((ip_connection*)  tolua_getusertype(tolua_S,2,0));
 if (!self) tolua_error(tolua_S,"invalid 'self' in function 'free_connection'");
 {
  self->free_connection(c);
 }
 }
 return 0;
tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'free_connection'.");
 return 0;
}

/* method: setup of class  zsock_hooks */
static int toluaI_z_pack_zsock_hooks_setup00(lua_State* tolua_S)
{
 if (
 !tolua_istype(tolua_S,1,tolua_tag(tolua_S,"zsock_hooks"),0) ||
 !tolua_istype(tolua_S,2,tolua_tag(tolua_S,"ip_connection"),0) ||
 !tolua_istype(tolua_S,3,LUA_TSTRING,0) ||
 !tolua_istype(tolua_S,4,LUA_TNUMBER,0) ||
 !tolua_istype(tolua_S,5,LUA_TNUMBER,0) ||
 !tolua_istype(tolua_S,6,LUA_TNUMBER,0) ||
 !tolua_isnoobj(tolua_S,7)
 )
 goto tolua_lerror;
 else
 {
  zsock_hooks* self = (zsock_hooks*)  tolua_getusertype(tolua_S,1,0);
  ip_connection* conn = ((ip_connection*)  tolua_getusertype(tolua_S,2,0));
  cptr conn_ip = ((cptr)  tolua_getstring(tolua_S,3,0));
  int port = ((int)  tolua_getnumber(tolua_S,4,0));
  byte conn_type = ((byte)  tolua_getnumber(tolua_S,5,0));
  bool server = ((bool)  tolua_getnumber(tolua_S,6,0));
 if (!self) tolua_error(tolua_S,"invalid 'self' in function 'setup'");
 {
  bool toluaI_ret = (bool)  self->setup(conn,conn_ip,port,conn_type,server);
 tolua_pushnumber(tolua_S,(long)toluaI_ret);
 }
 }
 return 1;
tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'setup'.");
 return 0;
}

/* method: unsetup of class  zsock_hooks */
static int toluaI_z_pack_zsock_hooks_unsetup00(lua_State* tolua_S)
{
 if (
 !tolua_istype(tolua_S,1,tolua_tag(tolua_S,"zsock_hooks"),0) ||
 !tolua_istype(tolua_S,2,tolua_tag(tolua_S,"ip_connection"),0) ||
 !tolua_isnoobj(tolua_S,3)
 )
 goto tolua_lerror;
 else
 {
  zsock_hooks* self = (zsock_hooks*)  tolua_getusertype(tolua_S,1,0);
  ip_connection* conn = ((ip_connection*)  tolua_getusertype(tolua_S,2,0));
 if (!self) tolua_error(tolua_S,"invalid 'self' in function 'unsetup'");
 {
  bool toluaI_ret = (bool)  self->unsetup(conn);
 tolua_pushnumber(tolua_S,(long)toluaI_ret);
 }
 }
 return 1;
tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'unsetup'.");
 return 0;
}

/* method: open of class  zsock_hooks */
static int toluaI_z_pack_zsock_hooks_open00(lua_State* tolua_S)
{
 if (
 !tolua_istype(tolua_S,1,tolua_tag(tolua_S,"zsock_hooks"),0) ||
 !tolua_istype(tolua_S,2,tolua_tag(tolua_S,"ip_connection"),0) ||
 !tolua_isnoobj(tolua_S,3)
 )
 goto tolua_lerror;
 else
 {
  zsock_hooks* self = (zsock_hooks*)  tolua_getusertype(tolua_S,1,0);
  ip_connection* conn = ((ip_connection*)  tolua_getusertype(tolua_S,2,0));
 if (!self) tolua_error(tolua_S,"invalid 'self' in function 'open'");
 {
  bool toluaI_ret = (bool)  self->open(conn);
 tolua_pushnumber(tolua_S,(long)toluaI_ret);
 }
 }
 return 1;
tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'open'.");
 return 0;
}

/* method: close of class  zsock_hooks */
static int toluaI_z_pack_zsock_hooks_close00(lua_State* tolua_S)
{
 if (
 !tolua_istype(tolua_S,1,tolua_tag(tolua_S,"zsock_hooks"),0) ||
 !tolua_istype(tolua_S,2,tolua_tag(tolua_S,"ip_connection"),0) ||
 !tolua_isnoobj(tolua_S,3)
 )
 goto tolua_lerror;
 else
 {
  zsock_hooks* self = (zsock_hooks*)  tolua_getusertype(tolua_S,1,0);
  ip_connection* conn = ((ip_connection*)  tolua_getusertype(tolua_S,2,0));
 if (!self) tolua_error(tolua_S,"invalid 'self' in function 'close'");
 {
  bool toluaI_ret = (bool)  self->close(conn);
 tolua_pushnumber(tolua_S,(long)toluaI_ret);
 }
 }
 return 1;
tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'close'.");
 return 0;
}

/* method: write of class  zsock_hooks */
static int toluaI_z_pack_zsock_hooks_write00(lua_State* tolua_S)
{
 if (
 !tolua_istype(tolua_S,1,tolua_tag(tolua_S,"zsock_hooks"),0) ||
 !tolua_istype(tolua_S,2,tolua_tag(tolua_S,"ip_connection"),0) ||
 !tolua_istype(tolua_S,3,LUA_TSTRING,0) ||
 !tolua_istype(tolua_S,4,LUA_TNUMBER,0) ||
 !tolua_isnoobj(tolua_S,5)
 )
 goto tolua_lerror;
 else
 {
  zsock_hooks* self = (zsock_hooks*)  tolua_getusertype(tolua_S,1,0);
  ip_connection* conn = ((ip_connection*)  tolua_getusertype(tolua_S,2,0));
  cptr str = ((cptr)  tolua_getstring(tolua_S,3,0));
  int size = ((int)  tolua_getnumber(tolua_S,4,0));
 if (!self) tolua_error(tolua_S,"invalid 'self' in function 'write'");
 {
  bool toluaI_ret = (bool)  self->write(conn,str,&size);
 tolua_pushnumber(tolua_S,(long)toluaI_ret);
 tolua_pushnumber(tolua_S,(long)size);
 }
 }
 return 2;
tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'write'.");
 return 0;
}

/* method: write_simple of class  zsock_hooks */
static int toluaI_z_pack_zsock_hooks_write_simple00(lua_State* tolua_S)
{
 if (
 !tolua_istype(tolua_S,1,tolua_tag(tolua_S,"zsock_hooks"),0) ||
 !tolua_istype(tolua_S,2,tolua_tag(tolua_S,"ip_connection"),0) ||
 !tolua_istype(tolua_S,3,LUA_TSTRING,0) ||
 !tolua_isnoobj(tolua_S,4)
 )
 goto tolua_lerror;
 else
 {
  zsock_hooks* self = (zsock_hooks*)  tolua_getusertype(tolua_S,1,0);
  ip_connection* conn = ((ip_connection*)  tolua_getusertype(tolua_S,2,0));
  cptr str = ((cptr)  tolua_getstring(tolua_S,3,0));
 if (!self) tolua_error(tolua_S,"invalid 'self' in function 'write_simple'");
 {
  bool toluaI_ret = (bool)  self->write_simple(conn,str);
 tolua_pushnumber(tolua_S,(long)toluaI_ret);
 }
 }
 return 1;
tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'write_simple'.");
 return 0;
}

/* method: read_simple of class  zsock_hooks */
static int toluaI_z_pack_zsock_hooks_read_simple00(lua_State* tolua_S)
{
 if (
 !tolua_istype(tolua_S,1,tolua_tag(tolua_S,"zsock_hooks"),0) ||
 !tolua_istype(tolua_S,2,tolua_tag(tolua_S,"ip_connection"),0) ||
 !tolua_istype(tolua_S,3,LUA_TSTRING,0) ||
 !tolua_istype(tolua_S,4,LUA_TNUMBER,0) ||
 !tolua_isnoobj(tolua_S,5)
 )
 goto tolua_lerror;
 else
 {
  zsock_hooks* self = (zsock_hooks*)  tolua_getusertype(tolua_S,1,0);
  ip_connection* conn = ((ip_connection*)  tolua_getusertype(tolua_S,2,0));
  char* str = ((char*)  tolua_getstring(tolua_S,3,0));
  int len = ((int)  tolua_getnumber(tolua_S,4,0));
 if (!self) tolua_error(tolua_S,"invalid 'self' in function 'read_simple'");
 {
  bool toluaI_ret = (bool)  self->read_simple(conn,str,len);
 tolua_pushnumber(tolua_S,(long)toluaI_ret);
 }
 }
 return 1;
tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'read_simple'.");
 return 0;
}

/* method: accept of class  zsock_hooks */
static int toluaI_z_pack_zsock_hooks_accept00(lua_State* tolua_S)
{
 if (
 !tolua_istype(tolua_S,1,tolua_tag(tolua_S,"zsock_hooks"),0) ||
 !tolua_istype(tolua_S,2,tolua_tag(tolua_S,"ip_connection"),0) ||
 !tolua_istype(tolua_S,3,tolua_tag(tolua_S,"ip_connection"),0) ||
 !tolua_isnoobj(tolua_S,4)
 )
 goto tolua_lerror;
 else
 {
  zsock_hooks* self = (zsock_hooks*)  tolua_getusertype(tolua_S,1,0);
  ip_connection* conn = ((ip_connection*)  tolua_getusertype(tolua_S,2,0));
  ip_connection* child = ((ip_connection*)  tolua_getusertype(tolua_S,3,0));
 if (!self) tolua_error(tolua_S,"invalid 'self' in function 'accept'");
 {
  bool toluaI_ret = (bool)  self->accept(conn,child);
 tolua_pushnumber(tolua_S,(long)toluaI_ret);
 }
 }
 return 1;
tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'accept'.");
 return 0;
}

/* method: can_read of class  zsock_hooks */
static int toluaI_z_pack_zsock_hooks_can_read00(lua_State* tolua_S)
{
 if (
 !tolua_istype(tolua_S,1,tolua_tag(tolua_S,"zsock_hooks"),0) ||
 !tolua_istype(tolua_S,2,tolua_tag(tolua_S,"ip_connection"),0) ||
 !tolua_isnoobj(tolua_S,3)
 )
 goto tolua_lerror;
 else
 {
  zsock_hooks* self = (zsock_hooks*)  tolua_getusertype(tolua_S,1,0);
  ip_connection* conn = ((ip_connection*)  tolua_getusertype(tolua_S,2,0));
 if (!self) tolua_error(tolua_S,"invalid 'self' in function 'can_read'");
 {
  bool toluaI_ret = (bool)  self->can_read(conn);
 tolua_pushnumber(tolua_S,(long)toluaI_ret);
 }
 }
 return 1;
tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'can_read'.");
 return 0;
}

/* method: wait of class  zsock_hooks */
static int toluaI_z_pack_zsock_hooks_wait00(lua_State* tolua_S)
{
 if (
 !tolua_istype(tolua_S,1,tolua_tag(tolua_S,"zsock_hooks"),0) ||
 !tolua_istype(tolua_S,2,tolua_tag(tolua_S,"ip_connection"),0) ||
 !tolua_istype(tolua_S,3,LUA_TNUMBER,0) ||
 !tolua_isnoobj(tolua_S,4)
 )
 goto tolua_lerror;
 else
 {
  zsock_hooks* self = (zsock_hooks*)  tolua_getusertype(tolua_S,1,0);
  ip_connection* conn = ((ip_connection*)  tolua_getusertype(tolua_S,2,0));
  int seconds = ((int)  tolua_getnumber(tolua_S,3,0));
 if (!self) tolua_error(tolua_S,"invalid 'self' in function 'wait'");
 {
  bool toluaI_ret = (bool)  self->wait(conn,seconds);
 tolua_pushnumber(tolua_S,(long)toluaI_ret);
 }
 }
 return 1;
tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'wait'.");
 return 0;
}

/* method: add_timer of class  zsock_hooks */
static int toluaI_z_pack_zsock_hooks_add_timer00(lua_State* tolua_S)
{
 if (
 !tolua_istype(tolua_S,1,tolua_tag(tolua_S,"zsock_hooks"),0) ||
 !tolua_istype(tolua_S,2,tolua_tag(tolua_S,"timer_callback"),0) ||
 !tolua_isnoobj(tolua_S,3)
 )
 goto tolua_lerror;
 else
 {
  zsock_hooks* self = (zsock_hooks*)  tolua_getusertype(tolua_S,1,0);
  timer_callback callback = *((timer_callback*)  tolua_getusertype(tolua_S,2,0));
 if (!self) tolua_error(tolua_S,"invalid 'self' in function 'add_timer'");
 {
  bool toluaI_ret = (bool)  self->add_timer(callback);
 tolua_pushnumber(tolua_S,(long)toluaI_ret);
 }
 }
 return 1;
tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'add_timer'.");
 return 0;
}

/* method: remove_timer of class  zsock_hooks */
static int toluaI_z_pack_zsock_hooks_remove_timer00(lua_State* tolua_S)
{
 if (
 !tolua_istype(tolua_S,1,tolua_tag(tolua_S,"zsock_hooks"),0) ||
 !tolua_istype(tolua_S,2,tolua_tag(tolua_S,"timer_callback"),0) ||
 !tolua_isnoobj(tolua_S,3)
 )
 goto tolua_lerror;
 else
 {
  zsock_hooks* self = (zsock_hooks*)  tolua_getusertype(tolua_S,1,0);
  timer_callback callback = *((timer_callback*)  tolua_getusertype(tolua_S,2,0));
 if (!self) tolua_error(tolua_S,"invalid 'self' in function 'remove_timer'");
 {
  bool toluaI_ret = (bool)  self->remove_timer(callback);
 tolua_pushnumber(tolua_S,(long)toluaI_ret);
 }
 }
 return 1;
tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'remove_timer'.");
 return 0;
}

/* get function: zsock */
static int toluaI_get_z_pack_zsock(lua_State* tolua_S)
{
 tolua_pushusertype(tolua_S,(void*)&zsock,tolua_tag(tolua_S,"zsock_hooks"));
 return 1;
}

/* set function: zsock */
static int toluaI_set_z_pack_zsock(lua_State* tolua_S)
{
 if (!tolua_istype(tolua_S,1,tolua_tag(tolua_S,"zsock_hooks"),0))
 TOLUA_ERR_ASSIGN;
  zsock = *((zsock_hooks*)  tolua_getusertype(tolua_S,1,0));
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
 tolua_constant(tolua_S,NULL,"TERM_XTRA_GET_DELAY",TERM_XTRA_GET_DELAY);
 tolua_constant(tolua_S,NULL,"TERM_XTRA_SCANSUBDIR",TERM_XTRA_SCANSUBDIR);
 tolua_globalvar(tolua_S,"Term_xtra_long",toluaI_get_z_pack_Term_xtra_long,toluaI_set_z_pack_Term_xtra_long);
 tolua_globalarray(tolua_S,"scansubdir_dir",toluaI_get_z_pack_scansubdir_dir,toluaI_set_z_pack_scansubdir_dir);
 tolua_globalvar(tolua_S,"scansubdir_max",toluaI_get_z_pack_scansubdir_max,toluaI_set_z_pack_scansubdir_max);
 tolua_globalarray(tolua_S,"scansubdir_result",toluaI_get_z_pack_scansubdir_result,toluaI_set_z_pack_scansubdir_result);
 tolua_function(tolua_S,NULL,"Term_xtra",toluaI_z_pack_Term_xtra00);
 tolua_function(tolua_S,NULL,"Term_set_cursor",toluaI_z_pack_Term_set_cursor00);
 tolua_function(tolua_S,NULL,"Term_gotoxy",toluaI_z_pack_Term_gotoxy00);
 tolua_function(tolua_S,NULL,"Term_putch",toluaI_z_pack_Term_putch00);
 tolua_function(tolua_S,NULL,"Term_putstr",toluaI_z_pack_Term_putstr00);
 tolua_function(tolua_S,NULL,"Term_clear",toluaI_z_pack_Term_clear00);
 tolua_function(tolua_S,NULL,"Term_redraw",toluaI_z_pack_Term_redraw00);
 tolua_function(tolua_S,NULL,"Term_redraw_section",toluaI_z_pack_Term_redraw_section00);
 tolua_function(tolua_S,NULL,"Term_get_size",toluaI_z_pack_Term_get_size00);
 tolua_function(tolua_S,NULL,"rand_int",toluaI_z_pack_rand_int00);
 tolua_function(tolua_S,NULL,"rand_range",toluaI_z_pack_rand_range00);
 tolua_function(tolua_S,NULL,"rand_spread",toluaI_z_pack_rand_spread00);
 tolua_function(tolua_S,NULL,"randint",toluaI_z_pack_randint00);
 tolua_function(tolua_S,NULL,"magik",toluaI_z_pack_magik00);
 tolua_globalvar(tolua_S,"Rand_quick",toluaI_get_z_pack_Rand_quick,toluaI_set_z_pack_Rand_quick);
 tolua_globalvar(tolua_S,"Rand_value",toluaI_get_z_pack_Rand_value,toluaI_set_z_pack_Rand_value);
 tolua_function(tolua_S,NULL,"damroll",toluaI_z_pack_damroll00);
 tolua_function(tolua_S,NULL,"maxroll",toluaI_z_pack_maxroll00);
 tolua_cclass(tolua_S,"ip_connection","");
 tolua_tablevar(tolua_S,"ip_connection","setup",toluaI_get_z_pack_ip_connection_setup,toluaI_set_z_pack_ip_connection_setup);
 tolua_tablevar(tolua_S,"ip_connection","conn_ip",toluaI_get_z_pack_ip_connection_conn_ip,toluaI_set_z_pack_ip_connection_conn_ip);
 tolua_tablevar(tolua_S,"ip_connection","conn_port",toluaI_get_z_pack_ip_connection_conn_port,toluaI_set_z_pack_ip_connection_conn_port);
 tolua_tablevar(tolua_S,"ip_connection","conn_type",toluaI_get_z_pack_ip_connection_conn_type,toluaI_set_z_pack_ip_connection_conn_type);
 tolua_tablevar(tolua_S,"ip_connection","connected",toluaI_get_z_pack_ip_connection_connected,toluaI_set_z_pack_ip_connection_connected);
 tolua_tablevar(tolua_S,"ip_connection","socket",toluaI_get_z_pack_ip_connection_socket,toluaI_set_z_pack_ip_connection_socket);
 tolua_tablevar(tolua_S,"ip_connection","server",toluaI_get_z_pack_ip_connection_server,toluaI_set_z_pack_ip_connection_server);
 tolua_constant(tolua_S,NULL,"ZSOCK_TYPE_TCP",ZSOCK_TYPE_TCP);
 tolua_constant(tolua_S,NULL,"ZSOCK_TIMER_DELAY",ZSOCK_TIMER_DELAY);
 tolua_cclass(tolua_S,"zsock_hooks","");
 tolua_function(tolua_S,"zsock_hooks","new_connection",toluaI_z_pack_zsock_hooks_new_connection00);
 tolua_function(tolua_S,"zsock_hooks","free_connection",toluaI_z_pack_zsock_hooks_free_connection00);
 tolua_function(tolua_S,"zsock_hooks","setup",toluaI_z_pack_zsock_hooks_setup00);
 tolua_function(tolua_S,"zsock_hooks","unsetup",toluaI_z_pack_zsock_hooks_unsetup00);
 tolua_function(tolua_S,"zsock_hooks","open",toluaI_z_pack_zsock_hooks_open00);
 tolua_function(tolua_S,"zsock_hooks","close",toluaI_z_pack_zsock_hooks_close00);
 tolua_function(tolua_S,"zsock_hooks","write",toluaI_z_pack_zsock_hooks_write00);
 tolua_function(tolua_S,"zsock_hooks","write_simple",toluaI_z_pack_zsock_hooks_write_simple00);
 tolua_function(tolua_S,"zsock_hooks","read_simple",toluaI_z_pack_zsock_hooks_read_simple00);
 tolua_function(tolua_S,"zsock_hooks","accept",toluaI_z_pack_zsock_hooks_accept00);
 tolua_function(tolua_S,"zsock_hooks","can_read",toluaI_z_pack_zsock_hooks_can_read00);
 tolua_function(tolua_S,"zsock_hooks","wait",toluaI_z_pack_zsock_hooks_wait00);
 tolua_function(tolua_S,"zsock_hooks","add_timer",toluaI_z_pack_zsock_hooks_add_timer00);
 tolua_function(tolua_S,"zsock_hooks","remove_timer",toluaI_z_pack_zsock_hooks_remove_timer00);
 tolua_globalvar(tolua_S,"zsock",toluaI_get_z_pack_zsock,toluaI_set_z_pack_zsock);
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
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TERM_XTRA_GET_DELAY");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TERM_XTRA_SCANSUBDIR");
 lua_getglobals(tolua_S);
 lua_pushstring(tolua_S,"Term_xtra_long"); lua_pushnil(tolua_S); lua_rawset(tolua_S,-3);
 lua_pop(tolua_S,1);
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"scansubdir_dir");
 lua_getglobals(tolua_S);
 lua_pushstring(tolua_S,"scansubdir_max"); lua_pushnil(tolua_S); lua_rawset(tolua_S,-3);
 lua_pop(tolua_S,1);
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"scansubdir_result");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"Term_xtra");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"Term_set_cursor");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"Term_gotoxy");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"Term_putch");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"Term_putstr");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"Term_clear");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"Term_redraw");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"Term_redraw_section");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"Term_get_size");
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
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"ip_connection");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"ZSOCK_TYPE_TCP");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"ZSOCK_TIMER_DELAY");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"zsock_hooks");
 lua_getglobals(tolua_S);
 lua_pushstring(tolua_S,"zsock"); lua_pushnil(tolua_S); lua_rawset(tolua_S,-3);
 lua_pop(tolua_S,1);
}
