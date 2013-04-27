
/*
** $Id: lapi.h,v 1.1 2003/04/09 00:19:01 cipher Exp $
** Auxiliary functions from Lua API
** See Copyright Notice in lua.h
*/

#ifndef lapi_h
#define lapi_h

#include "lobject.h"

TObject        *luaA_index(lua_State * L,
                           int index);
void            luaA_pushobject(lua_State * L,
                                const TObject * o);

#endif