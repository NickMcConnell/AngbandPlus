
/*
** $Id: lapi.h,v 1.2 2003/03/17 22:45:30 cipher Exp $
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
