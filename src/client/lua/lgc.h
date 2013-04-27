
/*
** $Id: lgc.h,v 1.1 2003/03/19 21:53:06 cipher Exp $
** Garbage Collector
** See Copyright Notice in lua.h
*/

#ifndef lgc_h
#define lgc_h

#include "lobject.h"

void            luaC_collect(lua_State * L,
                             int all);
void            luaC_collectgarbage(lua_State * L);
void            luaC_checkGC(lua_State * L);

#endif
