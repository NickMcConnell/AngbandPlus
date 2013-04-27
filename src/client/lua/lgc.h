
/*
** $Id: lgc.h,v 1.2 2003/03/17 22:45:30 cipher Exp $
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
