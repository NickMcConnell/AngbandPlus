/*
** $Id: lgc.h,v 1.1.1.1 2002/02/23 21:39:18 darkgod Exp $
** Garbage Collector
** See Copyright Notice in lua.h
*/

#ifndef lgc_h
#define lgc_h


#include "lobject.h"


void luaC_collect (lua_State *L, int all);
void luaC_checkGC (lua_State *L);


#endif
