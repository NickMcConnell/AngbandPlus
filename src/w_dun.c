/*
** Lua binding: dungeon
** Generated automatically by tolua 4.0a - angband on Sun Jun 15 19:53:34 2003.
*/

#include "lua/tolua.h"

/* Exported function */
int tolua_dungeon_open (lua_State* tolua_S);
void tolua_dungeon_close (lua_State* tolua_S);

#include "angband.h"
static wilderness_map* lua_get_wild_map(int y, int x) { return &wild_map[y][x]; }

/* function to register type */
static void toluaI_reg_types (lua_State* tolua_S)
{
 tolua_usertype(tolua_S,"rule_type");
 tolua_usertype(tolua_S,"wilderness_type_info");
 tolua_usertype(tolua_S,"wilderness_map");
 tolua_usertype(tolua_S,"town_type");
 tolua_usertype(tolua_S,"border_type");
 tolua_usertype(tolua_S,"store_type");
 tolua_usertype(tolua_S,"obj_theme");
 tolua_usertype(tolua_S,"dungeon_info_type");
}

/* error messages */
#define TOLUA_ERR_SELF tolua_error(tolua_S,"invalid 'self'")
#define TOLUA_ERR_ASSIGN tolua_error(tolua_S,"#vinvalid type in variable assignment.")

/* get function: dungeon_flags1 */
static int toluaI_get_dungeon_level_flags1(lua_State* tolua_S)
{
 tolua_pushnumber(tolua_S,(long)dungeon_flags1);
 return 1;
}

/* set function: dungeon_flags1 */
static int toluaI_set_dungeon_level_flags1(lua_State* tolua_S)
{
 if (!tolua_istype(tolua_S,1,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  dungeon_flags1 = ((u32b)  tolua_getnumber(tolua_S,1,0));
 return 0;
}

/* get function: dungeon_flags2 */
static int toluaI_get_dungeon_level_flags2(lua_State* tolua_S)
{
 tolua_pushnumber(tolua_S,(long)dungeon_flags2);
 return 1;
}

/* set function: dungeon_flags2 */
static int toluaI_set_dungeon_level_flags2(lua_State* tolua_S)
{
 if (!tolua_istype(tolua_S,1,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  dungeon_flags2 = ((u32b)  tolua_getnumber(tolua_S,1,0));
 return 0;
}

/* get function: north of class  border_type */
static int toluaI_get_dungeon_border_type_north(lua_State* tolua_S)
{
 int toluaI_index;
  border_type* self;
 lua_pushstring(tolua_S,".self");
 lua_rawget(tolua_S,1);
 self = (border_type*)  lua_touserdata(tolua_S,-1);
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 tolua_error(tolua_S,"invalid type in array indexing.");
 toluaI_index = (int)tolua_getnumber(tolua_S,2,0)-1;
 if (toluaI_index<0 || toluaI_index>=MAX_WID)
 tolua_error(tolua_S,"array indexing out of range.");
 tolua_pushnumber(tolua_S,(long)self->north[toluaI_index]);
 return 1;
}

/* set function: north of class  border_type */
static int toluaI_set_dungeon_border_type_north(lua_State* tolua_S)
{
 int toluaI_index;
  border_type* self;
 lua_pushstring(tolua_S,".self");
 lua_rawget(tolua_S,1);
 self = (border_type*)  lua_touserdata(tolua_S,-1);
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 tolua_error(tolua_S,"invalid type in array indexing.");
 toluaI_index = (int)tolua_getnumber(tolua_S,2,0)-1;
 if (toluaI_index<0 || toluaI_index>=MAX_WID)
 tolua_error(tolua_S,"array indexing out of range.");
  self->north[toluaI_index] = ((byte)  tolua_getnumber(tolua_S,3,0));
 return 0;
}

/* get function: south of class  border_type */
static int toluaI_get_dungeon_border_type_south(lua_State* tolua_S)
{
 int toluaI_index;
  border_type* self;
 lua_pushstring(tolua_S,".self");
 lua_rawget(tolua_S,1);
 self = (border_type*)  lua_touserdata(tolua_S,-1);
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 tolua_error(tolua_S,"invalid type in array indexing.");
 toluaI_index = (int)tolua_getnumber(tolua_S,2,0)-1;
 if (toluaI_index<0 || toluaI_index>=MAX_WID)
 tolua_error(tolua_S,"array indexing out of range.");
 tolua_pushnumber(tolua_S,(long)self->south[toluaI_index]);
 return 1;
}

/* set function: south of class  border_type */
static int toluaI_set_dungeon_border_type_south(lua_State* tolua_S)
{
 int toluaI_index;
  border_type* self;
 lua_pushstring(tolua_S,".self");
 lua_rawget(tolua_S,1);
 self = (border_type*)  lua_touserdata(tolua_S,-1);
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 tolua_error(tolua_S,"invalid type in array indexing.");
 toluaI_index = (int)tolua_getnumber(tolua_S,2,0)-1;
 if (toluaI_index<0 || toluaI_index>=MAX_WID)
 tolua_error(tolua_S,"array indexing out of range.");
  self->south[toluaI_index] = ((byte)  tolua_getnumber(tolua_S,3,0));
 return 0;
}

/* get function: east of class  border_type */
static int toluaI_get_dungeon_border_type_east(lua_State* tolua_S)
{
 int toluaI_index;
  border_type* self;
 lua_pushstring(tolua_S,".self");
 lua_rawget(tolua_S,1);
 self = (border_type*)  lua_touserdata(tolua_S,-1);
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 tolua_error(tolua_S,"invalid type in array indexing.");
 toluaI_index = (int)tolua_getnumber(tolua_S,2,0)-1;
 if (toluaI_index<0 || toluaI_index>=MAX_HGT)
 tolua_error(tolua_S,"array indexing out of range.");
 tolua_pushnumber(tolua_S,(long)self->east[toluaI_index]);
 return 1;
}

/* set function: east of class  border_type */
static int toluaI_set_dungeon_border_type_east(lua_State* tolua_S)
{
 int toluaI_index;
  border_type* self;
 lua_pushstring(tolua_S,".self");
 lua_rawget(tolua_S,1);
 self = (border_type*)  lua_touserdata(tolua_S,-1);
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 tolua_error(tolua_S,"invalid type in array indexing.");
 toluaI_index = (int)tolua_getnumber(tolua_S,2,0)-1;
 if (toluaI_index<0 || toluaI_index>=MAX_HGT)
 tolua_error(tolua_S,"array indexing out of range.");
  self->east[toluaI_index] = ((byte)  tolua_getnumber(tolua_S,3,0));
 return 0;
}

/* get function: west of class  border_type */
static int toluaI_get_dungeon_border_type_west(lua_State* tolua_S)
{
 int toluaI_index;
  border_type* self;
 lua_pushstring(tolua_S,".self");
 lua_rawget(tolua_S,1);
 self = (border_type*)  lua_touserdata(tolua_S,-1);
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 tolua_error(tolua_S,"invalid type in array indexing.");
 toluaI_index = (int)tolua_getnumber(tolua_S,2,0)-1;
 if (toluaI_index<0 || toluaI_index>=MAX_HGT)
 tolua_error(tolua_S,"array indexing out of range.");
 tolua_pushnumber(tolua_S,(long)self->west[toluaI_index]);
 return 1;
}

/* set function: west of class  border_type */
static int toluaI_set_dungeon_border_type_west(lua_State* tolua_S)
{
 int toluaI_index;
  border_type* self;
 lua_pushstring(tolua_S,".self");
 lua_rawget(tolua_S,1);
 self = (border_type*)  lua_touserdata(tolua_S,-1);
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 tolua_error(tolua_S,"invalid type in array indexing.");
 toluaI_index = (int)tolua_getnumber(tolua_S,2,0)-1;
 if (toluaI_index<0 || toluaI_index>=MAX_HGT)
 tolua_error(tolua_S,"array indexing out of range.");
  self->west[toluaI_index] = ((byte)  tolua_getnumber(tolua_S,3,0));
 return 0;
}

/* get function: north_west of class  border_type */
static int toluaI_get_dungeon_border_type_north_west(lua_State* tolua_S)
{
  border_type* self = (border_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->north_west);
 return 1;
}

/* set function: north_west of class  border_type */
static int toluaI_set_dungeon_border_type_north_west(lua_State* tolua_S)
{
  border_type* self = (border_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->north_west = ((byte)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: north_east of class  border_type */
static int toluaI_get_dungeon_border_type_north_east(lua_State* tolua_S)
{
  border_type* self = (border_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->north_east);
 return 1;
}

/* set function: north_east of class  border_type */
static int toluaI_set_dungeon_border_type_north_east(lua_State* tolua_S)
{
  border_type* self = (border_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->north_east = ((byte)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: south_west of class  border_type */
static int toluaI_get_dungeon_border_type_south_west(lua_State* tolua_S)
{
  border_type* self = (border_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->south_west);
 return 1;
}

/* set function: south_west of class  border_type */
static int toluaI_set_dungeon_border_type_south_west(lua_State* tolua_S)
{
  border_type* self = (border_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->south_west = ((byte)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: south_east of class  border_type */
static int toluaI_get_dungeon_border_type_south_east(lua_State* tolua_S)
{
  border_type* self = (border_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->south_east);
 return 1;
}

/* set function: south_east of class  border_type */
static int toluaI_set_dungeon_border_type_south_east(lua_State* tolua_S)
{
  border_type* self = (border_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->south_east = ((byte)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: name of class  wilderness_type_info */
static int toluaI_get_dungeon_wilderness_type_info_name(lua_State* tolua_S)
{
  wilderness_type_info* self = (wilderness_type_info*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->name);
 return 1;
}

/* set function: name of class  wilderness_type_info */
static int toluaI_set_dungeon_wilderness_type_info_name(lua_State* tolua_S)
{
  wilderness_type_info* self = (wilderness_type_info*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->name = ((u32b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: text of class  wilderness_type_info */
static int toluaI_get_dungeon_wilderness_type_info_text(lua_State* tolua_S)
{
  wilderness_type_info* self = (wilderness_type_info*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->text);
 return 1;
}

/* set function: text of class  wilderness_type_info */
static int toluaI_set_dungeon_wilderness_type_info_text(lua_State* tolua_S)
{
  wilderness_type_info* self = (wilderness_type_info*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->text = ((u32b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: entrance of class  wilderness_type_info */
static int toluaI_get_dungeon_wilderness_type_info_entrance(lua_State* tolua_S)
{
  wilderness_type_info* self = (wilderness_type_info*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->entrance);
 return 1;
}

/* set function: entrance of class  wilderness_type_info */
static int toluaI_set_dungeon_wilderness_type_info_entrance(lua_State* tolua_S)
{
  wilderness_type_info* self = (wilderness_type_info*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->entrance = ((u16b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: road of class  wilderness_type_info */
static int toluaI_get_dungeon_wilderness_type_info_road(lua_State* tolua_S)
{
  wilderness_type_info* self = (wilderness_type_info*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->road);
 return 1;
}

/* set function: road of class  wilderness_type_info */
static int toluaI_set_dungeon_wilderness_type_info_road(lua_State* tolua_S)
{
  wilderness_type_info* self = (wilderness_type_info*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->road = ((byte)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: level of class  wilderness_type_info */
static int toluaI_get_dungeon_wilderness_type_info_level(lua_State* tolua_S)
{
  wilderness_type_info* self = (wilderness_type_info*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->level);
 return 1;
}

/* set function: level of class  wilderness_type_info */
static int toluaI_set_dungeon_wilderness_type_info_level(lua_State* tolua_S)
{
  wilderness_type_info* self = (wilderness_type_info*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->level = ((int)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: flags1 of class  wilderness_type_info */
static int toluaI_get_dungeon_wilderness_type_info_flags1(lua_State* tolua_S)
{
  wilderness_type_info* self = (wilderness_type_info*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->flags1);
 return 1;
}

/* set function: flags1 of class  wilderness_type_info */
static int toluaI_set_dungeon_wilderness_type_info_flags1(lua_State* tolua_S)
{
  wilderness_type_info* self = (wilderness_type_info*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->flags1 = ((u32b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: feat of class  wilderness_type_info */
static int toluaI_get_dungeon_wilderness_type_info_feat(lua_State* tolua_S)
{
  wilderness_type_info* self = (wilderness_type_info*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->feat);
 return 1;
}

/* set function: feat of class  wilderness_type_info */
static int toluaI_set_dungeon_wilderness_type_info_feat(lua_State* tolua_S)
{
  wilderness_type_info* self = (wilderness_type_info*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->feat = ((byte)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: terrain_idx of class  wilderness_type_info */
static int toluaI_get_dungeon_wilderness_type_info_terrain_idx(lua_State* tolua_S)
{
  wilderness_type_info* self = (wilderness_type_info*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->terrain_idx);
 return 1;
}

/* set function: terrain_idx of class  wilderness_type_info */
static int toluaI_set_dungeon_wilderness_type_info_terrain_idx(lua_State* tolua_S)
{
  wilderness_type_info* self = (wilderness_type_info*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->terrain_idx = ((byte)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: terrain of class  wilderness_type_info */
static int toluaI_get_dungeon_wilderness_type_info_terrain(lua_State* tolua_S)
{
 int toluaI_index;
  wilderness_type_info* self;
 lua_pushstring(tolua_S,".self");
 lua_rawget(tolua_S,1);
 self = (wilderness_type_info*)  lua_touserdata(tolua_S,-1);
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 tolua_error(tolua_S,"invalid type in array indexing.");
 toluaI_index = (int)tolua_getnumber(tolua_S,2,0)-1;
 if (toluaI_index<0 || toluaI_index>=MAX_WILD_TERRAIN)
 tolua_error(tolua_S,"array indexing out of range.");
 tolua_pushnumber(tolua_S,(long)self->terrain[toluaI_index]);
 return 1;
}

/* set function: terrain of class  wilderness_type_info */
static int toluaI_set_dungeon_wilderness_type_info_terrain(lua_State* tolua_S)
{
 int toluaI_index;
  wilderness_type_info* self;
 lua_pushstring(tolua_S,".self");
 lua_rawget(tolua_S,1);
 self = (wilderness_type_info*)  lua_touserdata(tolua_S,-1);
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 tolua_error(tolua_S,"invalid type in array indexing.");
 toluaI_index = (int)tolua_getnumber(tolua_S,2,0)-1;
 if (toluaI_index<0 || toluaI_index>=MAX_WILD_TERRAIN)
 tolua_error(tolua_S,"array indexing out of range.");
  self->terrain[toluaI_index] = ((byte)  tolua_getnumber(tolua_S,3,0));
 return 0;
}

/* get function: feat of class  wilderness_map */
static int toluaI_get_dungeon_wilderness_map_feat(lua_State* tolua_S)
{
  wilderness_map* self = (wilderness_map*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->feat);
 return 1;
}

/* set function: feat of class  wilderness_map */
static int toluaI_set_dungeon_wilderness_map_feat(lua_State* tolua_S)
{
  wilderness_map* self = (wilderness_map*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->feat = ((int)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: seed of class  wilderness_map */
static int toluaI_get_dungeon_wilderness_map_seed(lua_State* tolua_S)
{
  wilderness_map* self = (wilderness_map*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->seed);
 return 1;
}

/* set function: seed of class  wilderness_map */
static int toluaI_set_dungeon_wilderness_map_seed(lua_State* tolua_S)
{
  wilderness_map* self = (wilderness_map*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->seed = ((u32b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: entrance of class  wilderness_map */
static int toluaI_get_dungeon_wilderness_map_entrance(lua_State* tolua_S)
{
  wilderness_map* self = (wilderness_map*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->entrance);
 return 1;
}

/* set function: entrance of class  wilderness_map */
static int toluaI_set_dungeon_wilderness_map_entrance(lua_State* tolua_S)
{
  wilderness_map* self = (wilderness_map*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->entrance = ((u16b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: known of class  wilderness_map */
static int toluaI_get_dungeon_wilderness_map_known(lua_State* tolua_S)
{
  wilderness_map* self = (wilderness_map*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->known);
 return 1;
}

/* set function: known of class  wilderness_map */
static int toluaI_set_dungeon_wilderness_map_known(lua_State* tolua_S)
{
  wilderness_map* self = (wilderness_map*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->known = ((bool)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: name of class  town_type */
static int toluaI_get_dungeon_town_type_name(lua_State* tolua_S)
{
  town_type* self = (town_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushstring(tolua_S,(const char*)self->name);
 return 1;
}

/* set function: name of class  town_type */
static int toluaI_set_dungeon_town_type_name(lua_State* tolua_S)
{
  town_type* self = (town_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TSTRING,0))
 TOLUA_ERR_ASSIGN;
  self->name = ((cptr)  tolua_getstring(tolua_S,2,0));
 return 0;
}

/* get function: seed of class  town_type */
static int toluaI_get_dungeon_town_type_seed(lua_State* tolua_S)
{
  town_type* self = (town_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->seed);
 return 1;
}

/* set function: seed of class  town_type */
static int toluaI_set_dungeon_town_type_seed(lua_State* tolua_S)
{
  town_type* self = (town_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->seed = ((u32b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: store of class  town_type */
static int toluaI_get_dungeon_town_type_store(lua_State* tolua_S)
{
  town_type* self = (town_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushusertype(tolua_S,(void*)self->store,tolua_tag(tolua_S,"store_type"));
 return 1;
}

/* set function: store of class  town_type */
static int toluaI_set_dungeon_town_type_store(lua_State* tolua_S)
{
  town_type* self = (town_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,tolua_tag(tolua_S,"store_type"),0))
 TOLUA_ERR_ASSIGN;
  self->store = ((store_type*)  tolua_getusertype(tolua_S,2,0));
 return 0;
}

/* get function: numstores of class  town_type */
static int toluaI_get_dungeon_town_type_numstores(lua_State* tolua_S)
{
  town_type* self = (town_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->numstores);
 return 1;
}

/* set function: numstores of class  town_type */
static int toluaI_set_dungeon_town_type_numstores(lua_State* tolua_S)
{
  town_type* self = (town_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->numstores = ((byte)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: flags of class  town_type */
static int toluaI_get_dungeon_town_type_flags(lua_State* tolua_S)
{
  town_type* self = (town_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->flags);
 return 1;
}

/* set function: flags of class  town_type */
static int toluaI_set_dungeon_town_type_flags(lua_State* tolua_S)
{
  town_type* self = (town_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->flags = ((byte)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: stocked of class  town_type */
static int toluaI_get_dungeon_town_type_stocked(lua_State* tolua_S)
{
  town_type* self = (town_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->stocked);
 return 1;
}

/* set function: stocked of class  town_type */
static int toluaI_set_dungeon_town_type_stocked(lua_State* tolua_S)
{
  town_type* self = (town_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->stocked = ((bool)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: destroyed of class  town_type */
static int toluaI_get_dungeon_town_type_destroyed(lua_State* tolua_S)
{
  town_type* self = (town_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->destroyed);
 return 1;
}

/* set function: destroyed of class  town_type */
static int toluaI_set_dungeon_town_type_destroyed(lua_State* tolua_S)
{
  town_type* self = (town_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->destroyed = ((bool)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: max_towns */
static int toluaI_get_dungeon_max_towns(lua_State* tolua_S)
{
 tolua_pushnumber(tolua_S,(long)max_towns);
 return 1;
}

/* set function: max_towns */
static int toluaI_set_dungeon_max_towns(lua_State* tolua_S)
{
 if (!tolua_istype(tolua_S,1,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  max_towns = ((u16b)  tolua_getnumber(tolua_S,1,0));
 return 0;
}

/* get function: town_info */
static int toluaI_get_dungeon_town_info(lua_State* tolua_S)
{
 int toluaI_index;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 tolua_error(tolua_S,"invalid type in array indexing.");
 toluaI_index = (int)tolua_getnumber(tolua_S,2,0)-1;
 if (toluaI_index<0 || toluaI_index>=max_towns)
 tolua_error(tolua_S,"array indexing out of range.");
 tolua_pushusertype(tolua_S,(void*)&town_info[toluaI_index],tolua_tag(tolua_S,"town_type"));
 return 1;
}

/* set function: town_info */
static int toluaI_set_dungeon_town_info(lua_State* tolua_S)
{
 int toluaI_index;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 tolua_error(tolua_S,"invalid type in array indexing.");
 toluaI_index = (int)tolua_getnumber(tolua_S,2,0)-1;
 if (toluaI_index<0 || toluaI_index>=max_towns)
 tolua_error(tolua_S,"array indexing out of range.");
  town_info[toluaI_index] = *((town_type*)  tolua_getusertype(tolua_S,3,0));
 return 0;
}

/* get function: mode of class  rule_type */
static int toluaI_get_dungeon_rule_type_mode(lua_State* tolua_S)
{
  rule_type* self = (rule_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->mode);
 return 1;
}

/* set function: mode of class  rule_type */
static int toluaI_set_dungeon_rule_type_mode(lua_State* tolua_S)
{
  rule_type* self = (rule_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->mode = ((byte)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: percent of class  rule_type */
static int toluaI_get_dungeon_rule_type_percent(lua_State* tolua_S)
{
  rule_type* self = (rule_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->percent);
 return 1;
}

/* set function: percent of class  rule_type */
static int toluaI_set_dungeon_rule_type_percent(lua_State* tolua_S)
{
  rule_type* self = (rule_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->percent = ((byte)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: mflags1 of class  rule_type */
static int toluaI_get_dungeon_rule_type_mflags1(lua_State* tolua_S)
{
  rule_type* self = (rule_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->mflags1);
 return 1;
}

/* set function: mflags1 of class  rule_type */
static int toluaI_set_dungeon_rule_type_mflags1(lua_State* tolua_S)
{
  rule_type* self = (rule_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->mflags1 = ((u32b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: mflags2 of class  rule_type */
static int toluaI_get_dungeon_rule_type_mflags2(lua_State* tolua_S)
{
  rule_type* self = (rule_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->mflags2);
 return 1;
}

/* set function: mflags2 of class  rule_type */
static int toluaI_set_dungeon_rule_type_mflags2(lua_State* tolua_S)
{
  rule_type* self = (rule_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->mflags2 = ((u32b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: mflags3 of class  rule_type */
static int toluaI_get_dungeon_rule_type_mflags3(lua_State* tolua_S)
{
  rule_type* self = (rule_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->mflags3);
 return 1;
}

/* set function: mflags3 of class  rule_type */
static int toluaI_set_dungeon_rule_type_mflags3(lua_State* tolua_S)
{
  rule_type* self = (rule_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->mflags3 = ((u32b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: mflags4 of class  rule_type */
static int toluaI_get_dungeon_rule_type_mflags4(lua_State* tolua_S)
{
  rule_type* self = (rule_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->mflags4);
 return 1;
}

/* set function: mflags4 of class  rule_type */
static int toluaI_set_dungeon_rule_type_mflags4(lua_State* tolua_S)
{
  rule_type* self = (rule_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->mflags4 = ((u32b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: mflags5 of class  rule_type */
static int toluaI_get_dungeon_rule_type_mflags5(lua_State* tolua_S)
{
  rule_type* self = (rule_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->mflags5);
 return 1;
}

/* set function: mflags5 of class  rule_type */
static int toluaI_set_dungeon_rule_type_mflags5(lua_State* tolua_S)
{
  rule_type* self = (rule_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->mflags5 = ((u32b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: mflags6 of class  rule_type */
static int toluaI_get_dungeon_rule_type_mflags6(lua_State* tolua_S)
{
  rule_type* self = (rule_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->mflags6);
 return 1;
}

/* set function: mflags6 of class  rule_type */
static int toluaI_set_dungeon_rule_type_mflags6(lua_State* tolua_S)
{
  rule_type* self = (rule_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->mflags6 = ((u32b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: mflags7 of class  rule_type */
static int toluaI_get_dungeon_rule_type_mflags7(lua_State* tolua_S)
{
  rule_type* self = (rule_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->mflags7);
 return 1;
}

/* set function: mflags7 of class  rule_type */
static int toluaI_set_dungeon_rule_type_mflags7(lua_State* tolua_S)
{
  rule_type* self = (rule_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->mflags7 = ((u32b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: mflags8 of class  rule_type */
static int toluaI_get_dungeon_rule_type_mflags8(lua_State* tolua_S)
{
  rule_type* self = (rule_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->mflags8);
 return 1;
}

/* set function: mflags8 of class  rule_type */
static int toluaI_set_dungeon_rule_type_mflags8(lua_State* tolua_S)
{
  rule_type* self = (rule_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->mflags8 = ((u32b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: mflags9 of class  rule_type */
static int toluaI_get_dungeon_rule_type_mflags9(lua_State* tolua_S)
{
  rule_type* self = (rule_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->mflags9);
 return 1;
}

/* set function: mflags9 of class  rule_type */
static int toluaI_set_dungeon_rule_type_mflags9(lua_State* tolua_S)
{
  rule_type* self = (rule_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->mflags9 = ((u32b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: r_char of class  rule_type */
static int toluaI_get_dungeon_rule_type_r_char(lua_State* tolua_S)
{
 int toluaI_index;
  rule_type* self;
 lua_pushstring(tolua_S,".self");
 lua_rawget(tolua_S,1);
 self = (rule_type*)  lua_touserdata(tolua_S,-1);
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 tolua_error(tolua_S,"invalid type in array indexing.");
 toluaI_index = (int)tolua_getnumber(tolua_S,2,0)-1;
 if (toluaI_index<0 || toluaI_index>=5)
 tolua_error(tolua_S,"array indexing out of range.");
 tolua_pushnumber(tolua_S,(long)self->r_char[toluaI_index]);
 return 1;
}

/* set function: r_char of class  rule_type */
static int toluaI_set_dungeon_rule_type_r_char(lua_State* tolua_S)
{
 int toluaI_index;
  rule_type* self;
 lua_pushstring(tolua_S,".self");
 lua_rawget(tolua_S,1);
 self = (rule_type*)  lua_touserdata(tolua_S,-1);
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 tolua_error(tolua_S,"invalid type in array indexing.");
 toluaI_index = (int)tolua_getnumber(tolua_S,2,0)-1;
 if (toluaI_index<0 || toluaI_index>=5)
 tolua_error(tolua_S,"array indexing out of range.");
  self->r_char[toluaI_index] = ((char)  tolua_getnumber(tolua_S,3,0));
 return 0;
}

/* get function: treasure of class  obj_theme */
static int toluaI_get_dungeon_obj_theme_treasure(lua_State* tolua_S)
{
  obj_theme* self = (obj_theme*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->treasure);
 return 1;
}

/* set function: treasure of class  obj_theme */
static int toluaI_set_dungeon_obj_theme_treasure(lua_State* tolua_S)
{
  obj_theme* self = (obj_theme*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->treasure = ((byte)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: combat of class  obj_theme */
static int toluaI_get_dungeon_obj_theme_combat(lua_State* tolua_S)
{
  obj_theme* self = (obj_theme*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->combat);
 return 1;
}

/* set function: combat of class  obj_theme */
static int toluaI_set_dungeon_obj_theme_combat(lua_State* tolua_S)
{
  obj_theme* self = (obj_theme*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->combat = ((byte)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: magic of class  obj_theme */
static int toluaI_get_dungeon_obj_theme_magic(lua_State* tolua_S)
{
  obj_theme* self = (obj_theme*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->magic);
 return 1;
}

/* set function: magic of class  obj_theme */
static int toluaI_set_dungeon_obj_theme_magic(lua_State* tolua_S)
{
  obj_theme* self = (obj_theme*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->magic = ((byte)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: tools of class  obj_theme */
static int toluaI_get_dungeon_obj_theme_tools(lua_State* tolua_S)
{
  obj_theme* self = (obj_theme*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->tools);
 return 1;
}

/* set function: tools of class  obj_theme */
static int toluaI_set_dungeon_obj_theme_tools(lua_State* tolua_S)
{
  obj_theme* self = (obj_theme*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->tools = ((byte)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: name of class  dungeon_info_type */
static int toluaI_get_dungeon_dungeon_info_type_name(lua_State* tolua_S)
{
  dungeon_info_type* self = (dungeon_info_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->name);
 return 1;
}

/* set function: name of class  dungeon_info_type */
static int toluaI_set_dungeon_dungeon_info_type_name(lua_State* tolua_S)
{
  dungeon_info_type* self = (dungeon_info_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->name = ((u32b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: text of class  dungeon_info_type */
static int toluaI_get_dungeon_dungeon_info_type_text(lua_State* tolua_S)
{
  dungeon_info_type* self = (dungeon_info_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->text);
 return 1;
}

/* set function: text of class  dungeon_info_type */
static int toluaI_set_dungeon_dungeon_info_type_text(lua_State* tolua_S)
{
  dungeon_info_type* self = (dungeon_info_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->text = ((u32b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: short_name of class  dungeon_info_type */
static int toluaI_get_dungeon_dungeon_info_type_short_name(lua_State* tolua_S)
{
 int toluaI_index;
  dungeon_info_type* self;
 lua_pushstring(tolua_S,".self");
 lua_rawget(tolua_S,1);
 self = (dungeon_info_type*)  lua_touserdata(tolua_S,-1);
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 tolua_error(tolua_S,"invalid type in array indexing.");
 toluaI_index = (int)tolua_getnumber(tolua_S,2,0)-1;
 if (toluaI_index<0 || toluaI_index>=3)
 tolua_error(tolua_S,"array indexing out of range.");
 tolua_pushnumber(tolua_S,(long)self->short_name[toluaI_index]);
 return 1;
}

/* set function: short_name of class  dungeon_info_type */
static int toluaI_set_dungeon_dungeon_info_type_short_name(lua_State* tolua_S)
{
 int toluaI_index;
  dungeon_info_type* self;
 lua_pushstring(tolua_S,".self");
 lua_rawget(tolua_S,1);
 self = (dungeon_info_type*)  lua_touserdata(tolua_S,-1);
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 tolua_error(tolua_S,"invalid type in array indexing.");
 toluaI_index = (int)tolua_getnumber(tolua_S,2,0)-1;
 if (toluaI_index<0 || toluaI_index>=3)
 tolua_error(tolua_S,"array indexing out of range.");
  self->short_name[toluaI_index] = ((char)  tolua_getnumber(tolua_S,3,0));
 return 0;
}

/* get function: floor1 of class  dungeon_info_type */
static int toluaI_get_dungeon_dungeon_info_type_floor1(lua_State* tolua_S)
{
  dungeon_info_type* self = (dungeon_info_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->floor1);
 return 1;
}

/* set function: floor1 of class  dungeon_info_type */
static int toluaI_set_dungeon_dungeon_info_type_floor1(lua_State* tolua_S)
{
  dungeon_info_type* self = (dungeon_info_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->floor1 = ((s16b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: floor_percent1 of class  dungeon_info_type */
static int toluaI_get_dungeon_dungeon_info_type_floor_percent1(lua_State* tolua_S)
{
 int toluaI_index;
  dungeon_info_type* self;
 lua_pushstring(tolua_S,".self");
 lua_rawget(tolua_S,1);
 self = (dungeon_info_type*)  lua_touserdata(tolua_S,-1);
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 tolua_error(tolua_S,"invalid type in array indexing.");
 toluaI_index = (int)tolua_getnumber(tolua_S,2,0)-1;
 if (toluaI_index<0 || toluaI_index>=2)
 tolua_error(tolua_S,"array indexing out of range.");
 tolua_pushnumber(tolua_S,(long)self->floor_percent1[toluaI_index]);
 return 1;
}

/* set function: floor_percent1 of class  dungeon_info_type */
static int toluaI_set_dungeon_dungeon_info_type_floor_percent1(lua_State* tolua_S)
{
 int toluaI_index;
  dungeon_info_type* self;
 lua_pushstring(tolua_S,".self");
 lua_rawget(tolua_S,1);
 self = (dungeon_info_type*)  lua_touserdata(tolua_S,-1);
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 tolua_error(tolua_S,"invalid type in array indexing.");
 toluaI_index = (int)tolua_getnumber(tolua_S,2,0)-1;
 if (toluaI_index<0 || toluaI_index>=2)
 tolua_error(tolua_S,"array indexing out of range.");
  self->floor_percent1[toluaI_index] = ((byte)  tolua_getnumber(tolua_S,3,0));
 return 0;
}

/* get function: floor2 of class  dungeon_info_type */
static int toluaI_get_dungeon_dungeon_info_type_floor2(lua_State* tolua_S)
{
  dungeon_info_type* self = (dungeon_info_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->floor2);
 return 1;
}

/* set function: floor2 of class  dungeon_info_type */
static int toluaI_set_dungeon_dungeon_info_type_floor2(lua_State* tolua_S)
{
  dungeon_info_type* self = (dungeon_info_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->floor2 = ((s16b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: floor_percent2 of class  dungeon_info_type */
static int toluaI_get_dungeon_dungeon_info_type_floor_percent2(lua_State* tolua_S)
{
 int toluaI_index;
  dungeon_info_type* self;
 lua_pushstring(tolua_S,".self");
 lua_rawget(tolua_S,1);
 self = (dungeon_info_type*)  lua_touserdata(tolua_S,-1);
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 tolua_error(tolua_S,"invalid type in array indexing.");
 toluaI_index = (int)tolua_getnumber(tolua_S,2,0)-1;
 if (toluaI_index<0 || toluaI_index>=2)
 tolua_error(tolua_S,"array indexing out of range.");
 tolua_pushnumber(tolua_S,(long)self->floor_percent2[toluaI_index]);
 return 1;
}

/* set function: floor_percent2 of class  dungeon_info_type */
static int toluaI_set_dungeon_dungeon_info_type_floor_percent2(lua_State* tolua_S)
{
 int toluaI_index;
  dungeon_info_type* self;
 lua_pushstring(tolua_S,".self");
 lua_rawget(tolua_S,1);
 self = (dungeon_info_type*)  lua_touserdata(tolua_S,-1);
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 tolua_error(tolua_S,"invalid type in array indexing.");
 toluaI_index = (int)tolua_getnumber(tolua_S,2,0)-1;
 if (toluaI_index<0 || toluaI_index>=2)
 tolua_error(tolua_S,"array indexing out of range.");
  self->floor_percent2[toluaI_index] = ((byte)  tolua_getnumber(tolua_S,3,0));
 return 0;
}

/* get function: floor3 of class  dungeon_info_type */
static int toluaI_get_dungeon_dungeon_info_type_floor3(lua_State* tolua_S)
{
  dungeon_info_type* self = (dungeon_info_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->floor3);
 return 1;
}

/* set function: floor3 of class  dungeon_info_type */
static int toluaI_set_dungeon_dungeon_info_type_floor3(lua_State* tolua_S)
{
  dungeon_info_type* self = (dungeon_info_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->floor3 = ((s16b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: floor_percent3 of class  dungeon_info_type */
static int toluaI_get_dungeon_dungeon_info_type_floor_percent3(lua_State* tolua_S)
{
 int toluaI_index;
  dungeon_info_type* self;
 lua_pushstring(tolua_S,".self");
 lua_rawget(tolua_S,1);
 self = (dungeon_info_type*)  lua_touserdata(tolua_S,-1);
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 tolua_error(tolua_S,"invalid type in array indexing.");
 toluaI_index = (int)tolua_getnumber(tolua_S,2,0)-1;
 if (toluaI_index<0 || toluaI_index>=2)
 tolua_error(tolua_S,"array indexing out of range.");
 tolua_pushnumber(tolua_S,(long)self->floor_percent3[toluaI_index]);
 return 1;
}

/* set function: floor_percent3 of class  dungeon_info_type */
static int toluaI_set_dungeon_dungeon_info_type_floor_percent3(lua_State* tolua_S)
{
 int toluaI_index;
  dungeon_info_type* self;
 lua_pushstring(tolua_S,".self");
 lua_rawget(tolua_S,1);
 self = (dungeon_info_type*)  lua_touserdata(tolua_S,-1);
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 tolua_error(tolua_S,"invalid type in array indexing.");
 toluaI_index = (int)tolua_getnumber(tolua_S,2,0)-1;
 if (toluaI_index<0 || toluaI_index>=2)
 tolua_error(tolua_S,"array indexing out of range.");
  self->floor_percent3[toluaI_index] = ((byte)  tolua_getnumber(tolua_S,3,0));
 return 0;
}

/* get function: outer_wall of class  dungeon_info_type */
static int toluaI_get_dungeon_dungeon_info_type_outer_wall(lua_State* tolua_S)
{
  dungeon_info_type* self = (dungeon_info_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->outer_wall);
 return 1;
}

/* set function: outer_wall of class  dungeon_info_type */
static int toluaI_set_dungeon_dungeon_info_type_outer_wall(lua_State* tolua_S)
{
  dungeon_info_type* self = (dungeon_info_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->outer_wall = ((s16b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: inner_wall of class  dungeon_info_type */
static int toluaI_get_dungeon_dungeon_info_type_inner_wall(lua_State* tolua_S)
{
  dungeon_info_type* self = (dungeon_info_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->inner_wall);
 return 1;
}

/* set function: inner_wall of class  dungeon_info_type */
static int toluaI_set_dungeon_dungeon_info_type_inner_wall(lua_State* tolua_S)
{
  dungeon_info_type* self = (dungeon_info_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->inner_wall = ((s16b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: fill_type1 of class  dungeon_info_type */
static int toluaI_get_dungeon_dungeon_info_type_fill_type1(lua_State* tolua_S)
{
  dungeon_info_type* self = (dungeon_info_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->fill_type1);
 return 1;
}

/* set function: fill_type1 of class  dungeon_info_type */
static int toluaI_set_dungeon_dungeon_info_type_fill_type1(lua_State* tolua_S)
{
  dungeon_info_type* self = (dungeon_info_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->fill_type1 = ((s16b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: fill_percent1 of class  dungeon_info_type */
static int toluaI_get_dungeon_dungeon_info_type_fill_percent1(lua_State* tolua_S)
{
 int toluaI_index;
  dungeon_info_type* self;
 lua_pushstring(tolua_S,".self");
 lua_rawget(tolua_S,1);
 self = (dungeon_info_type*)  lua_touserdata(tolua_S,-1);
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 tolua_error(tolua_S,"invalid type in array indexing.");
 toluaI_index = (int)tolua_getnumber(tolua_S,2,0)-1;
 if (toluaI_index<0 || toluaI_index>=2)
 tolua_error(tolua_S,"array indexing out of range.");
 tolua_pushnumber(tolua_S,(long)self->fill_percent1[toluaI_index]);
 return 1;
}

/* set function: fill_percent1 of class  dungeon_info_type */
static int toluaI_set_dungeon_dungeon_info_type_fill_percent1(lua_State* tolua_S)
{
 int toluaI_index;
  dungeon_info_type* self;
 lua_pushstring(tolua_S,".self");
 lua_rawget(tolua_S,1);
 self = (dungeon_info_type*)  lua_touserdata(tolua_S,-1);
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 tolua_error(tolua_S,"invalid type in array indexing.");
 toluaI_index = (int)tolua_getnumber(tolua_S,2,0)-1;
 if (toluaI_index<0 || toluaI_index>=2)
 tolua_error(tolua_S,"array indexing out of range.");
  self->fill_percent1[toluaI_index] = ((byte)  tolua_getnumber(tolua_S,3,0));
 return 0;
}

/* get function: fill_type2 of class  dungeon_info_type */
static int toluaI_get_dungeon_dungeon_info_type_fill_type2(lua_State* tolua_S)
{
  dungeon_info_type* self = (dungeon_info_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->fill_type2);
 return 1;
}

/* set function: fill_type2 of class  dungeon_info_type */
static int toluaI_set_dungeon_dungeon_info_type_fill_type2(lua_State* tolua_S)
{
  dungeon_info_type* self = (dungeon_info_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->fill_type2 = ((s16b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: fill_percent2 of class  dungeon_info_type */
static int toluaI_get_dungeon_dungeon_info_type_fill_percent2(lua_State* tolua_S)
{
 int toluaI_index;
  dungeon_info_type* self;
 lua_pushstring(tolua_S,".self");
 lua_rawget(tolua_S,1);
 self = (dungeon_info_type*)  lua_touserdata(tolua_S,-1);
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 tolua_error(tolua_S,"invalid type in array indexing.");
 toluaI_index = (int)tolua_getnumber(tolua_S,2,0)-1;
 if (toluaI_index<0 || toluaI_index>=2)
 tolua_error(tolua_S,"array indexing out of range.");
 tolua_pushnumber(tolua_S,(long)self->fill_percent2[toluaI_index]);
 return 1;
}

/* set function: fill_percent2 of class  dungeon_info_type */
static int toluaI_set_dungeon_dungeon_info_type_fill_percent2(lua_State* tolua_S)
{
 int toluaI_index;
  dungeon_info_type* self;
 lua_pushstring(tolua_S,".self");
 lua_rawget(tolua_S,1);
 self = (dungeon_info_type*)  lua_touserdata(tolua_S,-1);
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 tolua_error(tolua_S,"invalid type in array indexing.");
 toluaI_index = (int)tolua_getnumber(tolua_S,2,0)-1;
 if (toluaI_index<0 || toluaI_index>=2)
 tolua_error(tolua_S,"array indexing out of range.");
  self->fill_percent2[toluaI_index] = ((byte)  tolua_getnumber(tolua_S,3,0));
 return 0;
}

/* get function: fill_type3 of class  dungeon_info_type */
static int toluaI_get_dungeon_dungeon_info_type_fill_type3(lua_State* tolua_S)
{
  dungeon_info_type* self = (dungeon_info_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->fill_type3);
 return 1;
}

/* set function: fill_type3 of class  dungeon_info_type */
static int toluaI_set_dungeon_dungeon_info_type_fill_type3(lua_State* tolua_S)
{
  dungeon_info_type* self = (dungeon_info_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->fill_type3 = ((s16b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: fill_percent3 of class  dungeon_info_type */
static int toluaI_get_dungeon_dungeon_info_type_fill_percent3(lua_State* tolua_S)
{
 int toluaI_index;
  dungeon_info_type* self;
 lua_pushstring(tolua_S,".self");
 lua_rawget(tolua_S,1);
 self = (dungeon_info_type*)  lua_touserdata(tolua_S,-1);
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 tolua_error(tolua_S,"invalid type in array indexing.");
 toluaI_index = (int)tolua_getnumber(tolua_S,2,0)-1;
 if (toluaI_index<0 || toluaI_index>=2)
 tolua_error(tolua_S,"array indexing out of range.");
 tolua_pushnumber(tolua_S,(long)self->fill_percent3[toluaI_index]);
 return 1;
}

/* set function: fill_percent3 of class  dungeon_info_type */
static int toluaI_set_dungeon_dungeon_info_type_fill_percent3(lua_State* tolua_S)
{
 int toluaI_index;
  dungeon_info_type* self;
 lua_pushstring(tolua_S,".self");
 lua_rawget(tolua_S,1);
 self = (dungeon_info_type*)  lua_touserdata(tolua_S,-1);
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 tolua_error(tolua_S,"invalid type in array indexing.");
 toluaI_index = (int)tolua_getnumber(tolua_S,2,0)-1;
 if (toluaI_index<0 || toluaI_index>=2)
 tolua_error(tolua_S,"array indexing out of range.");
  self->fill_percent3[toluaI_index] = ((byte)  tolua_getnumber(tolua_S,3,0));
 return 0;
}

/* get function: fill_method of class  dungeon_info_type */
static int toluaI_get_dungeon_dungeon_info_type_fill_method(lua_State* tolua_S)
{
  dungeon_info_type* self = (dungeon_info_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->fill_method);
 return 1;
}

/* set function: fill_method of class  dungeon_info_type */
static int toluaI_set_dungeon_dungeon_info_type_fill_method(lua_State* tolua_S)
{
  dungeon_info_type* self = (dungeon_info_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->fill_method = ((byte)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: mindepth of class  dungeon_info_type */
static int toluaI_get_dungeon_dungeon_info_type_mindepth(lua_State* tolua_S)
{
  dungeon_info_type* self = (dungeon_info_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->mindepth);
 return 1;
}

/* set function: mindepth of class  dungeon_info_type */
static int toluaI_set_dungeon_dungeon_info_type_mindepth(lua_State* tolua_S)
{
  dungeon_info_type* self = (dungeon_info_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->mindepth = ((s16b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: maxdepth of class  dungeon_info_type */
static int toluaI_get_dungeon_dungeon_info_type_maxdepth(lua_State* tolua_S)
{
  dungeon_info_type* self = (dungeon_info_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->maxdepth);
 return 1;
}

/* set function: maxdepth of class  dungeon_info_type */
static int toluaI_set_dungeon_dungeon_info_type_maxdepth(lua_State* tolua_S)
{
  dungeon_info_type* self = (dungeon_info_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->maxdepth = ((s16b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: principal of class  dungeon_info_type */
static int toluaI_get_dungeon_dungeon_info_type_principal(lua_State* tolua_S)
{
  dungeon_info_type* self = (dungeon_info_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->principal);
 return 1;
}

/* set function: principal of class  dungeon_info_type */
static int toluaI_set_dungeon_dungeon_info_type_principal(lua_State* tolua_S)
{
  dungeon_info_type* self = (dungeon_info_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->principal = ((bool)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: next of class  dungeon_info_type */
static int toluaI_get_dungeon_dungeon_info_type_next(lua_State* tolua_S)
{
  dungeon_info_type* self = (dungeon_info_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->next);
 return 1;
}

/* set function: next of class  dungeon_info_type */
static int toluaI_set_dungeon_dungeon_info_type_next(lua_State* tolua_S)
{
  dungeon_info_type* self = (dungeon_info_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->next = ((byte)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: min_plev of class  dungeon_info_type */
static int toluaI_get_dungeon_dungeon_info_type_min_plev(lua_State* tolua_S)
{
  dungeon_info_type* self = (dungeon_info_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->min_plev);
 return 1;
}

/* set function: min_plev of class  dungeon_info_type */
static int toluaI_set_dungeon_dungeon_info_type_min_plev(lua_State* tolua_S)
{
  dungeon_info_type* self = (dungeon_info_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->min_plev = ((byte)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: min_m_alloc_level of class  dungeon_info_type */
static int toluaI_get_dungeon_dungeon_info_type_min_m_alloc_level(lua_State* tolua_S)
{
  dungeon_info_type* self = (dungeon_info_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->min_m_alloc_level);
 return 1;
}

/* set function: min_m_alloc_level of class  dungeon_info_type */
static int toluaI_set_dungeon_dungeon_info_type_min_m_alloc_level(lua_State* tolua_S)
{
  dungeon_info_type* self = (dungeon_info_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->min_m_alloc_level = ((int)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: max_m_alloc_chance of class  dungeon_info_type */
static int toluaI_get_dungeon_dungeon_info_type_max_m_alloc_chance(lua_State* tolua_S)
{
  dungeon_info_type* self = (dungeon_info_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->max_m_alloc_chance);
 return 1;
}

/* set function: max_m_alloc_chance of class  dungeon_info_type */
static int toluaI_set_dungeon_dungeon_info_type_max_m_alloc_chance(lua_State* tolua_S)
{
  dungeon_info_type* self = (dungeon_info_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->max_m_alloc_chance = ((int)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: flags1 of class  dungeon_info_type */
static int toluaI_get_dungeon_dungeon_info_type_flags1(lua_State* tolua_S)
{
  dungeon_info_type* self = (dungeon_info_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->flags1);
 return 1;
}

/* set function: flags1 of class  dungeon_info_type */
static int toluaI_set_dungeon_dungeon_info_type_flags1(lua_State* tolua_S)
{
  dungeon_info_type* self = (dungeon_info_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->flags1 = ((u32b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: flags2 of class  dungeon_info_type */
static int toluaI_get_dungeon_dungeon_info_type_flags2(lua_State* tolua_S)
{
  dungeon_info_type* self = (dungeon_info_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->flags2);
 return 1;
}

/* set function: flags2 of class  dungeon_info_type */
static int toluaI_set_dungeon_dungeon_info_type_flags2(lua_State* tolua_S)
{
  dungeon_info_type* self = (dungeon_info_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->flags2 = ((u32b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: size_x of class  dungeon_info_type */
static int toluaI_get_dungeon_dungeon_info_type_size_x(lua_State* tolua_S)
{
  dungeon_info_type* self = (dungeon_info_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->size_x);
 return 1;
}

/* set function: size_x of class  dungeon_info_type */
static int toluaI_set_dungeon_dungeon_info_type_size_x(lua_State* tolua_S)
{
  dungeon_info_type* self = (dungeon_info_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->size_x = ((int)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: size_y of class  dungeon_info_type */
static int toluaI_get_dungeon_dungeon_info_type_size_y(lua_State* tolua_S)
{
  dungeon_info_type* self = (dungeon_info_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->size_y);
 return 1;
}

/* set function: size_y of class  dungeon_info_type */
static int toluaI_set_dungeon_dungeon_info_type_size_y(lua_State* tolua_S)
{
  dungeon_info_type* self = (dungeon_info_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->size_y = ((int)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: rule_percents of class  dungeon_info_type */
static int toluaI_get_dungeon_dungeon_info_type_rule_percents(lua_State* tolua_S)
{
 int toluaI_index;
  dungeon_info_type* self;
 lua_pushstring(tolua_S,".self");
 lua_rawget(tolua_S,1);
 self = (dungeon_info_type*)  lua_touserdata(tolua_S,-1);
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 tolua_error(tolua_S,"invalid type in array indexing.");
 toluaI_index = (int)tolua_getnumber(tolua_S,2,0)-1;
 if (toluaI_index<0 || toluaI_index>=100)
 tolua_error(tolua_S,"array indexing out of range.");
 tolua_pushnumber(tolua_S,(long)self->rule_percents[toluaI_index]);
 return 1;
}

/* set function: rule_percents of class  dungeon_info_type */
static int toluaI_set_dungeon_dungeon_info_type_rule_percents(lua_State* tolua_S)
{
 int toluaI_index;
  dungeon_info_type* self;
 lua_pushstring(tolua_S,".self");
 lua_rawget(tolua_S,1);
 self = (dungeon_info_type*)  lua_touserdata(tolua_S,-1);
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 tolua_error(tolua_S,"invalid type in array indexing.");
 toluaI_index = (int)tolua_getnumber(tolua_S,2,0)-1;
 if (toluaI_index<0 || toluaI_index>=100)
 tolua_error(tolua_S,"array indexing out of range.");
  self->rule_percents[toluaI_index] = ((byte)  tolua_getnumber(tolua_S,3,0));
 return 0;
}

/* get function: rules of class  dungeon_info_type */
static int toluaI_get_dungeon_dungeon_info_type_rules(lua_State* tolua_S)
{
 int toluaI_index;
  dungeon_info_type* self;
 lua_pushstring(tolua_S,".self");
 lua_rawget(tolua_S,1);
 self = (dungeon_info_type*)  lua_touserdata(tolua_S,-1);
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 tolua_error(tolua_S,"invalid type in array indexing.");
 toluaI_index = (int)tolua_getnumber(tolua_S,2,0)-1;
 if (toluaI_index<0 || toluaI_index>=5)
 tolua_error(tolua_S,"array indexing out of range.");
 tolua_pushusertype(tolua_S,(void*)&self->rules[toluaI_index],tolua_tag(tolua_S,"rule_type"));
 return 1;
}

/* set function: rules of class  dungeon_info_type */
static int toluaI_set_dungeon_dungeon_info_type_rules(lua_State* tolua_S)
{
 int toluaI_index;
  dungeon_info_type* self;
 lua_pushstring(tolua_S,".self");
 lua_rawget(tolua_S,1);
 self = (dungeon_info_type*)  lua_touserdata(tolua_S,-1);
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 tolua_error(tolua_S,"invalid type in array indexing.");
 toluaI_index = (int)tolua_getnumber(tolua_S,2,0)-1;
 if (toluaI_index<0 || toluaI_index>=5)
 tolua_error(tolua_S,"array indexing out of range.");
  self->rules[toluaI_index] = *((rule_type*)  tolua_getusertype(tolua_S,3,0));
 return 0;
}

/* get function: final_object of class  dungeon_info_type */
static int toluaI_get_dungeon_dungeon_info_type_final_object(lua_State* tolua_S)
{
  dungeon_info_type* self = (dungeon_info_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->final_object);
 return 1;
}

/* set function: final_object of class  dungeon_info_type */
static int toluaI_set_dungeon_dungeon_info_type_final_object(lua_State* tolua_S)
{
  dungeon_info_type* self = (dungeon_info_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->final_object = ((int)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: final_artifact of class  dungeon_info_type */
static int toluaI_get_dungeon_dungeon_info_type_final_artifact(lua_State* tolua_S)
{
  dungeon_info_type* self = (dungeon_info_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->final_artifact);
 return 1;
}

/* set function: final_artifact of class  dungeon_info_type */
static int toluaI_set_dungeon_dungeon_info_type_final_artifact(lua_State* tolua_S)
{
  dungeon_info_type* self = (dungeon_info_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->final_artifact = ((int)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: final_guardian of class  dungeon_info_type */
static int toluaI_get_dungeon_dungeon_info_type_final_guardian(lua_State* tolua_S)
{
  dungeon_info_type* self = (dungeon_info_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->final_guardian);
 return 1;
}

/* set function: final_guardian of class  dungeon_info_type */
static int toluaI_set_dungeon_dungeon_info_type_final_guardian(lua_State* tolua_S)
{
  dungeon_info_type* self = (dungeon_info_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->final_guardian = ((int)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: ix of class  dungeon_info_type */
static int toluaI_get_dungeon_dungeon_info_type_ix(lua_State* tolua_S)
{
  dungeon_info_type* self = (dungeon_info_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->ix);
 return 1;
}

/* set function: ix of class  dungeon_info_type */
static int toluaI_set_dungeon_dungeon_info_type_ix(lua_State* tolua_S)
{
  dungeon_info_type* self = (dungeon_info_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->ix = ((int)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: iy of class  dungeon_info_type */
static int toluaI_get_dungeon_dungeon_info_type_iy(lua_State* tolua_S)
{
  dungeon_info_type* self = (dungeon_info_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->iy);
 return 1;
}

/* set function: iy of class  dungeon_info_type */
static int toluaI_set_dungeon_dungeon_info_type_iy(lua_State* tolua_S)
{
  dungeon_info_type* self = (dungeon_info_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->iy = ((int)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: ox of class  dungeon_info_type */
static int toluaI_get_dungeon_dungeon_info_type_ox(lua_State* tolua_S)
{
  dungeon_info_type* self = (dungeon_info_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->ox);
 return 1;
}

/* set function: ox of class  dungeon_info_type */
static int toluaI_set_dungeon_dungeon_info_type_ox(lua_State* tolua_S)
{
  dungeon_info_type* self = (dungeon_info_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->ox = ((int)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: oy of class  dungeon_info_type */
static int toluaI_get_dungeon_dungeon_info_type_oy(lua_State* tolua_S)
{
  dungeon_info_type* self = (dungeon_info_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->oy);
 return 1;
}

/* set function: oy of class  dungeon_info_type */
static int toluaI_set_dungeon_dungeon_info_type_oy(lua_State* tolua_S)
{
  dungeon_info_type* self = (dungeon_info_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->oy = ((int)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: objs of class  dungeon_info_type */
static int toluaI_get_dungeon_dungeon_info_type_objs(lua_State* tolua_S)
{
  dungeon_info_type* self = (dungeon_info_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushusertype(tolua_S,(void*)&self->objs,tolua_tag(tolua_S,"obj_theme"));
 return 1;
}

/* set function: objs of class  dungeon_info_type */
static int toluaI_set_dungeon_dungeon_info_type_objs(lua_State* tolua_S)
{
  dungeon_info_type* self = (dungeon_info_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,tolua_tag(tolua_S,"obj_theme"),0))
 TOLUA_ERR_ASSIGN;
  self->objs = *((obj_theme*)  tolua_getusertype(tolua_S,2,0));
 return 0;
}

/* get function: d_dice of class  dungeon_info_type */
static int toluaI_get_dungeon_dungeon_info_type_d_dice(lua_State* tolua_S)
{
 int toluaI_index;
  dungeon_info_type* self;
 lua_pushstring(tolua_S,".self");
 lua_rawget(tolua_S,1);
 self = (dungeon_info_type*)  lua_touserdata(tolua_S,-1);
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 tolua_error(tolua_S,"invalid type in array indexing.");
 toluaI_index = (int)tolua_getnumber(tolua_S,2,0)-1;
 if (toluaI_index<0 || toluaI_index>=4)
 tolua_error(tolua_S,"array indexing out of range.");
 tolua_pushnumber(tolua_S,(long)self->d_dice[toluaI_index]);
 return 1;
}

/* set function: d_dice of class  dungeon_info_type */
static int toluaI_set_dungeon_dungeon_info_type_d_dice(lua_State* tolua_S)
{
 int toluaI_index;
  dungeon_info_type* self;
 lua_pushstring(tolua_S,".self");
 lua_rawget(tolua_S,1);
 self = (dungeon_info_type*)  lua_touserdata(tolua_S,-1);
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 tolua_error(tolua_S,"invalid type in array indexing.");
 toluaI_index = (int)tolua_getnumber(tolua_S,2,0)-1;
 if (toluaI_index<0 || toluaI_index>=4)
 tolua_error(tolua_S,"array indexing out of range.");
  self->d_dice[toluaI_index] = ((int)  tolua_getnumber(tolua_S,3,0));
 return 0;
}

/* get function: d_side of class  dungeon_info_type */
static int toluaI_get_dungeon_dungeon_info_type_d_side(lua_State* tolua_S)
{
 int toluaI_index;
  dungeon_info_type* self;
 lua_pushstring(tolua_S,".self");
 lua_rawget(tolua_S,1);
 self = (dungeon_info_type*)  lua_touserdata(tolua_S,-1);
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 tolua_error(tolua_S,"invalid type in array indexing.");
 toluaI_index = (int)tolua_getnumber(tolua_S,2,0)-1;
 if (toluaI_index<0 || toluaI_index>=4)
 tolua_error(tolua_S,"array indexing out of range.");
 tolua_pushnumber(tolua_S,(long)self->d_side[toluaI_index]);
 return 1;
}

/* set function: d_side of class  dungeon_info_type */
static int toluaI_set_dungeon_dungeon_info_type_d_side(lua_State* tolua_S)
{
 int toluaI_index;
  dungeon_info_type* self;
 lua_pushstring(tolua_S,".self");
 lua_rawget(tolua_S,1);
 self = (dungeon_info_type*)  lua_touserdata(tolua_S,-1);
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 tolua_error(tolua_S,"invalid type in array indexing.");
 toluaI_index = (int)tolua_getnumber(tolua_S,2,0)-1;
 if (toluaI_index<0 || toluaI_index>=4)
 tolua_error(tolua_S,"array indexing out of range.");
  self->d_side[toluaI_index] = ((int)  tolua_getnumber(tolua_S,3,0));
 return 0;
}

/* get function: d_frequency of class  dungeon_info_type */
static int toluaI_get_dungeon_dungeon_info_type_d_frequency(lua_State* tolua_S)
{
 int toluaI_index;
  dungeon_info_type* self;
 lua_pushstring(tolua_S,".self");
 lua_rawget(tolua_S,1);
 self = (dungeon_info_type*)  lua_touserdata(tolua_S,-1);
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 tolua_error(tolua_S,"invalid type in array indexing.");
 toluaI_index = (int)tolua_getnumber(tolua_S,2,0)-1;
 if (toluaI_index<0 || toluaI_index>=4)
 tolua_error(tolua_S,"array indexing out of range.");
 tolua_pushnumber(tolua_S,(long)self->d_frequency[toluaI_index]);
 return 1;
}

/* set function: d_frequency of class  dungeon_info_type */
static int toluaI_set_dungeon_dungeon_info_type_d_frequency(lua_State* tolua_S)
{
 int toluaI_index;
  dungeon_info_type* self;
 lua_pushstring(tolua_S,".self");
 lua_rawget(tolua_S,1);
 self = (dungeon_info_type*)  lua_touserdata(tolua_S,-1);
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 tolua_error(tolua_S,"invalid type in array indexing.");
 toluaI_index = (int)tolua_getnumber(tolua_S,2,0)-1;
 if (toluaI_index<0 || toluaI_index>=4)
 tolua_error(tolua_S,"array indexing out of range.");
  self->d_frequency[toluaI_index] = ((int)  tolua_getnumber(tolua_S,3,0));
 return 0;
}

/* get function: d_type of class  dungeon_info_type */
static int toluaI_get_dungeon_dungeon_info_type_d_type(lua_State* tolua_S)
{
 int toluaI_index;
  dungeon_info_type* self;
 lua_pushstring(tolua_S,".self");
 lua_rawget(tolua_S,1);
 self = (dungeon_info_type*)  lua_touserdata(tolua_S,-1);
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 tolua_error(tolua_S,"invalid type in array indexing.");
 toluaI_index = (int)tolua_getnumber(tolua_S,2,0)-1;
 if (toluaI_index<0 || toluaI_index>=4)
 tolua_error(tolua_S,"array indexing out of range.");
 tolua_pushnumber(tolua_S,(long)self->d_type[toluaI_index]);
 return 1;
}

/* set function: d_type of class  dungeon_info_type */
static int toluaI_set_dungeon_dungeon_info_type_d_type(lua_State* tolua_S)
{
 int toluaI_index;
  dungeon_info_type* self;
 lua_pushstring(tolua_S,".self");
 lua_rawget(tolua_S,1);
 self = (dungeon_info_type*)  lua_touserdata(tolua_S,-1);
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 tolua_error(tolua_S,"invalid type in array indexing.");
 toluaI_index = (int)tolua_getnumber(tolua_S,2,0)-1;
 if (toluaI_index<0 || toluaI_index>=4)
 tolua_error(tolua_S,"array indexing out of range.");
  self->d_type[toluaI_index] = ((int)  tolua_getnumber(tolua_S,3,0));
 return 0;
}

/* get function: t_idx of class  dungeon_info_type */
static int toluaI_get_dungeon_dungeon_info_type_t_idx(lua_State* tolua_S)
{
 int toluaI_index;
  dungeon_info_type* self;
 lua_pushstring(tolua_S,".self");
 lua_rawget(tolua_S,1);
 self = (dungeon_info_type*)  lua_touserdata(tolua_S,-1);
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 tolua_error(tolua_S,"invalid type in array indexing.");
 toluaI_index = (int)tolua_getnumber(tolua_S,2,0)-1;
 if (toluaI_index<0 || toluaI_index>=TOWN_DUNGEON)
 tolua_error(tolua_S,"array indexing out of range.");
 tolua_pushnumber(tolua_S,(long)self->t_idx[toluaI_index]);
 return 1;
}

/* set function: t_idx of class  dungeon_info_type */
static int toluaI_set_dungeon_dungeon_info_type_t_idx(lua_State* tolua_S)
{
 int toluaI_index;
  dungeon_info_type* self;
 lua_pushstring(tolua_S,".self");
 lua_rawget(tolua_S,1);
 self = (dungeon_info_type*)  lua_touserdata(tolua_S,-1);
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 tolua_error(tolua_S,"invalid type in array indexing.");
 toluaI_index = (int)tolua_getnumber(tolua_S,2,0)-1;
 if (toluaI_index<0 || toluaI_index>=TOWN_DUNGEON)
 tolua_error(tolua_S,"array indexing out of range.");
  self->t_idx[toluaI_index] = ((s16b)  tolua_getnumber(tolua_S,3,0));
 return 0;
}

/* get function: t_level of class  dungeon_info_type */
static int toluaI_get_dungeon_dungeon_info_type_t_level(lua_State* tolua_S)
{
 int toluaI_index;
  dungeon_info_type* self;
 lua_pushstring(tolua_S,".self");
 lua_rawget(tolua_S,1);
 self = (dungeon_info_type*)  lua_touserdata(tolua_S,-1);
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 tolua_error(tolua_S,"invalid type in array indexing.");
 toluaI_index = (int)tolua_getnumber(tolua_S,2,0)-1;
 if (toluaI_index<0 || toluaI_index>=TOWN_DUNGEON)
 tolua_error(tolua_S,"array indexing out of range.");
 tolua_pushnumber(tolua_S,(long)self->t_level[toluaI_index]);
 return 1;
}

/* set function: t_level of class  dungeon_info_type */
static int toluaI_set_dungeon_dungeon_info_type_t_level(lua_State* tolua_S)
{
 int toluaI_index;
  dungeon_info_type* self;
 lua_pushstring(tolua_S,".self");
 lua_rawget(tolua_S,1);
 self = (dungeon_info_type*)  lua_touserdata(tolua_S,-1);
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 tolua_error(tolua_S,"invalid type in array indexing.");
 toluaI_index = (int)tolua_getnumber(tolua_S,2,0)-1;
 if (toluaI_index<0 || toluaI_index>=TOWN_DUNGEON)
 tolua_error(tolua_S,"array indexing out of range.");
  self->t_level[toluaI_index] = ((s16b)  tolua_getnumber(tolua_S,3,0));
 return 0;
}

/* get function: t_num of class  dungeon_info_type */
static int toluaI_get_dungeon_dungeon_info_type_t_num(lua_State* tolua_S)
{
  dungeon_info_type* self = (dungeon_info_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 tolua_pushnumber(tolua_S,(long)self->t_num);
 return 1;
}

/* set function: t_num of class  dungeon_info_type */
static int toluaI_set_dungeon_dungeon_info_type_t_num(lua_State* tolua_S)
{
  dungeon_info_type* self = (dungeon_info_type*)  tolua_getusertype(tolua_S,1,0);
 if (!self) TOLUA_ERR_SELF;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  self->t_num = ((s16b)  tolua_getnumber(tolua_S,2,0));
 return 0;
}

/* get function: max_d_idx */
static int toluaI_get_dungeon_max_d_idx(lua_State* tolua_S)
{
 tolua_pushnumber(tolua_S,(long)max_d_idx);
 return 1;
}

/* set function: max_d_idx */
static int toluaI_set_dungeon_max_d_idx(lua_State* tolua_S)
{
 if (!tolua_istype(tolua_S,1,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  max_d_idx = ((u16b)  tolua_getnumber(tolua_S,1,0));
 return 0;
}

/* get function: d_info */
static int toluaI_get_dungeon_d_info(lua_State* tolua_S)
{
 int toluaI_index;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 tolua_error(tolua_S,"invalid type in array indexing.");
 toluaI_index = (int)tolua_getnumber(tolua_S,2,0)-1;
 if (toluaI_index<0 || toluaI_index>=max_d_idx)
 tolua_error(tolua_S,"array indexing out of range.");
 tolua_pushusertype(tolua_S,(void*)&d_info[toluaI_index],tolua_tag(tolua_S,"dungeon_info_type"));
 return 1;
}

/* set function: d_info */
static int toluaI_set_dungeon_d_info(lua_State* tolua_S)
{
 int toluaI_index;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 tolua_error(tolua_S,"invalid type in array indexing.");
 toluaI_index = (int)tolua_getnumber(tolua_S,2,0)-1;
 if (toluaI_index<0 || toluaI_index>=max_d_idx)
 tolua_error(tolua_S,"array indexing out of range.");
  d_info[toluaI_index] = *((dungeon_info_type*)  tolua_getusertype(tolua_S,3,0));
 return 0;
}

/* get function: d_name */
static int toluaI_get_dungeon_d_name(lua_State* tolua_S)
{
 tolua_pushstring(tolua_S,(const char*)d_name);
 return 1;
}

/* set function: d_name */
static int toluaI_set_dungeon_d_name(lua_State* tolua_S)
{
 if (!tolua_istype(tolua_S,1,LUA_TSTRING,0))
 TOLUA_ERR_ASSIGN;
  d_name = ((char*)  tolua_getstring(tolua_S,1,0));
 return 0;
}

/* get function: d_text */
static int toluaI_get_dungeon_d_text(lua_State* tolua_S)
{
 tolua_pushstring(tolua_S,(const char*)d_text);
 return 1;
}

/* set function: d_text */
static int toluaI_set_dungeon_d_text(lua_State* tolua_S)
{
 if (!tolua_istype(tolua_S,1,LUA_TSTRING,0))
 TOLUA_ERR_ASSIGN;
  d_text = ((char*)  tolua_getstring(tolua_S,1,0));
 return 0;
}

/* get function: max_wild_x */
static int toluaI_get_dungeon_max_wild_x(lua_State* tolua_S)
{
 tolua_pushnumber(tolua_S,(long)max_wild_x);
 return 1;
}

/* set function: max_wild_x */
static int toluaI_set_dungeon_max_wild_x(lua_State* tolua_S)
{
 if (!tolua_istype(tolua_S,1,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  max_wild_x = ((u16b)  tolua_getnumber(tolua_S,1,0));
 return 0;
}

/* get function: max_wild_y */
static int toluaI_get_dungeon_max_wild_y(lua_State* tolua_S)
{
 tolua_pushnumber(tolua_S,(long)max_wild_y);
 return 1;
}

/* set function: max_wild_y */
static int toluaI_set_dungeon_max_wild_y(lua_State* tolua_S)
{
 if (!tolua_istype(tolua_S,1,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  max_wild_y = ((u16b)  tolua_getnumber(tolua_S,1,0));
 return 0;
}

/* get function: max_wf_idx */
static int toluaI_get_dungeon_max_wf_idx(lua_State* tolua_S)
{
 tolua_pushnumber(tolua_S,(long)max_wf_idx);
 return 1;
}

/* set function: max_wf_idx */
static int toluaI_set_dungeon_max_wf_idx(lua_State* tolua_S)
{
 if (!tolua_istype(tolua_S,1,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  max_wf_idx = ((u16b)  tolua_getnumber(tolua_S,1,0));
 return 0;
}

/* get function: wf_info */
static int toluaI_get_dungeon_wf_info(lua_State* tolua_S)
{
 int toluaI_index;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 tolua_error(tolua_S,"invalid type in array indexing.");
 toluaI_index = (int)tolua_getnumber(tolua_S,2,0)-1;
 if (toluaI_index<0 || toluaI_index>=max_wf_idx)
 tolua_error(tolua_S,"array indexing out of range.");
 tolua_pushusertype(tolua_S,(void*)&wf_info[toluaI_index],tolua_tag(tolua_S,"wilderness_type_info"));
 return 1;
}

/* set function: wf_info */
static int toluaI_set_dungeon_wf_info(lua_State* tolua_S)
{
 int toluaI_index;
 if (!tolua_istype(tolua_S,2,LUA_TNUMBER,0))
 tolua_error(tolua_S,"invalid type in array indexing.");
 toluaI_index = (int)tolua_getnumber(tolua_S,2,0)-1;
 if (toluaI_index<0 || toluaI_index>=max_wf_idx)
 tolua_error(tolua_S,"array indexing out of range.");
  wf_info[toluaI_index] = *((wilderness_type_info*)  tolua_getusertype(tolua_S,3,0));
 return 0;
}

/* get function: wf_name */
static int toluaI_get_dungeon_wf_name(lua_State* tolua_S)
{
 tolua_pushstring(tolua_S,(const char*)wf_name);
 return 1;
}

/* set function: wf_name */
static int toluaI_set_dungeon_wf_name(lua_State* tolua_S)
{
 if (!tolua_istype(tolua_S,1,LUA_TSTRING,0))
 TOLUA_ERR_ASSIGN;
  wf_name = ((char*)  tolua_getstring(tolua_S,1,0));
 return 0;
}

/* get function: wf_text */
static int toluaI_get_dungeon_wf_text(lua_State* tolua_S)
{
 tolua_pushstring(tolua_S,(const char*)wf_text);
 return 1;
}

/* set function: wf_text */
static int toluaI_set_dungeon_wf_text(lua_State* tolua_S)
{
 if (!tolua_istype(tolua_S,1,LUA_TSTRING,0))
 TOLUA_ERR_ASSIGN;
  wf_text = ((char*)  tolua_getstring(tolua_S,1,0));
 return 0;
}

/* get function: DUNGEON_DEATH */
static int toluaI_get_dungeon_DUNGEON_DEATH(lua_State* tolua_S)
{
 tolua_pushnumber(tolua_S,(long)DUNGEON_DEATH);
 return 1;
}

/* set function: DUNGEON_DEATH */
static int toluaI_set_dungeon_DUNGEON_DEATH(lua_State* tolua_S)
{
 if (!tolua_istype(tolua_S,1,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  DUNGEON_DEATH = ((s32b)  tolua_getnumber(tolua_S,1,0));
 return 0;
}

/* get function: dungeon_type */
static int toluaI_get_dungeon_current_dungeon_idx(lua_State* tolua_S)
{
 tolua_pushnumber(tolua_S,(long)dungeon_type);
 return 1;
}

/* set function: dungeon_type */
static int toluaI_set_dungeon_current_dungeon_idx(lua_State* tolua_S)
{
 if (!tolua_istype(tolua_S,1,LUA_TNUMBER,0))
 TOLUA_ERR_ASSIGN;
  dungeon_type = ((byte)  tolua_getnumber(tolua_S,1,0));
 return 0;
}

/* function: lua_get_wild_map */
static int toluaI_dungeon_wild_map00(lua_State* tolua_S)
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
  wilderness_map* toluaI_ret = (wilderness_map*)  lua_get_wild_map(y,x);
 tolua_pushusertype(tolua_S,(void*)toluaI_ret,tolua_tag(tolua_S,"wilderness_map"));
 }
 }
 return 1;
tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'wild_map'.");
 return 0;
}

/* function: place_trap */
static int toluaI_dungeon_place_trap00(lua_State* tolua_S)
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
  place_trap(y,x);
 }
 }
 return 0;
tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'place_trap'.");
 return 0;
}

/* function: place_floor */
static int toluaI_dungeon_place_floor00(lua_State* tolua_S)
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
  place_floor(y,x);
 }
 }
 return 0;
tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'place_floor'.");
 return 0;
}

/* function: place_filler */
static int toluaI_dungeon_place_filler00(lua_State* tolua_S)
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
  place_filler(y,x);
 }
 }
 return 0;
tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'place_filler'.");
 return 0;
}

/* function: add_scripted_generator */
static int toluaI_dungeon_add_scripted_generator00(lua_State* tolua_S)
{
 if (
 !tolua_istype(tolua_S,1,LUA_TSTRING,0) ||
 !tolua_istype(tolua_S,2,LUA_TNUMBER,0) ||
 !tolua_istype(tolua_S,3,LUA_TNUMBER,0) ||
 !tolua_istype(tolua_S,4,LUA_TNUMBER,0) ||
 !tolua_istype(tolua_S,5,LUA_TNUMBER,0) ||
 !tolua_isnoobj(tolua_S,6)
 )
 goto tolua_lerror;
 else
 {
  cptr name = ((cptr)  tolua_getstring(tolua_S,1,0));
  bool stairs = ((bool)  tolua_getnumber(tolua_S,2,0));
  bool monsters = ((bool)  tolua_getnumber(tolua_S,3,0));
  bool objects = ((bool)  tolua_getnumber(tolua_S,4,0));
  bool miscs = ((bool)  tolua_getnumber(tolua_S,5,0));
 {
  add_scripted_generator(name,stairs,monsters,objects,miscs);
 }
 }
 return 0;
tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'add_scripted_generator'.");
 return 0;
}

/* function: new_player_spot */
static int toluaI_dungeon_new_player_spot00(lua_State* tolua_S)
{
 if (
 !tolua_istype(tolua_S,1,LUA_TNUMBER,0) ||
 !tolua_isnoobj(tolua_S,2)
 )
 goto tolua_lerror;
 else
 {
  int branch = ((int)  tolua_getnumber(tolua_S,1,0));
 {
  bool toluaI_ret = (bool)  new_player_spot(branch);
 tolua_pushnumber(tolua_S,(long)toluaI_ret);
 }
 }
 return 1;
tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'new_player_spot'.");
 return 0;
}

/* function: get_level_desc */
static int toluaI_dungeon_get_level_desc00(lua_State* tolua_S)
{
 if (
 !tolua_istype(tolua_S,1,LUA_TSTRING,0) ||
 !tolua_isnoobj(tolua_S,2)
 )
 goto tolua_lerror;
 else
 {
  char* buf = ((char*)  tolua_getstring(tolua_S,1,0));
 {
  bool toluaI_ret = (bool)  get_level_desc(buf);
 tolua_pushnumber(tolua_S,(long)toluaI_ret);
 }
 }
 return 1;
tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'get_level_desc'.");
 return 0;
}

/* function: get_level_flags */
static int toluaI_dungeon_get_level_flags00(lua_State* tolua_S)
{
 if (
 !tolua_isnoobj(tolua_S,1)
 )
 goto tolua_lerror;
 else
 {
 {
  get_level_flags();
 }
 }
 return 0;
tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'get_level_flags'.");
 return 0;
}

/* function: get_dungeon_name */
static int toluaI_dungeon_get_dungeon_name00(lua_State* tolua_S)
{
 if (
 !tolua_istype(tolua_S,1,LUA_TSTRING,0) ||
 !tolua_isnoobj(tolua_S,2)
 )
 goto tolua_lerror;
 else
 {
  char* buf = ((char*)  tolua_getstring(tolua_S,1,0));
 {
  bool toluaI_ret = (bool)  get_dungeon_name(buf);
 tolua_pushnumber(tolua_S,(long)toluaI_ret);
 }
 }
 return 1;
tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'get_dungeon_name'.");
 return 0;
}

/* function: get_dungeon_special */
static int toluaI_dungeon_get_dungeon_special00(lua_State* tolua_S)
{
 if (
 !tolua_istype(tolua_S,1,LUA_TSTRING,0) ||
 !tolua_isnoobj(tolua_S,2)
 )
 goto tolua_lerror;
 else
 {
  char* buf = ((char*)  tolua_getstring(tolua_S,1,0));
 {
  bool toluaI_ret = (bool)  get_dungeon_special(buf);
 tolua_pushnumber(tolua_S,(long)toluaI_ret);
 }
 }
 return 1;
tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'get_dungeon_special'.");
 return 0;
}

/* function: get_command */
static int toluaI_dungeon_get_command00(lua_State* tolua_S)
{
 if (
 !tolua_istype(tolua_S,1,LUA_TSTRING,0) ||
 !tolua_istype(tolua_S,2,LUA_TNUMBER,0) ||
 !tolua_istype(tolua_S,3,LUA_TSTRING,0) ||
 !tolua_isnoobj(tolua_S,4)
 )
 goto tolua_lerror;
 else
 {
  const char* file = ((const char*)  tolua_getstring(tolua_S,1,0));
  char comm = ((char)  tolua_getnumber(tolua_S,2,0));
  char* param = ((char*)  tolua_getstring(tolua_S,3,0));
 {
  bool toluaI_ret = (bool)  get_command(file,comm,param);
 tolua_pushnumber(tolua_S,(long)toluaI_ret);
 }
 }
 return 1;
tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'get_command'.");
 return 0;
}

/* function: get_branch */
static int toluaI_dungeon_get_branch00(lua_State* tolua_S)
{
 if (
 !tolua_isnoobj(tolua_S,1)
 )
 goto tolua_lerror;
 else
 {
 {
  int toluaI_ret = (int)  get_branch();
 tolua_pushnumber(tolua_S,(long)toluaI_ret);
 }
 }
 return 1;
tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'get_branch'.");
 return 0;
}

/* function: get_fbranch */
static int toluaI_dungeon_get_fbranch00(lua_State* tolua_S)
{
 if (
 !tolua_isnoobj(tolua_S,1)
 )
 goto tolua_lerror;
 else
 {
 {
  int toluaI_ret = (int)  get_fbranch();
 tolua_pushnumber(tolua_S,(long)toluaI_ret);
 }
 }
 return 1;
tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'get_fbranch'.");
 return 0;
}

/* function: get_flevel */
static int toluaI_dungeon_get_flevel00(lua_State* tolua_S)
{
 if (
 !tolua_isnoobj(tolua_S,1)
 )
 goto tolua_lerror;
 else
 {
 {
  int toluaI_ret = (int)  get_flevel();
 tolua_pushnumber(tolua_S,(long)toluaI_ret);
 }
 }
 return 1;
tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'get_flevel'.");
 return 0;
}

/* function: get_dungeon_save */
static int toluaI_dungeon_get_dungeon_save00(lua_State* tolua_S)
{
 if (
 !tolua_istype(tolua_S,1,LUA_TSTRING,0) ||
 !tolua_isnoobj(tolua_S,2)
 )
 goto tolua_lerror;
 else
 {
  char* buf = ((char*)  tolua_getstring(tolua_S,1,0));
 {
  bool toluaI_ret = (bool)  get_dungeon_save(buf);
 tolua_pushnumber(tolua_S,(long)toluaI_ret);
 }
 }
 return 1;
tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'get_dungeon_save'.");
 return 0;
}

/* Open function */
int tolua_dungeon_open (lua_State* tolua_S)
{
 tolua_open(tolua_S);
 toluaI_reg_types(tolua_S);
 tolua_constant(tolua_S,NULL,"CAVE_MARK",CAVE_MARK);
 tolua_constant(tolua_S,NULL,"CAVE_GLOW",CAVE_GLOW);
 tolua_constant(tolua_S,NULL,"CAVE_ICKY",CAVE_ICKY);
 tolua_constant(tolua_S,NULL,"CAVE_ROOM",CAVE_ROOM);
 tolua_constant(tolua_S,NULL,"CAVE_SEEN",CAVE_SEEN);
 tolua_constant(tolua_S,NULL,"CAVE_VIEW",CAVE_VIEW);
 tolua_constant(tolua_S,NULL,"CAVE_TEMP",CAVE_TEMP);
 tolua_constant(tolua_S,NULL,"CAVE_WALL",CAVE_WALL);
 tolua_constant(tolua_S,NULL,"CAVE_TRDT",CAVE_TRDT);
 tolua_constant(tolua_S,NULL,"CAVE_IDNT",CAVE_IDNT);
 tolua_constant(tolua_S,NULL,"CAVE_SPEC",CAVE_SPEC);
 tolua_constant(tolua_S,NULL,"CAVE_FREE",CAVE_FREE);
 tolua_constant(tolua_S,NULL,"CAVE_DETECT",CAVE_DETECT);
 tolua_constant(tolua_S,NULL,"CAVE_PLIT",CAVE_PLIT);
 tolua_constant(tolua_S,NULL,"CAVE_MLIT",CAVE_MLIT);
 tolua_constant(tolua_S,NULL,"FEAT_NONE",FEAT_NONE);
 tolua_constant(tolua_S,NULL,"FEAT_FLOOR",FEAT_FLOOR);
 tolua_constant(tolua_S,NULL,"FEAT_FOUNTAIN",FEAT_FOUNTAIN);
 tolua_constant(tolua_S,NULL,"FEAT_GLYPH",FEAT_GLYPH);
 tolua_constant(tolua_S,NULL,"FEAT_OPEN",FEAT_OPEN);
 tolua_constant(tolua_S,NULL,"FEAT_BROKEN",FEAT_BROKEN);
 tolua_constant(tolua_S,NULL,"FEAT_LESS",FEAT_LESS);
 tolua_constant(tolua_S,NULL,"FEAT_MORE",FEAT_MORE);
 tolua_constant(tolua_S,NULL,"FEAT_QUEST_ENTER",FEAT_QUEST_ENTER);
 tolua_constant(tolua_S,NULL,"FEAT_QUEST_EXIT",FEAT_QUEST_EXIT);
 tolua_constant(tolua_S,NULL,"FEAT_QUEST_DOWN",FEAT_QUEST_DOWN);
 tolua_constant(tolua_S,NULL,"FEAT_QUEST_UP",FEAT_QUEST_UP);
 tolua_constant(tolua_S,NULL,"FEAT_SHAFT_DOWN",FEAT_SHAFT_DOWN);
 tolua_constant(tolua_S,NULL,"FEAT_SHAFT_UP",FEAT_SHAFT_UP);
 tolua_constant(tolua_S,NULL,"FEAT_EMPTY_FOUNTAIN",FEAT_EMPTY_FOUNTAIN);
 tolua_constant(tolua_S,NULL,"FEAT_TRAP",FEAT_TRAP);
 tolua_constant(tolua_S,NULL,"FEAT_DOOR_HEAD",FEAT_DOOR_HEAD);
 tolua_constant(tolua_S,NULL,"FEAT_DOOR_TAIL",FEAT_DOOR_TAIL);
 tolua_constant(tolua_S,NULL,"FEAT_SECRET",FEAT_SECRET);
 tolua_constant(tolua_S,NULL,"FEAT_RUBBLE",FEAT_RUBBLE);
 tolua_constant(tolua_S,NULL,"FEAT_MAGMA",FEAT_MAGMA);
 tolua_constant(tolua_S,NULL,"FEAT_QUARTZ",FEAT_QUARTZ);
 tolua_constant(tolua_S,NULL,"FEAT_MAGMA_H",FEAT_MAGMA_H);
 tolua_constant(tolua_S,NULL,"FEAT_QUARTZ_H",FEAT_QUARTZ_H);
 tolua_constant(tolua_S,NULL,"FEAT_MAGMA_K",FEAT_MAGMA_K);
 tolua_constant(tolua_S,NULL,"FEAT_QUARTZ_K",FEAT_QUARTZ_K);
 tolua_constant(tolua_S,NULL,"FEAT_WALL_EXTRA",FEAT_WALL_EXTRA);
 tolua_constant(tolua_S,NULL,"FEAT_WALL_INNER",FEAT_WALL_INNER);
 tolua_constant(tolua_S,NULL,"FEAT_WALL_OUTER",FEAT_WALL_OUTER);
 tolua_constant(tolua_S,NULL,"FEAT_WALL_SOLID",FEAT_WALL_SOLID);
 tolua_constant(tolua_S,NULL,"FEAT_PERM_EXTRA",FEAT_PERM_EXTRA);
 tolua_constant(tolua_S,NULL,"FEAT_PERM_INNER",FEAT_PERM_INNER);
 tolua_constant(tolua_S,NULL,"FEAT_PERM_OUTER",FEAT_PERM_OUTER);
 tolua_constant(tolua_S,NULL,"FEAT_PERM_SOLID",FEAT_PERM_SOLID);
 tolua_constant(tolua_S,NULL,"FEAT_MINOR_GLYPH",FEAT_MINOR_GLYPH);
 tolua_constant(tolua_S,NULL,"FEAT_PATTERN_START",FEAT_PATTERN_START);
 tolua_constant(tolua_S,NULL,"FEAT_PATTERN_1",FEAT_PATTERN_1);
 tolua_constant(tolua_S,NULL,"FEAT_PATTERN_2",FEAT_PATTERN_2);
 tolua_constant(tolua_S,NULL,"FEAT_PATTERN_3",FEAT_PATTERN_3);
 tolua_constant(tolua_S,NULL,"FEAT_PATTERN_4",FEAT_PATTERN_4);
 tolua_constant(tolua_S,NULL,"FEAT_PATTERN_END",FEAT_PATTERN_END);
 tolua_constant(tolua_S,NULL,"FEAT_PATTERN_OLD",FEAT_PATTERN_OLD);
 tolua_constant(tolua_S,NULL,"FEAT_PATTERN_XTRA1",FEAT_PATTERN_XTRA1);
 tolua_constant(tolua_S,NULL,"FEAT_PATTERN_XTRA2",FEAT_PATTERN_XTRA2);
 tolua_constant(tolua_S,NULL,"FEAT_SHOP",FEAT_SHOP);
 tolua_constant(tolua_S,NULL,"FEAT_QUEST1",FEAT_QUEST1);
 tolua_constant(tolua_S,NULL,"FEAT_QUEST2",FEAT_QUEST2);
 tolua_constant(tolua_S,NULL,"FEAT_QUEST3",FEAT_QUEST3);
 tolua_constant(tolua_S,NULL,"FEAT_QUEST4",FEAT_QUEST4);
 tolua_constant(tolua_S,NULL,"FEAT_SHAL_WATER",FEAT_SHAL_WATER);
 tolua_constant(tolua_S,NULL,"FEAT_DEEP_LAVA",FEAT_DEEP_LAVA);
 tolua_constant(tolua_S,NULL,"FEAT_SHAL_LAVA",FEAT_SHAL_LAVA);
 tolua_constant(tolua_S,NULL,"FEAT_DARK_PIT",FEAT_DARK_PIT);
 tolua_constant(tolua_S,NULL,"FEAT_DIRT",FEAT_DIRT);
 tolua_constant(tolua_S,NULL,"FEAT_GRASS",FEAT_GRASS);
 tolua_constant(tolua_S,NULL,"FEAT_ICE",FEAT_ICE);
 tolua_constant(tolua_S,NULL,"FEAT_SAND",FEAT_SAND);
 tolua_constant(tolua_S,NULL,"FEAT_DEAD_TREE",FEAT_DEAD_TREE);
 tolua_constant(tolua_S,NULL,"FEAT_ASH",FEAT_ASH);
 tolua_constant(tolua_S,NULL,"FEAT_MUD",FEAT_MUD);
 tolua_constant(tolua_S,NULL,"FEAT_ICE_WALL",FEAT_ICE_WALL);
 tolua_constant(tolua_S,NULL,"FEAT_TREES",FEAT_TREES);
 tolua_constant(tolua_S,NULL,"FEAT_MOUNTAIN",FEAT_MOUNTAIN);
 tolua_constant(tolua_S,NULL,"FEAT_SANDWALL",FEAT_SANDWALL);
 tolua_constant(tolua_S,NULL,"FEAT_SANDWALL_H",FEAT_SANDWALL_H);
 tolua_constant(tolua_S,NULL,"FEAT_SANDWALL_K",FEAT_SANDWALL_K);
 tolua_constant(tolua_S,NULL,"FEAT_BETWEEN",FEAT_BETWEEN);
 tolua_constant(tolua_S,NULL,"FEAT_ALTAR_HEAD",FEAT_ALTAR_HEAD);
 tolua_constant(tolua_S,NULL,"FEAT_ALTAR_TAIL",FEAT_ALTAR_TAIL);
 tolua_constant(tolua_S,NULL,"FEAT_MARKER",FEAT_MARKER);
 tolua_constant(tolua_S,NULL,"FEAT_TAINTED_WATER",FEAT_TAINTED_WATER);
 tolua_constant(tolua_S,NULL,"FEAT_MON_TRAP",FEAT_MON_TRAP);
 tolua_constant(tolua_S,NULL,"FEAT_BETWEEN2",FEAT_BETWEEN2);
 tolua_constant(tolua_S,NULL,"FEAT_LAVA_WALL",FEAT_LAVA_WALL);
 tolua_constant(tolua_S,NULL,"FEAT_GREAT_FIRE",FEAT_GREAT_FIRE);
 tolua_constant(tolua_S,NULL,"FEAT_WAY_MORE",FEAT_WAY_MORE);
 tolua_constant(tolua_S,NULL,"FEAT_WAY_LESS",FEAT_WAY_LESS);
 tolua_constant(tolua_S,NULL,"FEAT_EKKAIA",FEAT_EKKAIA);
 tolua_constant(tolua_S,NULL,"FEAT_DEEP_WATER",FEAT_DEEP_WATER);
 tolua_constant(tolua_S,NULL,"FEAT_GLASS_WALL",FEAT_GLASS_WALL);
 tolua_constant(tolua_S,NULL,"FEAT_ILLUS_WALL",FEAT_ILLUS_WALL);
 tolua_constant(tolua_S,NULL,"FEAT_FLOWER",FEAT_FLOWER);
 tolua_constant(tolua_S,NULL,"FEAT_SMALL_TREES",FEAT_SMALL_TREES);
 tolua_constant(tolua_S,NULL,"FEAT_TOWN",FEAT_TOWN);
 tolua_constant(tolua_S,NULL,"FEAT_FIRE",FEAT_FIRE);
 tolua_constant(tolua_S,NULL,"DF1_PRINCIPAL",DF1_PRINCIPAL);
 tolua_constant(tolua_S,NULL,"DF1_MAZE",DF1_MAZE);
 tolua_constant(tolua_S,NULL,"DF1_SMALLEST",DF1_SMALLEST);
 tolua_constant(tolua_S,NULL,"DF1_SMALL",DF1_SMALL);
 tolua_constant(tolua_S,NULL,"DF1_BIG",DF1_BIG);
 tolua_constant(tolua_S,NULL,"DF1_NO_DOORS",DF1_NO_DOORS);
 tolua_constant(tolua_S,NULL,"DF1_WATER_RIVER",DF1_WATER_RIVER);
 tolua_constant(tolua_S,NULL,"DF1_LAVA_RIVER",DF1_LAVA_RIVER);
 tolua_constant(tolua_S,NULL,"DF1_WATER_RIVERS",DF1_WATER_RIVERS);
 tolua_constant(tolua_S,NULL,"DF1_LAVA_RIVERS",DF1_LAVA_RIVERS);
 tolua_constant(tolua_S,NULL,"DF1_CAVE",DF1_CAVE);
 tolua_constant(tolua_S,NULL,"DF1_CAVERN",DF1_CAVERN);
 tolua_constant(tolua_S,NULL,"DF1_NO_UP",DF1_NO_UP);
 tolua_constant(tolua_S,NULL,"DF1_HOT",DF1_HOT);
 tolua_constant(tolua_S,NULL,"DF1_COLD",DF1_COLD);
 tolua_constant(tolua_S,NULL,"DF1_FORCE_DOWN",DF1_FORCE_DOWN);
 tolua_constant(tolua_S,NULL,"DF1_FORGET",DF1_FORGET);
 tolua_constant(tolua_S,NULL,"DF1_NO_DESTROY",DF1_NO_DESTROY);
 tolua_constant(tolua_S,NULL,"DF1_SAND_VEIN",DF1_SAND_VEIN);
 tolua_constant(tolua_S,NULL,"DF1_CIRCULAR_ROOMS",DF1_CIRCULAR_ROOMS);
 tolua_constant(tolua_S,NULL,"DF1_EMPTY",DF1_EMPTY);
 tolua_constant(tolua_S,NULL,"DF1_DAMAGE_FEAT",DF1_DAMAGE_FEAT);
 tolua_constant(tolua_S,NULL,"DF1_FLAT",DF1_FLAT);
 tolua_constant(tolua_S,NULL,"DF1_TOWER",DF1_TOWER);
 tolua_constant(tolua_S,NULL,"DF1_RANDOM_TOWNS",DF1_RANDOM_TOWNS);
 tolua_constant(tolua_S,NULL,"DF1_DOUBLE",DF1_DOUBLE);
 tolua_constant(tolua_S,NULL,"DF1_LIFE_LEVEL",DF1_LIFE_LEVEL);
 tolua_constant(tolua_S,NULL,"DF1_EVOLVE",DF1_EVOLVE);
 tolua_constant(tolua_S,NULL,"DF1_ADJUST_LEVEL_1",DF1_ADJUST_LEVEL_1);
 tolua_constant(tolua_S,NULL,"DF1_ADJUST_LEVEL_2",DF1_ADJUST_LEVEL_2);
 tolua_constant(tolua_S,NULL,"DF1_NO_RECALL",DF1_NO_RECALL);
 tolua_constant(tolua_S,NULL,"DF1_NO_STREAMERS",DF1_NO_STREAMERS);
 tolua_constant(tolua_S,NULL,"DF2_ADJUST_LEVEL_1_2",DF2_ADJUST_LEVEL_1_2);
 tolua_constant(tolua_S,NULL,"DF2_NO_SHAFT",DF2_NO_SHAFT);
 tolua_constant(tolua_S,NULL,"DF2_ADJUST_LEVEL_PLAYER",DF2_ADJUST_LEVEL_PLAYER);
 tolua_constant(tolua_S,NULL,"DF2_NO_TELEPORT",DF2_NO_TELEPORT);
 tolua_constant(tolua_S,NULL,"DF2_ASK_LEAVE",DF2_ASK_LEAVE);
 tolua_constant(tolua_S,NULL,"DF2_NO_STAIR",DF2_NO_STAIR);
 tolua_constant(tolua_S,NULL,"DF2_SPECIAL",DF2_SPECIAL);
 tolua_constant(tolua_S,NULL,"DF2_NO_NEW_MONSTER",DF2_NO_NEW_MONSTER);
 tolua_constant(tolua_S,NULL,"DF2_DESC",DF2_DESC);
 tolua_constant(tolua_S,NULL,"DF2_NO_GENO",DF2_NO_GENO);
 tolua_constant(tolua_S,NULL,"DF2_NO_BREATH",DF2_NO_BREATH);
 tolua_constant(tolua_S,NULL,"DF2_WATER_BREATH",DF2_WATER_BREATH);
 tolua_constant(tolua_S,NULL,"DF2_ELVEN",DF2_ELVEN);
 tolua_constant(tolua_S,NULL,"DF2_DWARVEN",DF2_DWARVEN);
 tolua_constant(tolua_S,NULL,"DF2_NO_EASY_MOVE",DF2_NO_EASY_MOVE);
 tolua_constant(tolua_S,NULL,"DF2_NO_RECALL_OUT",DF2_NO_RECALL_OUT);
 tolua_constant(tolua_S,NULL,"DF2_DESC_ALWAYS",DF2_DESC_ALWAYS);
 tolua_globalvar(tolua_S,"level_flags1",toluaI_get_dungeon_level_flags1,toluaI_set_dungeon_level_flags1);
 tolua_globalvar(tolua_S,"level_flags2",toluaI_get_dungeon_level_flags2,toluaI_set_dungeon_level_flags2);
 tolua_constant(tolua_S,NULL,"MAX_HGT",MAX_HGT);
 tolua_constant(tolua_S,NULL,"MAX_WID",MAX_WID);
 tolua_constant(tolua_S,NULL,"TOWN_RANDOM",TOWN_RANDOM);
 tolua_constant(tolua_S,NULL,"TOWN_DUNGEON",TOWN_DUNGEON);
 tolua_constant(tolua_S,NULL,"TOWN_CHANCE",TOWN_CHANCE);
 tolua_constant(tolua_S,NULL,"TERRAIN_EDGE",TERRAIN_EDGE);
 tolua_constant(tolua_S,NULL,"TERRAIN_TOWN",TERRAIN_TOWN);
 tolua_constant(tolua_S,NULL,"TERRAIN_DEEP_WATER",TERRAIN_DEEP_WATER);
 tolua_constant(tolua_S,NULL,"TERRAIN_SHALLOW_WATER",TERRAIN_SHALLOW_WATER);
 tolua_constant(tolua_S,NULL,"TERRAIN_SWAMP",TERRAIN_SWAMP);
 tolua_constant(tolua_S,NULL,"TERRAIN_DIRT",TERRAIN_DIRT);
 tolua_constant(tolua_S,NULL,"TERRAIN_GRASS",TERRAIN_GRASS);
 tolua_constant(tolua_S,NULL,"TERRAIN_TREES",TERRAIN_TREES);
 tolua_constant(tolua_S,NULL,"TERRAIN_DESERT",TERRAIN_DESERT);
 tolua_constant(tolua_S,NULL,"TERRAIN_SHALLOW_LAVA",TERRAIN_SHALLOW_LAVA);
 tolua_constant(tolua_S,NULL,"TERRAIN_DEEP_LAVA",TERRAIN_DEEP_LAVA);
 tolua_constant(tolua_S,NULL,"TERRAIN_MOUNTAIN",TERRAIN_MOUNTAIN);
 tolua_constant(tolua_S,NULL,"MAX_WILD_TERRAIN",MAX_WILD_TERRAIN);
 tolua_cclass(tolua_S,"border_type","");
 tolua_tablearray(tolua_S,"border_type","north",toluaI_get_dungeon_border_type_north,toluaI_set_dungeon_border_type_north);
 tolua_tablearray(tolua_S,"border_type","south",toluaI_get_dungeon_border_type_south,toluaI_set_dungeon_border_type_south);
 tolua_tablearray(tolua_S,"border_type","east",toluaI_get_dungeon_border_type_east,toluaI_set_dungeon_border_type_east);
 tolua_tablearray(tolua_S,"border_type","west",toluaI_get_dungeon_border_type_west,toluaI_set_dungeon_border_type_west);
 tolua_tablevar(tolua_S,"border_type","north_west",toluaI_get_dungeon_border_type_north_west,toluaI_set_dungeon_border_type_north_west);
 tolua_tablevar(tolua_S,"border_type","north_east",toluaI_get_dungeon_border_type_north_east,toluaI_set_dungeon_border_type_north_east);
 tolua_tablevar(tolua_S,"border_type","south_west",toluaI_get_dungeon_border_type_south_west,toluaI_set_dungeon_border_type_south_west);
 tolua_tablevar(tolua_S,"border_type","south_east",toluaI_get_dungeon_border_type_south_east,toluaI_set_dungeon_border_type_south_east);
 tolua_cclass(tolua_S,"wilderness_type_info","");
 tolua_tablevar(tolua_S,"wilderness_type_info","name",toluaI_get_dungeon_wilderness_type_info_name,toluaI_set_dungeon_wilderness_type_info_name);
 tolua_tablevar(tolua_S,"wilderness_type_info","text",toluaI_get_dungeon_wilderness_type_info_text,toluaI_set_dungeon_wilderness_type_info_text);
 tolua_tablevar(tolua_S,"wilderness_type_info","entrance",toluaI_get_dungeon_wilderness_type_info_entrance,toluaI_set_dungeon_wilderness_type_info_entrance);
 tolua_tablevar(tolua_S,"wilderness_type_info","road",toluaI_get_dungeon_wilderness_type_info_road,toluaI_set_dungeon_wilderness_type_info_road);
 tolua_tablevar(tolua_S,"wilderness_type_info","level",toluaI_get_dungeon_wilderness_type_info_level,toluaI_set_dungeon_wilderness_type_info_level);
 tolua_tablevar(tolua_S,"wilderness_type_info","flags1",toluaI_get_dungeon_wilderness_type_info_flags1,toluaI_set_dungeon_wilderness_type_info_flags1);
 tolua_tablevar(tolua_S,"wilderness_type_info","feat",toluaI_get_dungeon_wilderness_type_info_feat,toluaI_set_dungeon_wilderness_type_info_feat);
 tolua_tablevar(tolua_S,"wilderness_type_info","terrain_idx",toluaI_get_dungeon_wilderness_type_info_terrain_idx,toluaI_set_dungeon_wilderness_type_info_terrain_idx);
 tolua_tablearray(tolua_S,"wilderness_type_info","terrain",toluaI_get_dungeon_wilderness_type_info_terrain,toluaI_set_dungeon_wilderness_type_info_terrain);
 tolua_cclass(tolua_S,"wilderness_map","");
 tolua_tablevar(tolua_S,"wilderness_map","feat",toluaI_get_dungeon_wilderness_map_feat,toluaI_set_dungeon_wilderness_map_feat);
 tolua_tablevar(tolua_S,"wilderness_map","seed",toluaI_get_dungeon_wilderness_map_seed,toluaI_set_dungeon_wilderness_map_seed);
 tolua_tablevar(tolua_S,"wilderness_map","entrance",toluaI_get_dungeon_wilderness_map_entrance,toluaI_set_dungeon_wilderness_map_entrance);
 tolua_tablevar(tolua_S,"wilderness_map","known",toluaI_get_dungeon_wilderness_map_known,toluaI_set_dungeon_wilderness_map_known);
 tolua_cclass(tolua_S,"town_type","");
 tolua_tablevar(tolua_S,"town_type","name",toluaI_get_dungeon_town_type_name,toluaI_set_dungeon_town_type_name);
 tolua_tablevar(tolua_S,"town_type","seed",toluaI_get_dungeon_town_type_seed,toluaI_set_dungeon_town_type_seed);
 tolua_tablevar(tolua_S,"town_type","store",toluaI_get_dungeon_town_type_store,toluaI_set_dungeon_town_type_store);
 tolua_tablevar(tolua_S,"town_type","numstores",toluaI_get_dungeon_town_type_numstores,toluaI_set_dungeon_town_type_numstores);
 tolua_tablevar(tolua_S,"town_type","flags",toluaI_get_dungeon_town_type_flags,toluaI_set_dungeon_town_type_flags);
 tolua_tablevar(tolua_S,"town_type","stocked",toluaI_get_dungeon_town_type_stocked,toluaI_set_dungeon_town_type_stocked);
 tolua_tablevar(tolua_S,"town_type","destroyed",toluaI_get_dungeon_town_type_destroyed,toluaI_set_dungeon_town_type_destroyed);
 tolua_globalvar(tolua_S,"max_towns",toluaI_get_dungeon_max_towns,toluaI_set_dungeon_max_towns);
 tolua_globalarray(tolua_S,"town_info",toluaI_get_dungeon_town_info,toluaI_set_dungeon_town_info);
 tolua_cclass(tolua_S,"rule_type","");
 tolua_tablevar(tolua_S,"rule_type","mode",toluaI_get_dungeon_rule_type_mode,toluaI_set_dungeon_rule_type_mode);
 tolua_tablevar(tolua_S,"rule_type","percent",toluaI_get_dungeon_rule_type_percent,toluaI_set_dungeon_rule_type_percent);
 tolua_tablevar(tolua_S,"rule_type","mflags1",toluaI_get_dungeon_rule_type_mflags1,toluaI_set_dungeon_rule_type_mflags1);
 tolua_tablevar(tolua_S,"rule_type","mflags2",toluaI_get_dungeon_rule_type_mflags2,toluaI_set_dungeon_rule_type_mflags2);
 tolua_tablevar(tolua_S,"rule_type","mflags3",toluaI_get_dungeon_rule_type_mflags3,toluaI_set_dungeon_rule_type_mflags3);
 tolua_tablevar(tolua_S,"rule_type","mflags4",toluaI_get_dungeon_rule_type_mflags4,toluaI_set_dungeon_rule_type_mflags4);
 tolua_tablevar(tolua_S,"rule_type","mflags5",toluaI_get_dungeon_rule_type_mflags5,toluaI_set_dungeon_rule_type_mflags5);
 tolua_tablevar(tolua_S,"rule_type","mflags6",toluaI_get_dungeon_rule_type_mflags6,toluaI_set_dungeon_rule_type_mflags6);
 tolua_tablevar(tolua_S,"rule_type","mflags7",toluaI_get_dungeon_rule_type_mflags7,toluaI_set_dungeon_rule_type_mflags7);
 tolua_tablevar(tolua_S,"rule_type","mflags8",toluaI_get_dungeon_rule_type_mflags8,toluaI_set_dungeon_rule_type_mflags8);
 tolua_tablevar(tolua_S,"rule_type","mflags9",toluaI_get_dungeon_rule_type_mflags9,toluaI_set_dungeon_rule_type_mflags9);
 tolua_tablearray(tolua_S,"rule_type","r_char",toluaI_get_dungeon_rule_type_r_char,toluaI_set_dungeon_rule_type_r_char);
 tolua_cclass(tolua_S,"obj_theme","");
 tolua_tablevar(tolua_S,"obj_theme","treasure",toluaI_get_dungeon_obj_theme_treasure,toluaI_set_dungeon_obj_theme_treasure);
 tolua_tablevar(tolua_S,"obj_theme","combat",toluaI_get_dungeon_obj_theme_combat,toluaI_set_dungeon_obj_theme_combat);
 tolua_tablevar(tolua_S,"obj_theme","magic",toluaI_get_dungeon_obj_theme_magic,toluaI_set_dungeon_obj_theme_magic);
 tolua_tablevar(tolua_S,"obj_theme","tools",toluaI_get_dungeon_obj_theme_tools,toluaI_set_dungeon_obj_theme_tools);
 tolua_cclass(tolua_S,"dungeon_info_type","");
 tolua_tablevar(tolua_S,"dungeon_info_type","name",toluaI_get_dungeon_dungeon_info_type_name,toluaI_set_dungeon_dungeon_info_type_name);
 tolua_tablevar(tolua_S,"dungeon_info_type","text",toluaI_get_dungeon_dungeon_info_type_text,toluaI_set_dungeon_dungeon_info_type_text);
 tolua_tablearray(tolua_S,"dungeon_info_type","short_name",toluaI_get_dungeon_dungeon_info_type_short_name,toluaI_set_dungeon_dungeon_info_type_short_name);
 tolua_tablevar(tolua_S,"dungeon_info_type","floor1",toluaI_get_dungeon_dungeon_info_type_floor1,toluaI_set_dungeon_dungeon_info_type_floor1);
 tolua_tablearray(tolua_S,"dungeon_info_type","floor_percent1",toluaI_get_dungeon_dungeon_info_type_floor_percent1,toluaI_set_dungeon_dungeon_info_type_floor_percent1);
 tolua_tablevar(tolua_S,"dungeon_info_type","floor2",toluaI_get_dungeon_dungeon_info_type_floor2,toluaI_set_dungeon_dungeon_info_type_floor2);
 tolua_tablearray(tolua_S,"dungeon_info_type","floor_percent2",toluaI_get_dungeon_dungeon_info_type_floor_percent2,toluaI_set_dungeon_dungeon_info_type_floor_percent2);
 tolua_tablevar(tolua_S,"dungeon_info_type","floor3",toluaI_get_dungeon_dungeon_info_type_floor3,toluaI_set_dungeon_dungeon_info_type_floor3);
 tolua_tablearray(tolua_S,"dungeon_info_type","floor_percent3",toluaI_get_dungeon_dungeon_info_type_floor_percent3,toluaI_set_dungeon_dungeon_info_type_floor_percent3);
 tolua_tablevar(tolua_S,"dungeon_info_type","outer_wall",toluaI_get_dungeon_dungeon_info_type_outer_wall,toluaI_set_dungeon_dungeon_info_type_outer_wall);
 tolua_tablevar(tolua_S,"dungeon_info_type","inner_wall",toluaI_get_dungeon_dungeon_info_type_inner_wall,toluaI_set_dungeon_dungeon_info_type_inner_wall);
 tolua_tablevar(tolua_S,"dungeon_info_type","fill_type1",toluaI_get_dungeon_dungeon_info_type_fill_type1,toluaI_set_dungeon_dungeon_info_type_fill_type1);
 tolua_tablearray(tolua_S,"dungeon_info_type","fill_percent1",toluaI_get_dungeon_dungeon_info_type_fill_percent1,toluaI_set_dungeon_dungeon_info_type_fill_percent1);
 tolua_tablevar(tolua_S,"dungeon_info_type","fill_type2",toluaI_get_dungeon_dungeon_info_type_fill_type2,toluaI_set_dungeon_dungeon_info_type_fill_type2);
 tolua_tablearray(tolua_S,"dungeon_info_type","fill_percent2",toluaI_get_dungeon_dungeon_info_type_fill_percent2,toluaI_set_dungeon_dungeon_info_type_fill_percent2);
 tolua_tablevar(tolua_S,"dungeon_info_type","fill_type3",toluaI_get_dungeon_dungeon_info_type_fill_type3,toluaI_set_dungeon_dungeon_info_type_fill_type3);
 tolua_tablearray(tolua_S,"dungeon_info_type","fill_percent3",toluaI_get_dungeon_dungeon_info_type_fill_percent3,toluaI_set_dungeon_dungeon_info_type_fill_percent3);
 tolua_tablevar(tolua_S,"dungeon_info_type","fill_method",toluaI_get_dungeon_dungeon_info_type_fill_method,toluaI_set_dungeon_dungeon_info_type_fill_method);
 tolua_tablevar(tolua_S,"dungeon_info_type","mindepth",toluaI_get_dungeon_dungeon_info_type_mindepth,toluaI_set_dungeon_dungeon_info_type_mindepth);
 tolua_tablevar(tolua_S,"dungeon_info_type","maxdepth",toluaI_get_dungeon_dungeon_info_type_maxdepth,toluaI_set_dungeon_dungeon_info_type_maxdepth);
 tolua_tablevar(tolua_S,"dungeon_info_type","principal",toluaI_get_dungeon_dungeon_info_type_principal,toluaI_set_dungeon_dungeon_info_type_principal);
 tolua_tablevar(tolua_S,"dungeon_info_type","next",toluaI_get_dungeon_dungeon_info_type_next,toluaI_set_dungeon_dungeon_info_type_next);
 tolua_tablevar(tolua_S,"dungeon_info_type","min_plev",toluaI_get_dungeon_dungeon_info_type_min_plev,toluaI_set_dungeon_dungeon_info_type_min_plev);
 tolua_tablevar(tolua_S,"dungeon_info_type","min_m_alloc_level",toluaI_get_dungeon_dungeon_info_type_min_m_alloc_level,toluaI_set_dungeon_dungeon_info_type_min_m_alloc_level);
 tolua_tablevar(tolua_S,"dungeon_info_type","max_m_alloc_chance",toluaI_get_dungeon_dungeon_info_type_max_m_alloc_chance,toluaI_set_dungeon_dungeon_info_type_max_m_alloc_chance);
 tolua_tablevar(tolua_S,"dungeon_info_type","flags1",toluaI_get_dungeon_dungeon_info_type_flags1,toluaI_set_dungeon_dungeon_info_type_flags1);
 tolua_tablevar(tolua_S,"dungeon_info_type","flags2",toluaI_get_dungeon_dungeon_info_type_flags2,toluaI_set_dungeon_dungeon_info_type_flags2);
 tolua_tablevar(tolua_S,"dungeon_info_type","size_x",toluaI_get_dungeon_dungeon_info_type_size_x,toluaI_set_dungeon_dungeon_info_type_size_x);
 tolua_tablevar(tolua_S,"dungeon_info_type","size_y",toluaI_get_dungeon_dungeon_info_type_size_y,toluaI_set_dungeon_dungeon_info_type_size_y);
 tolua_tablearray(tolua_S,"dungeon_info_type","rule_percents",toluaI_get_dungeon_dungeon_info_type_rule_percents,toluaI_set_dungeon_dungeon_info_type_rule_percents);
 tolua_tablearray(tolua_S,"dungeon_info_type","rules",toluaI_get_dungeon_dungeon_info_type_rules,toluaI_set_dungeon_dungeon_info_type_rules);
 tolua_tablevar(tolua_S,"dungeon_info_type","final_object",toluaI_get_dungeon_dungeon_info_type_final_object,toluaI_set_dungeon_dungeon_info_type_final_object);
 tolua_tablevar(tolua_S,"dungeon_info_type","final_artifact",toluaI_get_dungeon_dungeon_info_type_final_artifact,toluaI_set_dungeon_dungeon_info_type_final_artifact);
 tolua_tablevar(tolua_S,"dungeon_info_type","final_guardian",toluaI_get_dungeon_dungeon_info_type_final_guardian,toluaI_set_dungeon_dungeon_info_type_final_guardian);
 tolua_tablevar(tolua_S,"dungeon_info_type","ix",toluaI_get_dungeon_dungeon_info_type_ix,toluaI_set_dungeon_dungeon_info_type_ix);
 tolua_tablevar(tolua_S,"dungeon_info_type","iy",toluaI_get_dungeon_dungeon_info_type_iy,toluaI_set_dungeon_dungeon_info_type_iy);
 tolua_tablevar(tolua_S,"dungeon_info_type","ox",toluaI_get_dungeon_dungeon_info_type_ox,toluaI_set_dungeon_dungeon_info_type_ox);
 tolua_tablevar(tolua_S,"dungeon_info_type","oy",toluaI_get_dungeon_dungeon_info_type_oy,toluaI_set_dungeon_dungeon_info_type_oy);
 tolua_tablevar(tolua_S,"dungeon_info_type","objs",toluaI_get_dungeon_dungeon_info_type_objs,toluaI_set_dungeon_dungeon_info_type_objs);
 tolua_tablearray(tolua_S,"dungeon_info_type","d_dice",toluaI_get_dungeon_dungeon_info_type_d_dice,toluaI_set_dungeon_dungeon_info_type_d_dice);
 tolua_tablearray(tolua_S,"dungeon_info_type","d_side",toluaI_get_dungeon_dungeon_info_type_d_side,toluaI_set_dungeon_dungeon_info_type_d_side);
 tolua_tablearray(tolua_S,"dungeon_info_type","d_frequency",toluaI_get_dungeon_dungeon_info_type_d_frequency,toluaI_set_dungeon_dungeon_info_type_d_frequency);
 tolua_tablearray(tolua_S,"dungeon_info_type","d_type",toluaI_get_dungeon_dungeon_info_type_d_type,toluaI_set_dungeon_dungeon_info_type_d_type);
 tolua_tablearray(tolua_S,"dungeon_info_type","t_idx",toluaI_get_dungeon_dungeon_info_type_t_idx,toluaI_set_dungeon_dungeon_info_type_t_idx);
 tolua_tablearray(tolua_S,"dungeon_info_type","t_level",toluaI_get_dungeon_dungeon_info_type_t_level,toluaI_set_dungeon_dungeon_info_type_t_level);
 tolua_tablevar(tolua_S,"dungeon_info_type","t_num",toluaI_get_dungeon_dungeon_info_type_t_num,toluaI_set_dungeon_dungeon_info_type_t_num);
 tolua_globalvar(tolua_S,"max_d_idx",toluaI_get_dungeon_max_d_idx,toluaI_set_dungeon_max_d_idx);
 tolua_globalarray(tolua_S,"d_info",toluaI_get_dungeon_d_info,toluaI_set_dungeon_d_info);
 tolua_globalvar(tolua_S,"d_name",toluaI_get_dungeon_d_name,toluaI_set_dungeon_d_name);
 tolua_globalvar(tolua_S,"d_text",toluaI_get_dungeon_d_text,toluaI_set_dungeon_d_text);
 tolua_globalvar(tolua_S,"max_wild_x",toluaI_get_dungeon_max_wild_x,toluaI_set_dungeon_max_wild_x);
 tolua_globalvar(tolua_S,"max_wild_y",toluaI_get_dungeon_max_wild_y,toluaI_set_dungeon_max_wild_y);
 tolua_globalvar(tolua_S,"max_wf_idx",toluaI_get_dungeon_max_wf_idx,toluaI_set_dungeon_max_wf_idx);
 tolua_globalarray(tolua_S,"wf_info",toluaI_get_dungeon_wf_info,toluaI_set_dungeon_wf_info);
 tolua_globalvar(tolua_S,"wf_name",toluaI_get_dungeon_wf_name,toluaI_set_dungeon_wf_name);
 tolua_globalvar(tolua_S,"wf_text",toluaI_get_dungeon_wf_text,toluaI_set_dungeon_wf_text);
 tolua_globalvar(tolua_S,"DUNGEON_DEATH",toluaI_get_dungeon_DUNGEON_DEATH,toluaI_set_dungeon_DUNGEON_DEATH);
 tolua_globalvar(tolua_S,"current_dungeon_idx",toluaI_get_dungeon_current_dungeon_idx,toluaI_set_dungeon_current_dungeon_idx);
 tolua_function(tolua_S,NULL,"wild_map",toluaI_dungeon_wild_map00);
 tolua_function(tolua_S,NULL,"place_trap",toluaI_dungeon_place_trap00);
 tolua_function(tolua_S,NULL,"place_floor",toluaI_dungeon_place_floor00);
 tolua_function(tolua_S,NULL,"place_filler",toluaI_dungeon_place_filler00);
 tolua_function(tolua_S,NULL,"add_scripted_generator",toluaI_dungeon_add_scripted_generator00);
 tolua_function(tolua_S,NULL,"new_player_spot",toluaI_dungeon_new_player_spot00);
 tolua_function(tolua_S,NULL,"get_level_desc",toluaI_dungeon_get_level_desc00);
 tolua_function(tolua_S,NULL,"get_level_flags",toluaI_dungeon_get_level_flags00);
 tolua_function(tolua_S,NULL,"get_dungeon_name",toluaI_dungeon_get_dungeon_name00);
 tolua_function(tolua_S,NULL,"get_dungeon_special",toluaI_dungeon_get_dungeon_special00);
 tolua_function(tolua_S,NULL,"get_command",toluaI_dungeon_get_command00);
 tolua_function(tolua_S,NULL,"get_branch",toluaI_dungeon_get_branch00);
 tolua_function(tolua_S,NULL,"get_fbranch",toluaI_dungeon_get_fbranch00);
 tolua_function(tolua_S,NULL,"get_flevel",toluaI_dungeon_get_flevel00);
 tolua_function(tolua_S,NULL,"get_dungeon_save",toluaI_dungeon_get_dungeon_save00);
 return 1;
}
/* Close function */
void tolua_dungeon_close (lua_State* tolua_S)
{
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"CAVE_MARK");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"CAVE_GLOW");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"CAVE_ICKY");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"CAVE_ROOM");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"CAVE_SEEN");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"CAVE_VIEW");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"CAVE_TEMP");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"CAVE_WALL");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"CAVE_TRDT");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"CAVE_IDNT");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"CAVE_SPEC");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"CAVE_FREE");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"CAVE_DETECT");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"CAVE_PLIT");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"CAVE_MLIT");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"FEAT_NONE");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"FEAT_FLOOR");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"FEAT_FOUNTAIN");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"FEAT_GLYPH");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"FEAT_OPEN");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"FEAT_BROKEN");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"FEAT_LESS");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"FEAT_MORE");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"FEAT_QUEST_ENTER");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"FEAT_QUEST_EXIT");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"FEAT_QUEST_DOWN");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"FEAT_QUEST_UP");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"FEAT_SHAFT_DOWN");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"FEAT_SHAFT_UP");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"FEAT_EMPTY_FOUNTAIN");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"FEAT_TRAP");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"FEAT_DOOR_HEAD");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"FEAT_DOOR_TAIL");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"FEAT_SECRET");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"FEAT_RUBBLE");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"FEAT_MAGMA");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"FEAT_QUARTZ");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"FEAT_MAGMA_H");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"FEAT_QUARTZ_H");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"FEAT_MAGMA_K");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"FEAT_QUARTZ_K");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"FEAT_WALL_EXTRA");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"FEAT_WALL_INNER");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"FEAT_WALL_OUTER");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"FEAT_WALL_SOLID");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"FEAT_PERM_EXTRA");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"FEAT_PERM_INNER");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"FEAT_PERM_OUTER");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"FEAT_PERM_SOLID");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"FEAT_MINOR_GLYPH");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"FEAT_PATTERN_START");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"FEAT_PATTERN_1");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"FEAT_PATTERN_2");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"FEAT_PATTERN_3");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"FEAT_PATTERN_4");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"FEAT_PATTERN_END");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"FEAT_PATTERN_OLD");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"FEAT_PATTERN_XTRA1");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"FEAT_PATTERN_XTRA2");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"FEAT_SHOP");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"FEAT_QUEST1");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"FEAT_QUEST2");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"FEAT_QUEST3");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"FEAT_QUEST4");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"FEAT_SHAL_WATER");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"FEAT_DEEP_LAVA");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"FEAT_SHAL_LAVA");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"FEAT_DARK_PIT");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"FEAT_DIRT");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"FEAT_GRASS");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"FEAT_ICE");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"FEAT_SAND");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"FEAT_DEAD_TREE");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"FEAT_ASH");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"FEAT_MUD");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"FEAT_ICE_WALL");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"FEAT_TREES");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"FEAT_MOUNTAIN");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"FEAT_SANDWALL");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"FEAT_SANDWALL_H");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"FEAT_SANDWALL_K");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"FEAT_BETWEEN");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"FEAT_ALTAR_HEAD");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"FEAT_ALTAR_TAIL");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"FEAT_MARKER");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"FEAT_TAINTED_WATER");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"FEAT_MON_TRAP");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"FEAT_BETWEEN2");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"FEAT_LAVA_WALL");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"FEAT_GREAT_FIRE");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"FEAT_WAY_MORE");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"FEAT_WAY_LESS");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"FEAT_EKKAIA");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"FEAT_DEEP_WATER");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"FEAT_GLASS_WALL");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"FEAT_ILLUS_WALL");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"FEAT_FLOWER");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"FEAT_SMALL_TREES");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"FEAT_TOWN");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"FEAT_FIRE");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"DF1_PRINCIPAL");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"DF1_MAZE");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"DF1_SMALLEST");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"DF1_SMALL");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"DF1_BIG");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"DF1_NO_DOORS");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"DF1_WATER_RIVER");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"DF1_LAVA_RIVER");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"DF1_WATER_RIVERS");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"DF1_LAVA_RIVERS");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"DF1_CAVE");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"DF1_CAVERN");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"DF1_NO_UP");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"DF1_HOT");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"DF1_COLD");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"DF1_FORCE_DOWN");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"DF1_FORGET");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"DF1_NO_DESTROY");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"DF1_SAND_VEIN");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"DF1_CIRCULAR_ROOMS");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"DF1_EMPTY");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"DF1_DAMAGE_FEAT");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"DF1_FLAT");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"DF1_TOWER");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"DF1_RANDOM_TOWNS");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"DF1_DOUBLE");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"DF1_LIFE_LEVEL");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"DF1_EVOLVE");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"DF1_ADJUST_LEVEL_1");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"DF1_ADJUST_LEVEL_2");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"DF1_NO_RECALL");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"DF1_NO_STREAMERS");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"DF2_ADJUST_LEVEL_1_2");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"DF2_NO_SHAFT");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"DF2_ADJUST_LEVEL_PLAYER");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"DF2_NO_TELEPORT");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"DF2_ASK_LEAVE");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"DF2_NO_STAIR");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"DF2_SPECIAL");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"DF2_NO_NEW_MONSTER");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"DF2_DESC");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"DF2_NO_GENO");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"DF2_NO_BREATH");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"DF2_WATER_BREATH");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"DF2_ELVEN");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"DF2_DWARVEN");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"DF2_NO_EASY_MOVE");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"DF2_NO_RECALL_OUT");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"DF2_DESC_ALWAYS");
 lua_getglobals(tolua_S);
 lua_pushstring(tolua_S,"level_flags1"); lua_pushnil(tolua_S); lua_rawset(tolua_S,-3);
 lua_pop(tolua_S,1);
 lua_getglobals(tolua_S);
 lua_pushstring(tolua_S,"level_flags2"); lua_pushnil(tolua_S); lua_rawset(tolua_S,-3);
 lua_pop(tolua_S,1);
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"MAX_HGT");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"MAX_WID");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TOWN_RANDOM");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TOWN_DUNGEON");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TOWN_CHANCE");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TERRAIN_EDGE");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TERRAIN_TOWN");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TERRAIN_DEEP_WATER");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TERRAIN_SHALLOW_WATER");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TERRAIN_SWAMP");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TERRAIN_DIRT");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TERRAIN_GRASS");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TERRAIN_TREES");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TERRAIN_DESERT");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TERRAIN_SHALLOW_LAVA");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TERRAIN_DEEP_LAVA");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"TERRAIN_MOUNTAIN");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"MAX_WILD_TERRAIN");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"border_type");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"wilderness_type_info");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"wilderness_map");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"town_type");
 lua_getglobals(tolua_S);
 lua_pushstring(tolua_S,"max_towns"); lua_pushnil(tolua_S); lua_rawset(tolua_S,-3);
 lua_pop(tolua_S,1);
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"town_info");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"rule_type");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"obj_theme");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"dungeon_info_type");
 lua_getglobals(tolua_S);
 lua_pushstring(tolua_S,"max_d_idx"); lua_pushnil(tolua_S); lua_rawset(tolua_S,-3);
 lua_pop(tolua_S,1);
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"d_info");
 lua_getglobals(tolua_S);
 lua_pushstring(tolua_S,"d_name"); lua_pushnil(tolua_S); lua_rawset(tolua_S,-3);
 lua_pop(tolua_S,1);
 lua_getglobals(tolua_S);
 lua_pushstring(tolua_S,"d_text"); lua_pushnil(tolua_S); lua_rawset(tolua_S,-3);
 lua_pop(tolua_S,1);
 lua_getglobals(tolua_S);
 lua_pushstring(tolua_S,"max_wild_x"); lua_pushnil(tolua_S); lua_rawset(tolua_S,-3);
 lua_pop(tolua_S,1);
 lua_getglobals(tolua_S);
 lua_pushstring(tolua_S,"max_wild_y"); lua_pushnil(tolua_S); lua_rawset(tolua_S,-3);
 lua_pop(tolua_S,1);
 lua_getglobals(tolua_S);
 lua_pushstring(tolua_S,"max_wf_idx"); lua_pushnil(tolua_S); lua_rawset(tolua_S,-3);
 lua_pop(tolua_S,1);
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"wf_info");
 lua_getglobals(tolua_S);
 lua_pushstring(tolua_S,"wf_name"); lua_pushnil(tolua_S); lua_rawset(tolua_S,-3);
 lua_pop(tolua_S,1);
 lua_getglobals(tolua_S);
 lua_pushstring(tolua_S,"wf_text"); lua_pushnil(tolua_S); lua_rawset(tolua_S,-3);
 lua_pop(tolua_S,1);
 lua_getglobals(tolua_S);
 lua_pushstring(tolua_S,"DUNGEON_DEATH"); lua_pushnil(tolua_S); lua_rawset(tolua_S,-3);
 lua_pop(tolua_S,1);
 lua_getglobals(tolua_S);
 lua_pushstring(tolua_S,"current_dungeon_idx"); lua_pushnil(tolua_S); lua_rawset(tolua_S,-3);
 lua_pop(tolua_S,1);
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"wild_map");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"place_trap");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"place_floor");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"place_filler");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"add_scripted_generator");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"new_player_spot");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"get_level_desc");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"get_level_flags");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"get_dungeon_name");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"get_dungeon_special");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"get_command");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"get_branch");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"get_fbranch");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"get_flevel");
 lua_pushnil(tolua_S); lua_setglobal(tolua_S,"get_dungeon_save");
}
