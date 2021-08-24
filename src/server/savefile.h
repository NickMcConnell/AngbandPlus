/*
 * File: savefile.h
 * Purpose: Savefile loading and saving main routines
 */

#ifndef INCLUDED_SAVEFILE_H
#define INCLUDED_SAVEFILE_H

/*
 * Define this to replace indices with strings in savefiles.
 *
 * This method is more robust, but greatly increases the size of savefiles and therefore
 * could induce server lag when the server state is autosaving (every SERVER_SAVE minutes).
 */
/*#define SAVE_AS_STRINGS*/

#define FINISHED_CODE 255
#define ITEM_VERSION 1
#define EGO_ART_KNOWN 255

/* Writing bits */
extern void wr_byte(byte v);
extern void wr_u16b(u16b v);
extern void wr_s16b(s16b v);
extern void wr_u32b(u32b v);
extern void wr_s32b(s32b v);
extern void wr_hturn(hturn* pv);
extern void wr_loc(struct loc *l);
extern void wr_string(const char *str);

/* Reading bits */
extern void rd_byte(byte *ip);
extern void rd_bool(bool *ip);
extern void rd_u16b(u16b *ip);
extern void rd_s16b(s16b *ip);
extern void rd_u32b(u32b *ip);
extern void rd_s32b(s32b *ip);
extern void rd_hturn(hturn *ip);
extern void rd_loc(struct loc *l);
extern void rd_string(char *str, int max);
extern void strip_bytes(int n);
extern void strip_string(int max);

/* load.c */
extern int rd_monster_memory(struct player *p);
extern int rd_object_memory(struct player *p);
extern int rd_player(struct player *p);
extern int rd_ignore(struct player *p);
extern int rd_player_misc(struct player *p);
extern int rd_misc(struct player *unused);
extern int rd_player_artifacts(struct player *p);
extern int rd_artifacts(struct player *unused);
extern int rd_player_hp(struct player *p);
extern int rd_player_spells(struct player *p);
extern int rd_gear(struct player *p);
extern int rd_stores(struct player *unused);
extern int rd_player_dungeon(struct player *p);
extern int rd_level(struct player *unused);
extern int rd_dungeon(struct player *unused);
extern int rd_player_objects(struct player *p);
extern int rd_objects(struct player *unused);
extern int rd_monsters(struct player *unused);
extern int rd_player_traps(struct player *p);
extern int rd_traps(struct player *unused);
extern int rd_history(struct player *p);
extern int rd_null(struct player *unused);
extern int rd_header(struct player *p);
extern int rd_wild_map(struct player *p);
extern int rd_home(struct player *p);
extern int rd_parties(struct player *unused);
extern int rd_houses(struct player *unused);
extern int rd_arenas(struct player *unused);
extern int rd_wilderness(struct player *unused);
extern int rd_player_names(struct player *unused);

/* save.c */
extern void wr_description(void *data);
extern void wr_monster_memory(void *data);
extern void wr_object_memory(void *data);
extern void wr_player_artifacts(void *data);
extern void wr_artifacts(void *unused);
extern void wr_player(void *data);
extern void wr_ignore(void *data);
extern void wr_player_misc(void *data);
extern void wr_misc(void *unused);
extern void wr_player_hp(void *data);
extern void wr_player_spells(void *data);
extern void wr_gear(void *data);
extern void wr_stores(void *unused);
extern void wr_player_dungeon(void *data);
extern void wr_level(void *data);
extern void wr_dungeon(void *unused);
extern void wr_player_objects(void *data);
extern void wr_objects(void *unused);
extern void wr_monsters(void *unused);
extern void wr_player_traps(void *data);
extern void wr_traps(void *unused);
extern void wr_history(void *data);
extern void wr_header(void *data);
extern void wr_wild_map(void *data);
extern void wr_home(void *data);
extern void wr_parties(void *unused);
extern void wr_houses(void *unused);
extern void wr_arenas(void *unused);
extern void wr_wilderness(void *unused);
extern void wr_player_names(void *unused);

/*
 * Try to get a description for this savefile.
 */
extern const char *savefile_get_description(const char *path);

extern bool save_player(struct player *p);
extern void save_dungeon_special(struct worldpos *wpos, bool town);
extern bool save_server_info(void);
extern bool load_player(struct player *p);
extern int scoop_player(char *nick, char *pass, byte *pridx, byte *pcidx, byte *psex);
extern bool load_server_info(void);
extern bool special_level(struct worldpos *wpos);
extern bool special_town(struct worldpos *wpos);
extern bool forbid_special(struct worldpos *wpos);
extern bool forbid_town(struct worldpos *wpos);
extern bool random_level(struct worldpos *wpos);
extern bool dynamic_town(struct worldpos *wpos);

#endif /* INCLUDED_SAVEFILE_H */
